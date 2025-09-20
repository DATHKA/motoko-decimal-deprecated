import Nat "mo:core/Nat";
import Int "mo:core/Int";
import Text "mo:core/Text";
import Iter "mo:core/Iter";
import Array "mo:core/Array";
import Float "mo:core/Float";
import Order "mo:base/Order";

// ------------------------------------------------------------
// Decimal.mo — Fixed-point decimal arithmetic for Motoko
// ------------------------------------------------------------
// Highlights
// - Explicit Decimal type: { value : Int; decimals : Nat }
// - Clear rounding modes: DecimalRoundMode = { #down; #up; #halfUp }
//   * #down  => toward zero
//   * #up    => away from zero (if any fraction)
//   * #halfUp => to nearest, ties away from zero
// - Result-based errors for operations that can fail
// - Safer parsing/formatting (negatives supported, optional rounding)
// - Utility functions: abs, neg, signum, isZero, cmp, min, max, clamp,
//   quantize, normalize, floorTo / ceilTo / truncTo, power (integer),
//   toInt, toNat, toFloat, fromFloat, format with separators
// ------------------------------------------------------------

module {
  /// Fixed-point decimal number represented by an integer magnitude and a decimal scale.
  public type Decimal = {
    /// Underlying integer magnitude. The actual value is `value * 10^{-decimals}`.
    value : Int;
    /// Number of decimal places encoded in `value`.
    decimals : Nat;
  };

  /// Supported rounding strategies used throughout the module.
  public type DecimalRoundMode = { #down; #up; #halfUp };

  /// Convenience alias mirroring Motoko's common `Result` shape.
  public type Result<T, E> = { #ok : T; #err : E };

  /// Errors that can be produced by parsing and arithmetic helpers.
  public type DecimalError = {
    /// Division attempted with a zero denominator.
    #DivideByZero;
    /// Input text is malformed or cannot be represented with the requested scale.
    #InvalidFormat;
    /// Requested scale has more fractional digits than supported by the value.
    #TooManyFractionDigits;
    /// Conversion to `Nat` would yield a negative number.
    #NegativeValue;
    /// Floating-point input is NaN or Infinity.
    #InvalidFloat;
    /// Tried to compute `0` to a negative power.
    #ZeroToNegativePower;
  };

  /// Default number of fractional digits to keep when callers don't request a specific scale.
  let defaultExtraPrecision : Nat = 12;

  /// Creates a zero `Decimal` with the provided scale.
  public func zero(decimals : Nat) : Decimal = { value = 0; decimals };

  /// Creates a `Decimal` from an `Int`, preserving the provided scale.
  public func ofInt(n : Int, decimals : Nat) : Decimal = { value = n; decimals };

  /// Creates a `Decimal` from a `Nat`, preserving the provided scale.
  public func ofNat(n : Nat, decimals : Nat) : Decimal = { value = Int.fromNat(n); decimals };

  // ------------ Core helpers ------------
  /// Computes `10^k` for non-negative `k`.
  func pow10(k : Nat) : Nat = 10 ** k;

  /// Determines whether a float is either positive or negative infinity.
  func isInfinity(x : Float) : Bool {
    let posInfinity : Float = 1.0 / 0.0;
    Float.abs(x) == posInfinity
  };

  /// Splits `text` into a prefix and suffix at `index`.
  func splitAt(text : Text, index : Nat) : (Text, Text) {
    (
      text.chars() |> Iter.take(_, index) |> Text.fromIter(_),
      text.chars() |> Iter.drop(_, index) |> Text.fromIter(_)
    );
  };

  /// Returns a substring of `text` starting at `startIndex` with length `length`.
  func slice(text : Text, startIndex : Nat, length : Nat) : Text {
    text.chars() |> Iter.drop(_, startIndex) |> Iter.take(_, length) |> Text.fromIter(_)
  };

  /// Inserts thousands separators into an unsigned integer string.
  func insertThousands(intPart : Text, sep : Text) : Text {
    let n = intPart.size();
    if (n <= 3) return intPart;

    var i : Nat = n;
    var out : Text = "";

    // Build from the right in chunks of 3
    while (i > 3) {
      let start = Nat.sub(i, 3);
      let chunk = slice(intPart, start, 3);
      out := sep # chunk # out;
      i -= 3;
    };
    slice(intPart, 0, i) # out
  };

  // ------------ Formatting & Parsing ------------
  /// Renders a decimal using canonical formatting (dot separator, optional sign).
  public func toText(d : Decimal) : Text {
    let sign = if (d.value < 0) "-" else "";
    let mag : Nat = Int.abs(d.value);
    let s = Nat.toText(mag);

    if (d.decimals == 0) return sign # s;

    if (s.size() <= d.decimals) {
      let zerosNeeded = Nat.sub(d.decimals, s.size());
      let zeros = Text.fromArray(Array.repeat('0', zerosNeeded));
      return sign # "0." # zeros # s;
    } else {
      let splitIx = Nat.sub(s.size(), d.decimals);
      let (ip, fp) = splitAt(s, splitIx);
      return sign # ip # "." # fp;
    };
  };

  /// Pretty-formats a decimal with optional custom thousands and decimal separators.
  public func format(d : Decimal, opts : { thousandsSep : ?Text; decimalSep : ?Text }) : Text {
    let thousands = switch (opts.thousandsSep) { case (null) { "," }; case (?t) { t } };
    let decSep = switch (opts.decimalSep) { case (null) { "." }; case (?t) { t } };

    let canonical = toText(d); // includes sign
    let (sign, body) = if (Text.startsWith(canonical, #text "-")) {
      ("-", slice(canonical, 1, canonical.size() - 1))
    } else { ("", canonical) };

    let parts = Iter.toArray(Text.split(body, #char '.'));
    if (parts.size() == 1) {
      return sign # insertThousands(parts[0], thousands);
    } else {
      let ip = insertThousands(parts[0], thousands);
      let fp = parts[1];
      return sign # ip # decSep # fp;
    };
  };

  /// Parses textual input into a decimal with the requested scale and rounding mode.
  /// Supports optional leading `-` and fractional part. Excess fractional digits are
  /// rounded according to `mode`.
  public func ofText(txt : Text, decimals : Nat, mode : DecimalRoundMode)
    : Result<Decimal, DecimalError> {
    if (txt.size() == 0) return #err(#InvalidFormat);

    let isNeg = Text.startsWith(txt, #text "-");
    let body = if (isNeg) slice(txt, 1, txt.size() - 1) else txt;

    let parts = Iter.toArray(Text.split(body, #char '.'));
    if (parts.size() > 2) return #err(#InvalidFormat);

    let intPart = parts[0];
    let fracSrc = if (parts.size() == 2) parts[1] else "";

    // intPart must be digits (allow empty -> treated as 0)
    let intDigits = if (intPart == "") "0" else intPart;

    // Decide on fractional part to use, possibly with rounding
    if (fracSrc.size() <= decimals) {
      let pad = Text.fromArray(Array.repeat('0', Nat.sub(decimals, fracSrc.size())));
      let magTxt = intDigits # fracSrc # pad;
      switch (Nat.fromText(magTxt)) {
        case (null) { #err(#InvalidFormat) };
        case (?m) {
          let signed = if (isNeg) Int.neg(Int.fromNat(m)) else Int.fromNat(m);
          #ok({ value = signed; decimals })
        };
      };
    } else {
      // Need to round according to mode based on first dropped digit
      let keep = slice(fracSrc, 0, decimals);
      let dropped = slice(fracSrc, decimals, fracSrc.size() - decimals);

      // Any non-zero in dropped?
      func anyNonZero(t : Text) : Bool {
        for (c in t.chars()) { if (c != '0') return true };
        false
      };

      let bump : Int = switch (mode) {
        case (#down) { 0 };
        case (#up) { if (anyNonZero(dropped)) 1 else 0 };
        case (#halfUp) {
          // look at first dropped digit
          let first = dropped.chars().next();
          switch (first) {
            case (null) { 0 };
            case (?c) {
              if ((c >= '5') and (c <= '9')) 1 else 0
            }
          }
        }
      };

      let magTxt = intDigits # keep;
      switch (Nat.fromText(magTxt)) {
        case (null) { #err(#InvalidFormat) };
        case (?m0) {
          let base = Int.fromNat(m0);
          let signedBase = if (isNeg) -base else base;
          let signedBump = if (bump == 0) 0 else if (isNeg) -bump else bump;
          #ok({ value = signedBase + signedBump; decimals })
        }
      }
    }
  };

  // ------------ Scaling & Rounding ------------
  /// Rescales a decimal to `targetDecimals`, applying the supplied rounding mode.
  public func quantize(x : Decimal, targetDecimals : Nat, rnd : DecimalRoundMode) : Decimal {
    if (x.decimals == targetDecimals) return x;

    if (x.decimals < targetDecimals) {
      let factorNat = pow10(targetDecimals - x.decimals);
      let factor = Int.fromNat(factorNat);
      { value = x.value * factor; decimals = targetDecimals }
    } else {
      let factorNat = pow10(x.decimals - targetDecimals);
      let factor = Int.fromNat(factorNat);
      let q = x.value / factor;
      let r = x.value % factor;
      if (r == 0) return { value = q; decimals = targetDecimals };

      let direction : Int = if (x.value >= 0) 1 else -1;
      let hasFraction = r != 0;
      let bump = switch (rnd) {
        case (#down) { 0 };
        case (#up) { if (hasFraction) direction else 0 };
        case (#halfUp) {
          let remainderMagnitude = Int.fromNat(Int.abs(r));
          let factorMagnitude = Int.fromNat(Int.abs(factor));
          if (remainderMagnitude * 2 >= factorMagnitude) direction else 0
        }
      };
      { value = q + bump; decimals = targetDecimals }
    }
  };

  /// Truncates toward zero when increasing scale.
  public func truncTo(x : Decimal, targetDecimals : Nat) : Decimal = quantize(x, targetDecimals, #down);

  /// Floors toward negative infinity when reducing scale.
  public func floorTo(x : Decimal, targetDecimals : Nat) : Decimal {
    // floor differs from trunc for negatives
    if (x.decimals <= targetDecimals) return quantize(x, targetDecimals, #down);

    let factorNat = pow10(x.decimals - targetDecimals);
    let factor = Int.fromNat(factorNat);
    let q = x.value / factor;
    let r = x.value % factor;

    if (r == 0) return { value = q; decimals = targetDecimals };
    if (x.value >= 0) { { value = q; decimals = targetDecimals } }
    else { { value = q - 1; decimals = targetDecimals } }
  };

  /// Ceils toward positive infinity when reducing scale.
  public func ceilTo(x : Decimal, targetDecimals : Nat) : Decimal {
    if (x.decimals <= targetDecimals) return quantize(x, targetDecimals, #down);

    let factorNat = pow10(x.decimals - targetDecimals);
    let factor = Int.fromNat(factorNat);
    let q = x.value / factor;
    let r = x.value % factor;

    if (r == 0) return { value = q; decimals = targetDecimals };
    if (x.value >= 0) { { value = q + 1; decimals = targetDecimals } }
    else { { value = q; decimals = targetDecimals } }
  };

  // ------------ Conversions ------------
  /// Converts to an `Int` by rescaling to zero decimals using `mode`.
  public func toInt(d : Decimal, mode : DecimalRoundMode) : Int = quantize(d, 0, mode).value;

  /// Attempts to convert to `Nat`, failing when the rounded integer would be negative.
  public func toNat(d : Decimal, mode : DecimalRoundMode) : Result<Nat, DecimalError> {
    let i = toInt(d, mode);
    if (i < 0) #err(#NegativeValue) else #ok(Int.abs(i))
  };

  /// Converts the decimal to a `Float` by dividing the scaled integer magnitude.
  public func toFloat(d : Decimal) : Float {
    let denomI : Int = Int.fromNat(pow10(d.decimals));
    Float.fromInt(d.value) / Float.fromInt(denomI)
  };

  /// Converts a float into a decimal, rounding according to `mode`.
  public func fromFloat(f : Float, decimals : Nat, mode : DecimalRoundMode) : Result<Decimal, DecimalError> {
    if (Float.isNaN(f) or isInfinity(f)) return #err(#InvalidFloat);
    let scaleI : Int = Int.fromNat(pow10(decimals));
    let scaled : Float = f * Float.fromInt(scaleI);

    let rounded : Int = switch (mode) {
      case (#down) { Float.toInt(scaled) }; // toward 0
      case (#up) {
        let t = Float.toInt(scaled);
        if (Float.fromInt(t) == scaled) t
        else t + (if (scaled >= 0) 1 else -1)
      };
      case (#halfUp) {
        let t = Float.toInt(scaled);
        let frac = Float.abs(scaled - Float.fromInt(t));
        if (frac >= 0.5) t + (if (scaled >= 0) 1 else -1) else t
      }
    };

    #ok({ value = rounded; decimals })
  };

  // ------------ Arithmetic ------------
  /// Adds two decimals, aligning scales optionally to `decimals`.
  public func add(a : Decimal, b : Decimal, decimals : ?Nat) : Decimal {
    let dec = switch (decimals) { case (null) { Nat.max(a.decimals, b.decimals) }; case (?v) { v } };
    let aa = quantize(a, dec, #halfUp);
    let bb = quantize(b, dec, #halfUp);
    { value = aa.value + bb.value; decimals = dec }
  };

  /// Subtracts `b` from `a`, aligning scales optionally to `decimals`.
  public func subtract(a : Decimal, b : Decimal, decimals : ?Nat) : Decimal {
    let dec = switch (decimals) { case (null) { Nat.max(a.decimals, b.decimals) }; case (?v) { v } };
    let aa = quantize(a, dec, #halfUp);
    let bb = quantize(b, dec, #halfUp);
    { value = aa.value - bb.value; decimals = dec }
  };

  /// Multiplies two decimals. If `decimals` is `null`, the raw scale (`a.decimals + b.decimals`) is kept.
  /// Otherwise the product is quantized to the requested number of fractional digits using `rnd`.
  public func multiply(a : Decimal, b : Decimal, decimals : ?Nat, rnd : DecimalRoundMode) : Decimal {
    let raw = { value = a.value * b.value; decimals = a.decimals + b.decimals };
    switch (decimals) {
      case (null) { raw };
      case (?target) { quantize(raw, target, rnd) };
    }
  };

  /// Divides `a` by `b`, producing a decimal with the requested scale (or a default when `decimals` is `null`).
  public func divide(a : Decimal, b : Decimal, decimals : ?Nat, rnd : DecimalRoundMode)
    : Result<Decimal, DecimalError> {
    if (b.value == 0) return #err(#DivideByZero);

    let targetDecimals = switch (decimals) {
      case (null) { Nat.max(a.decimals, b.decimals) + defaultExtraPrecision };
      case (?value) { value };
    };

    let resultIsNegative = (a.value < 0 and b.value > 0) or (a.value > 0 and b.value < 0);
    let direction : Int = if (resultIsNegative) -1 else 1;

    let aAbs : Nat = Int.abs(a.value);
    let bAbs : Nat = Int.abs(b.value);

    let exponent = Int.fromNat(targetDecimals + b.decimals) - Int.fromNat(a.decimals);

    let (numeratorAbs : Nat, denominatorAbs : Nat) =
      if (exponent >= 0) {
        let m = pow10(Int.abs(exponent));
        (aAbs * m, bAbs)
      } else {
        let m = pow10(Int.abs(exponent));
        (aAbs, bAbs * m)
      };

    if (denominatorAbs == 0) return #err(#DivideByZero);

    let qAbs = numeratorAbs / denominatorAbs;
    let rAbs = numeratorAbs % denominatorAbs;
    let hasRemainder = rAbs != 0;

    let bumpMagnitude : Nat = switch (rnd) {
      case (#down) { 0 };
      case (#up) { if (hasRemainder) 1 else 0 };
      case (#halfUp) {
        if (not hasRemainder) 0 else if (rAbs * 2 >= denominatorAbs) 1 else 0
      }
    };

    let baseValue = Int.fromNat(qAbs) * direction;
    let bumpValue = if (bumpMagnitude == 0) 0 else Int.fromNat(bumpMagnitude) * direction;

    #ok({ value = baseValue + bumpValue; decimals = targetDecimals })
  };

  /// Raises a decimal to an integer power using fast exponentiation.
  /// When `decimals` is `null`, positive exponents return the natural scale and negative ones fall back to the
  /// division default; otherwise the result is quantized to `decimals` using `rnd`.
  public func power(x : Decimal, n : Int, decimals : ?Nat, rnd : DecimalRoundMode)
    : Result<Decimal, DecimalError> {
    func finalize(d : Decimal) : Result<Decimal, DecimalError> {
      switch (decimals) {
        case (null) { #ok(d) };
        case (?target) { #ok(quantize(d, target, rnd)) };
      }
    };

    if (n == 0) return finalize(ofInt(1, 0));

    if (n < 0) {
      if (x.value == 0) return #err(#ZeroToNegativePower);
      switch (power(x, -n, null, rnd)) {
        case (#err e) { #err(e) };
        case (#ok p) { divide(ofInt(1, 0), p, decimals, rnd) }
      }
    } else {
      // fast exponentiation on integer magnitude while tracking scale
      var baseVal : Int = x.value;
      var baseDec : Nat = x.decimals;
      var resVal : Int = 1;
      var resDec : Nat = 0;
      var e : Nat = Int.abs(n);

      while (e > 0) {
        if (e % 2 == 1) {
          resVal := resVal * baseVal;
          resDec += baseDec;
        };
        e /= 2;
        if (e > 0) {
          baseVal := baseVal * baseVal;
          baseDec += baseDec;
        }
      };

      finalize({ value = resVal; decimals = resDec })
    }
  };

  // ------------ Utilities ------------
  /// Absolute value while keeping the scale unchanged.
  public func abs(d : Decimal) : Decimal = { value = Int.abs(d.value); decimals = d.decimals };

  /// Returns the negation of a decimal without altering the scale.
  public func neg(d : Decimal) : Decimal = { value = -d.value; decimals = d.decimals };

  /// Checks whether the stored magnitude is zero.
  public func isZero(d : Decimal) : Bool = (d.value == 0);

  /// Returns the sign of the decimal (`-1`, `0`, `1`).
  public func signum(d : Decimal) : Int = if (d.value < 0) -1 else if (d.value > 0) 1 else 0;

  /// Compares two decimals after aligning them to a common scale.
  public func compare(a : Decimal, b : Decimal) : Order.Order {
    let dec = Nat.max(a.decimals, b.decimals);
    let aa = quantize(a, dec, #down); // increasing scale (no rounding)
    let bb = quantize(b, dec, #down);
    if (aa.value < bb.value) #less else if (aa.value > bb.value) #greater else #equal
  };

  /// Tests whether two decimals have equal value after aligning them to a common scale.
  public func equal(a : Decimal, b : Decimal) : Bool {
    switch (compare(a, b)) {
      case (#equal) true;
      case (_) false;
    };
  };

  /// Returns the minimum of two decimals.
  public func min(a : Decimal, b : Decimal) : Decimal = switch (compare(a, b)) {
    case (#less) a;
    case (#equal) a;
    case (#greater) b;
  };
  /// Returns the maximum of two decimals.
  public func max(a : Decimal, b : Decimal) : Decimal = switch (compare(a, b)) {
    case (#less) b;
    case (#equal) a;
    case (#greater) a;
  };
  /// Clamps `x` into the inclusive range `[lo, hi]`.
  public func clamp(x : Decimal, lo : Decimal, hi : Decimal) : Decimal = max(lo, min(x, hi));

  /// Removes trailing zeros from the fractional part while preserving numeric value.
  public func normalize(d : Decimal) : Decimal {
    if (d.decimals == 0 or d.value == 0) return { value = d.value; decimals = d.decimals };

    var mag : Nat = Int.abs(d.value);
    var dec : Nat = d.decimals;

    while (dec > 0 and (mag % 10 == 0)) {
      mag /= 10;
      dec -= 1;
    };

    let signed = if (d.value < 0) -Int.fromNat(mag) else Int.fromNat(mag);
    { value = signed; decimals = dec }
  };
}
