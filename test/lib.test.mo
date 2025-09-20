import { test } "mo:test";
import Decimal "../src/";

// Helper to compare Decimal records
func assertDecimalEqual(actual : Decimal.Decimal, expected : Decimal.Decimal) {
  assert actual.value == expected.value;
  assert actual.decimals == expected.decimals;
};

test("Decimal constructors", func () {
  assertDecimalEqual(Decimal.zero(3), { value = 0; decimals = 3 });
  assertDecimalEqual(Decimal.ofInt(-123, 1), { value = -123; decimals = 1 });
  assertDecimalEqual(Decimal.ofNat(789, 4), { value = 789; decimals = 4 });
});

test("Decimal toText and format", func () {
  let sample : Decimal.Decimal = { value = 123456789; decimals = 4 }; // 12_345.6789
  assert Decimal.toText(sample) == "12345.6789";

  let negative : Decimal.Decimal = { value = -5; decimals = 3 };
  assert Decimal.toText(negative) == "-0.005";

  let formatted = Decimal.format(sample, { thousandsSep = ?"_"; decimalSep = ?"," });
  assert formatted == "12_345,6789";

  let formattedDefault = Decimal.format(sample, { thousandsSep = null; decimalSep = null });
  assert formattedDefault == "12,345.6789";
});

test("Decimal ofText parsing and rounding 1", func () {
  switch (Decimal.ofText("123.45", 2, #down)) {
    case (#ok value) { assertDecimalEqual(value, { value = 12345; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.ofText("1.239", 2, #down)) {
    case (#ok value) { assertDecimalEqual(value, { value = 123; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.ofText("1.231", 2, #up)) {
    case (#ok value) { assertDecimalEqual(value, { value = 124; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.ofText("-2.301", 2, #up)) {
    case (#ok value) { assertDecimalEqual(value, { value = -231; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.ofText("-2.345", 2, #halfUp)) {
    case (#ok value) { assertDecimalEqual(value, { value = -235; decimals = 2 }) };
    case (#err _) { assert false };
  };
});
test("Decimal ofText parsing and rounding 2", func () {
  switch (Decimal.ofText("1.2345", 3, #halfUp)) {
    case (#ok value) { assertDecimalEqual(value, { value = 1235; decimals = 3 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.ofText("1.23.4", 2, #down)) {
    case (#ok _) { assert false };
    case (#err e) { assert e == #InvalidFormat };
  };
});

test("Decimal quantize and rounding helpers", func () {
  let x : Decimal.Decimal = { value = 12345; decimals = 3 }; // 12.345

  assertDecimalEqual(Decimal.quantize(x, 5, #down), { value = 1234500; decimals = 5 });
  assertDecimalEqual(Decimal.quantize(x, 2, #down), { value = 1234; decimals = 2 });
  assertDecimalEqual(Decimal.quantize(x, 2, #up), { value = 1235; decimals = 2 });
  assertDecimalEqual(Decimal.quantize({ value = -12345; decimals = 3 }, 2, #halfUp), { value = -1235; decimals = 2 });

  let y : Decimal.Decimal = { value = -10987; decimals = 3 }; // -10.987
  assertDecimalEqual(Decimal.truncTo(y, 2), { value = -1098; decimals = 2 });
  assertDecimalEqual(Decimal.floorTo(y, 2), { value = -1099; decimals = 2 });
  assertDecimalEqual(Decimal.ceilTo(y, 2), { value = -1098; decimals = 2 });
});

test("Decimal conversions toInt, toNat, toFloat/fromFloat", func () {
  let d : Decimal.Decimal = { value = 12345; decimals = 3 }; // 12.345
  assert Decimal.toInt(d, #down) == 12;
  assert Decimal.toInt(d, #up) == 13;

  switch (Decimal.toNat({ value = 500; decimals = 2 }, #halfUp)) {
    case (#ok value) { assert value == 5 };
    case (#err _) { assert false };
  };

  switch (Decimal.toNat({ value = -1; decimals = 0 }, #down)) {
    case (#ok _) { assert false };
    case (#err e) { assert e == #NegativeValue };
  };

  assert Decimal.toFloat(d) == 12.345;

  switch (Decimal.fromFloat(12.345, 3, #halfUp)) {
    case (#ok value) { assertDecimalEqual(value, d) };
    case (#err _) { assert false };
  };

  switch (Decimal.fromFloat(-1.234, 2, #up)) {
    case (#ok value) { assertDecimalEqual(value, { value = -124; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.fromFloat(99.99, 2, #halfUp)) {
    case (#ok value) { assertDecimalEqual(value, { value = 9999; decimals = 2 }) };
    case (#err _) { assert false };
  };

  let nan = 0.0 / 0.0;
  switch (Decimal.fromFloat(nan, 2, #down)) {
    case (#ok _) { assert false };
    case (#err e) { assert e == #InvalidFloat };
  };

  let inf = 1.0 / 0.0;
  switch (Decimal.fromFloat(inf, 2, #down)) {
    case (#ok _) { assert false };
    case (#err e) { assert e == #InvalidFloat };
  };
});

test("Decimal equal helper", func () {
  let a : Decimal.Decimal = { value = 1234; decimals = 2 };  // 12.34
  let b : Decimal.Decimal = { value = 12340; decimals = 3 }; // 12.340
  let c : Decimal.Decimal = { value = 1235; decimals = 2 };  // 12.35

  assert Decimal.equal(a, b);
  assert Decimal.equal(Decimal.neg(a), { value = -12340; decimals = 3 });
  assert Decimal.equal(Decimal.zero(4), { value = 0; decimals = 0 });
  assert Decimal.equal(a, c) == false;
});

test("Decimal arithmetic operations", func () {
  let a : Decimal.Decimal = { value = 1234; decimals = 2 }; // 12.34
  let b : Decimal.Decimal = { value = 567; decimals = 1 };  // 56.7

  assertDecimalEqual(
    Decimal.add(a, b, null),
    { value = 6904; decimals = 2 }
  );

  assertDecimalEqual(
    Decimal.add(a, b, ?1),
    { value = 690; decimals = 1 }
  );

  assertDecimalEqual(
    Decimal.subtract(b, a, ?2),
    { value = 4436; decimals = 2 }
  );

  assertDecimalEqual(
    Decimal.multiply(a, b, ?3, #halfUp),
    { value = 699678; decimals = 3 }
  );

  switch (Decimal.divide(b, a, ?2, #halfUp)) {
    case (#ok value) { assertDecimalEqual(value, { value = 459; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.divide(a, { value = 0; decimals = 0 }, ?2, #down)) {
    case (#ok _) { assert false };
    case (#err e) { assert e == #DivideByZero };
  };
});

test("Decimal power", func () {
  let base : Decimal.Decimal = { value = 2; decimals = 0 };

  switch (Decimal.power(base, 3, ?0, #down)) {
    case (#ok value) { assertDecimalEqual(value, { value = 8; decimals = 0 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.power(base, 0, ?2, #down)) {
    case (#ok value) { assertDecimalEqual(value, { value = 100; decimals = 2 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.power({ value = 3; decimals = 0 }, -2, ?3, #halfUp)) {
    case (#ok value) { assertDecimalEqual(value, { value = 111; decimals = 3 }) };
    case (#err _) { assert false };
  };

  switch (Decimal.power({ value = 0; decimals = 0 }, -1, ?2, #down)) {
    case (#ok _) { assert false };
    case (#err e) { assert e == #ZeroToNegativePower };
  };
});

test("Decimal inverse identities", func () {
  let a : Decimal.Decimal = { value = 1234; decimals = 2 }; // 12.34
  let b : Decimal.Decimal = { value = 567; decimals = 1 };  // 56.7

  let sumDefault = Decimal.add(a, b, null);
  let recoverDefault = Decimal.subtract(sumDefault, b, null);
  assert Decimal.equal(recoverDefault, a);

  let sumScaled = Decimal.add(a, b, ?3);
  let recoverScaled = Decimal.subtract(sumScaled, Decimal.quantize(b, 3, #halfUp), ?3);
  assert Decimal.equal(recoverScaled, Decimal.quantize(a, 3, #halfUp));

  let c : Decimal.Decimal = { value = 1500; decimals = 2 }; // 15.00
  let d : Decimal.Decimal = { value = 200; decimals = 2 };  // 2.00

  let prod = Decimal.multiply(c, d, null, #halfUp);
  switch (Decimal.divide(prod, d, ?2, #halfUp)) {
    case (#ok recovered) { assert Decimal.equal(recovered, c) };
    case (#err _) { assert false };
  };

  let prodScaled = Decimal.multiply(c, d, ?5, #halfUp);
  let dScaled = Decimal.quantize(d, 5, #halfUp);
  switch (Decimal.divide(prodScaled, dScaled, ?2, #halfUp)) {
    case (#ok recoveredScaled) { assert Decimal.equal(recoveredScaled, c) };
    case (#err _) { assert false };
  };
});

test("Decimal utilities", func () {
  let neg : Decimal.Decimal = { value = -4500; decimals = 2 };
  let pos : Decimal.Decimal = { value = 750; decimals = 1 };

  assertDecimalEqual(Decimal.abs(neg), { value = 4500; decimals = 2 });
  assertDecimalEqual(Decimal.neg(pos), { value = -750; decimals = 1 });
  assert Decimal.isZero({ value = 0; decimals = 5 });
  assert Decimal.isZero({ value = 10; decimals = 1 }) == false;
  assert Decimal.signum(neg) == -1;
  assert Decimal.signum(pos) == 1;
  assert Decimal.signum({ value = 0; decimals = 0 }) == 0;

  assert Decimal.compare({ value = 100; decimals = 2 }, { value = 1; decimals = 0 }) == #equal;
  assert Decimal.compare({ value = -101; decimals = 1 }, { value = -10; decimals = 0 }) == #less;
  assert Decimal.compare({ value = 505; decimals = 2 }, { value = 50; decimals = 1 }) == #greater;

  assertDecimalEqual(Decimal.min(neg, pos), neg);
  assertDecimalEqual(Decimal.max(neg, pos), pos);

  assertDecimalEqual(
    Decimal.clamp({ value = 500; decimals = 2 }, { value = 400; decimals = 2 }, { value = 600; decimals = 2 }),
    { value = 500; decimals = 2 }
  );

  assertDecimalEqual(
    Decimal.clamp({ value = 300; decimals = 2 }, { value = 400; decimals = 2 }, { value = 600; decimals = 2 }),
    { value = 400; decimals = 2 }
  );

  assertDecimalEqual(
    Decimal.clamp({ value = 900; decimals = 2 }, { value = 400; decimals = 2 }, { value = 600; decimals = 2 }),
    { value = 600; decimals = 2 }
  );

  assertDecimalEqual(
    Decimal.normalize({ value = 1234000; decimals = 4 }),
    { value = 1234; decimals = 1 }
  );

  assertDecimalEqual(
    Decimal.normalize({ value = -450000; decimals = 5 }),
    { value = -45; decimals = 1 }
  );

  assertDecimalEqual(
    Decimal.normalize({ value = 0; decimals = 3 }),
    { value = 0; decimals = 3 }
  );
});
