import java.math.BigInteger;

public class _Int extends Data {
    public int value;
    public static _Int _make_Int(int x) {
        _Int i = new _Int();
        i.value = x;
        return i;
    }

    public static _Int add(_Int x, _Int y) {
        return _make_Int(x.value + y.value);
    }
    public static _Int sub(_Int x, _Int y) {
        return _make_Int(x.value - y.value);
    }
    public static _Int mult(_Int x, _Int y) {
        return _make_Int(x.value * y.value);
    }
    public static _Int div(_Int x, _Int y) {
        return _make_Int(x.value / y.value);
    }
    public static _Int negate(_Int x) {
        return _make_Int(-x.value);
    }
    public static _Int fromInteger(_Integer x) {
        final BigInteger low = new BigInteger("-2147483648");
        final BigInteger high = new BigInteger("2147483647");
        // Clamp to the range of an int then convert
        return _make_Int(x.value.max(low).min(high).intValue());
    }
    public static boolean eq(_Int x, _Int y) {
        return x.value == y.value;
    }
    public static boolean lessj(_Int x, _Int y) {
        return x.value < y.value;
    }

    @Override
    public String toString() {
        return "Int: " + value;
    }
}