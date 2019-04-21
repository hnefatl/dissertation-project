package ${package};

import java.math.BigInteger;

public class _Integer extends Data {
    public BigInteger value;
    public static _Integer _make_Integer(BigInteger x) {
        _Integer i = new _Integer();
        i.value = x;
        return i;
    }
    public static _Integer _make_Integer(String x) {
        return _make_Integer(new BigInteger(x));
    }

    public static _Integer add(_Integer x, _Integer y) {
        return _make_Integer(x.value.add(y.value));
    }
    public static _Integer sub(_Integer x, _Integer y) {
        return _make_Integer(x.value.subtract(y.value));
    }
    public static _Integer mult(_Integer x, _Integer y) {
        return _make_Integer(x.value.multiply(y.value));
    }
    public static _Integer negate(_Integer x) {
        return _make_Integer(x.value.negate());
    }
    public static boolean eq(_Integer x, _Integer y) {
        return x.value.equals(y.value);
    }
    public static _Integer div(_Integer x, _Integer y) {
        return _make_Integer(x.value.divide(y.value));
    }
    public static _Integer mod(_Integer x, _Integer y) {
        return _make_Integer(x.value.remainder(y.value));
    }

    public static String show(_Integer x) {
        return x.value.toString();
    }
}