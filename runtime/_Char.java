public class _Char extends Data {
    public char value;
    public static _Char _make_Char(char x) {
        _Char c = new _Char();
        c.value = x;
        return c;
    }

    public static boolean eq(_Char x, _Char y) {
        return x.value == y.value;
    }
}