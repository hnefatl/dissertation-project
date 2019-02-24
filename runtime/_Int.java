// This is a special primitive, as it subclasses Data instead of BoxedData
public class _Int extends Data {
    public int value;
    public static _Int _make_Int(int x) {
        _Int i = new _Int();
        i.value = x;
        return i;
    }

    @Override
    public String toString() {
        return "Int: " + value;
    }
}