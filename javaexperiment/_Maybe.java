// These classes are mock generated code.
public class _Maybe extends BoxedData {
    public static _Maybe _makeNothing() {
        _Maybe m = new _Maybe();
        m.branch = 0;
        m.data = new HeapObject[] {};
        return m;
    }
    public static _Maybe _makeJust(HeapObject x) {
        _Maybe m = new _Maybe();
        m.branch = 1;
        m.data = new HeapObject[] { x };
        return m;
    }
}