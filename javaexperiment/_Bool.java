// Mock generated code, can delete when we have datatype working properly
public class _Bool extends BoxedData {
    public static _Bool _makeFalse() {
        _Bool m = new _Bool();
        m.branch = 0;
        m.data = new HeapObject[] {};
        return m;
    }
    public static _Bool _makeTrue() {
        _Bool m = new _Bool();
        m.branch = 1;
        m.data = new HeapObject[] {};
        return m;
    }
}