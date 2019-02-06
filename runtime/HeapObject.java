public abstract class HeapObject implements Cloneable {
    public abstract HeapObject enter();

    public HeapObject force() {
        return enter();
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
}