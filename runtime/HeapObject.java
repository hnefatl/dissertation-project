package ${package};

public abstract class HeapObject implements Cloneable {
    public abstract HeapObject enter();

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
}