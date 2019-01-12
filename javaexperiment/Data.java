// Direct non-abstract subclasses of Data are builtin primitives like integers.
public abstract class Data extends HeapObject {
    @Override
    public HeapObject enter() {
        // No-op. A data constructor is terminal.
        return this;
    }
}