public abstract class HeapObject implements Cloneable {
    public abstract HeapObject enter();

    // Hacky not-sure-if-we-need method: iterates calling enter until we reach a fixed point (should be normal form?)
    public HeapObject force() {
        HeapObject last = null;
        HeapObject current = this;
        while (last != current) {
            last = current;
            current = current.enter();
        }
        return current;
    }
}