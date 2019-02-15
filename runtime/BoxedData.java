// Subclasses of BoxedData are standard "data"-defined datatypes.
public abstract class BoxedData extends Data {
    public int branch; // Which constructor of the datatype
    public HeapObject[] data;

    @Override 
    public String toString() {
        String res = "Data: { branch: " + String.valueOf(branch) + ", data: {";
        for (HeapObject o : data) {
            res += " " + String.valueOf(o);
        }
        res += " } }";
        return res;
    }

    private Boolean forced = false;
    @Override
    public HeapObject force() {
        if (!forced) {
            forced = true;
            data = java.util.Arrays.stream(data).map(d -> d.force()).toArray(HeapObject[]::new);
        }
        return this;
    }
}