// Subclasses of BoxedData are standard "data"-defined datatypes.
public abstract class BoxedData extends Data {
    public int branch; // Which constructor of the datatype
    public HeapObject[] data;

    @Override 
    public String toString() {
        String res = "Data: { branch: " + String.valueOf(branch) + ", data {";
        for (HeapObject o : data) {
            res += " " + String.valueOf(o);
        }
        res += " } }";
        return res;
    }

    private static Boolean forced = false;
    @Override
    public HeapObject force() {
        if (!forced) {
            forced = true;
            for (HeapObject o : data) {
                o.force();
            }
        }
        return this;
    }
}