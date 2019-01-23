// Subclasses of BoxedData are standard "data"-defined datatypes.
public abstract class BoxedData extends Data {
    public int branch; // Which constructor of the datatype
    public HeapObject[] data;

    @Override 
    public String toString() {
        String res = String.valueOf(branch);
        for (HeapObject o : data) {
            res += "(" + o.toString() + ")";
        }
        return res;
    }
}