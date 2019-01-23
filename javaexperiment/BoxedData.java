// Subclasses of BoxedData are standard "data"-defined datatypes.
public abstract class BoxedData extends Data {
    public int branch; // Which constructor of the datatype
    public HeapObject[] data;

    @Override 
    public String toString() {
        return String.valueOf(branch) + " " + data.toString();
    }
}