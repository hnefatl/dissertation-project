// Subclasses of BoxedData are standard "data"-defined datatypes.
public abstract class BoxedData extends Data {
    public int branch; // Which constructor of the datatype
    public HeapObject[] data;
}