// Wrapper around another object, basically memoises it.
// Consider a thunk wrapped around a function: we call enter on the thunk to evaluate it, which evaluates the function
// and updates the thunk to point to the results of the function call. Next time we evaluate the thunk, we just get the
// result without calling the function again.
public class Thunk extends HeapObject {
    protected HeapObject contained;

    public Thunk(HeapObject contained) {
        this.contained = contained;
    }

    @Override
    public HeapObject enter() {
        contained = contained.enter();
        return contained;
    }
}