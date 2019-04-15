package ${package};

import java.util.ArrayList;
import java.util.function.BiFunction;

public class Function extends HeapObject {
    private BiFunction<HeapObject[], HeapObject[], HeapObject> inner;
    private HeapObject[] freeVariables;
    // We can be given more arguments than we expect: eg. `(\x -> x) (\x -> x) 1`.
    private ArrayList<HeapObject> arguments;
    private int arity = 0; // The arity of this function
    private HeapObject result = null; // Result of previous evaluation of this function

    public Function(BiFunction<HeapObject[], HeapObject[], HeapObject> inner, int arity, HeapObject[] freeVariables) {
        this.inner = inner;
        this.arity = arity;
        this.freeVariables = freeVariables;
        arguments = new ArrayList<>();
    }

    // Use a "entered" variable to either return the old value or compute the new one?
    // We *want* to mutate this function so that multiple references to the same value don't recompute.
    @Override
    public HeapObject enter() {
        // Check if we've got a cached value
        if (result != null) {
            return result;
        }

        if (arguments.size() < arity) {
            return this; // If we're not fully applied, we get a partially applied function
        }
        else if (arguments.size() > arity) { // If we're over-applied, carry the arguments over
            try {
                Function fun = (Function)inner
                    .apply(arguments.subList(0, arity).toArray(new HeapObject[0]), freeVariables)
                    .enter()
                    .clone();
                for (HeapObject arg : arguments.subList(arity, arguments.size()))
                    fun.addArgument(arg);
                result = fun.enter();
                return result;
            }
            catch (CloneNotSupportedException e) {
                throw new RuntimeException(e);
            }
        }
        else { // Perfect number of arguments
            result = inner.apply(arguments.toArray(new HeapObject[0]), freeVariables).enter();
            return result;
        }
    }

    @Override
    public HeapObject force() {
        return enter().force();
    }

    public void addArgument(HeapObject arg) {
        arguments.add(arg);
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        // Return a Function with all the same references
        Function f = (Function)super.clone();
        f.inner = inner;
        f.arity = arity;
        f.freeVariables = freeVariables.clone();
        f.arguments = new ArrayList<>(arguments);
        return f;
    }
}