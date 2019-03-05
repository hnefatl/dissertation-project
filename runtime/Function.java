import java.util.ArrayList;
import java.util.function.BiFunction;

public class Function extends HeapObject {
    private BiFunction<HeapObject[], HeapObject[], HeapObject> inner;
    private HeapObject[] freeVariables;
    // We can be given more arguments than we expect: eg. `(\x -> x) (\x -> x) 1`.
    private ArrayList<HeapObject> arguments;
    private int arity = 0; // The arity of this function

    public Function(BiFunction<HeapObject[], HeapObject[], HeapObject> inner, int arity, HeapObject[] freeVariables) {
        this.inner = inner;
        this.arity = arity;
        this.freeVariables = freeVariables;
        arguments = new ArrayList<>();
    }

    @Override
    public HeapObject enter() {
        if (arguments.size() < arity) {
            return this; // If we're not fully applied, we get a partially applied function
        }
        else if (arguments.size() > arity) { // If we're over-applied, carry the arguments over
            try {
                Function result = (Function)inner
                    .apply(arguments.subList(0, arity).toArray(new HeapObject[0]), freeVariables)
                    .enter()
                    .clone();
                for (HeapObject arg : arguments.subList(arity, arguments.size()))
                    result.addArgument(arg);
                return result;
            }
            catch (CloneNotSupportedException e) {
                throw new RuntimeException(e);
            }
        }
        else { // Perfect number of arguments
            return inner.apply(arguments.toArray(new HeapObject[0]), freeVariables).enter();
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