// https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects

import java.util.ArrayList;
import java.util.function.BiFunction;

abstract class HeapObject {
    public abstract HeapObject enter();
}
class Function extends HeapObject {
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
            Function result = (Function)inner.apply(arguments.subList(0, arity).toArray(new HeapObject[0]), freeVariables);
            for (HeapObject arg : arguments.subList(arity, arguments.size()))
                result.addArgument(arg);
            return result;
        }
        else { // Perfect number of arguments
            return inner.apply(arguments.toArray(new HeapObject[0]), freeVariables);
        }
    }

    public void addArgument(HeapObject arg) {
        arguments.add(arg);
    }
}
// Wrapper around another object, basically memoises it.
// Consider a thunk wrapped around a function: we call enter on the thunk to evaluate it, which evaluates the function
// and updates the thunk to point to the results of the function call. Next time we evaluate the thunk, we just get the
// result without calling the function again.
class Thunk extends HeapObject {
    protected HeapObject contained;
    protected Boolean entered;

    public Thunk(HeapObject contained) {
        this.contained = contained;
        this.entered = false;
    }

    @Override
    public HeapObject enter() {
        // Inefficient branch: don't think there's a way around though.
        if (!entered) {
            contained = contained.enter();
            entered = true;
        }
        return contained;
    }
}
// Direct non-abstract subclasses of Data are builtin primitives like integers.
abstract class Data extends HeapObject {
    @Override
    public HeapObject enter() {
        // No-op. A data constructor is terminal.
        return this;
    }
}
// Subclasses of BoxedData are standard "data"-defined datatypes.
abstract class BoxedData extends Data {
    public int branch; // Which constructor of the datatype
    public HeapObject[] data;
}

// This is a special primitive, as it subclasses Data instead of BoxedData
class _Int extends Data {
    public int value;
    public static _Int _makeInt(int x) {
        _Int i = new _Int();
        i.value = x;
        return i;
    }
}
// These classes are mock generated code.
class _Maybe extends BoxedData {
    public static _Maybe _makeNothing() {
        _Maybe m = new _Maybe();
        m.branch = 0;
        m.data = new HeapObject[] {};
        return m;
    }
    public static _Maybe _makeJust(HeapObject x) {
        _Maybe m = new _Maybe();
        m.branch = 1;
        m.data = new HeapObject[] { x };
        return m;
    }
}
// Typeclass class: provides the publicly visible function that extracts the implementation function from the dictionary
class _Num {
    public static HeapObject _make2B() {
        return new Function(_Num::_2BImpl, 3, new HeapObject[0]);
    }
    private static HeapObject _2BImpl(HeapObject[] arguments, HeapObject[] freeVariables) {
        BoxedData d = (BoxedData)arguments[0].enter();
        Function f = (Function)d.data[0].enter();
        f.addArgument(arguments[1]);
        f.addArgument(arguments[2]);
        return f.enter();
    }
}
class _NumInt extends BoxedData {
    public static _NumInt _makeNumInt() {
        _NumInt n = new _NumInt();
        n.branch = 0;
        n.data = new HeapObject[1];
        n.data[0] = new Function(_NumInt::_NumInt2BImpl, 2, new HeapObject[0]);
        return n;
    }

    private static HeapObject _NumInt2BImpl(HeapObject[] arguments, HeapObject[] freeVariables) {
        int x = ((_Int)arguments[0].enter()).value;
        int y = ((_Int)arguments[1].enter()).value;
        return _Int._makeInt(x + y);
    }
}
public class foo {
    public static void main(String[] args) {
        //_Maybe m1 = _Maybe._makeNothing();
        //_Maybe m2 = _Maybe._makeJust(x);
        HeapObject x = new Thunk(_Int._makeInt(5));
        HeapObject y = new Thunk(_Int._makeInt(6));
        HeapObject dNumInt = new Thunk(_NumInt._makeNumInt());
        Function f = (Function)foo._makeExampleFunction(x, dNumInt).enter();
        f.addArgument(y);
        HeapObject result = f.enter();
        System.out.println(result);
    }
    public static HeapObject _makeExampleFunction(HeapObject x, HeapObject dNumInt) {
        return new Function(foo::_exampleFunctionImpl, 1, new HeapObject[] { x, dNumInt });
    }
    private static HeapObject _exampleFunctionImpl(HeapObject[] arguments, HeapObject[] freeVariables) {
        Function f = (Function)_Num._make2B().enter();
        f.addArgument(freeVariables[1]); // dNumInt
        f.addArgument(freeVariables[0]); // x
        f.addArgument(arguments[0]); // y
        return f.enter();
    }
}
// Wrap everything in thunks: they auto-update their value once evaluated.
// Do need a force function, maybe force_evaluate which calls evaluate at the end?