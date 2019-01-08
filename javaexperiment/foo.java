// https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects

import java.util.ArrayList;

abstract class HeapObject {
    public abstract HeapObject enter();
}
abstract class Function extends HeapObject {
    protected HeapObject[] freeVariables;
    // We can be given more arguments than we expect: eg. `(\x -> x) (\x -> x) 1`.
    protected ArrayList<HeapObject> arguments;
    protected int arity = 0; // The arity of this function

    protected Function() {
        arguments = new ArrayList<>();
    }

    @Override
    public HeapObject enter() {
        if (arguments.size() < arity) {
            return this; // If we're not fully applied, we get a partially applied function
        }
        else if (arguments.size() > arity) { // If we're over-applied, carry the arguments over
            Function result = (Function)evaluate().enter();
            for (HeapObject arg : arguments.subList(arity, arguments.size()))
                result.addArgument(arg);
            return result;
        }
        else { // Perfect number of arguments
            return evaluate();
        }
    }

    protected abstract HeapObject evaluate();

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
class TooManyArguments extends RuntimeException {
    static final long serialVersionUID = 0;
}
class NotEnoughArguments extends RuntimeException {
    static final long serialVersionUID = 0;
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
class _2B extends Function { // Addition function: 2B is hex UTF-8 for +.
    public static _2B _make2B() {
        _2B n = new _2B();
        n.freeVariables = new HeapObject[0];
        n.arity = 3;
        return n;
    }

    @Override
    public HeapObject evaluate() {
        BoxedData d = (BoxedData)arguments.get(0).enter();
        Function f = (Function)d.data[0].enter();
        f.addArgument(arguments.get(1));
        f.addArgument(arguments.get(2));
        return f.enter();
    }
}
class _NumInt2B extends Function { // Implementation of addition for the Num Int instance
    public static _NumInt2B _makeNumInt2B() {
        _NumInt2B n = new _NumInt2B();
        n.freeVariables = new HeapObject[0];
        n.arity = 2;
        return n;
    }

    @Override
    public HeapObject evaluate() {
        int x = ((_Int)arguments.get(0).enter()).value;
        int y = ((_Int)arguments.get(1).enter()).value;
        return _Int._makeInt(x + y);
    }
}
class _NumInt extends BoxedData {
    public static _NumInt _makeNumInt() {
        _NumInt n = new _NumInt();
        n.branch = 0;
        n.data = new HeapObject[1];
        n.data[0] = _NumInt2B._makeNumInt2B();
        return n;
    }
}
class _ExampleFunction extends Function { // Implementation of `\y -> x + y :: Int`
    public static _ExampleFunction _makeExampleFunction(HeapObject x) {
        _ExampleFunction f = new _ExampleFunction();
        f.freeVariables = new HeapObject[] { x }; // x is free, passed in as an argument
        f.arity = 1;
        return f;
    }

    @Override
    protected HeapObject evaluate() {
        // This would be a free variable allocated in a let statement...
        HeapObject d = new Thunk(_NumInt._makeNumInt());
        Function f = _2B._make2B();
        // This is stateful: safe? We're modifying the function application
        f.addArgument(d);
        f.addArgument(freeVariables[0]);
        f.addArgument(arguments.get(0));
        return f.enter();
    }
}
public class foo {
    public static void main(String[] args) {
        //_Maybe m1 = _Maybe._makeNothing();
        //_Maybe m2 = _Maybe._makeJust(x);
        HeapObject x = new Thunk(_Int._makeInt(5));
        HeapObject y = new Thunk(_Int._makeInt(6));
        _ExampleFunction f = _ExampleFunction._makeExampleFunction(x); // x is free
        f.addArgument(y);
        HeapObject result = f.enter();
        System.out.println(result);
    }
}
// Wrap everything in thunks: they auto-update their value once evaluated.
// Do need a force function, maybe force_evaluate which calls evaluate at the end?