// https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects


// These functions should just be in `foo`, no extra classes
class _Num {
    public static Function _make2B() {
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
        n.data[0] = _makeNumInt2B();
        return n;
    }
    // Implementation of superclass methods
    public static Function _makeNumInt2B() {
        return new Function(_NumInt::_NumInt2BImpl, 2, new HeapObject[0]);
    }
    private static HeapObject _NumInt2BImpl(HeapObject[] arguments, HeapObject[] freeVariables) {
        int x = ((_Int)arguments[0].enter()).value;
        int y = ((_Int)arguments[1].enter()).value;
        return _Int._makeInt(x + y);
    }
}
public class foo {
    public static _NumInt dNumInt; 

    public static void main(String[] args) {
        dNumInt = _NumInt._makeNumInt();

        // Pretend let-bound variables
        //HeapObject x = new Thunk(_Maybe._makeNothing());
        HeapObject x = new Thunk(_Maybe._makeJust(new Thunk(_Int._makeInt(5))));
        HeapObject y = new Thunk(_Int._makeInt(6));

        // Invoke function
        Function f = foo._makeExampleFunction(x, dNumInt);
        f.addArgument(y);
        HeapObject result = f.enter();
        System.out.println(result.force());
    }
    public static Function _makeExampleFunction(HeapObject x, HeapObject dNumInt) {
        return new Function(foo::_exampleFunctionImpl, 1, new HeapObject[] { x, dNumInt });
    }
    // exampleFunction y = case x of
    //     Nothing -> Nothing
    //     Just x' -> Just (x' + y)
    private static HeapObject _exampleFunctionImpl(HeapObject[] arguments, HeapObject[] freeVariables) {
        BoxedData d = (BoxedData)freeVariables[0].enter(); // Evaluation
        switch (d.branch) {
            case 0:
                return new Thunk(_Maybe._makeNothing()); // Nothing
            case 1:
                Function f = _Num._make2B();
                f.addArgument(freeVariables[1]); // dNumInt
                f.addArgument(d.data[0]); // x
                f.addArgument(arguments[0]); // y
                return new Thunk(f);
            default: throw new RuntimeException("Invalid data branch");
        }
    }
}
// Wrap everything in thunks: they auto-update their value once evaluated.
// Do need a force function, maybe force_evaluate which calls evaluate at the end?