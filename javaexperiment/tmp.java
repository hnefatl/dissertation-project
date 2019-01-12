import java.util.function.BiFunction;

public class tmp {
    public static void main(String[] args) {
        BiFunction<Integer, String, Boolean> f = tmp::fa;
        BiFunction<String, Integer, Boolean> g = tmp::fb;
    }
    
    private static Boolean fa(Integer x, String y) {
        return true;
    }
    private static Boolean fb(String x, Integer y) {
        return true;
    }
}
