package factorial;

import java.math.BigInteger;

public class Factorial {
    public static void main(String[] args) {
        System.out.println(fact(100).toString());
    }

    public static BigInteger fact(int n) {
        if (n == 0) {
            return BigInteger.ONE;
        }
        else {
            return BigInteger.valueOf(n).multiply(fact(n - 1));
        }
    }
}