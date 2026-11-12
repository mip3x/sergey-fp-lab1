public class Problem3 {
    public static void main(String[] args) {
        long n = 600851475143L;
        long d = 2;
        while (d * d <= n) {
            if (n % d == 0) n /= d;
            else d++;
        }
        System.out.println(n);
    }
}