package io.consoles.ch5.exercises;

import java.util.Random;

public class Test4 {
    /**
     * 返回 2 个给定整数的最大公约数
     *
     * @param u
     * @param v
     * @return
     */
    private static int greatestCommonDivisor(int u, int v) {
        final int originalU = u, originalV = v; // 开启断言

        if (u == 0 || v == 0) {
            if (u == Integer.MIN_VALUE || v == Integer.MIN_VALUE) {
                throw new ArithmeticException("overflow: gcd is 2^31");
            }
            return Math.abs(u) + Math.abs(v);
        }
        if (Math.abs(u) == 1 || Math.abs(v) == 1) {
            return 1;
        }
        if (u > 0) { u = -u; }
        if (v > 0) { v = -v; }
        int k = 0;
        while ((u & 1) == 0 && (v & 1) == 0 && k < 31) {
            u /= 2;
            v /= 2;
            k++;
        }
        if (k == 31) {
            throw new ArithmeticException("overflow: gcd is 2^31");
        }
        int t = (u & 1) == 1 ? v : -(u / 2);
        do {
            while ((t & 1) == 0) { t /= 2; }
            if (t > 0) { u = -t; }
            else { v = t; }
            t = (v - u) / 2;
        } while (t != 0);
        int gcd = -u * (1 << k);
        assert isGcd(gcd, originalU, originalU) : String.format("u: %d, v: %d, gcd: %d", originalU, originalV, gcd);
        return gcd;
    }

    /**
     * 这个实现性能比较低，更加合理的做法是使用欧几里得算法或者调用一个现有的 gcd 实现，例如 JDK 中自带的 BigInteger.gcd
     * @param gcd
     * @param u
     * @param v
     * @return
     */
    private static boolean isGcd(int gcd, int u, int v) {
        if (u % gcd == 0 || v % gcd == 0) {
            return false;
        }
        for (int i = gcd + 1; i <= u && i <= v; i++) {
            if (u % i == 0 && v % i == 0) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        final Random random = new Random();
        final int ITERATIONS = 1000;

        for (int i = 0; i < ITERATIONS; i++) {
            int a = random.nextInt(), b = random.nextInt();
            int g = greatestCommonDivisor(a, b);
            System.out.println(g);
        }
    }
}
