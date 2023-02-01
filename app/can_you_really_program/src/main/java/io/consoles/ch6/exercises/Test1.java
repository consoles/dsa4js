package io.consoles.ch6.exercises;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Test1 {
    /**
     * 法返回一个整型的 List，包含 n 的所有除数。对于 n==0，它返回空列表。
     * 对于 n 为负值的情况，它返回与其正值相同的列表
     *
     * @param n
     * @return
     */
    public static List<Integer> getDivisors(int n) {
        if (n == 0) return Collections.emptyList();
        if (n < 0) n = -n;
        List<Integer> list = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            if (n % i == 0) {
                list.add(i);
            }
        }
        return list;
    }

    public static void main(String[] args) {
        System.out.println(getDivisors(12));
        System.out.println(getDivisors(-12));
    }
}
