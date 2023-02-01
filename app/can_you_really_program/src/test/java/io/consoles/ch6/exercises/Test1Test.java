package io.consoles.ch6.exercises;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

/**
 * 取第一个特征 C1 作为输入 n 的符号，第二个特证 C2
 * C1 的可能取值 {负数，0，正数}
 * C2 的可能取值 {零，一个，两个，更多}
 * 其中 C1 == 0 只能和 C2 == 0 相匹配，全组合为 2 * 3 + 1 = 7 种
 */
public class Test1Test {

    // C1 = C2 = zero
    @Test
    public void testZero() {
        List<Integer> divisors = Test1.getDivisors(0);
        assertTrue("Divisors of zero should be the empty list", divisors.isEmpty());
    }

    // C1 = negative, C2 = one
    @Test
    public void testMinusOne() {
        List<Integer> divisors = Test1.getDivisors(-1);
        List<Integer> expected = List.of(1);
        assertEquals("Wrong divisors of -1", expected, divisors);
    }

    // C1 = negative, C2 = two
    @Test
    public void testMinusPrime() {
        List<Integer> divisors = Test1.getDivisors(-11);
        List<Integer> expected = List.of(1, 11);
        assertEquals("Wrong divisors of negative prime", expected, divisors);
    }

    // C1 = negative, C2 = more than two
    @Test
    public void testMinusNonPrime() {
        List<Integer> divisors = Test1.getDivisors(-12);
        List<Integer> expected = List.of(1, 2, 3, 4, 6, 12);
        assertEquals("Wrong divisors of negative prime", expected, divisors);
    }

    // C1 = positive, C2 = one
    @Test
    public void testOne() {
        List<Integer> divisors = Test1.getDivisors(1);
        List<Integer> expected = List.of(1);
        assertEquals("Wrong divisors of 1", expected, divisors);
    }

    // C1 = positive, C2 = two
    @Test
    public void testPrime() {
        List<Integer> divisors = Test1.getDivisors(17);
        List<Integer> expected = List.of(1, 17);
        assertEquals("Wrong divisors of positive prime", expected, divisors);
    }

    // C1 = positive, C2 = more than two
    @Test
    public void testNonPrime() {
        List<Integer> divisors = Test1.getDivisors(14);
        List<Integer> expected = List.of(1, 2, 7, 14);
        assertEquals("Wrong divisors of positive prime", expected, divisors);
    }
}
