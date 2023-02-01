package io.consoles.ch6.exercises;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import static org.junit.Assert.*;

public class Test3Test {

    /**
     * C1: list1 分为 3 种情况 null, 空， 非空
     * C2: list2 分为 3 种情况 null, 空， 非空
     * C3: list1 和 list2 的长度分为 2 种情况：相同，不同
     * C1, C2, C3 的全组合有下面的几种有意义的情况：
     * null，null，否 1
     * null, 空， 否 2
     * null, 非空, 否 3
     * <p>
     * 空, null， 否 4
     * 空, 空， 是 5
     * 空，非空，否 6
     * <p>
     * 非空， null， 否 7
     * 非空，空，否 8
     * 非空，非空，是 9
     * 非空，非空，否 10
     */
    public static <E> List<E> interleaveLists(List<E> list1, List<E> list2) {
        Objects.requireNonNull(list1, "list1 must not null");
        Objects.requireNonNull(list2, "list2 must not null");
        if (list1.size() != list2.size()) {
            throw new IllegalArgumentException("list1 and list2 must has same size");
        }
        List<E> list = new ArrayList<>(list1.size() + list2.size());
        Iterator<E> it1 = list1.iterator();
        Iterator<E> it2 = list2.iterator();
        while (true) {
            boolean it1HasNext = it1.hasNext();
            boolean it2HasNext = it2.hasNext();
            if (it1HasNext) {
                list.add(it1.next());
            }
            if (it2HasNext) {
                list.add(it2.next());
            }
            if (!it1HasNext && !it2HasNext) {
                break;
            }
        }
        return list;
    }

    private List<Integer> a, b, result;

    @Before
    public void setUp() {
        a = List.of(1, 2, 3);
        b = List.of(4, 5, 6);
        result = List.of(1, 4, 2, 5, 3, 6);
    }

    @Test(expected = NullPointerException.class)
    public void test1() {
        interleaveLists(null, null);
    }

    @Test(expected = NullPointerException.class)
    public void test2() {
        interleaveLists(null, List.of());
    }

    @Test(expected = NullPointerException.class)
    public void test3() {
        interleaveLists(null, a);
    }

    @Test(expected = NullPointerException.class)
    public void test4() {
        interleaveLists(List.of(), null);
    }

    @Test
    public void test5() {
        List<Integer> list = interleaveLists(List.of(), List.of());
        assertTrue("should be empty", list.isEmpty());
    }

    @Test(expected = IllegalArgumentException.class)
    public void test6() {
        interleaveLists(List.of(), a);
    }

    @Test(expected = NullPointerException.class)
    public void test7() {
        interleaveLists(a, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test8() {
        interleaveLists(a, List.of());
    }

    @Test
    public void test9() {
        List<Integer> list = interleaveLists(a, b);
        assertEquals("should be: " + result, list, result);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test10() {
        interleaveLists(List.of(1, 2), a);
    }
}
