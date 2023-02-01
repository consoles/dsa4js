package io.consoles.ch6.exercises;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * 采用输入领域建模的方法设计并执行 String 类中如下方法的测试计划
 * public int indexOf(int ch,int fromIndex)
 */
public class Test2Test {
    private final static String TESTME = "test me";

    @Test
    public void testNominal() {
        int result = TESTME.indexOf((int)'t', 2);
        assertEquals("test with nominal arguments", 3, result);
    }

    @Test
    public void testNegativeIndex() {
        int result = TESTME.indexOf((int)'t', -2);
        assertEquals("negative index should be treated as 0", 0, result);
    }

    @Test
    public void testZeroCharAndOverflowingIndex() {
        int result = TESTME.indexOf(0, TESTME.length() + 10);
        assertEquals("testing index beyond upper bound", -1, result);
    }

    @Test
    public void testNegativeCharAndZeroIndex() {
        int result = TESTME.indexOf(-1, 0);
        assertEquals("testing negative character (invalid)", -1, result);
    }

    @Test
    public void testEmptyString() {
        int result = "".indexOf((int)'t', 0);
        assertEquals(null, -1, result);
    }
}
