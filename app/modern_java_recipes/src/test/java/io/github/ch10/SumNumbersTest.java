package io.github.ch10;

import junit.framework.TestCase;

class PrivateDemo implements SumNumbers {
}

public class SumNumbersTest extends TestCase {

    private SumNumbers demo = new PrivateDemo();

    public void testAddEvens() {
        assertEquals(2 + 4 + 6, demo.addEvens(1, 2, 3, 4, 5, 6));
    }

    public void testAddOdds() {
        assertEquals(1 + 3 + 5, demo.addOdds(1, 2, 3, 4, 5, 6));
    }
}
