package io.github.ch9;

import junit.framework.TestCase;
import org.junit.Test;

import java.util.concurrent.ExecutionException;

public class App5Test extends TestCase {

    private App5 app;

    public void setUp() throws Exception {
        super.setUp();
        app = new App5();
    }

    @Test(expected = ExecutionException.class)
    public void testException() {
        app.getProduct(666);
    }

    @Test
    public void testExceptionWithCause() throws Exception {
        try {
            app.getProduct(666).get();
        } catch (ExecutionException e) {
            assertEquals(ExecutionException.class, e.getClass());
            assertEquals(RuntimeException.class, e.getClass().getClass());
        }
    }
}
