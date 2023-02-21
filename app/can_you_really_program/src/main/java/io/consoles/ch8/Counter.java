package io.consoles.ch8;

/**
 * 线程安全问题：race condition
 */
public class Counter {
    private int n;

    private Object monitor = new Object();

    /**
     * 线程不安全的写法
     */
    public void increment1() {
        n++;
    }

    /**
     * 类级别的并发
     * 将所有方法使用一个全局锁进行包装
     */
    public void increment2() {
        synchronized (Counter.class) {
            n++;
        }
    }

    /**
     * 对象级别并发
     * 在所有实例方法上加锁
     */
    public synchronized void increment3() {
        n++;
    }

    /**
     * 对象级别并发2
     * 使用类内部的对象作为监视器，替代方法上的锁
     * increment3 的缺点：当客户端使用 Counter.class 作为监视器的时候会发生冲突
     */
    public void increment4() {
        synchronized (monitor) {
            n++;
        }
    }

    @Override
    public String toString() {
        return "" + n;
    }

    public static void main(String... args) throws InterruptedException {
        final int NTHREADS = 5, NINCREMENTS = 1000;
        final Counter counter = new Counter();
        final Thread[] thread = new Thread[NTHREADS];

        for (int i = 0; i < NTHREADS; i++) {
            thread[i] = new Thread() {
                public void run() {
                    for (int j = 0; j < NINCREMENTS; j++)
//                        counter.increment1();
//                        counter.increment2();
//                    counter.increment3();
                    counter.increment4();
                }
            };
            thread[i].start();
        }
        for (Thread t : thread)
            t.join();
        System.out.println(counter + " vs " + NTHREADS * NINCREMENTS); // 4168 vs 5000
    }
}
