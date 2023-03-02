package io.consoles.ch8.exercises;

/**
 * 实现这样的并发策略：调用 setBoth 是一个原子操作。也就是说一个现成调用了 setBoth。那么后续任意调用 getFirst 和 getSecond 都会得到 2 个新值
 *
 * 在 3 个方法中都使用同步块，并且使用同一个监视器作为锁。在私有对象上同步比在 this 上同步更好，即使是后置支持使用一个更时髦的方法修饰符来替换同步块
 * @param <S>
 * @param <T>
 */
public class AtomicPair<S, T> {

    private S first;
    private T second;
    private final Object lock = new Object(); // 提供一个私有监视器

    public void setBoth(S first, T second) {
        synchronized (lock) {
            this.first = first;
            this.second = second;
        }
    }

    public S getFirst() {
        synchronized (lock) {
            return this.first;
        }
    }

    public T getSecond() {
        synchronized (lock) {
            return this.second;
        }
    }
}
