package io.consoles.ch8.exercises;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public class SocialUserNoDeadLock {
    private final String name;
    private final Set<SocialUserNoDeadLock> friends = new HashSet<>();
    private final int id;
    private static final AtomicInteger instanceCounter = new AtomicInteger();

    public SocialUserNoDeadLock(String name) {
        this.name = name;
        this.id = instanceCounter.incrementAndGet();
    }

    // 使用有序锁技术来解决死锁问题
    public void beFriend(SocialUserNoDeadLock other) {
        SocialUserNoDeadLock firstMonitor, secondMonitor;
        if (id < other.id) {
            firstMonitor = this;
            secondMonitor = other;
        } else {
            firstMonitor = other;
            secondMonitor = this;
        }
        synchronized (firstMonitor) {
            synchronized (secondMonitor) {
                friends.add(other);
                other.friends.add(this);
            }
        }
    }

    public synchronized boolean isFriend(SocialUserNoDeadLock other) {
        return friends.contains(other);
    }
}
