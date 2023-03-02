package io.consoles.ch8.exercises;

import java.util.HashSet;
import java.util.Set;

public class SocialUser {
    private final String name;
    private final Set<SocialUser> friends = new HashSet<>();

    public SocialUser(String name) {
        this.name = name;
    }

    // 多个线程同时调用这个方法的时候系统可能会停止运行，必须重启
    // 原因在于 a.beFriend(b), b.beFriend(a) 两个方法可能互相持有对方的锁造成死锁，解决方案是使用有序锁
    public synchronized void beFriend(SocialUser other) {
        friends.add(other);
        synchronized (other) {
            other.friends.add(this);
        }
    }

    public synchronized boolean isFriend(SocialUser other) {
        return friends.contains(other);
    }
}
