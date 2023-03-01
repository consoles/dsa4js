package io.consoles.ch8;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public class Container {
    private Group group = new Group(this);

    private static class Group {
        static final AtomicInteger nGroups = new AtomicInteger();
        // group ids ensure consistent ordering and avoid deadlocks
        final int id = nGroups.incrementAndGet();
        double amountPerContainer;
        Set<Container> members;

        Group(Container c) {
            members = new HashSet<>();
            members.add(c);
        }
    }

    // 简单读取一个 double 类型的值并不是原子操作
    // 原因是读取 64 位的操作可能被分成读取 2 个读取 32 的操作，而这两个读操作可能会与另一个线程的写操作交错运行。
    // 如果没有同步块，可能最终读取到一个荒谬的值（高 32 位是新的，低 32 位是旧的，或者相反）
    // 给 amountPerContainer 加上 volatile 关键字能解决这个问题，因为它能使读操作成为原子性
    public double getAmount() {
        synchronized (group) {
            return group.amountPerContainer;
        }
    }

    public void addWater(double amount) {
        while (true) {
            Object monitor = group;
            // 尝试获取监视器
            synchronized (monitor) {
                // 确保监视器是最新的
                if (monitor == group) {
                    double amountPerContainer = amount / group.members.size();
                    group.amountPerContainer += amountPerContainer;
                    return;
                }
            }
            // 如果监视器过时了就重试
        }
    }

    /**
     * 这是一种乐观的同步方法：假设没有其他线程操作这两个容器，如果假设不成立就进行重试
     * @param other
     */
    public void connectTo(Container other) {
        while (true) {
            Object firstMonitor = group.id < other.group.id ? group : other.group,
                secondMonitor = group.id < other.group.id ? other.group : group;
            // check if they are already connected
            if (firstMonitor == secondMonitor) return;

            // 尝试获取监视器
            synchronized (firstMonitor) {
                synchronized (secondMonitor) {
                    if ((firstMonitor == group && secondMonitor == other.group) ||
                        (secondMonitor == group && firstMonitor == other.group)) {
                        int size1 = group.members.size(),
                            size2 = other.group.members.size();
                        double to1 = group.amountPerContainer * size1,
                            to2 = other.group.amountPerContainer * size2,
                            newAmount = (to1 + to2) / (size1 + size2);
                        group.members.addAll(other.group.members);
                        group.amountPerContainer = newAmount;
                        for (Container x : other.group.members) {
                            x.group = group;
                        }
                        return;
                    }
                }
            }
            // 如果获取到的两个监视器至少有一个是过时的，就重试
        }
    }

    public static void main(String[] args) {
        Container a = new Container(),
            b = new Container(),
            c = new Container(),
            d = new Container();
        a.addWater(12);
        d.addWater(8);
        a.connectTo(b);
        b.connectTo(c);
        b.connectTo(d);
        System.out.println(String.format("a:%f,b:%f,c:%f,d:%f", a.getAmount(), b.getAmount(), c.getAmount(), d.getAmount()));
    }
}
