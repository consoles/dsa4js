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

    public double getAmount() {
        synchronized (group) {
            return group.amountPerContainer;
        }
    }

    public void addWater(double amount) {
        while (true) {
            Object monitor = group;
            synchronized (monitor) {
                if (monitor == group) {
                    double amountPerContainer = amount / group.members.size();
                    group.amountPerContainer += amountPerContainer;
                    return;
                }
            }
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
