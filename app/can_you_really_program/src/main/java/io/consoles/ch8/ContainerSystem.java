package io.consoles.ch8;

import java.util.Arrays;

public class ContainerSystem {
    // final 关键字用来 “提醒” 是不可变的
    private final int group[]; // 索引为为容器 ID ，对应的元素为容器的组 ID
    private final double amount[]; // 索引为组 ID, 对应的元素为组中容器的水量

    public ContainerSystem(int containerCount) {
        group = new int[containerCount];
        amount = new double[containerCount];
        for (int i = 0; i < containerCount; i++) {
            group[i] = i; // 第 i 个容器的组 ID 是 i
        }
    }

    private ContainerSystem(ContainerSystem old, int length) {
        // 复制连续的内存块非常快
        group = Arrays.copyOf(old.group, length);
        amount = Arrays.copyOf(old.amount, length);
    }

    /**
     * 系统中的容器数量
     *
     * @return
     */
    public int containerCount() {
        return group.length;
    }

    public ContainerSystem addContainer() {
        final int containerCount = group.length;
        ContainerSystem result = new ContainerSystem(this, containerCount + 1);
        result.group[containerCount] = containerCount;
        return result;
    }

    public double getAmount(int containerID) {
        final int groupID = group[containerID];
        return amount[groupID];
    }

    private int groupSize(int groupID) {
        int size = 0;
        for (int otherGroupID : group) {
            if (otherGroupID == groupID) {
                size++;
            }
        }
        return size;
    }

    public ContainerSystem connect(int containerID1, int containerID2) {
        int groupID1 = group[containerID1],
            groupID2 = group[containerID2];
        if (groupID1 == groupID2) {
            return this;
        }

        ContainerSystem result = new ContainerSystem(this, group.length);
        int size1 = groupSize(groupID1),
            size2 = groupSize(groupID2);
        double amount1 = amount[groupID1] * size1,
            amount2 = amount[groupID2] * size2;
        result.amount[groupID1] = (amount1 + amount2) / (size1 + size2);

        for (int i = 0; i < group.length; i++) {
            if (group[i] == groupID2) {
                result.group[i] = groupID1;
            }
        }
        return result;
    }

    public ContainerSystem addWater(int containerID, double amount) {
        // 不用创建洗系统
        if (amount == 0) {
            return this;
        }
        ContainerSystem result = new ContainerSystem(this, group.length);
        int groupID = group[containerID],
            groupSize = groupSize(groupID);
        result.amount[groupID] += amount / groupSize;
        return result;
    }

    @Override
    public String toString() {
        return String.format("group: \t%s\namount: \t%s", Arrays.toString(group), Arrays.toString(amount));
    }

    public static void main(String[] args) {
        ContainerSystem s1 = new ContainerSystem(10); // 创建一个包含 10 个容器的新系统
        ContainerSystem s2 = s1.addWater(5, 42); // 向索引为 5 （第 6 个）的容器添加 42L 水
        ContainerSystem s3 = s2.addContainer(); // 添加了第 11 个容器
        ContainerSystem s4 = s3.connect(5, 6); // 连接容器 5 和 容器 6
        double amount = s4.getAmount(5);
        System.out.println("amount: " + amount); // 21.0
    }
}
