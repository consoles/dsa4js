import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

// 需要使用命令行参数开启断言
// java -ea Container12.java
public class Container12 {

    /**
     * 保存后置条件检查需要使用的数据
     */
    private static class ConnectPostData {
        Set<Container12> group1, group2;
        double amount1, amount2;
    }

    private Set<Container12> group;
    private double amount;

    public Container12() {
        this.group = new HashSet<>();
        this.group.add(this);
        this.amount = 0;
    }

    private boolean invariantsArePreservedByAddWater() {
        return isGroupNonNegative() && isGroupBalanced();
    }

    public void addWater(double amount) {
        double amountPerContainer = amount / this.group.size();
        if (this.amount + amountPerContainer < 0) {
            throw new IllegalArgumentException("Not enough water to match the addWater request.");
        }
        double oldTotal = 0; // 保存后置条件数据
        assert (oldTotal = groupAmount()) >= 0; // 虚拟断言
        for (Container12 c : group) {
            c.amount += amountPerContainer;
        }
        assert invariantsArePreservedByAddWater() : "addWater broke an invariant!"; // 检查不变式
    }

    /**
     * 不变式1： 每个容器的 amount 字段均为非负值
     *
     * @return
     */
    private boolean isGroupNonNegative() {
        for (Container12 c : group) {
            if (c.amount < 0) return false;
        }
        return true;
    }

    /**
     * 不变式2 && 不变式3: 每个容器都只属于一个组 && 每个容器的 group 字段都不为 null，并且都指向一个包含 this 的组
     *
     * @return
     */
    private boolean isGroupConsistent() {
        for (Container12 c : group) {
            if (c.group != group) return false;
        }
        return true;
    }

    /**
     * 检查不变式
     *
     * @param other
     * @return
     */
    private boolean invariantsArePreservedByConnectTo(Container12 other) {
        return group == other.group
            && isGroupNonNegative()
            && isGroupBalanced()
            && isGroupConsistent();
    }

    public void connectTo(Container12 other) {
        Objects.requireNonNull(other, "Cannot connect to a null container"); // 检查前置条件
        if (group == other.group) return;

        int size1 = group.size(),
            size2 = other.group.size();
        double to1 = amount * size1,
            to2 = other.amount * size2;
        double newAmount = (to1 + to2) / (size1 + size2);
        for (Container12 c : other.group) {
            group.add(c); // idea 报错：Cannot access Container10
        }
        for (Container12 c : other.group) {
            c.group = group;
        }
        for (Container12 c : group) {
            c.amount = newAmount;
        }
        assert invariantsArePreservedByConnectTo(other) : "connectTo broke an invariant!"; // 检查不变式
    }

    /**
     * 容器组中的总水量
     *
     * @return
     */
    private double groupAmount() {
        double total = 0;
        for (Container12 c : group) {
            total += c.amount;
        }
        return total;
    }

    /**
     * 不变式4: 同一组中的所有容器有相同的 amount 值
     *
     * @return
     */
    private boolean isGroupBalanced() {
        for (Container12 c : group) {
            if (c.amount != amount) return false;
        }
        return true;
    }

    private boolean postAddWater(double oldTotal, double addedAmount) {
        return isGroupBalanced() && almostEqual(groupAmount(), oldTotal + addedAmount);
    }

    private static boolean almostEqual(double x, double y) {
        final double EPSILON = 1e-4; // 允许误差
        return Math.abs(x - y) < EPSILON;
    }


    public static void main(String[] args) {
        Container12 a = new Container12(),
            b = new Container12(),
            c = new Container12();
        a.connectTo(b);
        b.connectTo(c);
        a.addWater(0.9);
    }
}
