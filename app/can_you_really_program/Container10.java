import java.util.HashSet;
import java.util.Set;

// 需要使用命令行参数开启断言
// java -ea Container10.java
public class Container10 {
    private Set<Container10> group;
    private double amount;

    public Container10() {
        this.group = new HashSet<>();
        this.group.add(this);
        this.amount = 0;
    }

    public void addWater(double amount) {
        double amountPerContainer = amount / this.group.size();
        if (this.amount + amountPerContainer < 0) {
            throw new IllegalArgumentException("Not enough water to match the addWater request.");
        }
        double oldTotal = 0; // 保存后置条件数据
        assert (oldTotal = groupAmount()) >= 0; // 虚拟断言
        for (Container10 c : group) {
            c.amount += amountPerContainer;
        }
        assert postAddWater(oldTotal, amount) : "addWater failed its postcondition!";
    }

    public void connectTo(Container10 other) {
        if (group == other.group) return;
        int size1 = group.size(),
            size2 = other.group.size();
        double to1 = amount * size1,
            to2 = other.amount * size2;
        double newAmount = (to1 + to2) / (size1 + size2);
        for (Container10 c : other.group) {
            group.add(c); // idea 报错：Cannot access Container10
        }
        for (Container10 c : other.group) {
            c.group = group;
        }
        for (Container10 c : group) {
            c.amount = newAmount;
        }
    }

    /**
     * 容器组中的总水量
     *
     * @return
     */
    private double groupAmount() {
        double total = 0;
        for (Container10 c : group) {
            total += c.amount;
        }
        return total;
    }

    private boolean postAddWater(double oldTotal, double addedAmount) {
//        return isGroupBalanced() &&
//            groupAmount() == oldTotal + addedAmount; // 双精度浮点数的精确比较

        return isGroupBalanced() && almostEqual(groupAmount(), oldTotal + addedAmount);
    }

    private static boolean almostEqual(double x, double y) {
        final double EPSILON = 1e-4; // 允许误差
        return Math.abs(x - y) < EPSILON;
    }

    /**
     * 检查组中所有容器的水量相同
     *
     * @return
     */
    private boolean isGroupBalanced() {
        for (Container10 c : group) {
            if (c.amount != amount) return false;
        }
        return true;
    }

    public static void main(String[] args) {
        Container10 a = new Container10(),
            b = new Container10(),
            c = new Container10();
        a.connectTo(b);
        b.connectTo(c);
        a.addWater(0.9);
        System.out.println(0.3 + 0.3 + 0.3); // 0.8999999999999999
    }
}
