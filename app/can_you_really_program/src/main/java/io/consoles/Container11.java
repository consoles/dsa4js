import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

// 需要使用命令行参数开启断言
// java -ea Container11.java
public class Container11 {

    /**
     * 保存后置条件检查需要使用的数据
     */
    private static class ConnectPostData {
        Set<Container11> group1, group2;
        double amount1, amount2;
    }

    private Set<Container11> group;
    private double amount;

    public Container11() {
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
        for (Container11 c : group) {
            c.amount += amountPerContainer;
        }
        assert postAddWater(oldTotal, amount) : "addWater failed its postcondition!";
    }

    private ConnectPostData saveConnectPostData(Container11 other) {
        ConnectPostData data = new ConnectPostData();
        data.group1 = new HashSet<>(group);
        data.group2 = new HashSet<>(other.group);
        data.amount1 = amount;
        data.amount2 = other.amount;
        return data;
    }

    private boolean postConnect(ConnectPostData postData) {
        return areGroupMembersCorrect(postData)
            && isGroupAmountCorrect(postData)
            && isGroupBalanced()
            && isGroupConsistent();
    }

    /**
     * 新容器组中的每个容器都指回该组
     *
     * @return
     */
    private boolean isGroupConsistent() {
        for (Container11 c : group) {
            if (c.group != group) return false;
        }
        return true;
    }

    /**
     * 新容器组是两个旧容器组的并集
     *
     * @param postData
     * @return
     */
    private boolean areGroupMembersCorrect(ConnectPostData postData) {
        return group.containsAll(postData.group1)
            && group.containsAll(postData.group2)
            && group.size() == postData.group1.size() + postData.group2.size();
    }

    /**
     * 新容器组中的所有容器都有相同的水量
     *
     * @return
     */
    private boolean isGroupBalanced() {
        for (Container11 c : group) {
            if (c.amount != amount) return false;
        }
        return true;
    }

    /**
     * 新容器组中的总水量和旧容器组中的总水量相等
     *
     * @param postData
     * @return
     */
    private boolean isGroupAmountCorrect(ConnectPostData postData) {
        int size1 = postData.group1.size(),
            size2 = postData.group2.size();
        double to1 = postData.amount1 * size1,
            to2 = postData.amount2 * size2,
            newAmount = (to1 + to2) / (size1 + size2);
        return almostEqual(amount, newAmount);
    }

    public void connectTo(Container11 other) {
        Objects.requireNonNull(other, "Cannot connect to a null container"); // 检查前置条件
        if (group == other.group) return;

        ConnectPostData postData = null; // 准备后置条件所需要的数据
        assert (postData = saveConnectPostData((other))) != null; // 虚拟断言

        int size1 = group.size(),
            size2 = other.group.size();
        double to1 = amount * size1,
            to2 = other.amount * size2;
        double newAmount = (to1 + to2) / (size1 + size2);
        for (Container11 c : other.group) {
            group.add(c); // idea 报错：Cannot access Container10
        }
        for (Container11 c : other.group) {
            c.group = group;
        }
        for (Container11 c : group) {
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
        for (Container11 c : group) {
            total += c.amount;
        }
        return total;
    }

    private boolean postAddWater(double oldTotal, double addedAmount) {
        return isGroupBalanced() && almostEqual(groupAmount(), oldTotal + addedAmount);
    }

    private static boolean almostEqual(double x, double y) {
        final double EPSILON = 1e-4; // 允许误差
        return Math.abs(x - y) < EPSILON;
    }


    public static void main(String[] args) {
        Container11 a = new Container11(),
            b = new Container11(),
            c = new Container11();
        a.connectTo(b);
        b.connectTo(c);
        a.addWater(0.9);
    }
}
