import java.util.Arrays;

// Memory4：设法让每个额外容器只使用一个静态数组，每个数组占用一个单元格，但是表示双重含义（索引和水量）
// 表示索引时，数组元素存储的是同一组容器中下一个容器的索引，就像使用链表来存储组一样。
// 对于那些没有下一个容器的容器，它们或者是孤立的，或者就是列表中的最后一个，此时这个数组元素表示水量（包括同一组中所有容器的水量）。

public class Container9 {
    // 把一个正数解释为下一个容器的索引，负数则解释为该容器中水量的相反数
    // nextOrAmount[4] = -2.5 意味着容器4是组中的最后一个（或者是孤立的），并且存着 2.5L 水
    // 0 既是索引，也是一个有效的 amount 值。可以通过假设零是一个 amount， 并且永远不使用 0 作为下一个容器的索引来避免这种歧义。
    // 因为不想牺牲 0 号单元格，所以在数组的所有索引加 1，例如 容器 4 后面是容器 7 ，就会有 nextOrAmount[4] = 8
    private static float[] nextOrAmount = new float[0];

    public static int newContainer() {
        int nContainers = nextOrAmount.length;
        nextOrAmount = Arrays.copyOf(nextOrAmount, nContainers + 1);
        return nContainers;
    }

    public static float getAmount(int containerId) {
        // 查找数组的最后一个
        while (nextOrAmount[containerId] > 0) {
            containerId = (int) (nextOrAmount[containerId]) - 1; // 消除偏离值
        }
        return -nextOrAmount[containerId]; // 恢复正确符号
    }

    public static void addWater(int containerId, float amount) {
        int firstOfGroup = findFirstOfGroup(containerId);
        int[] lastAndSize = findLastOfGroupAndCount(firstOfGroup);
        nextOrAmount[lastAndSize[0]] -= amount / lastAndSize[1];
    }

    public static void connectTo(int containerId1, int containerId2) {
        int first1 = findFirstOfGroup(containerId1),
            first2 = findFirstOfGroup(containerId2);
        if (first1 == first2) return;

        int[] lastAndSize1 = findLastOfGroupAndCount(first1),
            lastAndSize2 = findLastOfGroupAndCount(first2);
        int last1 = lastAndSize1[0],
            last2 = lastAndSize2[0];
        float amount1 = -nextOrAmount[last1],
            amount2 = -nextOrAmount[last2],
            newAmount = (amount1 * lastAndSize1[1] + amount2 * lastAndSize2[1]) / (lastAndSize1[1] + lastAndSize2[1]);

        nextOrAmount[last1] = first2 + 1;
        nextOrAmount[last2] = -newAmount;
    }

    /**
     * O(N^2)
     *
     * @param containerId
     * @return
     */
    private static int findFirstOfGroup(int containerId) {
        int current = containerId, i = 0;
        do {
            for (i = 0; i < nextOrAmount.length; i++) {
                if (nextOrAmount[i] == current + 1) {
                    current = i;
                    break;
                }
            }
        } while (i < nextOrAmount.length);
        return current;
    }

    private static int[] findLastOfGroupAndCount(int containerId) {
        int[] result = new int[]{containerId, 1};
        while (nextOrAmount[result[0]] > 0) {
            result[0] = (int) nextOrAmount[result[0]] - 1;
            result[1]++;
        }
        return result;
    }

    public static void main(String[] args) {
        int a = Container9.newContainer(),
            b = Container9.newContainer(),
            c = Container9.newContainer(),
            d = Container9.newContainer();

        Container9.addWater(a, 12);
        Container9.addWater(d, 8);
        Container9.connectTo(a, b);

        // a: 6.000000, b: 6.000000, c: 0.000000, d: 8.000000
        System.out.println(String.format("a: %f, b: %f, c: %f, d: %f", Container9.getAmount(a), Container9.getAmount(b), Container9.getAmount(c), Container9.getAmount(d)));
    }
}

// 这种实现牺牲了可读性，并且牺牲了可维护性。对内存效率的追求导致使用更底层的数据结构替代上层集合，并使用特殊编码，以至于在这个实现中使用 float 作为数据索引。
// 这种方案只会在一些内存严重受限的场景中会用到这些技术,例如嵌入式
