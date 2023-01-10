import java.util.Arrays;

// Memory3：摒弃 OOP 设计思想
// 不为每个容器专门设置一个对象，而是让客户端使用一个整型的 id 来标识每个容器
public class container8 {
  class Container {
    private static int group[] = new int[0]; // 容器 ID 映射到它的组, [1, 1] 表示 id0, id1 的两个容器相连接
    private static float amount[] = new float[0]; // 从组 id 映射到容器水量

    /**
     * 工厂方法，返回容器 id
     * @return
     */
    public static int newContainer() {
      int nContainers = group.length,
        nGroups = amount.length;
      amount = Arrays.copyOf(amount, nGroups + 1); // 在 amount 后面补 0
      group = Arrays.copyOf(group, nContainers + 1);
      group[nContainers] = nGroups; // 设置新容器的组 id
      return nContainers;
    }

    public static float getAmount(int containerId) {
      int groupId = group[containerId];
      return amount[groupId];
    }

    public static void addWater(int containerId, float amount) {
      int groupId = group[containerId],
        groupSize = groupSize(groupId);
      Container.amount[groupId] += amount / groupSize;
    }

    public static void connectTo(int containerId1, int containerId2) {
      int groupId1 = group[containerId1],
        groupId2 = group[containerId2],
        size1 = groupSize(groupId1),
        size2 = groupSize(groupId2);

      if (groupId1 == groupId2) return;

      float amount1 = amount[groupId1] * size1,
        amount2 = amount[groupId2] * size2;

      amount[groupId1] = (amount1 + amount2) / (size1 + size2);

      for (int i = 0; i < group.length; i++) {
        if (group[i] == groupId2) {
          group[i] = groupId1;
        }
      }
      // 没有任何容器属于 groupId2 了
      removeGroupAndDefrag(groupId2);
    }

    /**
     * defrag 即碎片整理(defragmenttation),指的是文件系统维护操作,用来确保文件占用连续的空间
     *
     * @param groupId
     */
    private static void removeGroupAndDefrag(int groupId) {
      for (int containerId = 0; containerId < group.length; containerId++) {
        // 将 groupId 分配给之前与组 n-1 相关联的所有容器
        if (group[containerId] == amount.length - 1) {
          group[containerId] = groupId;
        }
      }
      amount[groupId] = amount[amount.length - 1]; // 将旧组 n-1 的 amount 复制到组 groupId
      amount = Arrays.copyOf(amount, amount.length - 1); // 舍弃最后一个单元格，删除组 n-1，组 id 的范围变成了 [0,n-2]
    }

    private static int groupSize(int groupId) {
//    int size = 0;
//    for (int otherGroupId : group) {
//      if (otherGroupId == groupId) {
//        size++;
//      }
//    }
//    return size;

//    可以使用 java8 的 stream 库简化代码
      return (int) Arrays.stream(group).filter(otherGroupId -> otherGroupId == groupId).count();
    }
  }

  public static void main(String[] args) {
    int a = Container.newContainer(),
      b = Container.newContainer(),
      c = Container.newContainer(),
      d = Container.newContainer();

    Container.addWater(a, 12);
    Container.addWater(d, 8);
    Container.connectTo(a, b);

    // a: 6.000000, b: 6.000000, c: 0.000000, d: 8.000000
    System.out.println(String.format("a: %f, b: %f, c: %f, d: %f", Container.getAmount(a), Container.getAmount(b), Container.getAmount(c), Container.getAmount(d)));
  }
}
