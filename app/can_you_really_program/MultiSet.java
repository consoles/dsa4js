import java.util.ArrayList;
import java.util.List;

/**
 * 多重集（可包含重复元素的 Set）
 */
public class MultiSet<T> {
    private List<T> data = new ArrayList<>();

    public void add(T elem) {
        data.add(elem);
    }

    public long count(T elem) {
//        long count = 0;
//        for (T other : data) {
//            if (other.equals(elem)) {
//                count++;
//            }
//        }
//        return count;
        return data.stream().filter(x -> x.equals(elem)).count();
    }

    public static void main(String[] args) {
        MultiSetWithManyDuplicates<String> set = new MultiSetWithManyDuplicates<>();
        set.add("a");
        set.add("a");
        set.add("b");
        System.out.println(set.count("a"));
    }
}

/**
 * 重复元素多的多重集
 * 为了节省内存，使用 2 个数组，一个用来存储元素本身，另一个用来存储元素数量
 */
class MultiSetWithManyDuplicates<T> {
    private List<T> elements = new ArrayList<>();
    private List<Long> replications = new ArrayList<>();

    public void add(T elem) {
        if (elements.contains(elem)) {
            int index = elements.indexOf(elem);
            replications.set(index, replications.get(index) + 1);
        } else {
            elements.add(elem);
            replications.add(1L);
        }
    }

    public long count(T elem) {
        if (!elements.contains(elem)) {
            return 0;
        }
        int index = elements.indexOf(elem);
        return replications.get(index);
    }
}
