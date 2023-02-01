import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 时间占用小的版本
 * <p>
 * 将元素同时存储在 2 个数据结构中实现
 * list 用于快速检索，set 用于检查是否存在该元素
 *
 * @param <E>
 */
class UniqueListFast<E> {
    private final List<E> dataByIndex;
    private final Set<E> dataSet;

    public UniqueListFast(int capacity) {
        dataByIndex = new ArrayList<>(capacity);
        dataSet = new HashSet<>(capacity);
        for (int i = 0; i < capacity; i++) {
            dataByIndex.add(null);
        }
    }

    public boolean set(int index, E element) {
        if (index < 0 || index >= dataByIndex.size()) {
            return false;
        }
        if (dataSet.contains(element)) {
            return false;
        }
        E old = dataByIndex.set(index, element);
        dataSet.remove(old);
        dataSet.add(element);
        return true;
    }

    public E get(int index) {
        if (index < 0 || index >= dataByIndex.size()) {
            return null;
        }
        return dataByIndex.get(index);
    }
}

/**
 * 一个固定大小的有索引列表，且没有重复元素
 * 空间占用小的版本
 *
 * @param <E>
 */
public class UniqueList<E> {
    private final List<E> list; // 使用普通数组来取代这个实现，空间效率并没有得到有效提高

    public UniqueList(int capacity) {
        list = new ArrayList<>(capacity); // 指定初始容量，后续 add 就不需要扩容了
        for (int i = 0; i < capacity; i++) {
            list.add(null);
        }
    }

    /**
     * 在给定的索引上插入指定元素，返回 true，前提是索引范围在 [0,capacity) 上，并且该元素没有出现在其他索引上。否则它不改变列表并返回 false
     *
     * @param index
     * @param element
     * @return
     */
    public boolean set(int index, E element) {
        if (index < 0 || index >= list.size()) {
            return false;
        }
        if (list.contains(element)) {
            return false;
        }
        list.set(index, element);
        return true;
    }

    /**
     * 返回给定索引处的元素，如果索引元素无效或者为空（未分配），则返回 null
     *
     * @param index
     * @return
     */
    public E get(int index) {
        if (index < 0 || index >= list.size()) {
            return null;
        }
        return list.get(index);
    }

    public static void main(String[] args) {
        UniqueList<String> list = new UniqueList<>(5);
        boolean success = list.set(1, "a");
        System.out.println(success);
        success = list.set(1, "a");
        System.out.println(success);
        success = list.set(2, "b");
        System.out.println(success);
        success = list.set(3, "c");
        System.out.println(success);
        success = list.set(4, "d");
        System.out.println(success);
        success = list.set(5, "e");
        System.out.println(success);
        success = list.set(0, "f");
        System.out.println(success);
    }
}
