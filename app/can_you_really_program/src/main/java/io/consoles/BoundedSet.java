import java.util.*;

public class BoundedSet<T> {
    private final LinkedList<T> data;
    private final int capacity;

    public BoundedSet(int capacity) {
        this.data = new LinkedList<>();
        this.capacity = capacity;
    }

    /**
     * 副本构造函数，用来复制类
     *
     * @param other
     */
    public BoundedSet(BoundedSet<T> other) {
        data = new LinkedList<>(other.data);
        capacity = other.capacity;
    }

    /**
     * 将指定元素添加到有界集。如果添加操作使 set 大小超出其容量，
     * 则删除其中最旧的元素（最先插入的元素）。添加已经属于该 set 的元素会对其进行更新
     * （也就是说，该元素会成为 set 中最新的元素）
     *
     * @param elem
     */
    public void add(T elem) {
        BoundedSet<T> copy = null;
        assert (copy = new BoundedSet<>(this)) != null; // 虚拟断言

        if (elem == null) {
            throw new NullPointerException();
        }
        data.remove(elem);
        if (data.size() == capacity) {
            data.removeFirst(); // 删除最先插入的元素
        }
        data.addLast(elem); // 添加元素，使其成为最新的元素
        assert postAdd(copy, elem) : "add failed its postcondition!";
        assert checkInvariants() : "add broke an invariant!";
    }

    public boolean contains(T elem) {
        return data.contains(elem);
    }

    private boolean postAdd(BoundedSet<T> oldSet, T newElement) {
        if (!data.getLast().equals(newElement)) { // newElement 必须在最前面
            return false;
        }
        // 从旧 set 和新 set 中删除 newElement
        List<T> copyOfCurrent = new ArrayList<>(data);
        copyOfCurrent.remove(newElement);
        oldSet.data.remove(newElement);
        if (oldSet.data.size() == capacity) {
            oldSet.data.removeFirst(); // 如果 set 满了，则删除最旧的元素
        }
        return oldSet.data.equals(copyOfCurrent); // 所有元素都应该是相同的并且顺序也相同
    }

    private boolean checkInvariants() {
        if (data.size() > capacity) { // 不变式1：data 字段（是一个列表）长度不能超过 capacity
            return false;
        }
        // 不变式2：data 字段（是一个列表）不能包含重复的元素
        Set<T> elements = new HashSet<>();
        for (T element : data) {
            if (!elements.add(element)) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {

    }
}
