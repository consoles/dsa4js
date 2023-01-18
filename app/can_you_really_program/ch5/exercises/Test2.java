package ch5.exercises;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * 根据以下契约，实现静态方法 interleaveLists。
 * <p>
 *  前置条件：该方法接收两个长度相同的 List 作为参数。
 *  后置条件：该方法返回一个新的 List，该 List 以交替的方式包含两个列表中的所有元素。
 *  惩罚：如果两个列表中至少有一个为 null，则该方法抛出 NullPointerException 异常。如果两个列表的长度不同，则该方法抛出 IllegalArgumentException 异常。
 * <p>
 * 确保始终检查前置条件，并且仅在开启断言的情况下才检查后置条件。关闭断言时，尽可能降低性能开销。
 * <p>
 * java -ea Test2.java
 */
public class Test2<E> {

    private static boolean postCheck(List<?> list1, List<?> list2, List<?> list) {
        if (list.size() != list1.size() + list2.size()) {
            return false;
        }
        if (list == list1 || list == list2) {
            return false;
        }
        boolean checkList1 = true;
        Iterator<?> it1 = list1.iterator(),
            it2 = list2.iterator();
        for (Object e : list) {
            if (checkList1 && e != it1.next()) {
                return false;
            }
            if (!checkList1 && e != it2.next()) {
                return false;
            }
            checkList1 = !checkList1;
        }
        return true;
    }

    public static <E> List<E> interleaveLists(List<E> list1, List<E> list2) {
        Objects.requireNonNull(list1, "list1 must not null");
        Objects.requireNonNull(list2, "list2 must not null");
        if (list1.size() != list2.size()) {
            throw new IllegalArgumentException("list1 and list2 must has same size");
        }
        List<E> list = new ArrayList<>(list1.size() + list2.size());
        Iterator<E> it1 = list1.iterator();
        Iterator<E> it2 = list2.iterator();
        while (true) {
            boolean it1HasNext = it1.hasNext();
            boolean it2HasNext = it2.hasNext();
            if (it1HasNext) {
                list1.add(it1.next());
            }
            if (it2HasNext) {
                list2.add(it2.next());
            }
            if (!it1HasNext && !it2HasNext) {
                break;
            }
        }
        return list;
    }

    public static void main(String[] args) {
        System.out.println(Test2.class);
    }
}
