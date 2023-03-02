package io.consoles.ch8.exercises;

import java.util.ArrayList;
import java.util.List;

/**
 * 对象级别并发：最容易实现的方案。每个实例有一个锁，所有方法都通过这个锁同步
 * 性能更高的并发策略：禁止并发访问同一个索引，并允许其他操作并发执行，具体说明如下：
 * 1. 相同索引上的 set 操作必须是串行的
 * 2. 两个对 swap 方法的调用如果共享一个以上的索引，则必须是串行的
 * 3. 调用 swap(i, j) 和调用 i 或者 j 的 set 方法是串行的
 * 4. 其他所有操作允许并行
 *
 * @param <T>
 */
public class Repository<T> {

    private final List<T> elems;
    private final List<Object> monitors;

    /**
     * 创建一个包含 n 个单元格的 Repository 实例，元素的初始值为 null
     *
     * @param n
     */
    public Repository(int n) {
        elems = new ArrayList<>(n);
        monitors = new ArrayList<>(n);
        for (int i = 0; i < n; i++) {
            elems.add(null);
            monitors.add(new Object());
        }
    }

    /**
     * 将第 i 个单元格的值更新为 elem，并返回旧的值（或者为 null）
     *
     * @param i
     * @param elem
     * @return
     */
    public T set(int i, T elem) {
        synchronized (monitors.get(i)) {
            return elems.set(i, elem);
        }
    }

    /**
     * 交换第 i 和 第 j 个单元格的值
     * 按照从小到大的顺序获取监视器，避免死锁
     *
     * @param i
     * @param j
     */
    public void swap(int i, int j) {
        if (i == j) return;
        if (i > j) {
            int temp = i;
            i = j;
            j = temp;
        }
        synchronized (monitors.get(i)) {
            synchronized (monitors.get(j)) {
                // 骚操作: List.set 返回当前索引处的旧值
                elems.set(i, elems.set(j, elems.get(i)));
            }
        }
    }
}
