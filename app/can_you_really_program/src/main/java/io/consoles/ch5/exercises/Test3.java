package io.consoles.ch5.exercises;

/**
 * Javadoc 提到不能在构造函数执行期间修改 val 字段。在多线程上下文中，程序在执行构造函数的时候可能还会执行其他代码。因此，此规范并不完全适合本章介绍的经典契约形式，本章讨论的契约专注于顺序执行的程序。
 * 另外，构造函数的源代码还隐式地假设数组 val 不能为 null 且不能为空，从而得到以下契约：
 *
 *  前置条件：val 不能为 null，且不能为空。
 *  后置条件：会创建一个 BigInteger 类的实例，表示一个值为 val 的整数，采用二进制补码的大端（big-endian）编码格式。
 *  惩罚：抛出以下异常。
 *      ■ NullPointerException：如果 val 为 null。
 *      ■ NumberFormatException：如果 val 为空（长度为 0）。
 *
 * parseInt 方法前面的注释声明了“假设 start < end”这个明确的前置条件。
 * 在方法的主体代码中，你还将注意到 source 参数不能为 null，并且 start 和 end 必须为 source 中的有效索引。
 * 最后，在指定区间内 source 参数的字符串值中每个字符都必须是有效数字。可以将这些结果以方法契约的形式表示如下。
 *
 *  前置条件：source 不能为 null，且由数字字符组成；start 和 end 是 source 的有效索引，并且 start < end。
 *  后置条件：返回 start 和 end 两个索引之间的数字所表示的整数。
 *  惩罚：抛出以下异常。
 *      ■ NullPointerException：如果 source 是 null。
 *      ■ NumberFormatException：如果在指定区间内有任意一个字符是非数字。
 *      ■ ArrayIndexOutOfBoundsException：如果 start 或 end 不是 source 的一个有效索引。
 */
public class Test3 {
    public static void main(String[] args) {

    }
}
