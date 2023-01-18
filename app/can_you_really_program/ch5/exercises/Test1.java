package ch5.exercises;

/**
 * 写下 java.util.Collection 接口中 add 方法的契约。
 *
 * 与具体（非抽象）方法相比，抽象方法的契约往往包含更多内容。抽象方法没有具体的
 * 实现，基本上是纯粹的契约，因此其契约需要清晰明了。在类似于 Collection 这样的接口中，
 * 情况甚至更加明显，它是集合类继承结构的根节点，必须满足各种各样的特殊要求（精确地说，
 * 有 34 个类和接口）。
 *
 * Collection.add 方法的 Javadoc 包含大量信息。从限定符“可选操作”开始。可以将其理
 * 解为此方法包括两个互斥的契约。首先，Collection 接口的具体实现可以选择不支持插入，例
 * 如不可变集合。在这种情况下，它包含以下契约。
 *
 *  前置条件：任何调用都是有效的。
 *  后置条件：无。
 *  惩罚：抛出 UnsupportedOperationException 异常
 *
 * 其次，如果 Collection 接口的实现类支持插入，则它必须遵守其他一些契约。实现类可
 * 以自由地选择 add 方法的前置条件，来保证插入的元素类型是有效的，如以下契约所述，它在
 * 拒绝插入时必须引发特定的惩罚。
 *
 *  前置条件：由接口的具体实现类来定义。
 *  后置条件：确保此集合包含指定的元素。如果调用 add 方法实际影响了此集合，则返回 true。
 *  惩罚：抛出异常。
 *      ■ ClassCastException：如果参数的类型无效。
 *      ■ NullPointerException：如果参数为 null，并且此集合拒绝 null 值。
 *      ■ IllegalArgumentException：如果参数由于一些其他特性而无效。
 *      ■ IllegalStateException：如果此时无法插入参数。
 * 请注意，该契约未指定在哪些情况下插入操作会更改底层的集合。这些工作由子类来承担。
 */

/**
 * 写下 java.util.HashSet 接口中 add 方法的契约。
 *
 *  前置条件：无。（所有参数都是有效的。）
 *  后置条件：将指定元素插入此集合，除非已经存在与其相同的元素（根据 equals 判断）。如果此集合在调用前不包含指定的元素，则返回 true。
 *  惩罚：无。
 */

/**
 * java.util.Collection 和 java.util.HashSet 中 add 方法契约的不同：
 *
 * HashSet 类的契约指定了集合不能包含重复元素。尝试插入一个重复元素不会产生错误：它不违反前置条件，也不会抛出异常。它不会对集合产生任何影响。
 */
public class Test1 {
}
