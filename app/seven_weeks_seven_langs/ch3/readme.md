- 通过复制其他对象来制造对象
- 对象是一组槽
- 通过发送消息来获取槽的值

只需要和对象打交道（没有类的概念），必要的时候将对象复制一下就行，这些被复制的对象就叫做原型。

对象不过是槽的容器而已，发送槽名给对象可以获得该槽。如果该槽不存在，则调用父对象的槽

大写字母开头的对象是类型，Io 会为它设置 type 槽，而类型的复制品如果以小写字母开头则会调用其父对象的 type 槽。类型仅仅是帮助程序员更好组织代码的工具。`bruce := Person clone` 这条语句从 Person 原型创建一个名为 bruce 的复制品。Person 和 bruce 都是对象，Person 还有类型，因为它有 type 槽，其他方面 Person 和 bruce 完全相同。

方法也是对象，可以赋值给槽。

原型编程这种泛型的几条基本原则：
- 所有事务都是对象
- 所有对象的交互都是消息
- 要做的不是实例化类，而是复制哪些叫做原型的对象
- 对象会记住它的原型
- 对象有槽
- 槽包含对象（包括方法对象）
- 消息返回槽中的值，或者调用槽中的方法
- 如果对象无法响应某消息，则它会把该消息发送给自己的原型

消息的组成部分：发送者（sender），目标（target），参数（arguments），可以用 `call` 方法访问所有消息的元信息。
