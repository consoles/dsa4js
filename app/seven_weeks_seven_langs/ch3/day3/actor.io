slower := Object clone
faster := Object clone

slower start := method(wait(2);writeln("slowly"))
faster start := method(wait(1);writeln("quickly"))

// slower start;faster start // slowly 先输出，quickly 后输出
// faster start;slower start // quickly 先输出，slowly 后输出

// 发送异步消息给 2 个对象，它们全部变成了 actor
// 在 2 个消息之前加上 @@，让对象在自己的线程中运行，这样做会立即返回 nil,wait 让所有线程执行完
slower @@start;faster @@start;wait(3)
