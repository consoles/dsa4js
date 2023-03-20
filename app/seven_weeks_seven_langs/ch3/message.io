// 消息
// 邮局
postOffice := Object clone
postOffice packageSender := method(call sender)
// 寄件人
mailer := Object clone
mailer deliver := method(postOffice packageSender) // mailer 有一个 deliver 槽，这个槽发送 packageSender 给 postOffice
mailer deliver println
//  Object_0xffffffff:
//   deliver          = method(...)
postOffice messageTarget := method(call target) // 获取目标
postOffice messageTarget println
//  Object_0xffffffff:
//   messageTarget    = method(...)
//   packageSender    = method(...)
postOffice messageArgs := method(call message arguments)
postOffice messageName := method(call message name)
postOffice messageArgs("one", 2, :three) println // list("one", 2, : three)
postOffice messageName println // messageName
