// doMessage 类似于 eval 
// doMessage 可以执行任何消息，Io 会对消息参数进行解释，但是会延迟绑定和执行。
unless := method(
    (call sender doMessage(call message argAt(0))) ifFalse(
    call sender doMessage(call message argAt(1))) ifTrue(
    call sender doMessage(call message argAt(2))
    )
)

unless(1 == 2, write("one is not two\n"), write("one is two\n")) // one is not two
