// 对列表增加一个名为 myAverage 的槽，以计算列表中所有数字的平均值。
// 如果列表中没有数字会发生什么？（加分题：如果列表中有任何一项不是数字，则产生一个Io异常。）

List myAverage := method(
    sum := 0
    count := 0
    self foreach(v,
        if(v asNumber isNan, Exception raise("Some item in the list is not Number"))
        sum = sum + v
        count = count + 1
    )
    sum / count
)

list(1,2,3,4) myAverage println // 2.5
list(1,2,3,4,"PHP") myAverage println // 2.5
//   Exception: Some item in the list is not Number
//   ---------
//   Exception raise                      t4.io 8
//   List myAverage                       t4.io 16
//   CLI doFile                           Z_CLI.io 140
//   CLI run                              IoState_runCLI() 1

