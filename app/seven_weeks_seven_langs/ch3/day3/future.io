futureResult := URL with("http://www.baidu.com") @fetch
writeln("do something else when fetch http response")
// 阻塞
writeln("fetched", futureResult size, " bytes.")
futureResult println
