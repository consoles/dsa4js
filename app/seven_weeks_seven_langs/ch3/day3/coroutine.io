vizzini := Object clone
vizzini talk := method(
    "Fezzik, are there rocks ahead?" println
    yield // 控制权转交
    "No more rhymes now, I mean it." println
    yield
)

fezzik := Object clone
fezzik rhyme := method(
    yield
    "If there are, we'll all be dead." println
    yield
    "Anybody want a peanut?" println
)

// @@ 表示异步触发，这里并发执行 talk 和 rhyme
vizzini @@talk; fezzik @@rhyme
Coroutine currentCoroutine pause
// $ io coroutine.io 
// Fezzik, are there rocks ahead?
// If there are, we'll all be dead.
// No more rhymes now, I mean it.
// Anybody want a peanut?
// Scheduler: nothing left to resume so we are exiting
