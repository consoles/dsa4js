package test

class ClassifierMemoized {
    def static dividesBy = { a, b -> a % b == 0 }
    def static isFactor = dividesBy.memoize()

    def static factorsOf(number) {
        (1..number).findAll { i -> isFactor.call(number, i) }
    }

    def static sumFactors = { number -> 
        factorsOf(number).inject(0) { sum, i -> sum + i }
    }
    // 可以指定最大缓存数量，让语言本身替我们管理缓存
    def static sumOfFactors = sumFactors.memoizeAtMost(1000)

    def static isPrefect(number) {
        def success = sumFactors(number) == 2 * number
        // println "number: $number, success: $success"
        return success
    }

    def static isPrefectWithMemoize(number) {
        sumOfFactors(number) == 2 * number
    }
}

class Timer {
    long startTime
    String label

    def start(label) {
        this.label = label
        this.startTime = System.currentTimeMillis()
    }

    def stop() {
        def endTime = System.currentTimeMillis()
        println "$label: ${endTime - this.startTime} ms"
    }
}

range = 1..1000

timer = new Timer()
timer.start("寻找1-1000之间的完美数(不使用缓存)#1")
range.each { ClassifierMemoized.isPrefect(it) }
timer.stop()
timer.start("寻找1-1000之间的完美数(不使用缓存)#2")
range.each { ClassifierMemoized.isPrefect(it) }
timer.stop()

timer.start("寻找1-1000之间的完美数(使用缓存)#1")
range.each { ClassifierMemoized.isPrefectWithMemoize(it) }
timer.stop()
timer.start("寻找1-1000之间的完美数(使用缓存)#2")
range.each { ClassifierMemoized.isPrefectWithMemoize(it) }
timer.stop()

// 寻找1-1000之间的完美数(不使用缓存)#1: 867 ms
// 寻找1-1000之间的完美数(不使用缓存)#2: 535 ms
// 寻找1-1000之间的完美数(使用缓存)#1: 292 ms
// 寻找1-1000之间的完美数(使用缓存)#2: 4 ms
