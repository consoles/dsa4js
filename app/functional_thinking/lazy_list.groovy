class LazyList {
    private head, tail

    LazyList(head, tail) {
        this.head = head
        this.tail = tail
    }

    def LazyList getTail() {
        tail ? tail() : null
    }

    def List getHead(n) {
        def harvestedValues = []
        def current = this
        n.times {
            harvestedValues << current.head
            current = current.tail
        }
        harvestedValues
    }

    def LazyList filter(Closure p) {
        if (p(head))
            p.owner.prepend(head, { getTail().filter(p) })
        else
            getTail().filter(p)    
    }
}

def prepend(val, closure) { 
    new LazyList(val, closure) 
}

def integers(n) { 
    prepend(n, { integers(n + 1) })
}

def natutalNumbers = integers(1)
def evenNumbers = natutalNumbers.filter { it % 2 == 0 }

println natutalNumbers.getHead(10).join(',')
println evenNumbers.getHead(10).join(',')

println "推迟初始化的完美数的缓求值列表"

enum NumberClassfication {
    PREFECT,
    ABUNDANT,
    DEFICIENT
}

class NumberClassifier {
    static def factorsOf(number) {
        (1..number).findAll { i -> number % i == 0 }
    }

    static def classify(number) {
        switch(factorsOf(number).inject(0, { sum, i -> sum + i })) {
            case { it < 2 * number }: return NumberClassfication.DEFICIENT
            case { it == 2 * number }: return NumberClassfication.PREFECT
            case { it > 2 * number }: return NumberClassfication.ABUNDANT
        }
    }

    static def isPrefect(number) {
        classify(number) == NumberClassfication.PREFECT
    }

    static def nextPrefectNumber(n) {
        while (!isPrefect(++n));
        n
    }
}

def prefectNumbersLazy(n) {
    prepend(n, { prefectNumbersLazy(NumberClassifier.nextPrefectNumber(n)) })
}

println NumberClassifier.nextPrefectNumber(1) // 6
println NumberClassifier.nextPrefectNumber(6+1) // 28
println NumberClassifier.nextPrefectNumber(28+1) // 496

def p = prefectNumbersLazy(NumberClassifier.nextPrefectNumber(1))
println p.getHead(3) // [6, 28, 496]
