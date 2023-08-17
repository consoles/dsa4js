class PLazyList {
    private Closure list

    // 私有构造器
    private PLazyList(list) {
        this.list = list
    }

    // 生成一个空列表，传入一个空列表作为起始
    static PLazyList nil() {
        new PLazyList({-> []})
    }

    // 在头部添加元素，然后将添加结果包装进一个闭包
    // 这是一个 prepend 的操作
    PLazyList cons(head) {
        new PLazyList({-> [head, list]})
    }

    // 返回列表的第一个元素
    def head() {
        def lst = list.call() // 闭包调用，返回一个列表
        lst ? lst[0] : null
    }

    // 返回列表剩下的部分（除第一个元素之外的子列表）
    def tail() {
        def lst = list.call()
        lst ? new PLazyList(lst.tail()[0]) : nil()
    }

    boolean isEmpty() {
        list.call() == []
    }

    def fold(n, acc, f) {
        n == 0 || isEmpty() ? acc : tail().fold(n - 1, f.call(acc, head()), f)
    }

    def foldAll(acc, f) {
        isEmpty() ? acc : tail().foldAll(f.call(acc, head()), f)
    }

    def take(n) {
        fold(n, []) {acc, item -> acc << item}
    }

    def takeAll() {
        foldAll([]) {acc, item -> acc << item}
    }

    def toList() {
        takeAll()
    }
}

def lazylist = PLazyList.nil().cons(4).cons(3).cons(2).cons(1)
println lazylist.takeAll() // [1, 2, 3, 4]
println lazylist.foldAll(0, {i, j -> i + j}) // 10
lazylist = PLazyList.nil().cons(1).cons(2).cons(4).cons(8)
// 求解前 2 个元素
println lazylist.take(2) // [8, 4]
