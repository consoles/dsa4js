'use strict'

/**
 * RingBuffer可以保证无锁情况下的线程安全，读写锁可以保证读写互斥，读读不互斥
 * RingBuffer可用一个数组进行存储，数组内元素的内存地址是连续的，这是对CPU缓存友好的——也就是说，在硬件级别，数组中的元素是会被预加载的，
 * 因此在RingBuffer中，CPU无需时不时去主内存加载数组中的下一个元素。通过对head和tail指针的移动，可以实现数据在数组中的环形存取。
 * 当head==tail时，说明buffer为空，当head==(tail+1)%bufferSize则说明buffer满了。
 *
 * 在进行读操作的时候，我们只修改head的值，而在写操作的时候我们只修改tail的值。
 * 在写操作时，我们在写入内容到buffer之后才修改tail的值；而在进行读操作的时候，我们会读取tail的值并将其赋值给copyTail。
 * 赋值操作是原子操作。所以在读到copyTail之后，从head到copyTail之间一定是有数据可以读的，不会出现数据没有写入就进行读操作的情况。
 * 同样的，读操作完成之后，才会修改head的数值；而在写操作之前会读取head的值判断是否有空间可以用来写数据。
 * 所以，这时候tail到head - 1之间一定是有空间可以写数据的，而不会出现一个位置的数据还没有读出就被写操作覆盖的情况。
 * 这样就保证了RingBuffer的线程安全性。
 */
export default class RingBuffer<T> {
  private static DEFAULT_SIZE = 1024
  /**
   * 指向下一次读的位置
   */
  private head = 0
  /**
   * 指向下一次写的位置
   */
  private tail = 0
  private readonly bufferSize
  private readonly buffer: any[]

  constructor(size?: number) {
    this.bufferSize = size || RingBuffer.DEFAULT_SIZE
    this.buffer = new Array(this.bufferSize)
  }

  isEmpty(): boolean {
    return this.head === this.tail
  }

  isFull(): boolean {
    return (this.tail + 1) % this.bufferSize === this.head
  }

  clear() {
    for (let i = 0; i < this.bufferSize; i++) {
      this.buffer[i] = null
    }
    this.head = 0
    this.tail = 0
  }

  put(v: T): boolean {
    if (this.isFull()) return false
    this.buffer[this.tail] = v
    this.tail = (this.tail + 1) % this.bufferSize
    return true
  }

  get(): T {
    if (this.isEmpty()) return null
    const res = this.buffer[this.head]
    this.head = (this.head + 1) % this.bufferSize
    return res
  }

  getAll(): T[] {
    if (this.isEmpty()) {
      return []
    }
    const copyTail = this.tail
    const cnt = this.head < copyTail ? copyTail - this.head : this.bufferSize - this.head + copyTail
    const res = new Array(cnt)
    if (this.head < copyTail) {
      for (let i = this.head; i < copyTail; i++) {
        res[i - this.head] = this.buffer[i]
      }
    } else {
      for (let i = this.head; i < this.bufferSize; i++) {
        res[i - this.head] = this.buffer[i]
      }
      for (let i = 0; i < copyTail; i++) {
        res[this.bufferSize - this.head + i] = this.buffer[i]
      }
    }
    this.head = copyTail
    return res
  }
}

const buffer = new RingBuffer<number>()
buffer.put(1)
buffer.put(2)
console.log(buffer.get())
buffer.put(3)
console.log(buffer.getAll())
