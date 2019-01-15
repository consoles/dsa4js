// 1.3.39 环形缓冲区。环形缓冲区，又称为环形队列，是一种定长为 N 的先进先出的数据结构。它在进程间的异步数据传输或记录日志文件时十分有用。当缓冲区为空时，消费者会在数据存入缓冲区前等待；当缓冲区满时，生产者会等待数据存入缓冲区。为 RingBuffer 设计一份 API 并用（回环）数组将其实现。

class RingBuffer {
    constructor(capacity) {
        capacity += 1; // 多分配一个，最后一个空间不存储数据，这样判断缓冲区满和缓冲区空就很方便了
        this.readPos = 0; // 下一次读的位置
        this.writePos = 0; // 下一次写的位置
        this.capacity = capacity;
        this.data = new Array(capacity);
    }
    isEmpty() {
        return this.readPos === this.writePos;
    }
    isFull() {
        return (this.writePos + 1) % this.capacity === this.readPos;
    }
    put(item) {
        if (this.isFull()) {
            return false;
        }
        this.data[this.writePos] = item;
        this.writePos = (this.writePos + 1) % this.capacity;
        return true;
    }
    take() {
        if (this.isEmpty()) {
            return null;
        }
        const item = this.data[this.readPos];
        this.readPos = (this.readPos + 1) % this.capacity;
        return item;
    }
}

class RingBuffer2 {
    constructor(capacity) {
        this.capacity = capacity;
        this.writePos = 0; // 下一个要写的位置
        this.avaiable = 0; // 有效元素个数
        this.data = new Array(capacity);
    }
    put(item) {
        if (this.avaiable >= this.capacity) return false;
        if (this.writePos >= this.capacity) {
            this.writePos = 0;
        }
        this.data[this.writePos] = item;
        this.writePos++;
        this.avaiable++;
        return true;
    }
    take() {
        if (this.avaiable <= 0) return null;
        // 根据有效元素个数，分：读指针在前和写指针在前2种情况计算读指针的位置
        let readPos = this.writePos - this.avaiable;
        if (readPos < 0) {
            readPos += this.capacity;
        }
        const item = this.data[readPos];
        this.avaiable--;
        return item;
    }
}

class RingBuffer3 {
    constructor(capacity) {
        this.capacity = capacity;
        this.writePos = 0; // 下一个要写的位置
        this.readPos = 0; // 下一个要读的位置
        this.flipped = false; // 写指针是否到了读指针之前 
        this.data = [];
    }
    put(item) {
        if (!this.flipped) {
            if (this.writePos === this.capacity) {
                this.writePos = 0;
                this.flipped = true;
                if (this.writePos >= this.readPos) {
                    return false;
                }
            }
            this.data[this.writePos++] = item;
            return true;
        }
        if (this.writePos >= this.readPos) {
            return false;
        }
        this.data[this.writePos++] = item;
        return true;
    }
    take() {
        if (!this.flipped) {
            return this.readPos < this.writePos ? this.data[this.readPos++] : null;
        }
        if (this.readPos === this.capacity) {
            this.readPos = 0;
            this.flipped = false;
            return this.readPos < this.writePos ? this.data[this.readPos++] : null;
        }
        return this.data[this.readPos++];
    }
}

const buf = new RingBuffer3(3);
let ret = buf.put(1);
ret = buf.put(2);
ret = buf.put(3);
ret = buf.put(4);
ret = buf.take();
ret = buf.take();
ret = buf.take();
ret = buf.take();
debugger