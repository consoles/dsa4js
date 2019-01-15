// 文本编辑的缓冲区。为文本编辑器的缓冲区设计一种数据类型并实现insert，delete,left,right,size。提示：使用2个栈

const Stack = require('../../LinkedStack');

class EditorBuffer {
    constructor() {
        this.leftStack = new Stack();
        this.rightStack = new Stack();
    }
    /**
     * 在光标位置插入字符
     */
    insert(char) {
        this.leftStack.push(char);
    }
    /**
     * 删除并返回光标位置的字符
     */
    delete() {
        return this.leftStack.pop();
    }
    /**
     * 将光标左移k个位置
     */
    left(k) {
        if (k > this.leftStack.size) {
            return;
        }
        for (let i = 0; i < k; i++) {
            this.rightStack.push(this.leftStack.pop());
        }
    }
    /**
     * 将光标右移k个位置
     */
    right(k) {
        if (k > this.rightStack.size) {
            return;
        }
        for (let i = 0; i < k; i++) {
            this.leftStack.push(this.rightStack.pop());
        }
    }
    /**
     * 缓冲区中的字符数量
     */
    size() {
        return this.leftStack.size + this.rightStack.size;
    }

    inspect() {
        const pos = this.leftStack.size;
        const tmpStack = new Stack();
        for (const c of this.leftStack) {
            tmpStack.push(c);
        }
        const left = [];
        for (const c of tmpStack) {
            left.push(c);
        }
        const right = [];
        for (const c of this.rightStack) {
            right.push(c);
        }
        return `pos = ${pos},size = ${this.size()},left = ${JSON.stringify(left)},right = ${JSON.stringify(right)}`;
    }
}

const buf = new EditorBuffer();
for (const c of 'sfdssemkf') {
    buf.insert(c);
}
buf.left(4);
buf.delete();
buf.right(2);
buf.insert('x');
buf.right(1);
console.log(buf);