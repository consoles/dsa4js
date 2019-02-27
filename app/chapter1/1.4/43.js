// 大小可变的数组与链表。通过实验验证对于栈来说基于大小可变的数组的实现快于基于链表的实现的猜想（请见练习 1.4.35 和练习 1.4.36）。为此实现另一个版本的 DoublingRatio，计算两个程序的运行时间的比例。

class ArrayStack {
    constructor() {
        this.data = new Array();
    }
    push(item) {
        this.data.push(item);
    }
    pop() {
        return this.data.pop();
    }
    isEmpty() {
        return this.data.length === 0;
    }
}

const LinkedStack = require('../../LinkedStack');

const _ = require('lodash');

const arr = [];
for (let i = 0; i < 1e7; i++) {
    arr.push(_.random());
}

let n = 1000;
while (true) {
    console.log(`数据量\t数组栈\t链表栈\t比值`);
    const start = Date.now();
    let stack = new ArrayStack();
    for (let i = 0; i < n; i++) {
        stack.push(arr[i]);
    }
    while (!stack.isEmpty()) {
        stack.pop();
    }
    const time1 = Date.now();

    stack = new LinkedStack();
    for (let i = 0; i < n; i++) {
        stack.push(arr[i]);
    }
    while (!stack.isEmpty()) {
        stack.pop();
    }
    const time2 = Date.now();

    const costArray = time1 - start;
    const costLinked = time2 - time1;

    console.log(`${n}\t${costArray}\t${costLinked}\t${costLinked / costArray}`);
    n += n;
}

// 链表实现慢于数组实现的原因，创建node节点占用额外的空间和时间

// 数据量 	数组栈 	链表栈 	比值
// 1000   	0      	1      	Infinity
// 数据量 	数组栈 	链表栈 	比值
// 2000   	0      	1      	Infinity
// 数据量 	数组栈 	链表栈 	比值
// 4000   	1      	1      	1
// 数据量 	数组栈 	链表栈 	比值
// 8000   	1      	5      	5
// 数据量 	数组栈 	链表栈 	比值
// 16000  	0      	1      	Infinity
// 数据量 	数组栈 	链表栈 	比值
// 32000  	0      	0      	NaN
// 数据量 	数组栈 	链表栈 	比值
// 64000  	2      	1      	0.5
// 数据量 	数组栈 	链表栈 	比值
// 128000 	3      	10     	3.3333333333333335
// 数据量 	数组栈 	链表栈 	比值
// 256000 	5      	21     	4.2
// 数据量 	数组栈 	链表栈 	比值
// 512000 	11     	38     	3.4545454545454546
// 数据量 	数组栈 	链表栈 	比值
// 1024000	25     	141    	5.64
// 数据量 	数组栈 	链表栈 	比值
// 2048000	38     	200    	5.2631578947368425
// 数据量 	数组栈 	链表栈 	比值
// 4096000	138    	387    	2.8043478260869565
// 数据量 	数组栈 	链表栈 	比值
// 8192000	194    	983    	5.06701030927835
// 数据量 	数组栈 	链表栈 	比值
// 16384000       	434    	2903   	6.688940092165899
// 数据量 	数组栈 	链表栈 	比值
// 32768000       	1372   	13067  	9.524052478134111