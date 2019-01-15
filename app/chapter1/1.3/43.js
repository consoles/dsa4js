// 1.3.43 文件列表。文件夹就是一列文件和文件夹的列表。编写一个程序，从命令行接受一个文件夹名作为参数，打印出该文件夹下的所有文件并用递归的方式在所有子文件夹的名下（缩进）列出其下的所有文件。

const fs = require('fs');
const path = require('path');

const listFiles = (dir, depth) => {
    const absolutePath = path.resolve(dir);
    const stat = fs.statSync(absolutePath);
    if (stat.isDirectory()) {
        const files = fs.readdirSync(absolutePath);
        for (let file of files) {
            listFiles(path.join(absolutePath, file), depth + 1);
        }
    } else {
        console.log('-'.repeat(depth), absolutePath);
    }
}

// listFiles('/Users/yiihua-013/consoles-projects/dsa4js/test', 0);

// 使用队列，参考java.io.File

const listFiles2 = (dir, depth, queue) => {
    const absolutePath = path.resolve(dir);
    const stat = fs.statSync(absolutePath);
    if (stat.isFile()) {
        queue.enqueue({ file: dir, depth });
    } else {
        const files = fs.readdirSync(absolutePath);
        for (const file of files) {
            listFiles2(path.join(absolutePath, file), depth + 1, queue);
        }
    }
};

const Queue = require('../../LinkedQueue');
const q = new Queue();
listFiles2('/Users/yiihua-013/consoles-projects/dsa4js/test', 0, q);
for (const item of q) {
    console.log('-'.repeat(item.depth), item.file);
}