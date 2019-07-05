// 离散概率分布的取样。

/* 编写一个 Sample 类，
* 其构造函数接受一个 double 类型的数组 p[] 作为参数并支持以下操作：
* random()——返回任意索引 i 及其概率 p[i]/T（T 是 p[] 中所有元素之和）；
* change(i, v)——将 p[i] 的值修改为 v。
* 提示：使用完全二叉树，每个结点对应一个权重 p[i]。
* 在每个结点记录其下子树的权重之和。
* 为了产生一个随机的索引，
* 取 0 到 T 之间的一个随机数并根据各个结点的权重之和来判断沿着哪条子树搜索下去。
* 在更新 p[i] 时，同时更新从根节点到 i 的路径上的所有结点。
* 不要像堆的实现那样显式的使用指针。
*/

class Sample {
  constructor(data) {
    const len = data.length;
    this.P = new Array(len + 1).fill(0);
    this.T = 0;
    for (let i = 1; i <= len; i++) {
      const p = this.P[i] = data[i - 1];
      this.T += p;
    }
    // 记录子节点权重和
    this.sumP = new Array(len + 1).fill(0);
    for (let i = len; parseInt(i / 2) > 0; i--) {
      this.sumP[parseInt(i / 2)] += this.P[i];
    }
  }

  random() {
    let percentage = Math.random() * this.T;
    let index = 1;
    while (index * 2 <= this.P.length) {
      // 找到节点
      if (percentage <= this.P[index]) {
        break;
      }
      // 在左子树范围内
      percentage -= this.P[index];
      index *= 2;
      // 在右子树范围内
      if (percentage <= this.sumP[index] + this.P[index]) {
        continue;
      }
      // 在右子树范围内，减去左子树
      percentage -= this.sumP[index] + this.P[index];
      index++;
    }
    return index - 1;
  }

  change(i, v) {
    i++;
    this.P[i] = v;
    // 重新计算总和
    while (i > 0) {
      i = parseInt(i / 2);
      this.sumP[i] = this.P[i * 2] + this.sumP[i * 2];
      if (i * 2 + 1 < this.P.length) {
        this.sumP[i] += this.P[i * 2 + 1] + this.sumP[i * 2 + 1];
      }
    }
  }
}

function test(n, sample) {
  const testResult = new Array(sample.P.length - 1).fill(0);
  for (let i = 0; i < n; i++) {
    testResult[sample.random()]++;
  }
  // 一般测试
  console.log('重复次数n = ', n);
  console.log('预设概率:');
  for (let i = 1; i < sample.P.length; i++) {
    process.stdout.write(`${sample.P[i]}\t`);
  }
  console.log();
  console.log('出现次数：');
  for (let i = 0; i < testResult.length; i++) {
    process.stdout.write(`${testResult[i]}\t`);
  }
  console.log();
}

const p = [0.2, 0.1, 0.3, 0.3, 0.1];
const sample = new Sample(p);
const n = 1e5;
test(n, sample);
sample.change(3, 0.1);
sample.change(4, 0.2);
sample.change(1, 0.2);
test(n, sample);
