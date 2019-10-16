class Str {
  constructor(value) {
    this.value = value;
  }

  hashCode() {
    let hash = 0;
    for (let i = 0; i < this.value.length; i++) {
      hash = hash * 31 + this.value.charCodeAt(i);
    }
    return hash & 0x7fffffff;
  }

  equals(other) {
    return this.value === other.value;
  }
}

const path = require('path');
const fs = require('fs');

const _ = require('lodash');

const {readLinesAsync, randomPick} = require('../../util');

const tale = path.join(__dirname, '../../../test/input/algs4-data/tale.txt');

class Node {
  constructor(key, value, next) {
    this.key = key;
    this.value = value;
    this.next = next || null;
  }
}

// 符号表的实现：顺序查找表
class SequentialSearchST {

  constructor() {
    this.head = null;
    this.sz = 0;
    this.scanCount = 0; // 当前操作需要扫描的元素数目
  }

  put(key, value) {
    this.scanCount = 0;
    if (!this.head) {
      this.head = new Node(key, value);
      this.sz++;
    } else {
      // 向后扫描，如果找到相同的元素就将key替换为value
      let cur = this.head;
      while (cur) {
        if (cur.key === key) {
          cur.value = value;
          return;
        }
        this.scanCount++;
        cur = cur.next;
      }
      // 遍历完链表，没有找到，将新节点插入头部
      this.head = new Node(key, value, this.head);
      this.sz++;
    }
  }

  get(key) {
    if (!this.head) return null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        return cur.value;
      }
      cur = cur.next;
    }
    return null;
  }

  contains(key) {
    return this.get(key) !== null;
  }

  get size() {
    return this.sz;
  }

  * keys() {
    if (!this.head) return;
    for (let cur = this.head; cur; cur = cur.next) {
      yield cur.key;
    }
  }

  delete(key) {
    // 找到待删除的节点的父节点
    const dummyHead = new Node(null, null, this.head);
    let parent = dummyHead;
    while (parent) {
      if (parent.next && parent.next.key === key) {
        break;
      }
      parent = parent.next;
    }
    if (!parent) {
      throw new Error(`no such key ${key}`);
    }
    parent.next = parent.next.next;
    this.head = dummyHead.next;
    dummyHead.next = null;
    this.sz--;
  }

  delete2(key) {
    // 不用虚拟头结点的删除
    let parent = null;
    let cur = this.head;
    while (cur) {
      if (cur.key === key) {
        break;
      }
      parent = cur;
      cur = cur.next;
    }
    if (!cur) {
      throw new Error(`no such key ${key}`);
    }
    this.sz--;
    if (!parent) {
      this.head = cur.next;
    } else {
      parent.next = cur.next;
    }
  }
}

// 拉链法
class SeparateChainingHashST {
  constructor(M) {
    const st = [];
    for (let i = 0; i < M; i++) {
      st[i] = new SequentialSearchST();
    }
    this.M = M;
    this.st = st;
    this.scanCount = 0;
  }

  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }

  get size() {
    let count = 0;
    for (const list of this.st) {
      count += list.size;
    }
    return count;
  }

  put(key, value) {
    const h = this._hash(key);
    const st = this.st[h];
    st.put(key, value);
    this.scanCount = st.scanCount;
  }

  get(key) {
    return this.st[this._hash(key)].get(key);
  }
}

// 线性探测法
class LinearProbingHashST {
  constructor(M = 16) {
    this.N = 0;
    this.M = M;
    this._keys = new Array(M);
    this._values = new Array(M);
    this.scanCount = 0;
  }

  _resize(cap) {
    const st = new LinearProbingHashST(cap);
    for (let i = 0; i < this.N; i++) {
      const key = this._keys[i];
      if (key) {
        st.put(key, this._values[i]);
      }
      this.scanCount++;
    }
    this._keys = st._keys;
    this._values = st._values;
    this.M = cap;
  }

  _hash(key) {
    return key.hashCode() % this.M;
  }

  put(key, value) {
    this.scanCount = 0;
    if (2 * this.N > this.M) {
      this._resize(2 * this.M);
    }
    let i = this._hash(key);
    for (; this._keys[i]; i = (i + 1) % this.M) {
      this.scanCount++;
      if (key.equals(this._keys[i])) {
        this._values[i] = value;
        return;
      }
    }
    this._keys[i] = key;
    this._values[i] = value;
    this.N++;
  }
}

// 可以扩容的拉链法
class SeparateChainingResizeHashST {
  constructor(M){
    this.N = 0;
    this.M = M || 7;
    this.st = [];
    for (let i = 0;i< this.M;i++) {
      this.st[i] = new SequentialSearchST();
    }
    this.UPPER_TOLERANCE = 8;

    this.scanCount = 0;
  }
  _hash(key) {
    return (key.hashCode() & 0x7fffffff) % this.M;
  }
  _resize(newM) {
    const newSTs = [];
    for (let i = 0;i < newM;i++) {
      newSTs[i] = new SequentialSearchST();
    }
    const oldM = this.M;
    this.M = newM;
    for (let i = 0; i < oldM; i++) {
      this.scanCount++;
      let cur = this.st[i].head;
      while (cur) {
        const {key, value} = cur;
        const hash = this._hash(key);
        newSTs[hash].put(key, value);
        cur = cur.next;
      }
    }
    this.st = newSTs;
  }
  put(key, value) {
    this.scanCount = 0;
    const index = this._hash(key);
    this.st[index].put(key, value);
    this.scanCount += this.st[index].scanCount;
    // 平均每个索引承载的元素哈希冲突的数量大于容忍上界就扩容
    if (this.size >= this.UPPER_TOLERANCE * this.M) {
      this._resize(2 * this.M);
    }
  }
  get size() {
    let count = 0;
    for (const st of this.st) {
      count += st.size;
    }
    return count;
  }
}

(async () => {
  const words = [];
  const lines = await readLinesAsync(tale);

  for (const line of lines) {
    const wordsInLine = line.split(/\s+/).map(x => x.trim()).filter(x => x.length > 0);
    for (const word of wordsInLine) {
      words.push(word);
    }
  }

  // 一个良好的hash函数hash的分布应该是均匀的

  const hashCounter = {};

  const uniqWords = _.uniq(words);

  for (const word of uniqWords) {
    const h = new Str(word).hashCode() % 97;
    hashCounter[h] = hashCounter[h] || 0;
    hashCounter[h]++;
  }

  let option = {
    title: {
      text: '双城记单词散列值出现频率分布',
      subtext: `${uniqWords.length}个键值对`
    },
    tooltip: {
      trigger: 'axis'
    },
    legend: {
      data: ['频率分布']
    },
    toolbox: {
      show: true,
      feature: {
        dataView: {show: true, readOnly: false},
        magicType: {show: true, type: ['line', 'bar']},
        restore: {show: true},
        saveAsImage: {show: true}
      }
    },
    calculable: true,
    xAxis: [
      {
        type: 'category',
        data: Object.keys(hashCounter).map(x => parseInt(x))
      }
    ],
    yAxis: [
      {
        type: 'value'
      }
    ],
    series: [
      {
        name: '频次',
        type: 'bar',
        data: Object.keys(hashCounter).map(x => hashCounter[x] / uniqWords.length),
        markPoint: {
          data: [
            {type: 'max', name: '最大值'},
            {type: 'min', name: '最小值'}
          ]
        },
        markLine: {
          data: [
            {
              type: 'average', name: '平均值', lineStyle: {
                type: 'normal',
                width: 5,
                color: '#f0f'
              }
            }
          ]
        }
      }
    ]
  };

  fs.writeFileSync(path.join(__dirname, '40.echarts.hash-distribute.option.json'), JSON.stringify(option));

  let scatterData = [];
  let lineData = [];

  let total = 0;

  // 基于拉链法的散列表中每个链表的长度服从正态分布
  let st = new SeparateChainingHashST(997);
  for (let i = 0; i < words.length; i++) {
    st.put(new Str(words[i]), i);
    scatterData.push([i, st.scanCount]);
    total += st.scanCount;
    lineData.push([i, total / i]);
  }

  const listLenCounter = {};

  for (const list of st.st) {
    const sz = list.size;
    listLenCounter[sz] = listLenCounter[sz] || 0;
    listLenCounter[sz]++;
  }

  option = {
    title: {
      text: '链表长度分布'
    },
    tooltip: {
      trigger: 'axis',
      axisPointer: {
        type: 'cross'
      }
    },
    toolbox: {
      feature: {
        dataView: {show: true, readOnly: false},
        restore: {show: true},
        saveAsImage: {show: true}
      }
    },
    legend: {
      data: ['频度分布']
    },
    xAxis: [
      {
        type: 'category',
        axisTick: {
          alignWithLabel: true
        },
        data: Object.keys(listLenCounter).map(x => parseInt(x))
      }
    ],
    yAxis: [
      {
        type: 'value',
        name: '频度'
      }
    ],
    series: [
      {
        name: '频度',
        type: 'bar',
        data: Object.keys(listLenCounter).map(x => listLenCounter[x] / st.size)
      }
    ]
  };

  fs.writeFileSync(path.join(__dirname, '40.echarts.list_len-distribute.option.json'), JSON.stringify(option));

  // 线性探测法中的键簇分布
  st = new LinearProbingHashST();
  for (let i = 0; i < words.length; i++) {
    st.put(new Str(words[i]), i);
  }

  // 数据升维（一维数据变128维）一维数组变二维数组
  const len = 128;
  const points = [];
  for (let i = 0; i < st._keys.length; i++) {
    if (st._keys[i]) {
      const x = parseInt(i / len);
      const y = i % len;
      points.push([x, y]);
    }
  }

  option = {
    title: {
      text: '线性探测法中的键簇分布'
    },
    xAxis: {},
    yAxis: {},
    series: [{
      data: points,
      type: 'scatter'
    }]
  };

  fs.writeFileSync(path.join(__dirname, '40.echarts.key_cluster-distribute.option.json'), JSON.stringify(option));

  // 固定长度的拉链法的成本
  option = {
    legend: {
      right: 10,
      data: ['操作成本', '累计平均']
    },
    title: {
      text: '固定长度的拉链法的成本(M = 997)'
    },
    xAxis: {},
    yAxis: {},
    series: [{
      data: randomPick(scatterData, 1000).sort((a, b) => a[0] - b[0]),
      type: 'scatter',
      name: '操作成本'
    }, {
      data: randomPick(lineData, 1000).sort((a, b) => a[0] - b[0]),
      type: 'line',
      name: '累计平均'
    }]
  };
  fs.writeFileSync(path.join(__dirname, '40.echarts.fix-size-separate-chain-hash.option.json'), JSON.stringify(option));

  // 自动调整大小的拉链法的成本
  scatterData =  [];
  lineData = [];
  total = 0;
  st = new SeparateChainingResizeHashST();
  for (let i = 0; i < words.length; i++) {
    st.put(new Str(words[i]), i);
    scatterData.push([i, st.scanCount]);
    total += st.scanCount;
    lineData.push([i, total / i]);
  }
  option = {
    legend: {
      right: 10,
      data: ['操作成本', '累计平均']
    },
    title: {
      text: '自动调整数组大小的拉链法的成本'
    },
    xAxis: {},
    yAxis: {},
    series: [{
      data: randomPick(scatterData, 1000).sort((a, b) => a[0] - b[0]),
      type: 'scatter',
      name: '操作成本'
    }, {
      data: randomPick(lineData, 1000).sort((a, b) => a[0] - b[0]),
      type: 'line',
      name: '累计平均'
    }]
  };
  fs.writeFileSync(path.join(__dirname, '40.echarts.resize-separate-chain-hash.option.json'), JSON.stringify(option));

  // 线性探测法的成本
  scatterData =  [];
  lineData = [];
  total = 0;
  st = new LinearProbingHashST();
  for (let i = 0; i < words.length; i++) {
    st.put(new Str(words[i]), i);
    scatterData.push([i, st.scanCount]);
    total += st.scanCount;
    lineData.push([i, total / i]);
  }
  option = {
    legend: {
      right: 10,
      data: ['操作成本', '累计平均']
    },
    title: {
      text: '线性探测法的成本'
    },
    xAxis: {},
    yAxis: {},
    series: [{
      data: randomPick(scatterData, 1000).sort((a, b) => a[0] - b[0]),
      type: 'scatter',
      name: '操作成本'
    }, {
      data: randomPick(lineData, 1000).sort((a, b) => a[0] - b[0]),
      type: 'line',
      name: '累计平均'
    }]
  };
  fs.writeFileSync(path.join(__dirname, '40.echarts.linear-probing-hash.option.json'), JSON.stringify(option));

})();
