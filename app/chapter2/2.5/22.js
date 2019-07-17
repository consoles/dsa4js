class Stock {
  constructor(price, count) {
    this.price = price;
    this.count = count;
  }

  compareTo(other) {
    return this.price - other.price;
  }
}

class MinPQ {
  constructor() {
    this._data = [-1];
    this.sz = 0;
  }

  get min() {
    return this._data[1];
  }

  insert(value) {
    const sz = ++this.sz;
    this._data[sz] = value;
    this._swim(sz);
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = k >> 1;
      if (this._less(k, parentIndex)) {
        this._swap(parentIndex, k);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _less(i, j) {
    return this._data[i].compareTo(this._data[j]) < 0;
  }

  _swap(i, j) {
    [this._data[i], this._data[j]] = [this._data[j], this._data[i]];
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this._less(j + 1, j)) {
        j++;
      }
      if (this._less(j, k)) {
        this._swap(k, j);
        k = j;
      } else {
        break;
      }
    }
  }

  delMin() {
    const item = this._data[1];
    const sz = this.sz--;
    this._swap(sz, 1);
    this._sink(1);
    return item;
  }

  isEmpty() {
    return this.sz === 0;
  }
}

class MaxPQ extends MinPQ {
  _less(i, j) {
    return !super._less(i, j);
  }

  get max() {
    return super.min;
  }

  delMin() {
    throw new Error('no such method');
  }

  delMax() {
    return super.delMin();
  }
}

const actions = [
  ['sell', 10, 100], // 以10的单价卖出100支股票
  ['buy', 8, 10], // 以8的单价买入10支股票
  ['buy', 12, 50],
  ['sell', 7, 5],
  ['buy', 11, 100],
  ['sell', 8, 80]
];

// 交易达成 => 买入价格低于卖出价格(卖家最便宜的价格 < 买家最贵的价格)
// 所以对于卖家维护最小堆，买家维护最大堆
const buyer = new MaxPQ();
const seller = new MinPQ();
for (const [action, price, count] of actions) {
  const stock = new Stock(price, count);
  if (action === 'sell') {
    seller.insert(stock);
  } else {
    buyer.insert(stock);
  }
}

while (!seller.isEmpty() && !buyer.isEmpty()) {
  // 买家最高出价小于卖家最低出价
  if (seller.min.price > buyer.max.price) {
    console.log(`卖出价${seller.min.price}大于买入价${buyer.max.price}，交易终止！`);
    break;
  }
  const buy = buyer.delMax();
  const sell = seller.delMin();
  if (buy.count === sell.count) {
    console.log('卖出价', sell.price, '卖出数量', buy.count, '买入价', buy.price, '买入数量', sell.count, '刚好交易完成');
  } else if (buy.count > sell.count) {
    const count = sell.count;
    const restCount = buy.count - count;
    console.log('卖出价', sell.price, '卖出数量', count, '买入价', buy.price, '买入数量', count, '卖家还剩余', restCount);
    buy.count = restCount;
    buyer.insert(buy);
  } else {
    const count = buy.count;
    const restCount = sell.count - count;
    console.log('卖出价', sell.price, '卖出数量', count, '买入价', buy.price, '买入数量', count, '买家', restCount);
    sell.count = restCount;
    seller.insert(sell);
  }
}
