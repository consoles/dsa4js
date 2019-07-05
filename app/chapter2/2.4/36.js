class MaxPQ {
  constructor() {
    this.data = [-1];
    this.sz = 0;
  }

  less(i, j) {
    return this.data[i] < this.data[j];
  }

  swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  _swim(k) {
    while (k > 1) {
      const parentIndex = parseInt(k / 2);
      if (this.less(parentIndex, k)) {
        this.swap(k, parentIndex);
        k = parentIndex;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    const sz = this.sz;
    while (2 * k <= sz) {
      let j = 2 * k;
      if (j < sz && this.less(j, j + 1)) {
        j++;
      }
      if (this.less(j, k)) {
        break;
      } else {
        this.swap(j, k);
        k = j;
      }
    }
  }

  insert(value) {
    const sz = ++this.sz;
    this.data[sz] = value;
    this._swim(sz);
  }

  delMax() {
    const value = this.data[1];
    const sz = this.sz--;
    this.swap(1, sz);
    this.data[sz] = null;
    this._sink(1);
    return value;
  }

  max() {
    return this.data[1];
  }

  isEmpty() {
    return this.sz === 0;
  }
}

const ns = [1e5, 2e5, 4e5, 8e5];
const testCount = 10;

const points = [];

for (let n of ns) {
  let total = 0;
  for (let i = 0; i < testCount; i++) {
    const start = Date.now();
    const pq = new MaxPQ();
    for (let j = 0; j < n; j++) {
      pq.insert(Math.random());
    }
    let half = parseInt(n / 2);
    for (let j = 0; j < half; j++) {
      pq.delMax();
    }
    for (let j = 0; j < half; j++) {
      pq.insert(Math.random());
    }
    while (!pq.isEmpty()) {
      pq.delMax();
    }
    const elapse = Date.now() - start;
    total += elapse;
    console.log('n = ', n, `${i} / ${testCount}`, 'done,took ', elapse);
  }
  points.push([n, total / testCount]);
}

const ecStat = require('echarts-stat');

const regression = ecStat.regression('logarithmic', points);
regression.points.sort(function (a, b) {
  return a[0] - b[0];
});


const option = {
  title: {
    text: '优先队列运行时间散点图',
    subtext: 'By ecStat.regression',
    sublink: 'https://github.com/ecomfe/echarts-stat',
    left: 'center'
  },
  tooltip: {
    trigger: 'axis',
    axisPointer: {
      type: 'cross'
    }
  },
  xAxis: {
    type: 'value',
    splitLine: {
      lineStyle: {
        type: 'dashed'
      }
    },
    splitNumber: 20
  },
  yAxis: {
    type: 'value',
    splitLine: {
      lineStyle: {
        type: 'dashed'
      }
    }
  },
  series: [{
    name: 'scatter',
    type: 'scatter',
    label: {
      emphasis: {
        show: true,
        position: 'left',
        textStyle: {
          color: 'blue',
          fontSize: 16
        }
      }
    },
    data: points
  }, {
    name: 'line',
    type: 'line',
    showSymbol: false,
    smooth: true,
    data: regression.points,
    markPoint: {
      itemStyle: {
        normal: {
          color: 'transparent'
        }
      },
      label: {
        normal: {
          show: true,
          position: 'left',
          formatter: regression.expression,
          textStyle: {
            color: '#333',
            fontSize: 14
          }
        }
      },
      data: [{
        coord: regression.points[regression.points.length - 1]
      }]
    }
  }]
};

require('fs').writeFileSync('./36.echarts.option.json', JSON.stringify(option));
