const path = require('path');

const mathjs = require('mathjs');

const BST = require('../BST');

const {randomArray} = require('../../util');

function meanAbsoluteDeviation(arr) {
  const mean = mathjs.mean(arr);
  return mathjs.sum(arr.map(x => mathjs.abs(x - mean))) / arr.length;
}

function buildData(n) {
  const testCount = 100;
  const pathSums = [];
  for (let i = 0; i < testCount; i++) {
    const bst = new BST();
    const arr = randomArray(0, n, n);
    for (let i = 0; i < arr.length; i++) {
      bst.put(arr[i], i);
    }
    // 内部路径长度除以N再加1
    pathSums.push(bst.pathSum / n + 1);
  }

  return {
    pathSums,
    mad: meanAbsoluteDeviation(pathSums),
    std: mathjs.std(pathSums),
    mean: mathjs.mean(pathSums),
    regression: 1.39 * Math.log2(n) - 1.85
  };
}

let pathSums = [];

const data = {
  mad: [],
  std: [],
  mean: [],
  regression: []
};

for (let n = 100; n <= 10000; n += 100) {
  const ret = buildData(n);
  for (const key of Object.keys(data)) {
    const value = ret[key];
    data[key].push([n, value]);
  }
  pathSums = pathSums.concat(ret.pathSums.map(x => [n,x]));
  console.log(n);
}

const option = {
  title: {
    text: '根节点到达任意节点的平均路径长度',
  },
  color:['#0f0', '#00f', '#f0f','#f00','#0ff'],
  legend: {},
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
  series: []
};

for (const key of Object.keys(data)) {
  const s = {
    type: key === 'regression' ? 'line' : 'scatter',
    data: data[key],
    name: key
  };
  option.series.push(s);
}

option.series.push({
  type:'scatter',
  data:pathSums,
  name:'路径长度'
});

require('fs').writeFileSync(path.join(__dirname, '47.echarts.option.json'), JSON.stringify(option));
