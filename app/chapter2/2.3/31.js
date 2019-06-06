// 运行时间直方图

// 编写一个程序，接收命令行参数N和T，用快排对大小为N的随机浮点数数组进行T次试验，并将所有运行时间绘制成直方图。令N=10^3~10^6,为了使曲线更平滑，T值越大越好。这个联系最关键的地方在于找到适当的比例绘制实验结果

const ecStat = require('echarts-stat');

const util = require('util');

const {quickSort} = require('../../sort');
const {randomDoubleArray} = require('../../util');

const T = 10;

const points = [];

// 在[start,end]之间等分len个点
function rangeDivide(start, end, len) {
  const gap = parseInt((end - start + 1) / len);
  const arr = [];
  for (let i = start; i <= end; i += gap) {
    arr.push(i);
  }
  return arr;
}

const nums = rangeDivide(10 ** 3, 10 ** 6, 100);

for (const n of nums) {
  let sum = 0;
  for (let i = 0; i < T; i++) {

    const arr = randomDoubleArray(n);
    const start = Date.now();
    quickSort(arr);
    sum += (Date.now() - start);
  }
  util.log('n = ', n, 'done');
  points.push([n, sum / T]);
}

util.log('start regression');

const regression = ecStat.regression('logarithmic', points);
regression.points.sort(function (a, b) {
  return a[0] - b[0];
});

util.log('end regression');

const option = {
  title: {
    text: '快排随机数组规模和运行时间的曲线图',
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

require('fs').writeFileSync('./31.echarts.option.json', JSON.stringify(option));
