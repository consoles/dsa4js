const _ = require('lodash');

class SparseVector {
  constructor() {
    this.st = new Map();
  }

  get size() {
    return this.st.size;
  }

  put(i, x) {
    this.st.set(i, x);
  }

  get(i) {
    if (!this.st.has(i)) return 0;
    return this.st.get(i);
  }

  delete(i) {
    this.st.delete(i);
  }

  dot(that) {
    let sum = 0;
    for (let i of this.st.keys()) {
      sum += that[i] * this.get(i);
    }
    return sum;
  }
}

/**
 * 生成 m*n 稀疏矩阵
 * @param m
 * @param n
 */
function genMatrix(m, n) {
  const arr = new Array(m);
  for (let i = 0; i < m; i++) {
    arr[i] = new Array(n);
    for (let j = 0; j < n; j++) {
      arr[i][j] = Math.random() > .7 ? _.random(-34334, 344334) : 0;
    }
  }
  return arr;
}

function ofVector(matrix) {
  const arr = [];
  for (let i = 0; i < matrix.length; i++) {
    arr[i] = new SparseVector();
    for (let j = 0; j < matrix[i].length; j++) {
      arr[i].put(j, matrix[i][j]);
    }
  }
  return arr;
}

function genDotVector(m) {
  const arr = [];
  for (let i = 0; i < m; i++) {
    arr[i] = _.random(-34343333, 99373434);
  }
  return arr;
}

for (const n of [10, 100, 1000, 10000]) {
  const x = genDotVector(n);
  const m = genMatrix(n, n);
  const v = ofVector(m);

  let b = new Array(n);

  let start = Date.now();
  for (let i = 0; i < n; i++) {
    let sum = 0;
    for (let j = 0; j < n; j++) {
      sum += m[i][j] * x[j];
    }
    b[i] = sum;
  }
  let end = Date.now();
  console.log('n = ', n, '普通矩阵乘法', end - start);

  b = new Array(n);
  start = Date.now();
  for (let i = 0; i < n; i++) {
    b[i] = v[i].dot(x);
  }
  end = Date.now();
  console.log('n = ', n, '稀疏向量矩阵乘法', end - start);
}

// 垃圾 JS 本来期待稀疏矩阵性能表现好，怎么知道报错了

// n =  10 普通矩阵乘法 0
// n =  10 稀疏向量矩阵乘法 0
// n =  100 普通矩阵乘法 1
// n =  100 稀疏向量矩阵乘法 2
// n =  1000 普通矩阵乘法 23
// n =  1000 稀疏向量矩阵乘法 32
//
// FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory
// <--- Last few GCs --->
//
// [21602:0x103004400]     8736 ms: Mark-sweep 1980.4 (2092.2) -> 1978.5 (2091.3) MB, 235.5 / 0.0 ms  (average mu = 0.130, current mu = 0.044) allocation failure scavenge might not succeed
//   [21602:0x103004400]     8939 ms: Mark-sweep 1979.4 (2091.9) -> 1979.0 (2091.7) MB, 200.7 / 0.0 ms  (average mu = 0.077, current mu = 0.012) allocation failure scavenge might not succeed
//
//
// <--- JS stacktrace --->
//
// ==== JS stack trace =========================================
//
// 0: ExitFrame [pc: 0x1009104f9]
// Security context: 0x03bac6daddb9 <JSObject>
// 1: set [0x3bac6db2891](this=0x03ba026c1199 <Map map = 0x3ba8ac00d19>,8192,0)
// 2: /* anonymous */ [0x3ba5cac0239] [/Users/yiihua-013/consoles-projects/dsa4js/app/chapter3/3.5/34.js:~1] [pc=0x243b0cacb640](this=0x03ba77dbeb51 <Object map = 0x3ba8ac00459>,0x03ba77dbeb51 <Object map = 0x3ba8ac00459>,0x03ba5cac0299 <JSFunction require (sfi = 0x3ba84f3b921)>,0...
//
// 1: 0x100077a0b node::Abort() [/usr/local/bin/node]
// 2: 0x100077b8f node::OnFatalError(char const*, char const*) [/usr/local/bin/node]
// 3: 0x10016cc27 v8::Utils::ReportOOMFailure(v8::internal::Isolate*, char const*, bool) [/usr/local/bin/node]
// 4: 0x10016cbbc v8::internal::V8::FatalProcessOutOfMemory(v8::internal::Isolate*, char const*, bool) [/usr/local/bin/node]
// 5: 0x10033a175 v8::internal::Heap::FatalProcessOutOfMemory(char const*) [/usr/local/bin/node]
// 6: 0x10033b263 v8::internal::Heap::CheckIneffectiveMarkCompact(unsigned long, double) [/usr/local/bin/node]
// 7: 0x100338a43 v8::internal::Heap::PerformGarbageCollection(v8::internal::GarbageCollector, v8::GCCallbackFlags) [/usr/local/bin/node]
// 8: 0x1003366ff v8::internal::Heap::CollectGarbage(v8::internal::AllocationSpace, v8::internal::GarbageCollectionReason, v8::GCCallbackFlags) [/usr/local/bin/node]
// 9: 0x1003413f4 v8::internal::Heap::AllocateRawWithLightRetry(int, v8::internal::AllocationType, v8::internal::AllocationAlignment) [/usr/local/bin/node]
// 10: 0x10034146f v8::internal::Heap::AllocateRawWithRetryOrFail(int, v8::internal::AllocationType, v8::internal::AllocationAlignment) [/usr/local/bin/node]
// 11: 0x10030d6d9 v8::internal::Factory::NewFixedArrayWithFiller(v8::internal::RootIndex, int, v8::internal::Object, v8::internal::AllocationType) [/usr/local/bin/node]
// 12: 0x1004e4e22 v8::internal::OrderedHashTable<v8::internal::OrderedHashMap, 2>::Rehash(v8::internal::Isolate*, v8::internal::Handle<v8::internal::OrderedHashMap>, int) [/usr/local/bin/node]
// 13: 0x1005b1289 v8::internal::Runtime_MapGrow(int, unsigned long*, v8::internal::Isolate*) [/usr/local/bin/node]
// 14: 0x1009104f9 Builtins_CEntry_Return1_DontSaveFPRegs_ArgvOnStack_NoBuiltinExit [/usr/local/bin/node]
