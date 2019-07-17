const fs = require('fs');
const path = require('path');

const compareFns = {
  compareToSizeAsc(self, other) {
    return self.size - other.size;
  },
  compareToMtimeAsc(self, other) {
    return self.mtime - other.mtime;
  },
  compareToSizeDesc(self, other) {
    return other.size - self.size;
  },
  compareToMtimeDesc(self, other) {
    return other.mtime - self.mtime;
  }
};

class File {
  constructor(path) {
    const {size, mtime} = fs.statSync(path);
    this.path = path;
    this.size = size;
    this.mtime = mtime;
  }

  inspect() {
    return `path:${this.path},size:${this.size},mtime:${this.mtime}`;
  }
}

// 使用插入排序保证排序的稳定性
function insertSort(arr, compareFns) {
  function less(i, j) {
    for (const compareFn of compareFns) {
      const cmp = compareFn(arr[i], arr[j]);
      if (cmp !== 0) {
        return cmp;
      }
    }
    return 0;
  }

  for (let i = 1; i < arr.length; i++) {
    for (let j = i; j > 0 && less(j, j - 1) < 0; j--) {
      [arr[j], arr[j - 1]] = [arr[j - 1], arr[j]];
    }
  }
}

// ls 目录名 比较字段名1 比较顺序1 比较字段2 比较顺序2

// ./ ['Size','Asc','Mtime','Desc']
function ls(dirName, sortOptions) {
  const files = fs.readdirSync(dirName).map(file => new File(path.join(dirName, file)));
  console.log('before', files);
  const compares = [];
  for (let i = 0; i < sortOptions.length; i += 2) {
    const [sortKey, sortFlag] = [sortOptions[i], sortOptions[i + 1]];
    const fnName = `compareTo${sortKey}${sortFlag}`;
    compares.push(compareFns[fnName]);
  }
  insertSort(files, compares);
  console.log('after', files);
}

ls('./', ['Mtime', 'Asc', 'Size', 'Asc']);
