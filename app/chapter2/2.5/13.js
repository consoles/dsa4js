class MinPQ {
  constructor() {
    this._data = [-1];
    this.sz = 0;
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

class Job {
  constructor(name, time) {
    this.name = name;
    this.time = time;
  }

  compareTo(other) {
    return this.time - other.time;
  }
}

class Processor {
  constructor(name) {
    this.name = name;
    this.jobs = [];
    this.busyTime = 0;
  }

  addJob(job) {
    this.jobs.push(job);
    this.busyTime += job.time;
  }

  compareTo(other) {
    return this.busyTime - other.busyTime;
  }
}

class LPT {
  constructor(numOfProcessor) {
    const pq = new MinPQ();
    for (let i = 0;i < numOfProcessor;i++) {
      pq.insert(new Processor('processor-' + i));
    }
    this.pq = pq;
  }

  dispatchJob(job) {
    const processor = this.pq.delMin();
    processor.addJob(job);
    this.pq.insert(processor);
    console.log(processor.name, 'get the job', job.name, 'job.time = ', job.time, 'processor busy time = ', processor.busyTime);
  }
}

const jobs = [1,2,9,5,7,8,4,3,6].map((value,index) => new Job('job' + index,value)).sort((a,b) => a.compareTo(b));
const lpt = new LPT(3);
for (const job of jobs) {
  lpt.dispatchJob(job);
}

// processor-0 get the job job0 job.time =  1 processor busy time =  1
// processor-2 get the job job1 job.time =  2 processor busy time =  2
// processor-1 get the job job7 job.time =  3 processor busy time =  3
// processor-0 get the job job6 job.time =  4 processor busy time =  5
// processor-2 get the job job3 job.time =  5 processor busy time =  7
// processor-1 get the job job8 job.time =  6 processor busy time =  9
// processor-0 get the job job4 job.time =  7 processor busy time =  12
// processor-2 get the job job5 job.time =  8 processor busy time =  15
// processor-1 get the job job2 job.time =  9 processor busy time =  18
