class Job{
  constructor(name,time){
    this.name = name;
    this.time = time;
  }
  compareTo(other){
    return this.time - other.time;
  }
}

// 按照时间升序排列即可
