class Time {
  constructor(hour,minute,second){
    this.hour = hour;
    this.minute = minute;
    this.second = second;
  }
  compareTo(other){
    let diff = this.hour - other.hour;
    if (diff !== 0) return diff;
    diff = this.minute - other.minute;
    return diff !== 0 ? diff : this.second - other.second;
  }
}

class Event {
  constructor(name) {
    this.name = name;
  }
}

// 利用Time记录时间，Event记录事件内容
// key 为Time,value为Event构造符号表
