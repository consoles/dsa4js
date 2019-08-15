module.exports = class StopWatch {
  constructor() {
    this.startTime = 0;
    this.label = '';
  }
  start(label){
    this.startTime = Date.now();
    this.label = label;
  }
  stopAndPrint(){
    console.log(this.label,' => ',Date.now() - this.startTime);
  }
  restart(label){
    this.start(label);
  }
  get elapseTime(){
    return Date.now() - this.startTime;
  }
};
