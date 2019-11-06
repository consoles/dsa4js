const classRecord = [
  {name:'A',time:'9:00'},
  {name:'A',time:'10:00'},
  {name:'A',time:'9:00'},
  {name:'A',time:'2:00'},
  {name:'A',time:'3:00'},
  {name:'B',time:'9:00'},
  {name:'A',time:'10:00'},
  {name:'B',time:'9:00'},
  {name:'B',time:'2:00'},
  {name:'B',time:'3:00'},
];

const obj = {};
for (let i = 0;i < classRecord.length;i++){
  const record = classRecord[i];
  const {name,time} = record;
  obj[name] = obj[name] ||  new Set();
  if (obj[name].has(time)) {
    console.error(i,'老师',name,'在',time,'已经有课程了');
  } else {
    obj[name].add(time);
  }
}
