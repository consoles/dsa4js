const map = new Map();

map.set("A", 4.00);
map.set("B", 3.00);
map.set("C", 2.00);
map.set("D", 1.00);
map.set("F", 0.00);
map.set("A+", 4.33);
map.set("B+", 3.33);
map.set("C+", 2.33);
map.set("A-", 3.67);
map.set("B-", 2.67);

let total = 0;
let count = 0;

for (const value of map.values()) {
  total += value;
  count++;
}

console.log('GPA', total / count);
