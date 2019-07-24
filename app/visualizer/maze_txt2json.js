const fs = require('fs');
const path = require('path');

const {readLines} = require('./utils');

async function convertTxt2Json(filename) {
  const file = path.join(__dirname, filename);
  const lines = await readLines(file);
  const [rowCount, colCount] = lines[0].split(/\s+/).map(x => Number(x));
  const data = new Array(rowCount);
  for (let i = 1; i <= rowCount; i++) {
    data[i - 1] = [];
    for (let j = 0; j < colCount; j++) {
      data[i - 1][j] = lines[i][j];
    }
  }
  const {name} = path.parse(filename);
  fs.writeFileSync(path.join(__dirname, `${name}.json`), JSON.stringify(data));
}

convertTxt2Json('maze_101_101.txt');
