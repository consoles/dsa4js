const fs = require('fs');

function filenameSort(dirName) {
  const files = fs.readdirSync(dirName).sort();
  console.log(files);
}

filenameSort('/etc/');
