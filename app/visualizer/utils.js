const $utils = {
  sleep(s) {
    return new Promise(resolve => setTimeout(resolve, s * 1e3));
  },
  readLines(path) {
    const rl = require('readline').createInterface({
      input: require('fs').createReadStream(path)
    });
    return new Promise(resolve => {
      const lines = [];
      rl.on('line', line => {
        lines.push(line);
      });
      rl.on('close', () => resolve(lines));
    });
  }
};

if (typeof global !== "undefined") {
  module.exports = $utils;
}
