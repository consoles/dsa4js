const SeparateChainingHashST = require('../SeparateChainingHashST');

class SeparateChainingHashST18 extends SeparateChainingHashST {
  /**
   * @param M
   * @param avgProbCount 平均探测次数
   */
  constructor(M, avgProbCount) {
    super(M);
    this.avgProbCount = avgProbCount;
  }
}
