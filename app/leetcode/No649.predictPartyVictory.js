/**
 * @param {string} senate
 * @return {string}
 */
var predictPartyVictory = function (senate) {
    // 没有理解题目意思，其实很简单：
    // 1.乙方阵营要达成胜利
    // 2.胜利条件：只剩自己人，即尽可能干掉敌方阵营的人
    // 3.限制：每一轮中每个人只能干掉一个敌人，我们只需要贪心地干掉后面最近的一个敌人就行了

    const n = senate.length;
    const radiant = [], dire = [];
    for (const [i, ch] of Array.from(senate).entries()) {
        if (ch === 'R') {
            radiant.push(i);
        } else {
            dire.push(i);
        }
    }
    // 如果所有议员都属于某一方，则直接可以宣布某一方胜利
    // 如果有敌方成员，应该贪心选择投票顺序下一位的敌方成员：既然只能挑选一名夜魇方的议员，那么就应该挑最早可以进行投票的那一名议员；如果挑选了其它较晚投票的议员，那么等到最早可以进行投票的那一名议员行使权利时，一名天辉方议员就会丧失权利，这样就得不偿失了

    // 模拟过程
    while (radiant.length && dire.length) {
        const rItem = radiant[0];
        const dItem = dire[0];
        if (rItem < dItem) {
            dire.shift(); // 敌方永久弹出
            radiant.shift(); // 己方首元素弹出后 增加n后再重新放到队列中，表示该议员参与下一轮的投票
            radiant.push(rItem + n);
        } else {
            radiant.shift();
            dire.shift();
            dire.push(dItem + n);
        }
    }
    return radiant.length ? 'Radiant' : 'Dire';
};

s = 'RD';
s = 'RDD';
s = 'RRR';
ret = predictPartyVictory(s);
debugger