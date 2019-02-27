// 热还是冷。
// 你的目标是猜出 1 到 N 之间的一个秘密的整数。
// 每次猜完一个整数后，你会直到你的猜测距离该秘密整数是否相等（如果是则游戏结束）。
// 如果不相等，你会知道你的猜测相比上一次猜测距离秘密整数是比较热（接近），还是比较冷（远离）。
// 设计一个算法在 ~2lgN 之内找到这个秘密整数，然后设计一个算法在 ~1lgN 之内找到这个秘密整数。

// 第一种方案，类似于二分查找：先猜测左边界(lo) ，再猜测右边界(hi) ，如果边界值猜中的话直接返回，否则：
//如果右边界比较热，那么左边界向右边界靠，lo = mid - 1；否则，右边界向左边界靠，hi = mid + 1。其中，mid = lo + (hi – lo) /2。
// 每次二分查找都要猜测两次，~2lgN。

const GUESS_RESULT = {
    COLD: Symbol('cold'),
    HOT: Symbol('hot'),
    EQ: Symbol('eq'),
    FIRST_GUESS: Symbol('first_guess')
};

class Game {
    constructor() {
        this.N = 100000;
        this.MAGIC = 33;
        this.lastGuess = -1;
        this.guessCount = 0;
    }
    guess(n) {
        this.guessCount++;
        if (n === this.MAGIC) {
            return GUESS_RESULT.EQ;
        }
        if (this.lastGuess === -1) {
            this.lastGuess = n;
            return GUESS_RESULT.FIRST_GUESS;
        }
        const lastDiff = Math.abs(this.MAGIC - this.lastGuess);
        this.lastGuess = n;
        const nowDiff = Math.abs(this.MAGIC - n);
        return nowDiff > lastDiff ? GUESS_RESULT.COLD : GUESS_RESULT.HOT;
    }
}

const task1 = () => {
    const game = new Game();
    let lo = 1, hi = game.N;
    while (lo <= hi) {
        // 无法比较大小，所以需要guess两次，最小边界和最大边界
        let guessRes = game.guess(lo);
        if (guessRes === GUESS_RESULT.EQ) {
            return game.guessCount;
        }
        guessRes = game.guess(hi);
        if (guessRes === GUESS_RESULT.EQ) {
            return game.guessCount;
        }
        const mid = lo + Math.floor((hi - lo) / 2);
        if (guessRes === GUESS_RESULT.HOT) {
            lo = mid;
        } else {
            hi = mid;
        }
    }
}

const task2 = () => {
    const game = new Game();
    let lo = 1, hi = game.N;
    let isRightSide = true;
    // 第一次猜测
    let guessRes = game.guess(lo);
    if (guessRes === GUESS_RESULT.EQ) {
        return game.guessCount;
    }
    while (lo < hi) {
        const mid = lo + Math.floor((hi - lo) / 2);
        const nowGuess = (lo + hi) - game.lastGuess;
        guessRes = game.guess(nowGuess);
        if (guessRes === GUESS_RESULT.EQ) {
            return game.guessCount;
        }
        if (guessRes === GUESS_RESULT.HOT) {
            if (isRightSide) {
                lo = mid;
            } else {
                hi = mid;
            }
        } else {
            if (isRightSide) {
                hi = mid;
            } else {
                lo = mid;
            }
        }
        isRightSide = !isRightSide;
        if (hi - lo <= 1) {
            if (game.guess(lo) === GUESS_RESULT.EQ) {
                return game.guessCount;
            }
            if (game.guess(hi) === GUESS_RESULT.EQ) {
                return game.guessCount;
            }
        }
    }
}

console.log(task1());
console.log(task2());