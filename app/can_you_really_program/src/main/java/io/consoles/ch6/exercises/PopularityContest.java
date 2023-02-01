package io.consoles.ch6.exercises;

/**
 * 在 T 类型的对象集中的一场 “人气竞赛”
 * @param <T>
 */
public interface PopularityContest <T>{
    /**
     * 添加一个新的参赛者，添加重复的参赛者会被忽略。
     * @param contestant
     */
    void addContestant(T contestant);

    /**
     * 为指定参赛者投票。如果该参赛者不属于本次比赛，则抛出 IllegalArgumentException
     * @param contestant
     */
    void voteFor(T contestant);

    /**
     * 返回到目前为止获得最高票数的参赛者。如果这个比赛是空的（没有参赛者），那么它将抛出一个 IllegalStateException。
     * @return
     */
    T getMostVoted();
}

// 提高以上泛型接口的可测试性

// 给定的接口很容易控制，但你可以增强其可观察性。就目前而言，getMostVoted 是访问对
// 象内部状态的唯一点，而且比较有限。你只能知道获得最多投票的参赛者是谁，但不知道任何参
// 赛者获得的投票数。为了改善这种情况，可以先给另外两个方法配备返回值:

// boolean addContestant(T contestant); 如果参赛者还不是这个比赛的成员，则增加参赛者并返回 true。否则，它将保持比赛不变并返回 false
// int voteFor(T contestant)：为指定的参赛者投票，并返回更新后的票数。如果该参赛者不属于本次比赛，则抛出 IllegalArgumentException。
// 新版本的 voteFor 是一个强大的测试工具，但它会把投票数和读取票数混为一谈。如果再加上一个获取投票数的只读方法，对测试也是很有用的。
// int getVotes(T contestant)：返回指定参赛者的当前票数。如果选手不属于这个比赛，就会抛出一个 IllegalArgumentException
// 此外，getVotes 方法还提供了一个检查参赛者是否属于本次比赛的途径，而且不需要修改此方法
