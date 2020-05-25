// 设计一个简化版的推特(Twitter)，可以让用户实现发送推文，关注/取消关注其他用户，能够看见关注人（包括自己）的最近十条推文。你的设计需要支持以下的几个功能：
//
// postTweet(userId, tweetId): 创建一条新的推文
// getNewsFeed(userId): 检索最近的十条推文。每个推文都必须是由此用户关注的人或者是用户自己发出的。推文必须按照时间顺序由最近的开始排序。
// follow(followerId, followeeId): 关注一个用户
// unfollow(followerId, followeeId): 取消关注一个用户
// 示例:
//
//   Twitter twitter = new Twitter();
//
// // 用户1发送了一条新推文 (用户id = 1, 推文id = 5).
// twitter.postTweet(1, 5);
//
// // 用户1的获取推文应当返回一个列表，其中包含一个id为5的推文.
// twitter.getNewsFeed(1);
//
// // 用户1关注了用户2.
// twitter.follow(1, 2);
//
// // 用户2发送了一个新推文 (推文id = 6).
// twitter.postTweet(2, 6);
//
// // 用户1的获取推文应当返回一个列表，其中包含两个推文，id分别为 -> [6, 5].
// // 推文id6应当在推文id5之前，因为它是在5之后发送的.
// twitter.getNewsFeed(1);
//
// // 用户1取消关注了用户2.
// twitter.unfollow(1, 2);
//
// // 用户1的获取推文应当返回一个列表，其中包含一个id为5的推文.
// // 因为用户1已经不再关注用户2.
// twitter.getNewsFeed(1);
//
// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/design-twitter
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。

/**
 * 最大堆实现优先队列
 */
class PriorityQueue {
  constructor(cmpFun) {
    this.cmpFun = cmpFun;
    // 最大堆，索引从1开始计数，左孩子2*k,右孩子2*k+1,父节点k/2
    this.data = [-1];
  }

  _swap(i, j) {
    [this.data[i], this.data[j]] = [this.data[j], this.data[i]];
  }

  _swim(k) {
    while (k > 1) {
      const p = parseInt(k / 2);
      if (this.cmpFun(this.data[k], this.data[p]) > 0) {
        this._swap(k, p);
        k = p;
      } else {
        break;
      }
    }
  }

  _sink(k) {
    while (2 * k < this.data.length) {
      let j = 2 * k;
      if (this.cmpFun(this.data[j + 1], this.data[j]) > 0) {
        j++;
      }
      if (this.cmpFun(this.data[j], this.data[k]) > 0) {
        this._swap(j, k);
        k = j;
      } else {
        break;
      }
    }
  }

  offer(e) {
    this.data.push(e);
    const k = this.data.length - 1;
    this._swim(k);
  }

  isEmpty() {
    return this.data.length === 1;
  }

  clear() {
    this.data = [-1];
  }

  poll() {
    const e = this.data[1];
    if (!e) return e;
    const len = this.data.length - 1;
    this._swap(1, len);
    this.data.length = len;
    this._sink(1);
    return e;
  }
}

// const q = new PriorityQueue((a, b) => a - b);
// q.offer(1);
// q.offer(5);
// q.offer(3);
// q.offer(2);
// q.offer(4);
//
// while (!q.isEmpty()) {
//   console.log(q.poll());
// }

/**
 * 推文类：是一个单链表（节点视角）
 */
class Tweet {
  constructor(id, timestamp) {
    this.id = id;
    this.timestamp = timestamp;
    this.next = null;
  }
}

class Twitter {
  constructor() {
    /**
     * uid和推文（单链表）之间的对应关系
     * @type {Map<any, any>}
     */
    this.twitter = new Map();

    /**
     * uid和他关注的用户列表的对应关系
     * @type {Map<any, any>}
     */
    this.followings = new Map();

    /**
     * 全局使用的时间戳字段，用户每发布一条推文之前加1
     * @type {number}
     */
    this.timestamp = 0;

    /**
     * 合并k组推文所使用的数据结构
     * @type {PriorityQueue}
     */
    this.maxHeap = new PriorityQueue((a, b) => a.timestamp - b.timestamp);
  }

  postTweet(userId, tweetId) {
    const newHead = new Tweet(tweetId, ++this.timestamp);
    if (this.twitter.has(userId)) {
      newHead.next = this.twitter.get(userId);
    }
    this.twitter.set(userId, newHead);
  }

  getNewsFeed(userId) {
    // 由于是全局使用，所以使用的时候要清空
    this.maxHeap.clear();

    // 合并k个有序链表

    if (this.twitter.has(userId)) {
      this.maxHeap.offer(this.twitter.get(userId));
    }

    const followingList = this.followings.get(userId);
    if (followingList && followingList.size > 0) {
      for (const followingId of followingList) {
        const tweet = this.twitter.get(followingId);
        if (tweet) {
          this.maxHeap.offer(tweet);
        }
      }
    }

    const res = [];
    while (!this.maxHeap.isEmpty() && res.length < 10) {
      const head = this.maxHeap.poll();
      res.push(head.id);
      if (head.next) {
        this.maxHeap.offer(head.next);
      }
    }
    return res;
  }

  follow(followerId, followeeId) {
    // 被关注的人不能是自己
    if (followeeId === followerId) {
      return;
    }
    const followingList = this.followings.get(followerId);
    if (!followingList) {
      const set = new Set();
      set.add(followeeId);
      this.followings.set(followerId, set);
    } else {
      if (followingList.has(followerId)) {
        return;
      }
      followingList.add(followeeId);
    }
  }

  unfollow(followerId, followeeId) {
    if (followeeId === followerId) {
      return;
    }
    const followingList = this.followings.get(followerId);
    if (!followingList) {
      return;
    }
    followingList.delete(followeeId);
  }
}

let twitter = new Twitter();

// 用户1发送了一条新推文 (用户id = 1, 推文id = 5).
twitter.postTweet(1, 5);

// 用户1的获取推文应当返回一个列表，其中包含一个id为5的推文.
let ret = twitter.getNewsFeed(1);
console.log(ret);

// 用户1关注了用户2.
twitter.follow(1, 2);

// 用户2发送了一个新推文 (推文id = 6).
twitter.postTweet(2, 6);

// 用户1的获取推文应当返回一个列表，其中包含两个推文，id分别为 -> [6, 5].
// 推文id6应当在推文id5之前，因为它是在5之后发送的.
ret = twitter.getNewsFeed(1);
console.log(ret);

// 用户1取消关注了用户2.
twitter.unfollow(1, 2);

// 用户1的获取推文应当返回一个列表，其中包含一个id为5的推文.
// 因为用户1已经不再关注用户2.
ret = twitter.getNewsFeed(1);
console.log(ret);

// 来源：力扣（LeetCode）
// 链接：https://leetcode-cn.com/problems/design-twitter
//   著作权归领扣网络所有。商业转载请联系官方授权，非商业转载请注明出处。
