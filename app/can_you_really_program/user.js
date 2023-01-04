class User {
    constructor(name) {
        this.name = name;
        this.friends = new Set()
    }
    befriend(other) {
        this.friends.add(other)
        other.friends.add(this)
    }
    /**
     * 是否直接好友
     */
    isDirectFriendOf(other) {
        return this.friends.has(other)
    }
    /**
     * a - b
     * b - c
     * 则 a - c 是间接好友，即存在一条路径 a - c
     * 问题转化为：是否存在一条路径，从 this -> other
     * 是否间接好友
     */
    isIndirectFriendOf(other) {
        if (this.isDirectFriendOf(other)) return false
        for (const friend of this.friends) {
            if (friend.isDirectFriendOf(this) && friend.isDirectFriendOf(other)) return true
            const flag = friend.isIndirectFriendOf(other)
            if (flag) return true
        }
        return false
    }
}

const a = new User('a')
const b = new User('b')
const c = new User('c')
const d = new User('d')
a.befriend(b)
b.befriend(c)
c.befriend(d)
console.log(a.isDirectFriendOf(b));
console.log(c.isDirectFriendOf(b));
console.log(c.isDirectFriendOf(a));
console.log(c.isIndirectFriendOf(a));
console.log(d.isDirectFriendOf(a));
console.log(d.isIndirectFriendOf(a));
