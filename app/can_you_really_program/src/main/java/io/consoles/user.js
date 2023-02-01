class User {
    constructor(name) {
        this.name = name;
        this.directFriends = new Set()
    }
    befriend(other) {
        this.directFriends.add(other)
        other.directFriends.add(this)
    }
    /**
     * 是否直接好友
     */
    isDirectFriendOf(other) {
        return this.directFriends.has(other)
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
        for (const friend of this.directFriends) {
            if (friend.isDirectFriendOf(this) && friend.isDirectFriendOf(other)) return true
            const flag = friend.isIndirectFriendOf(other)
            if (flag) return true
        }
        return false
    }
    /**
     * 检查间接连接需要访问一个（无向）图，实现这个算法最简单的方式是使用 BFS
     * 从当前节点向外扩散，直到找到 other
     */
    isIndirectFriendOf2(other) {
        const queue = [this]
        const visited = new Set()
        while (queue.length > 0) {
            const user = queue.shift()
            if (user === other) {
                return true
            }
            if (!visited.has(user)) {
                visited.add(user)
                for (const u of user.directFriends) {
                    queue.push(u)
                }
            }
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
console.log(c.isIndirectFriendOf(a), c.isIndirectFriendOf2(a));
console.log(d.isDirectFriendOf(a));
console.log(d.isIndirectFriendOf(a), d.isIndirectFriendOf2(a));
