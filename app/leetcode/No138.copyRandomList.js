/**
 * // Definition for a _Node.
 * function _Node(val, next, random) {
 *    this.val = val;
 *    this.next = next;
 *    this.random = random;
 * };
 */

/**
 * @param {_Node} head
 * @return {_Node}
 */
var copyRandomList = function(head) {
    function copy(head, cachedNodeMap) {
        if (head === null) return null
        if (!cachedNodeMap.has(head)) {
            const node = {
                val: head.val,
                next: null,
                random: null
            }
            cachedNodeMap.set(head, node)
            node.next = copy(head.next, cachedNodeMap)
            node.random = copy(head.random, cachedNodeMap)
            return node
        }
        return cachedNodeMap.get(head)
    }
    return copy(head, new Map())
};

var copyRandomList2 = function(head) {
    if (head === null) return null
    let cur = head
    // 建立老节点到新节点的映射
    const old2NewMap = new Map()
    while (cur !== null) {
        const node = {
            val: cur.val,
            next: null,
            random: null
        }
        old2NewMap.set(cur, node)
        cur = cur.next
    }
    cur = head
    // 修复新节点的next和random指针
    while (cur !== null) {
        const node = old2NewMap.get(cur)
        node.next = old2NewMap.get(cur.next) || null
        node.random = old2NewMap.get(cur.random) || null
        cur = cur.next
    }
    return old2NewMap.get(head)
}

const node7 = {val: 7}
const node13 = {val: 13}
const node11 = {val: 11}
const node10 = {val: 10}
const node1 = {val: 1}

node7.next = node13
node13.next = node11
node11.next = node10
node10.next = node1
node1.next = null

node7.random = null
node13.random = node7
node11.random = node1
node10.random = node11
node1.random = node7

const r = copyRandomList2(node7)
let cur = r
while (cur) {
    console.log(cur.val, cur.random ? cur.random.val : null)
    cur = cur.next  
}
// 7 null
// 13 7
// 11 1
// 10 11
// 1 7
