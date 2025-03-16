var removeNthFromEnd = function(head, n) {
    // 1. 计算链表长度
    let len = 0;
    let cur = head;
    while (cur) {
        len++;
        cur = cur.next;
    }
    // 删除倒数第 n 个节点 -> 删除正数第 len - n 个节点(从 0 开始)
    // 2. 找到要删除的节点的前一个节点（这里引入 dummyHead 简化操作）
    let dummy = {next: head};
    let prev = dummy;
    for (let i = 0; i < len - n; i++) {
        prev = prev.next;
    }
    // 3. 删除节点
    if (prev.next) {
        prev.next = prev.next.next;
    }
    return dummy.next;
}

var removeNthFromEnd2 = function(head, n) {
    const dummy = {next: head}
    // fast 和 slow 同时指向头部
    // 接下来 fast 先走 n 步，fast 和 slow 相差 n
    // 再加下来 fast 和 slow 同时移动，fast 到末尾的前一个的时候，slow 指向需要删除节点的前一个
    let slow = dummy
    let fast = dummy
    while(n > 0) {
        fast = fast.next
        n--
    }
    while(fast.next) {
        fast = fast.next
        slow = slow.next
    }
    slow.next = slow.next.next
    return dummy.next
};
