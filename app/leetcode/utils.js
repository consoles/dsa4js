module.exports = {
    buildBinaryTreeFromArray(arr) {
        const n = arr.length
        function buildTree(index) {
            if (index >= n || arr[index] === null) return null
            return {
                val: arr[index],
                left: buildTree(2 * index + 1),
                right: buildTree(2 * index + 2)
            }
        }
        return buildTree(0)
    },
    buildLinkedListFromArray(arr){
        let head = null
        let prev = null
        for(const val of arr) {
            const node = {val,next:null}
            if (!head) {
                prev = head = node
            } else {
                prev.next = node
                prev = node
            }
        }
        return head
    },
    dumyLinkedList(head){
        const arr = []
        let cur = head
        while(cur) {
            arr.push(cur.val)
            cur = cur.next
        }
        return arr
    }
}

// arr = [1, 2, 2, 3, 3, null, null, 4, 4]
// root = module.exports.buildBinaryTreeFromArray(arr)
// debugger
// const arr = [1,2,3,4,5]
// const head = module.exports.buildLinkedListFromArray(arr)
// debugger
// const dumyArr = module.exports.dumyLinkedList(head)
// debugger