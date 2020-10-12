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
    }
}

// arr = [1, 2, 2, 3, 3, null, null, 4, 4]
// root = module.exports.buildBinaryTreeFromArray(arr)
// debugger