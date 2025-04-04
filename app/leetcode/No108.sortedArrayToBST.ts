class TreeNode {
    val: number
    left: TreeNode | null
    right: TreeNode | null
    constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
        this.val = (val === undefined ? 0 : val)
        this.left = (left === undefined ? null : left)
        this.right = (right === undefined ? null : right)
    }
}


function sortedArrayToBST(nums: number[]): TreeNode | null {
    if (nums.length === 0) {
        return null
    }
    const middleIndex = Math.floor(nums.length / 2)
    const root = new TreeNode(nums[middleIndex])
    root.left = sortedArrayToBST(nums.slice(0, middleIndex))
    root.right = sortedArrayToBST(nums.slice(middleIndex + 1))
    return root
};

function sortedArrayToBST2(nums: number[]): TreeNode | null {
    function buildTree(left: number, right: number): TreeNode | null {
        if (left > right) {
            return null
        }
        const middleIndex = Math.floor((left + right) / 2)
        const root = new TreeNode(nums[middleIndex])
        root.left = buildTree(left, middleIndex - 1)
        root.right = buildTree(middleIndex + 1, right)
        return root
    }
    return buildTree(0, nums.length - 1)
};
