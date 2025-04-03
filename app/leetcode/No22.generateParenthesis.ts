function generateParenthesis(n: number): string[] {
    const len = 2 * n
    const paths = new Array<string>()

    function isValid(s: string): boolean {
        // 合法的括号只能是以左括号开头
        // 不需要使用显式的栈结构
        let count = 0
        for (const c of s) {
            if (c === '(') {
                count++
            } else {
                count--
                if (count < 0) {
                    return false
                }
            }
        }
        return count === 0
    }

    function dfs(index: number, path: string) {
        if (index === len) {
            if (isValid(path)) {
                paths.push(path)
            }
            return
        }
        // 当前的位置可能是左括号也有可能是右括号
        dfs(index + 1, path + '(')
        dfs(index + 1, path + ')')
    }
    dfs(0, '')
    return paths
};

function generateParenthesis2(n: number): string[] {
    // 上面的方法还有改进的余地：我们可以只在序列仍然保持有效时才添加 ‘(’ 或 ‘)’，而不是像 方法一 那样每次添加
    // 如果左括号数量不大于 n，我们可以放一个左括号。
    // 如果右括号数量小于左括号的数量，我们可以放一个右括号。
    const paths = new Array<string>()
    /**
     * @param leftCount 左括号数量
     * @param rightCount 右括号数量
     * @param path 
     */
    function dfs(leftCount: number, rightCount: number, path: string) {
        if (path.length === 2 * n) {
            paths.push(path)
            return
        }
        if (leftCount < n) {
            dfs(leftCount + 1, rightCount, path + '(')
        }
        if (rightCount < leftCount) {
            dfs(leftCount, rightCount + 1, path + ')')
        }
    }
    dfs(0, 0, '');
    return paths
}

const r = generateParenthesis(3)
console.log(r);
