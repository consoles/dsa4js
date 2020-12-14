/**
 * @param {string[]} strs
 * @return {string[][]}
 */
var groupAnagrams = function (strs) {
    // 采用类似桶排序的思想
    const map = {};
    for (const str of strs) {
        const newStr = str.split('').sort();
        map[newStr] = map[newStr] || [];
        map[newStr].push(str);
    }
    return Object.values(map);
};

const res = groupAnagrams(["eat", "tea", "tan", "ate", "nat", "bat"]);
debugger