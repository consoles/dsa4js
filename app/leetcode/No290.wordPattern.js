/**
 * @param {string} pattern
 * @param {string} s
 * @return {boolean}
 */
var wordPattern = function (pattern, s) {
    const len1 = pattern.length;
    const ss = s.split(' ');
    const len2 = ss.length;
    if (len1 !== len2) return false;

    const map = new Map();
    const recvMap = new Map();
    for (let i = 0; i < len1; i++) {
        const c1 = pattern[i];
        const c2 = ss[i];
        if (map.has(c1)) {
            if (map.get(c1) !== c2) {
                return false;
            }
        } else {
            if (recvMap.has(c2)) {
                return false;
            }
            recvMap.set(c2, c1);
            map.set(c1, c2);
        }
    }
    return true;
};

pattern = "abba", str = "dog cat cat dog"
pattern = "abba", str = "dog cat cat fish"
pattern = "aaaa", str = "dog cat cat dog"
pattern = "abba", str = "dog dog dog dog"
flag = wordPattern(pattern, str)
debugger
