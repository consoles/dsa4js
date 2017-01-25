'use strict'

/**
 * 括号匹配问题
 */

var checkMatch = function(str){
    var stack = []
    var i;
    for(i = 0;i < str.length;i++) {
        let s = str[i]
        if(s == '(' || s == '[' || s == '{') stack.push(s)
        if(stack.length === 0) break
        else if (s == '}' || s == ']' || s == ')') {
            var symbol = stack.pop()
            if (s == '}' && symbol != '{') break;
            else if (s == ')' && symbol != '(') break;
            else if (s == ']' && symbol != '[') break;
        }
    }
    return stack.length === 0 && i === str.length
}

console.log(checkMatch('[()]{}{[()()]()}'))
console.log(checkMatch('[(])'))
console.log(checkMatch('[fgf'))
console.log(checkMatch('[[]]'))