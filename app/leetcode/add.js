// 两个大正整数相加

function add(str1, str2) {
  let i = str1.length - 1, j = str2.length - 1;
  let carry = 0;
  let res = '';
  while (i >= 0 || j >= 0 || carry) {
    const x = i >= 0 ? parseInt(str1[i]) : 0;
    const y = j >= 0 ? parseInt(str2[j]) : 0;
    const sum = x + y + carry;
    res = sum % 10 + res; // 新产生的结果是高位
    carry = parseInt(sum / 10);
    i >= 0 ? i-- : 0;
    j >= 0 ? j-- : 0;
  }
  return res;
}

// 整数减法，不支持负数
function sub(str1, str2) {
  let s1Max = str1.length > str2.length;
  // 两个字符串相等特殊判断
  if (str1.length === str2.length) {
    for (let i = 0; i < str1.length; i++) {
      if (str1[i] === str2[i]) {
        continue;
      }
      s1Max = str1[i] > str2[i];
      break;
    }
  }
  if (!s1Max) [str1, str2] = [str2, str1];
  let res = '';
  let carry = 0;
  let i = str1.length - 1, j = str2.length - 1;
  while (i >= 0) {
    const x = parseInt(str1[i]);
    const y = parseInt(str2[j]) || 0;
    let tmp = x - y - carry;
    if (tmp >= 0) {
      res = tmp + res;
      carry = 0;
    } else {
      res = tmp + 10 + res;
      carry = 1;
    }
    i--;
    j--;
  }
  // 去除前面的0
  i = 0;
  while (i < res.length) {
    if (res[i] !== '0') {
      break;
    }
    i++;
  }
  res = res.substring(i);
  return {
    sign: s1Max ? '' : '-',
    sum: res ? res : '0'
  };
}

function bigNumAdd(str1, str2) {
  let sign = '';
  let sum = '';
  const n1Negative = str1.indexOf('-') === 0;
  const n2Negative = str2.indexOf('-') === 0;
  if (n1Negative && n2Negative) {
    sign = '-';
    sum = add(str1.substring(1), str2.substring(1));
  } else if (!n1Negative && !n2Negative) {
    sum = add(str1, str2);
  } else {
    // 一正一负，按照减法，先确定被减数，保证被减数的绝对值比较大
    let s1 = n1Negative ? str1.substring(1) : str1;
    let s2 = n2Negative ? str2.substring(1) : str2;
    const res = sub(s1, s2);
    sum = res.sum;
    sign = res.sign;
  }

  console.log(sign, sum);
  if (sum === '0') sign = '';
  return sign + sum;
}

// let res = bigNumAdd('4322131234666', '-3322131234666');
// let res = bigNumAdd('3322131234666', '-4322131234666');
// console.log(res);

const res = sub('1000','345')
console.log(res);
