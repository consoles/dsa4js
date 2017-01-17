/**
 * 矩阵库
 */

'use strict'

class Martix {
  /**
   * 向量点乘
   */
  dot(arr1,arr2) {
    var result = 0;
    if (arr1.length !== arr2.length) throw new TypeError('向量不对')
    for (let i = 0;i < arr1.length;i++) result += arr1[i] * arr2[i];
    return result;  
  }

  /**
   * 矩阵相乘
   */
  mult(martix1,martix2){
    // a * b 的矩阵和 b * c 的矩阵才能进行运算，得到的结果是a * c的矩阵
    if (martix1[0].length !== martix2.length) throw new TypeError('向量不对')

    var result = new Array(martix1.length);
    for(let i = 0;i < result.length;i++) result[i] = new Array(martix2[0].length);

    // fill result array
    for(let i = 0;i < result.length;i++) 
      for(let j = 0;j < result[i].length;j++)
        for (let k = 0;k < martix2.length;k++)
          result[i][j] = martix1[i][k] * martix2[k][j]

    return result       
  }

  /**
   * 转置
   */
  transpose(martix) {
    var result = new Array(martix[0].length)
    for(let i = 0;i < result.length;i++) result[i] = new Array(martix.length)

    for(let i = 0;i < result.length;i++)
      for(let j = 0;j < result[i].length;j++)
        result[i][j] = martix[j][j]

    return martix      
  }

  /**
   * 矩阵和向量之积
   *
   * a * b 与 b * 1的矩阵得到 a * 1的矩阵
   */
  mul2(martix,arr) {
    if (martix[0].length !== arr.length) throw new TypeError('向量不对')

    var martix2 = new Array(arr.length);
    for (let i = 0;i < martix2.length;i++) martix2[i] = [arr[i]];
    
    return mult(martix,martix2);
  }

}