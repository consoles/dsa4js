/**
 * @param {number[]} nums
 * @return {number[][]}
 */
var permute = function(nums) {
  const res = [];

  function backtrack(nums,track){
    if (track.length === nums.length) {
      res.push(track.slice);
      return;
    }
    for (let i = 0; i < nums.length;i++) {
      track.push(nums[i]);

    }
  }

  console.log(res);
  return res;
};

permute([1,2,3]);
