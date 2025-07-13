from typing import List

class Solution:
    def matchPlayersAndTrainers(self, players: List[int], trainers: List[int]) -> int:
        """
        贪心：
        排序 + 双指针
        """
        players.sort()
        trainers.sort()
        i = j = 0
        while i < len(players) and j < len(trainers):
            if players[i] <= trainers[j]:
                i += 1
                j += 1
            else:
                j += 1
        return i
    
# 2
# players = [4,7,9]
# trainers = [8,2,5,8]    

# 1
players = [1,1,1]
trainers = [10]
r = Solution().matchPlayersAndTrainers(players, trainers)
print(r)