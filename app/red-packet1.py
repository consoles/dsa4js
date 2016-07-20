#!/usr/bin python
#coding:utf-8
#
# 发一个随机红包，100块钱给10个人。每个人最多18块钱，最少4块钱。怎么分？
# ref:https://segmentfault.com/q/1010000006002081?_ea=979811

import random

total = 100.0
MIN = 4
MAX = 18
PEOPLE_NUM = 10

moneyLeft = total # 未分配的钱数
peopleLeft = PEOPLE_NUM # 未分配的人数

result = []
lowerBound = MIN
upperBound = MAX
moneyNow = 0
sum = 0

for x in range(PEOPLE_NUM):
	# 每次取随机的范围都是变化的
	# 下限从4和（剩余钱数-18*（剩余人数-1））中取大的
	# 上限从18和（剩余钱数-4*（剩余人数-1））中取小的
	peopleLeft = PEOPLE_NUM - x
	lowerBound = max(MIN, moneyLeft - MAX * (peopleLeft - 1))
	upperBound = min(MAX, moneyLeft - MIN * (peopleLeft - 1))
	m = random.uniform(lowerBound,upperBound)
	moneyNow = round(m,2)
	print str(x + 1) + ' -> ' + str(moneyNow)
	result.append(moneyNow)
	sum += moneyNow
	moneyLeft -= moneyNow

print 'total = ' + str(total) +	',and sum = ' + str(sum)