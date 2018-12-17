#!/usr/bin python
#coding:utf-8
#
# 发一个随机红包，100块钱给10个人。每个人最多18块钱，最少4块钱。怎么分？
# ref:https://segmentfault.com/q/1010000006002081?_ea=979811
#
# 问题转化：
# 每个人先分配4元，问题就转化为“60块分给10个人，每人不多于14元”
# 将2个限制变成了一个限制

import random

total = 100.0
MIN = 4
MAX = 18
PEOPLE_NUM = 10

result = [MIN for x in range(PEOPLE_NUM)] # 每人先分4块
base = MAX - MIN
moneyLeft = total - MIN * PEOPLE_NUM

# 剩下的分:
# 在总钱数大于6块的时候，只要做一个0到6的随机就可以，
# 小于6块的时候，做0到这个总数的随机就可以，
# 最后一个人拿剩下的
for x in range(PEOPLE_NUM):
	if moneyLeft < 0:
		break
	prevMoney =  result[x]
	if x == PEOPLE_NUM - 1:
		addMoney = moneyLeft
	elif moneyLeft > base:
		addMoney = random.uniform(0,base)
	else:
		addMoney = random.uniform(0,moneyLeft)
	nowMoney = prevMoney + addMoney
	print str(x + 1) + ' -> ' + str(nowMoney)
	result[x] = nowMoney
	moneyLeft -= addMoney

print 'total = ' + str(total) + ',sum = ' + str(reduce(lambda x,y: x + y,result))
