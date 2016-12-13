#!/usr/bin python
#coding:utf-8
#
# 生成10^7个乱序整数

import random

RANGE = 10000000

f = open('../test/input/bitSort.input','w')

for i in random.sample(range(RANGE),RANGE):
	f.write(str(i) + '\n')
f.close()

print 'generator input file success!'	