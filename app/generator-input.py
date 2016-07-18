#!/usr/bin python
#coding:utf-8
#
# 生成10^7个乱序整数

import random

RANGE = 10000000
arr = [i for i in range(RANGE)]
random.shuffle(arr)

f = open('../test/input/bitSort.input','w')

for i in arr:
	f.write(str(i) + '\n')
f.close()

print 'generator input file success!'	