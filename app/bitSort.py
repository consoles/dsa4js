#!/use/bin python
#coding:utf-8
# 位排序

import time

start = time.time()

#1.init set to empty
RANGE = 10000000
arr = [0 for i in range(RANGE)]

#2.insert each present elements into the set
for line in open('../test/input/bitSort.input'):
	num = int(line)
	arr[num] = 1

#3.write sorted ouput
f = open('../test/output/bitSort.output','w')
for i in range(RANGE):
	if arr[i] == 1:
		f.write(str(i) + '\n')	
f.close()

end = time.time()

print 'arrar is sorted and generator output file success! and take time:' + str(end - start) 