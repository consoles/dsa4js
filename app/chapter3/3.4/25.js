// 各个字段乘以一个素数就行后叠加即可。对象中新增加一个域hash，注意多线程的问题，参见String#hashCode，使用临时变量，不要直接修改。