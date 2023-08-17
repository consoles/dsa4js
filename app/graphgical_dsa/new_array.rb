# 取出数组中相邻的元素组成新的数组

# O(1.5N), N 遍历，0.5N 插入
def every_other(arr)
    new_array = []
    arr.each_with_index do |ele, i|
        new_array << ele if i.even?
    end
    return new_array
end

# O(N), 0.5N 遍历， 0.5N 插入
def every_other2(arr)
    new_array = []
    index = 0
    while index < arr.length
        new_array << arr[index]
        index += 2
    end
    return new_array
end

puts every_other([1, 2, 3, 4, 5])
puts every_other2([1, 2, 3, 4, 5])
