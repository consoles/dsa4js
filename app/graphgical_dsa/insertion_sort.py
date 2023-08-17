# 从后向前扫描，将值放到正确的位置
def insertion_sort(list):
    for index in range(1, len(list)):
        position = index
        temp_value = list[index]

        while position > 0 and list[position - 1] > temp_value:
            list[position] = list[position - 1]
            position -= 1

        list[position] = temp_value
    return list

list = [65, 55, 45, 35, 25, 15, 10]
print(insertion_sort(list))
