def selection_sort(list):
    for i in range(len(list)):
        min_index = i
        for j in range(i + 1, len(list)):
            if list[j] < list[min_index]:
                min_index = j
        if i != min_index:        
            list[i], list[min_index] = list[min_index], list[i]
    return list

list = [65, 55, 45, 35, 25, 15, 10]
print(selection_sort(list))
