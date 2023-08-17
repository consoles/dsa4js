def buddle_sort(list):
    unsorted_until_index = len(list) - 1
    sorted = False

    while not sorted:
        sorted = True
        for i in range(unsorted_until_index):
            if list[i] > list[i + 1]:
                list[i], list[i + 1] = list[i + 1], list[i]
                sorted = False
        unsorted_until_index -= 1

list = [65, 55, 45, 35, 25, 15, 10]
buddle_sort(list)
# [10, 15, 25, 35, 45, 55, 65]
print(list)
