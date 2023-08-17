# O(N^2)
def has_duplicate_elements(list):
    for i in range(len(list)):
        for j in range(len(list)):
            if list[i] == list[j] and i != j:
                return True
    return False

# O(N)
# 线性时间解决，将已经出现过的元素保存起来
def has_duplicate_elements2(list):
    exist_elements = set()
    for i in range(len(list)):
        if list[i] in exist_elements:
            return True
        exist_elements.add(list[i])
    return False

list = [65, 55, 45, 35, 25, 15, 10]
# False
print(has_duplicate_elements(list)) 
print(has_duplicate_elements2(list)) 

list = [15,16,17,8,16,17]
# True
print(has_duplicate_elements(list))
print(has_duplicate_elements2(list))
