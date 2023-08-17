class Node:
    def __init__(self, value):
        self.value = value
        self.next = None

class LinkedList:
    def __init__(self, head):
        self.head = head

    def read(self, index):
        current_node = self.head
        current_index = 0

        while current_index < index:
            current_node = current_node.next
            current_index += 1
            if not current_node:
                return None
        if not current_node:
            return None
        return current_node.value

    def index_of(self, value):
        current_node = self.head
        current_index = 0

        while current_node:
            if current_node.value == value:
                return current_index
            current_node = current_node.next
            current_index += 1

        return None

    def insert_at_index(self, index, value):
        # 创建新节点
        new_node = Node(value)

        if index == 0:
            new_node.next = self.head
            self.head = new_node
            return
        
        current_node = self.head
        current_index = 0

        # 新插入节点的前一个节点
        prev_index = index - 1
        while current_index < prev_index:
            current_node = current_node.next
            current_index += 1
        
        new_node.next = current_node.next
        current_node.next = new_node

    def delete_at_index(self, index):
        # 删除头结点
        if index == 0:
            deleted_node = self.head
            self.head = self.head.next
            return deleted_node
        
        current_node = self.head
        current_index = 0
        # 先找出被删除节点的前一个节点
        while current_index < index - 1:
            current_node = current_node.next
            current_index += 1
        
        deleted_node = current_node.next
        current_node.next = deleted_node.next
        return deleted_node

