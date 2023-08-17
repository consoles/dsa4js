class Node:
    def __init__(self, value):
        self.value = value
        self.prev = None
        self.next = None

class DoublyLinkedList:
    def __init__(self):
        self.head = None
        self.tail = None

    def insert_at_end(self, value):
        new_node = Node(value)
        if not self.head:
            self.head = new_node
            self.tail = new_node
            return
        
        new_node.prev = self.tail
        self.tail.next = new_node
        self.tail = new_node

    def remove_from_front(self):
        if not self.head:
            return
        remove_node = self.head
        self.head = self.head.next    
        return remove_node
        
class Queue:
    def __init__(self):
        self.queue = DoublyLinkedList()

    def enqueue(self, value):
        self.queue.insert_at_end(value)

    def dequeue(self):
        remove_node = self.queue.remove_from_front()
        return remove_node.value
    
    def peek(self):
        return self.queue.head.value
