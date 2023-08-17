class TreeNode:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

def search(value, node):
    # 基准场景：node 不存在或者 node 的值符合
    if node is None or node.value == value:
        return node
    
    if value < node.value:
        return search(value, node.left)
    else:
        return search(value, node.right)
    
def insert(value, node):
    if value < node.value:
        # 如果左子节点不存在，则新值作为左子节点
        if node.left is None:
            node.left = TreeNode(value)
        else:
            insert(value, node.left)

    elif value > node.value:
        # 如果右子节点不存在，则新值作为右子节点
        if node.right is None:
            node.right = TreeNode(value)
        else:
            insert(value, node.right)        

def delete(value, node):
    # 基准场景：已经到达树的底层
    if node is None:
        return node
    
    if value < node.value:
        node.left = delete(value, node.left)
        return node
    
    if value > node.value:
        node.right = delete(value, node.right)
        return node
    
    # 要删除的是当前节点
    # 如果当前结点没有左子结点, 则以右子结点（及其子树，如果存在的话）替换当前结点成为当前结点之父结点的新子结点
    if node.left is None:
        return node.right
    # 如果当前结点没有左子结点，也没有右子结点，那这里就是返回 None
    if node.right is None:
        return node.left
    # 如果当前节点有 2 个子节点，它会使当前结点的值变成其后继结点的值
    node.right = lift(node.right, node)
    return node

def lift(node, node_to_delete):
    # 如果当前节点有左子节点，则从左子节点中找出后继节点
    if node.left:
        node.left = lift(node.left, node_to_delete)
        return node
    # 当前节点没有左子节点，则当前节点就是后继节点，于是将它的值设置为被删除节点的新值
    node_to_delete.value = node.value
    # 用后继节点的右子节点替换后继节点的父节点的左子节点
    return node.right

def inorder(node):
    if node:
        inorder(node.left)
        print(node.value)
        inorder(node.right)
