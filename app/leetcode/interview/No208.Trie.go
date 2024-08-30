package main

import "fmt"

// a -> p -> p -> l -> e(isEnd)
// a -> a -> p(isEnd)
type Trie struct {
	// child[0] -> 'a'
	// child[1] -> 'b'
	// child[25] -> 'z'
	children [26]*Trie // 26 叉树
	// 表示该节点是否是字符串的结尾
	isEnd bool
}

func Constructor() Trie {
	return Trie{}
}

func (this *Trie) Insert(word string) {
	node := this
	for _, ch := range word {
		ch -= 'a' // 确保了只有小写英文字母组成
		if node.children[ch] == nil {
			node.children[ch] = &Trie{}
		}
		node = node.children[ch]
	}
	node.isEnd = true
}

func (this *Trie) SearchPrefix(prefix string) *Trie {
	node := this
	for _, ch := range prefix {
		ch -= 'a'
		if node.children[ch] == nil {
			return nil
		}
		node = node.children[ch]
	}
	return node
}

func (this *Trie) Search(word string) bool {
	node := this.SearchPrefix(word)
	return node != nil && node.isEnd
}

func (this *Trie) StartsWith(prefix string) bool {
	return this.SearchPrefix(prefix) != nil
}

/**
 * Your Trie object will be instantiated and called as such:
 * obj := Constructor();
 * obj.Insert(word);
 * param_2 := obj.Search(word);
 * param_3 := obj.StartsWith(prefix);
 */

func main() {
	t := Constructor()
	t.Insert("apple")
	a := t.Search("apple")
	fmt.Println(a)
	a = t.Search("app")
	fmt.Println(a)
	a = t.StartsWith("app")
	fmt.Println(a)
	t.Insert("app")
	a = t.Search("app")
	fmt.Println(a)
}
