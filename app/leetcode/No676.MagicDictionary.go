package main

type MagicDictionary struct {
	words []string
}

func Constructor() MagicDictionary {
	return MagicDictionary{
		words: make([]string, 0),
	}
}

func (this *MagicDictionary) BuildDict(dictionary []string) {
	for _, word := range dictionary {
		this.words = append(this.words, word)
	}
}

func (this *MagicDictionary) Search(searchWord string) bool {
	length := len(searchWord)
	for _, word := range this.words {
		if len(word) != length {
			continue
		}
		diff := 0
		for i := 0; i < length; i++ {
			if searchWord[i] != word[i] {
				diff++
				if diff > 1 {
					break
				}
			}
		}
		if diff == 1 {
			return true
		}
	}
	return false
}

/**
 * Your MagicDictionary object will be instantiated and called as such:
 * obj := Constructor();
 * obj.BuildDict(dictionary);
 * param_2 := obj.Search(searchWord);
 */

type Trie struct {
	children [26]*Trie
	isEnd    bool
}

func NewTrie() *Trie {
	return &Trie{}
}

func (t *Trie) Insert(word string) {
	node := t
	for _, c := range word {
		i := c - 'a'
		if node.children[i] == nil {
			node.children[i] = NewTrie()
		}
		node = node.children[i]
	}
	node.isEnd = true
}

func (t *Trie) Search(word string) bool {
	var dfs func(int, *Trie, int) bool
	dfs = func(i int, node *Trie, diff int) bool {
		if i >= len(word) {
			return diff == 1 && node.isEnd
		}
		j := int(word[i] - 'a')
		if node.children[j] != nil && dfs(i+1, node.children[j], diff) {
			return true
		}
		if diff == 0 {
			for k := 0; k < 26; k++ {
				if k != j && node.children[k] != nil && dfs(i+1, node.children[k], 1) {
					return true
				}
			}
		}
		return false
	}
	return dfs(0, t, 0)
}
