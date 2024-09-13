package main

import (
	"container/heap"
	"fmt"
	"sort"
)

func topKFrequent(words []string, k int) []string {
	counter := make(map[string]int, 0)
	for _, word := range words {
		counter[word]++
	}
	uniqWords := make([]string, len(counter))
	for word := range counter {
		uniqWords = append(uniqWords, word)
	}
	sort.Slice(uniqWords, func(i, j int) bool {
		cA := counter[uniqWords[i]]
		cB := counter[uniqWords[j]]
		if cA == cA {
			return uniqWords[i] < uniqWords[j]
		}
		return cB < cA
	})
	return uniqWords[:k]
}

type pair struct {
	word  string
	count int
}

type hp []pair

func (h hp) Len() int {
	return len(h)
}

func (h hp) Less(i, j int) bool {
	a, b := h[i], h[j]
	return a.count < b.count || a.count == b.count && a.word > b.word
}

func (h hp) Swap(i, j int) {
	h[i], h[j] = h[j], h[i]
}

func (h *hp) Push(v interface{}) {
	*h = append(*h, v.(pair))
}

func (h *hp) Pop() interface{} {
	a := *h
	v := a[len(a)-1]
	*h = a[:len(a)-1]
	return v
}

func topKFrequent2(words []string, k int) []string {
	wordCounter := map[string]int{}
	for _, word := range words {
		wordCounter[word]++
	}
	h := &hp{}
	for word, count := range wordCounter {
		heap.Push(h, pair{word, count})
		if h.Len() > k {
			heap.Pop(h)
		}
	}
	ans := make([]string, k)
	for i := k - 1; i >= 0; i-- {
		ans[i] = heap.Pop(h).(pair).word
	}
	return ans
}

func main() {
	words := []string{"i", "love", "leetcode", "i", "love", "coding"}
	r := topKFrequent2(words, 2)
	fmt.Println(r)
}
