package main

import (
	"fmt"
	"strconv"
)

type Stack struct {
	vals []int
}

func NewStack() Stack {
	return Stack{}
}

func (this *Stack) Push(val int) {
	this.vals = append(this.vals, val)
}

func (this *Stack) Pop() int {
	size := len(this.vals)
	v := this.vals[size-1]
	this.vals = this.vals[:size-1]
	return v
}

func (this *Stack) Empty() bool {
	return len(this.vals) == 0
}

func evalRPN(tokens []string) int {
	valStack := NewStack()
	ops := map[string]struct{}{}
	ops["+"] = struct{}{}
	ops["-"] = struct{}{}
	ops["*"] = struct{}{}
	ops["/"] = struct{}{}
	for _, token := range tokens {
		_, isOp := ops[token]
		if isOp {
			num2 := valStack.Pop()
			num1 := valStack.Pop()
			r := 0
			switch token {
			case "+":
				r = num1 + num2
			case "-":
				r = num1 - num2
			case "*":
				r = num1 * num2
			case "/":
				r = num1 / num2
			}
			valStack.Push(r)
		} else {
			v, err := strconv.Atoi(token)
			if err != nil {
				panic(err)
			}
			valStack.Push(v)
		}
	}
	return valStack.Pop()
}

func main() {
	// tokens := []string{"2", "1", "+", "3", "*"}
	// tokens := []string{"4", "13", "5", "/", "+"}
	tokens := []string{"10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"}
	r := evalRPN(tokens)
	fmt.Println(r)
}
