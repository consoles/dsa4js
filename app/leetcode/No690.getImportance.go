package main

import "fmt"

type Employee struct {
	Id           int
	Importance   int
	Subordinates []int
}

/**
 * Definition for Employee.
 * type Employee struct {
 *     Id int
 *     Importance int
 *     Subordinates []int
 * }
 */

func getImportance(employees []*Employee, id int) int {
	total := 0
	for _, emp := range employees {
		if emp.Id == id {
			total += emp.Importance
			for _, subId := range emp.Subordinates {
				total += getImportance(employees, subId)
			}
		}
	}
	return total
}

func getImportance2(employees []*Employee, id int) int {
	total := 0
	//  使用 map 优化上面的在在数组中循环查找特定 id 的 emp
	empMap := make(map[int]*Employee, len(employees))
	for _, emp := range employees {
		empMap[emp.Id] = emp
	}
	var dfs func(int)
	dfs = func(id int) {
		e := empMap[id]
		total += e.Importance
		for _, subId := range e.Subordinates {
			dfs(subId)
		}
	}
	dfs(id)
	return total
}

func getImportance3(employees []*Employee, id int) int {
	// bfs
	empMap := make(map[int]*Employee, len(employees))
	for _, emp := range employees {
		empMap[emp.Id] = emp
	}
	total := 0
	q := []int{id}
	for len(q) > 0 {
		curId := q[0]
		e := empMap[curId]
		q = q[1:]
		total += e.Importance
		for _, subId := range e.Subordinates {
			q = append(q, subId)
		}
	}
	return total
}

func main() {
	emps := []*Employee{
		&Employee{
			Id:           1,
			Importance:   5,
			Subordinates: []int{2, 3},
		},
		&Employee{
			Id:           2,
			Importance:   3,
			Subordinates: []int{},
		},
		&Employee{
			Id:           3,
			Importance:   3,
			Subordinates: []int{},
		},
	}
	r := getImportance3(emps, 1)
	fmt.Println(r)
}
