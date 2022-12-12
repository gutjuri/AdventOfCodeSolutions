package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

func readInput() ([][]rune, Item, Item) {
	scanner := bufio.NewScanner(os.Stdin)
	inp := make([][]rune, 0)
	var startPos, endPos Item
	i := 0
	for scanner.Scan() {
		line := scanner.Text()
		inp = append(inp, make([]rune, 0, len(line)))
		for j, c := range line {
			inp[len(inp)-1] = append(inp[len(inp)-1], c)
			if c == 'S' {
				startPos = Item{x: j, y: i, steps: 0}
				inp[i][j] = 'a'
			}
			if c == 'E' {
				endPos = Item{x: j, y: i, steps: 0}
				inp[i][j] = 'z'
			}
		}
		i++
	}
	return inp, startPos, endPos
}

type Item struct {
	x, y, steps int
}

type PriorityQueue []Item

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].steps < pq[j].steps
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
}

func (pq *PriorityQueue) Push(x any) {
	item := x.(Item)
	*pq = append(*pq, item)
}

func (pq *PriorityQueue) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = Item{}
	*pq = old[0 : n-1]
	return item
}

func printQueue(pq *PriorityQueue) {
	fmt.Print("[")
	for _, x := range *pq {
		fmt.Print(x, " ")
	}
	fmt.Println("]")
}

func dijkstra(field [][]rune, pq *PriorityQueue, vis *[][]bool, endX, endY int) int {
	citemx := heap.Pop(pq)
	citem := citemx.(Item)

	if citem.x == endX && citem.y == endY {
		return citem.steps
	}

	(*vis)[citem.y][citem.x] = true
	currentHeight := field[citem.y][citem.x]
	nextItems := make([]Item, 0)
	if citem.x > 0 && field[citem.y][citem.x-1]-currentHeight <= 1 && !(*vis)[citem.y][citem.x-1] {
		nextItems = append(nextItems, Item{x: citem.x - 1, y: citem.y, steps: citem.steps + 1})
	}
	if citem.x < len(field[0])-1 && field[citem.y][citem.x+1]-currentHeight <= 1 && !(*vis)[citem.y][citem.x+1] {
		nextItems = append(nextItems, Item{x: citem.x + 1, y: citem.y, steps: citem.steps + 1})
	}

	if citem.y > 0 && field[citem.y-1][citem.x]-currentHeight <= 1 && !(*vis)[citem.y-1][citem.x] {
		nextItems = append(nextItems, Item{x: citem.x, y: citem.y - 1, steps: citem.steps + 1})
	}
	if citem.y < len(field)-1 && field[citem.y+1][citem.x]-currentHeight <= 1 && !(*vis)[citem.y+1][citem.x] {
		nextItems = append(nextItems, Item{x: citem.x, y: citem.y + 1, steps: citem.steps + 1})
	}
	for _, ni := range nextItems {
		heap.Push(pq, ni)
		(*vis)[ni.y][ni.x] = true
	}
	return dijkstra(field, pq, vis, endX, endY)
}

func dijkstraRev(field [][]rune, pq *PriorityQueue, vis *[][]bool) int {
	citemx := heap.Pop(pq)
	citem := citemx.(Item)

	if field[citem.y][citem.x] == 'a' {
		return citem.steps
	}

	(*vis)[citem.y][citem.x] = true
	currentHeight := field[citem.y][citem.x]
	nextItems := make([]Item, 0)
	if citem.x > 0 && currentHeight-field[citem.y][citem.x-1] <= 1 && !(*vis)[citem.y][citem.x-1] {
		nextItems = append(nextItems, Item{x: citem.x - 1, y: citem.y, steps: citem.steps + 1})
	}
	if citem.x < len(field[0])-1 && currentHeight-field[citem.y][citem.x+1] <= 1 && !(*vis)[citem.y][citem.x+1] {
		nextItems = append(nextItems, Item{x: citem.x + 1, y: citem.y, steps: citem.steps + 1})
	}

	if citem.y > 0 && currentHeight-field[citem.y-1][citem.x] <= 1 && !(*vis)[citem.y-1][citem.x] {
		nextItems = append(nextItems, Item{x: citem.x, y: citem.y - 1, steps: citem.steps + 1})
	}
	if citem.y < len(field)-1 && currentHeight-field[citem.y+1][citem.x] <= 1 && !(*vis)[citem.y+1][citem.x] {
		nextItems = append(nextItems, Item{x: citem.x, y: citem.y + 1, steps: citem.steps + 1})
	}
	for _, ni := range nextItems {
		heap.Push(pq, ni)
		(*vis)[ni.y][ni.x] = true
	}
	return dijkstraRev(field, pq, vis)
}

func main() {
	inp, startPos, endPos := readInput()
	vis := make([][]bool, 0, len(inp))
	for _, r := range inp {
		vis = append(vis, make([]bool, len(r)))
	}
	pq := make(PriorityQueue, 1)
	pq[0] = startPos
	heap.Init(&pq)
	fmt.Println(dijkstra(inp, &pq, &vis, endPos.x, endPos.y))

	pq2 := make(PriorityQueue, 1)
	vis2 := make([][]bool, 0, len(inp))
	for _, r := range inp {
		vis2 = append(vis2, make([]bool, len(r)))
	}
	pq2[0] = endPos
	heap.Init(&pq2)
	fmt.Println(dijkstraRev(inp, &pq2, &vis2))
}
