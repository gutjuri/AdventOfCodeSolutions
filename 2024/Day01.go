package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func readInput() ([]int, []int) {
	sc := bufio.NewScanner(os.Stdin)
	l1, l2 := make([]int, 0), make([]int, 0)

	for sc.Scan() {
		line := sc.Text()
		textRaw := strings.Split(line, "   ")
		n1, _ := strconv.Atoi(textRaw[0])
		n2, _ := strconv.Atoi(textRaw[1])
		l1 = append(l1, n1)
		l2 = append(l2, n2)
	}

	return l1, l2
}

func abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func problem1(l1, l2 []int) int {

	sum := 0

	for i := range l1 {
		//fmt.Println(l1[i], l2[i])
		sum += abs(l1[i] - l2[i])
	}
	return sum
}

func problem2(l1, l2 []int) int {
	idx := make(map[int]int)

	sum := 0

	for _, v := range l2 {
		idx[v] += 1
	}
	for _, v := range l1 {
		sum += v * idx[v]
	}
	return sum
}

func main() {
	l1, l2 := readInput()
	sort.Slice(l1, func(i, j int) bool { return l1[i] < l1[j] })
	sort.Slice(l2, func(i, j int) bool { return l2[i] < l2[j] })
	// P1
	fmt.Println(problem1(l1, l2))
	// P2
	fmt.Println(problem2(l1, l2))

}
