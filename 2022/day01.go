package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func readInput() [][]int {
	calories := make([][]int, 0)

	newElve := true

	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		if newElve {
			newElve = false
			calories = append(calories, make([]int, 0))
		}
		ln := scanner.Text()
		if ln == "" {
			newElve = true
		} else {
			value, err := strconv.Atoi(ln)
			if err != nil {
				log.Fatal(err)
			}
			calories[len(calories)-1] = append(calories[len(calories)-1], value)
		}
	}
	return calories
}

func foldl1[T any, A any](arr []T, f func(A, T) A, st A) A {
	acc := st
	for _, e := range arr {
		acc = f(acc, e)
	}
	return acc
}

func sum(a, b int) int { return a + b }

type bySum [][]int

func (arr bySum) Swap(i, j int) {
	arr[i], arr[j] = arr[j], arr[i]
}

func (arr bySum) Less(i, j int) bool {
	return foldl1(arr[i], sum, 0) < foldl1(arr[j], sum, 0)
}

func (arr bySum) Len() int {
	return len(arr)
}

func main() {
	calories := readInput()
	sort.Sort(bySum(calories))
	largestArr := calories[len(calories)-1]
	fmt.Println(foldl1(largestArr, sum, 0))
	total := foldl1(calories[len(calories)-3:], func(acc int, x []int) int { return acc + foldl1(x, sum, 0) }, 0)
	fmt.Println(total)
}
