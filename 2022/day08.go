package main

import (
	"bufio"
	"fmt"
	"os"
)

func readInput() [][]int {
	input := make([][]int, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]int, len(line))
		for i, c := range line {
			treeHight := int(c - '0')
			row[i] = treeHight
		}
		input = append(input, row)
	}
	return input
}

func howManyVisible(trees [][]int) int {
	mask := make([][]bool, len(trees))
	for i, treeRow := range trees {
		mask[i] = make([]bool, len(treeRow))
	}

	for i, _ := range trees {
		mkVisibleRow(trees, i, mask)
	}

	for i, _ := range trees[0] {
		mkVisibleCol(trees, i, mask)
	}

	n := 0
	for _, row := range mask {
		for _, v := range row {
			if v {
				n++
			}
		}
	}
	return n

}

func mkVisibleRow(trees [][]int, row int, mask [][]bool) {
	cHighest := -1
	cHighestRev := -1
	for i := 0; i < len(trees[row]); i++ {
		if trees[row][i] > cHighest {
			cHighest = trees[row][i]
			mask[row][i] = true
		}
		if trees[row][len(trees[row])-1-i] > cHighestRev {
			cHighestRev = trees[row][len(trees[row])-1-i]
			mask[row][len(trees[row])-1-i] = true
		}
	}
}

func mkVisibleCol(trees [][]int, col int, mask [][]bool) {
	cHighest := -1
	cHighestRev := -1
	for i := 0; i < len(trees); i++ {
		if trees[i][col] > cHighest {
			cHighest = trees[i][col]
			mask[i][col] = true
		}
		if trees[len(trees)-1-i][col] > cHighestRev {
			cHighestRev = trees[len(trees)-1-i][col]
			mask[len(trees)-1-i][col] = true
		}
	}
}

func highstScenicScore(trees [][]int) int {
	h := 0
	for i := 0; i < len(trees); i++ {
		for j := 0; j < len(trees[i]); j++ {
			sc := getScenicScore(trees, i, j)
			if sc > h {
				h = sc
			}
		}
	}
	return h
}

func getScenicScore(trees [][]int, row int, col int) int {
	nRight := 0
	for i := col + 1; i < len(trees[row]); i++ {
		nRight++
		if trees[row][col] <= trees[row][i] {
			break
		}
	}

	nLeft := 0
	for i := col - 1; i >= 0; i-- {
		nLeft++
		if trees[row][col] <= trees[row][i] {
			break
		}
	}

	nDown := 0
	for i := row + 1; i < len(trees); i++ {
		nDown++
		if trees[row][col] <= trees[i][col] {
			break
		}
	}

	nUp := 0
	for i := row - 1; i >= 0; i-- {
		nUp++
		if trees[row][col] <= trees[i][col] {
			break
		}
	}
	return nRight * nLeft * nUp * nDown

}

func main() {
	trees := readInput()
	fmt.Println(howManyVisible(trees))
	fmt.Println(highstScenicScore(trees))
}
