package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func readInput() ([][]string, [][]int) {
	scanner := bufio.NewScanner(os.Stdin)
	cratesFinished := false
	crates := make([][]string, 0)
	instructions := make([][]int, 0)
	for scanner.Scan() {
		ln := scanner.Text()
		if ln == "" || strings.HasPrefix(ln, " 1") {
			cratesFinished = true
			continue
		}
		if cratesFinished {
			newLn := ""
			for _, c := range ln {
				if unicode.IsDigit(c) || unicode.IsSpace(c) {
					newLn = newLn + string(c)
				}
			}
			numsS := strings.Fields(newLn)
			nums := make([]int, 0, 3)
			for _, n := range numsS {
				num, err := strconv.Atoi(n)
				if err != nil {
					log.Fatal(err)
				}
				nums = append(nums, num)
			}
			instructions = append(instructions, nums)
		} else {
			for i := 0; i < len(ln); i += 4 {
				cr := ln[i : i+3]
				if len(crates) <= i/4 {
					crates = append(crates, nil)
				}
				if cr != "   " {
					crates[i/4] = append(crates[i/4], cr[1:2])
				}

			}

		}
	}
	for i := 0; i < len(crates); i++ {
		for j := 0; j < len(crates[i])/2; j++ {
			crates[i][j], crates[i][len(crates[i])-j-1] = crates[i][len(crates[i])-j-1], crates[i][j]
		}
	}
	return crates, instructions
}

func moveCrates(crates [][]string, n int, from int, to int) {
	e := crates[from][len(crates[from])-n:]
	crates[from] = crates[from][:len(crates[from])-n]
	crates[to] = append(crates[to], e...)
}

func execInstruction(crates [][]string, instr []int) {
	for i := 0; i < instr[0]; i++ {
		moveCrates(crates, 1, instr[1]-1, instr[2]-1)
	}
}

func execInstruction2(crates [][]string, instr []int) {

}

func main() {
	crates, instructions := readInput()
	cratesClone := make([][]string, len(crates))
	for i, st := range crates {
		cratesClone[i] = make([]string, len(st))
		for j, c := range st {
			cratesClone[i][j] = c
		}
	}

	for _, instr := range instructions {
		// part 1
		execInstruction(crates, instr)
		// part 2
		moveCrates(cratesClone, instr[0], instr[1]-1, instr[2]-1)
	}
	for _, st := range crates {
		fmt.Print(st[len(st)-1])
	}
	fmt.Println()

	for _, st := range cratesClone {
		fmt.Print(st[len(st)-1])
	}
	fmt.Println()
}
