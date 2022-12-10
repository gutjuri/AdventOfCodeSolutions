package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"

	"golang.org/x/exp/slices"
)

type instruction struct {
	kind     string
	duration int
	add      int
}

func readInput() []instruction {
	inp := make([]instruction, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		ln := scanner.Text()
		lns := strings.Split(ln, " ")
		if lns[0] == "noop" {
			inp = append(inp, instruction{kind: "noop", duration: 1, add: 0})
		} else {
			v, err := strconv.Atoi(lns[1])
			if err != nil {
				log.Fatal(err)
			}
			inp = append(inp, instruction{kind: "addx", duration: 2, add: v})
		}

	}
	return inp
}

func addIfCheckpoint(cycle int, reg int, checkpoints []int) int {
	if slices.Contains(checkpoints, cycle) {
		return cycle * reg
	} else {
		return 0
	}
}

func appendRightPixel(display []bool, reg int) []bool {
	col := len(display) % 40
	if col == reg || col == reg-1 || col == reg+1 {
		display = append(display, true)
	} else {
		display = append(display, false)
	}
	return display
}

func calcSigStrenghts(instrs []instruction) (int, []bool) {
	checkpoints := []int{20, 60, 100, 140, 180, 220}
	display := make([]bool, 0, 40*6)
	x := 1
	cycle := 1
	sstren := 0
	for _, instr := range instrs {
		if instr.kind == "noop" {
			sstren += addIfCheckpoint(cycle, x, checkpoints)
			display = appendRightPixel(display, x)
			cycle++
		} else {
			for j := 0; j < instr.duration; j++ {
				sstren += addIfCheckpoint(cycle, x, checkpoints)
				display = appendRightPixel(display, x)
				cycle++

			}
			x += instr.add
		}

	}
	return sstren, display
}

func printDisplay(display []bool) {
	for i := 0; i < 6; i++ {
		for j := 0; j < 40; j++ {
			if display[i*40+j] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func main() {
	instrs := readInput()
	res, display := calcSigStrenghts(instrs)
	fmt.Println(res)
	printDisplay(display)
}
