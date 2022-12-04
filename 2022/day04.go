package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func readInput() [][]int {
	scanner := bufio.NewScanner(os.Stdin)
	ret := make([][]int, 0)
	for scanner.Scan() {
		cpair := make([]int, 0)
		line := scanner.Text()
		splitStr := strings.Split(line, ",")
		for _, s := range splitStr {
			startend := strings.Split(s, "-")
			st, err := strconv.Atoi(startend[0])
			if err != nil {
				log.Fatal(err)
			}
			end, err := strconv.Atoi(startend[1])
			if err != nil {
				log.Fatal(err)
			}
			cpair = append(cpair, st, end)
		}
		ret = append(ret, cpair)
	}
	return ret
}

func main() {
	pairs := readInput()
	nIncluded := 0
	for _, pair := range pairs {
		if (pair[0] <= pair[2] && pair[1] >= pair[3]) ||
			(pair[0] >= pair[2] && pair[1] <= pair[3]) {
			nIncluded++
		}
	}
	fmt.Println(nIncluded)
	nOverlapping := 0
	for _, pair := range pairs {
		if pair[0] > pair[2] {
			pair[0], pair[1], pair[2], pair[3] = pair[2], pair[3], pair[0], pair[1]
		}
		if pair[0] <= pair[2] && pair[2] <= pair[1] {
			nOverlapping++
		}
	}
	fmt.Println(nOverlapping)
}
