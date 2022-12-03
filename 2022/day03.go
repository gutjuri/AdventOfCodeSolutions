package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/scylladb/go-set"
	"github.com/scylladb/go-set/iset"
)

func toPriority(c rune) int {
	v := int(c - 'a')
	if v < 0 {
		return int(c-'A') + 27
	}
	return v + 1

}

func strToSet(r string) *iset.Set {
	s := set.NewIntSet()
	for _, c := range r {
		s.Add(toPriority(c))
	}
	return s
}

func getBadgeValue(r1, r2, r3 string) int {
	s := iset.Intersection(strToSet(r1), strToSet(r2), strToSet(r3))
	return s.Pop()
}

func commonItemValues(content string) int {
	commons := 0
	counts := make(map[rune]bool)
	for i, r := range content {
		p := toPriority(r)
		if i < len(content)/2 {
			counts[r] = true
		} else {
			if counts[r] {
				commons += p
				counts[r] = false
			}
		}
	}
	return commons
}

func main() {
	input := make([]string, 0)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	sum := 0
	for _, s := range input {
		sum += commonItemValues(s)
	}
	fmt.Println(sum)

	sum = 0
	for i := 0; i < len(input); i += 3 {
		sum += getBadgeValue(input[i], input[i+1], input[i+2])
	}
	fmt.Println(sum)

}
