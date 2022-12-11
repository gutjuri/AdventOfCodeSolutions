package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

type monkey struct {
	items          []int
	op             func(int) int
	testDivBy      int
	ifTrue         int
	ifFalse        int
	inspectedTimes int
}

func newMonkey() monkey {
	return monkey{items: make([]int, 0)}
}

func mkOp(op1, op, op2 string) func(int) int {
	return func(i int) int {
		operand1 := i
		var err error
		if op1 != "old" {
			operand1, err = strconv.Atoi(op1)
			if err != nil {
				log.Fatalln(err)
			}
		}
		operand2 := i
		if op2 != "old" {
			operand2, err = strconv.Atoi(op2)
			if err != nil {
				log.Fatalln(err)
			}
		}
		if op == "+" {
			//fmt.Println("calculating ", operand1, "+", operand2)
			return operand1 + operand2
		} else {
			//fmt.Println("calculating ", operand1, "*", operand2)
			return operand1 * operand2
		}
	}
}

func parseInput() []monkey {
	monkeys := make([]monkey, 0)
	scanner := bufio.NewScanner(os.Stdin)
	newMonkeyReq := true
	numberRegex := regexp.MustCompile("\\d+(, \\d*)*")
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			newMonkeyReq = true
			continue
		}
		if newMonkeyReq {
			monkeys = append(monkeys, newMonkey())
			newMonkeyReq = false
		}
		currentMonkey := &monkeys[len(monkeys)-1]
		if strings.HasPrefix(line, "Monkey") {
			continue
		} else if strings.HasPrefix(line, "  Starting items:") {
			nums := strings.Split(numberRegex.FindString(line), ", ")
			for _, n := range nums {
				num, err := strconv.Atoi(n)
				if err != nil {
					log.Fatalln(err)
				}
				currentMonkey.items = append(currentMonkey.items, num)

			}
		} else if strings.HasPrefix(line, "  Operation:") {
			line = strings.Split(line, ": new = ")[1]
			tokens := strings.Split(line, " ")

			currentMonkey.op = mkOp(tokens[0], tokens[1], tokens[2])
		} else if strings.HasPrefix(line, "  Test:") {
			num, err := strconv.Atoi(numberRegex.FindString(line))
			if err != nil {
				log.Fatalln(err)
			}
			currentMonkey.testDivBy = num
		} else if strings.HasPrefix(line, "    If true:") {
			num, err := strconv.Atoi(numberRegex.FindString(line))
			if err != nil {
				log.Fatalln(err)
			}
			currentMonkey.ifTrue = num
		} else if strings.HasPrefix(line, "    If false:") {
			num, err := strconv.Atoi(numberRegex.FindString(line))
			if err != nil {
				log.Fatalln(err)
			}
			currentMonkey.ifFalse = num
		} else {
			log.Fatalln("could not parse line", line)
		}

	}
	return monkeys
}

func evalRound(monkeys []monkey, divBy3 int) {
	for i := 0; i < len(monkeys); i++ {
		m := monkeys[i]
		for _, item := range m.items {
			monkeys[i].inspectedTimes++
			var newLvl int
			if divBy3 == 3 {
				newLvl = m.op(item) / 3
			} else {
				newLvl = m.op(item) % divBy3
			}

			if newLvl%m.testDivBy == 0 {
				monkeys[m.ifTrue].items = append(monkeys[m.ifTrue].items, newLvl)
			} else {
				monkeys[m.ifFalse].items = append(monkeys[m.ifFalse].items, newLvl)
			}
		}
		monkeys[i].items = make([]int, 0)

	}
}

type byTimesInsp []monkey

func (arr byTimesInsp) Swap(i, j int) {
	arr[i], arr[j] = arr[j], arr[i]
}

func (arr byTimesInsp) Less(i, j int) bool {
	return arr[i].inspectedTimes > arr[j].inspectedTimes
}

func (arr byTimesInsp) Len() int {
	return len(arr)
}

func p1(monkeys []monkey) int {
	for i := 0; i < 20; i++ {
		evalRound(monkeys, 3)
	}
	sort.Sort(byTimesInsp(monkeys))
	return monkeys[0].inspectedTimes * monkeys[1].inspectedTimes
}

func p2(monkeys []monkey) int {
	divs := 1
	for _, m := range monkeys {
		divs *= m.testDivBy
	}
	for i := 0; i < 10000; i++ {
		evalRound(monkeys, divs)
	}
	sort.Sort(byTimesInsp(monkeys))
	return monkeys[0].inspectedTimes * monkeys[1].inspectedTimes
}

func CopyMonkey(m monkey) monkey {
	newM := newMonkey()
	newM.op = m.op
	newM.testDivBy = m.testDivBy
	newM.ifFalse = m.ifFalse
	newM.ifTrue = m.ifTrue
	newM.inspectedTimes = m.inspectedTimes
	for _, i := range m.items {
		newM.items = append(newM.items, i)
	}
	return newM
}

func main() {
	monkeys := parseInput()
	monkeys2 := make([]monkey, 0, len(monkeys))
	for _, m := range monkeys {
		monkeys2 = append(monkeys2, CopyMonkey(m))
	}
	fmt.Println(p1(monkeys))
	fmt.Println(p2(monkeys2))
}
