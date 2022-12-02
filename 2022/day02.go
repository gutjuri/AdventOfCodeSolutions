package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type round struct {
	opponentsMove int
	ownMove       int
}

func (r round) getPoints() int {
	if r.opponentsMove == r.ownMove {
		return r.ownMove + 3
	}
	if (r.ownMove == 3 && r.opponentsMove == 1) ||
		(r.ownMove == 1 && r.opponentsMove == 2) ||
		(r.ownMove == 2 && r.opponentsMove == 3) {
		return r.ownMove
	}
	return r.ownMove + 6
}

func getValue(x string) int {
	if x == "A" || x == "X" {
		return 1
	}
	if x == "B" || x == "Y" {
		return 2
	}
	if x == "C" || x == "Z" {
		return 3
	}
	log.Fatal("error")
	return -1
}

func readInput() []round {
	buf := make([]round, 0)
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		opponentsMove := getValue(scanner.Text())
		scanner.Scan()
		ownMove := getValue(scanner.Text())
		buf = append(buf, round{opponentsMove: opponentsMove, ownMove: ownMove})
	}
	return buf
}

func calcPoints(rounds []round) int {
	acc := 0
	for _, r := range rounds {
		acc += r.getPoints()
	}
	return acc
}

func playForTarget(tar []round) int {
	actualRounds := make([]round, 0)
	for _, cround := range tar {
		enemy := cround.opponentsMove
		target := cround.ownMove
		move := 0

		if target == 2 {
			move = enemy
		} else if target == 1 {
			if enemy == 1 {
				move = 3
			} else {
				move = enemy - 1
			}
		} else {
			if enemy == 3 {
				move = 1
			} else {
				move = enemy + 1
			}
		}

		actualRounds = append(actualRounds, round{opponentsMove: enemy, ownMove: move})
	}
	return calcPoints(actualRounds)
}

func main() {
	rounds := readInput()
	fmt.Println(calcPoints(rounds))

	fmt.Println(playForTarget(rounds))
}
