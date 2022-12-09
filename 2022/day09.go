package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type instr struct {
	dir    string
	amount int
}

func readInput() []instr {
	inp := make([]instr, 0, 2000)
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		ln := strings.Split(scanner.Text(), " ")
		amount, err := strconv.Atoi(ln[1])
		if err != nil {
			log.Fatal(err)
		}
		inp = append(inp, instr{dir: ln[0], amount: amount})
	}
	return inp
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func sign(a int) int {
	if a < 0 {
		return -1
	}
	return 1
}

func follow(hx, hy, tx, ty int) (int, int) {
	if hx != tx && hy != ty {
		if abs((hx-tx)*(hy-ty)) > 1 {
			return tx + sign(hx-tx), ty + sign(hy-ty)
		} else {
			return tx, ty
		}
	} else if hx != tx {
		if hx == tx-2 {
			return tx - 1, ty
		} else if hx == tx+2 {
			return tx + 1, ty
		}
	} else if hy != ty {
		if hy == ty-2 {
			return tx, ty - 1
		} else if hy == ty+2 {
			return tx, ty + 1
		}
	}
	return tx, ty

}

type pt struct {
	x int
	y int
}

func doInstrs(instrs []instr) int {
	hx, hy, tx, ty := 0, 0, 0, 0
	vis := make(map[pt]bool)
	vis[pt{tx, ty}] = true

	for _, instr := range instrs {
		for i := 0; i < instr.amount; i++ {
			switch instr.dir {
			case "U":
				hy++
			case "D":
				hy--
			case "R":
				hx++
			case "L":
				hx--
			}
			tx, ty = follow(hx, hy, tx, ty)
			vis[pt{tx, ty}] = true
		}

	}
	return len(vis)
}

func doInstrs2(instrs []instr) int {
	positions := make([]pt, 10)
	vis := make(map[pt]bool)
	vis[positions[9]] = true

	for _, instr := range instrs {
		for i := 0; i < instr.amount; i++ {
			switch instr.dir {
			case "U":
				positions[0].y++
			case "D":
				positions[0].y--
			case "R":
				positions[0].x++
			case "L":
				positions[0].x--
			}
			for j := 1; j < 10; j++ {
				positions[j].x, positions[j].y = follow(positions[j-1].x, positions[j-1].y, positions[j].x, positions[j].y)
			}
			vis[positions[9]] = true
		}

	}
	return len(vis)
}

func main() {
	instrs := readInput()
	fmt.Println(doInstrs(instrs))
	fmt.Println(doInstrs2(instrs))

}
