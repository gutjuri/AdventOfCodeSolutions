package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	input, err := reader.ReadString('\n')
	if err != nil && err != io.EOF {
		log.Fatal(err)
	}
	for i := 3; i < len(input); i++ {
		if input[i-3] != input[i-2] && input[i-2] != input[i-1] && input[i-1] != input[i] &&
			input[i-3] != input[i-1] && input[i-2] != input[i] && input[i-3] != input[i] {
			fmt.Println(i + 1)
			break

		}
	}

	m := make(map[byte]int, 14)
	for i := 0; i < len(input); i++ {
		v, found := m[input[i]]
		if found {
			m[input[i]] = v + 1
		} else {
			m[input[i]] = 1
		}
		if i >= 14 {
			v = m[input[i-14]]
			if v != 1 {
				m[input[i-14]] = v - 1
			} else {
				delete(m, input[i-14])
			}
		}
		if len(m) == 14 {
			fmt.Println(i + 1)
			break
		}
	}
}
