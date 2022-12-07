package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

const (
	totalSpace = 70000000
	reqSpace   = 30000000
)

type dir struct {
	subdirs map[string]*dir
	files   map[string]int
	size    int
	parent  *dir
}

func NewDir(parent *dir) *dir {
	d := dir{subdirs: make(map[string]*dir), files: make(map[string]int), size: 0, parent: parent}
	return &d
}

func mkdir_p(cdir *dir, name string) {
	if _, ok := cdir.subdirs[name]; !ok {
		cdir.subdirs[name] = NewDir(cdir)
	}
}

func parseInput() *dir {
	root := NewDir(nil)
	cdir := root
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "$") {
			cmd := strings.Split(line[2:], " ")
			if len(cmd) != 1 {
				target := cmd[1]
				if target == "/" {
					cdir = root
				} else if target == ".." {
					cdir = cdir.parent
				} else {
					mkdir_p(cdir, target)
					cdir = cdir.subdirs[target]
				}
			}
		} else {
			outLine := strings.Split(line, " ")
			if outLine[0] == "dir" {
				mkdir_p(cdir, outLine[1])
			} else {
				fsize, err := strconv.Atoi(outLine[0])
				if err != nil {
					log.Fatal(err)
				}
				cdir.files[outLine[1]] = fsize
				ccdir := cdir
				for ; ccdir != nil; ccdir = ccdir.parent {
					ccdir.size += fsize
				}
			}
		}
	}
	return root
}

func getSumDirSizeAtMost100000(root *dir) int {
	csum := 0
	if root.size <= 100000 {
		csum = root.size
	}
	for _, childDir := range root.subdirs {
		csum += getSumDirSizeAtMost100000(childDir)
	}

	return csum
}

func getSmallestWithAtLeast(root *dir, least int) int {
	smallest := math.MaxInt64
	for _, childdir := range root.subdirs {
		csmallest := getSmallestWithAtLeast(childdir, least)
		if csmallest < smallest {
			smallest = csmallest
		}
	}
	if root.size >= least && root.size < smallest {
		smallest = root.size
	}
	return smallest
}

func main() {
	root := parseInput()
	fmt.Println(getSumDirSizeAtMost100000(root))
	freeAtLeast := reqSpace - (totalSpace - root.size)
	fmt.Println(getSmallestWithAtLeast(root, freeAtLeast))
}
