// DAY 15
package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

const (
	wall  = '#'
	box   = 'O'
	empty = '.'

	up    = '^'
	right = '>'
	down  = 'v'
	left  = '<'

	bot = "@"
)

type simstate struct {
	board   [][]rune
	size    vec2
	bot     vec2
	program []rune
	pc      int
}

type vec2 struct {
	x int
	y int
}

func parseInput() simstate {
	bytes, error := os.ReadFile("../input.txt")
	if error != nil {
		panic(error)
	}

	content := string(bytes)
	parts := strings.Split(content, "\n\n")
	boardStr := parts[0]
	var board [][]rune
	var botx, boty int
	for y, line := range strings.Split(boardStr, "\n") {
		x := strings.Index(line, bot)
		if x != -1 {
			botx = x
			boty = y
			line = strings.Replace(line, bot, string(empty), 1)
		}
		board = append(board, []rune(line))
	}

	height := len(board)
	width := len(board[0])

	programStr := strings.ReplaceAll(parts[1], "\n", "")
	program := []rune(programStr)

	return simstate{
		board,
		vec2{width, height},
		vec2{botx, boty},
		program,
		0,
	}
}

func showBoard(board [][]rune, bot vec2) string {
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("     %s\n", strings.Repeat(".123456789", 5)))
	for y, row := range board {
		var str string
		if y == bot.y {
			str = string(row[:bot.x]) + "\033[33m@\033[0m" + string(row[bot.x+1:])
		} else {
			str = string(row)
		}
		sb.WriteString(fmt.Sprintf("%3d: %s\n", y, str))
	}
	return sb.String()
}

func decode(instr rune) vec2 {
	mx := 0
	my := 0
	switch instr {
	case up:
		my = -1
	case down:
		my = 1
	case left:
		mx = -1
	case right:
		mx = 1
	}
	return vec2{mx, my}
}

func get(board *[][]rune, xy vec2) rune {
	return (*board)[xy.y][xy.x]
}

func put(board *[][]rune, xy vec2, glyph rune) {
	(*board)[xy.y][xy.x] = glyph
}

func inbounds(pos vec2, size vec2) bool {
	return 0 <= pos.x && pos.x < size.x &&
		0 <= pos.y && pos.y < size.y
}

func padd(a vec2, b vec2) vec2 {
	return vec2{a.x + b.x, a.y + b.y}
}

func getBoxChainLength(board *[][]rune, size vec2, firstBox vec2, dir vec2) (int, vec2, bool) {
	pos := firstBox
	n := 0
	for inbounds(pos, size) {
		glyph := get(board, pos)
		switch glyph {
		case empty:
			return n, pos, true
		case wall:
			return n, pos, false
		}
		pos = padd(pos, dir)
		n++
	}
	// in case there's input where the perimeter is not a wall.
	return n, pos, false
}

func moveBoxChain(board *[][]rune, start vec2, dir vec2, length int) {
	pos := start
	for i := 0; i < length; i++ {
		next := padd(pos, dir)
		r := get(board, next)
		put(board, pos, r)
		pos = next
	}
	put(board, pos, empty)
}

func tryMoveBox(state *simstate, boxpos vec2, movedir vec2) bool {
	n, emptyPos, ok := getBoxChainLength(&state.board, state.size, boxpos, movedir)
	if !ok {
		return false
	}
	moveBoxChain(&state.board, emptyPos, vec2{-movedir.x, -movedir.y}, n)
	return true
}

func fetch(state *simstate) (rune, bool) {
	if state.pc >= len(state.program) {
		return '?', false
	}
	instr := state.program[state.pc]
	state.pc++
	return instr, true
}

func step(state *simstate) (bool, rune, string) {
	var action string
	instr, ok := fetch(state)
	if !ok {
		return false, instr, "program halted"
	}
	move := decode(instr)
	newpos := padd(state.bot, move)
	tile := get(&state.board, newpos)
	switch tile {
	case empty:
		state.bot = newpos
		action = "move to empty"
	case box:
		if tryMoveBox(state, newpos, move) {
			action = "push boxes"
			state.bot = newpos
		} else {
			action = "failed to push boxes"
		}
	default:
		action = "ran into wall"
	}
	return true, instr, action
}

func interactivePart1(state simstate) {
	fmt.Println(showBoard(state.board, state.bot))
	n := 0
	for {
		more, instr, action := step(&state)
		fmt.Printf("\033[1;0H%c, %s, %+v:\n%s",
			instr, action, state.bot, showBoard(state.board, state.bot))
		if !more {
			break
		}
		n++
		time.Sleep(250 * time.Millisecond)
	}
	fmt.Print("\033[0m")
}

func solvePart1(state simstate) {
	for {
		more, _, _ := step(&state)
		if !more {
			break
		}
	}
	sum := 0
	n := 0
	for y, row := range state.board {
		for x, char := range row {
			if char == box {
				sum += x
				sum += 100 * y
				n++
			}
		}
	}
	fmt.Printf("Result: %d, Boxes: %d\033[0m\n", sum, n)
}

func main() {
	state := parseInput()
	if len(os.Args) > 1 && os.Args[1] == "-i" {
		interactivePart1(state)
	} else {
		solvePart1(state)
	}
}
