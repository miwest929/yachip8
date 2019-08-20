package main

import (
	"fmt"
	sdl "github.com/veandco/go-sdl2/sdl"
	"io/ioutil"
	"math/rand"
	//	"os"
	"strings"
)

type Chip8 struct {
	cpu     *CPU
	rom     *ROM
	memory  *Memory
	display *Display
}

const (
	NOP   = "NOP"
	SYS   = "SYS"
	CLS   = "CLS"
	RET   = "RET"
	JP_1  = "JP_1"
	CALL  = "CALL"
	SE_1  = "SE1"
	SNE_1 = "SNE_1"
	SE_2  = "SE2"
	LD_1  = "LD_1"
	ADD   = "ADD"
	LD_2  = "LD_2"
	OR    = "OR"
	AND_1 = "AND_1"
	XOR   = "XOR"
	ADD_2 = "ADD_2"
	SUB   = "SUB"
	SHR   = "SHR"
	SUBN  = "SUBN"
	SHL   = "SHL"
	SNE_2 = "SNE_2"
	LDI   = "LDI"
	JPV0  = "JPV0"
	RND   = "RND"
	DRW   = "DRW"
	SKP   = "SKP"
	SKNP  = "SKNP"
	LDVDT = "LDVDT"
	LDK   = "LDK"
	LDDTV = "LDDTV"
	LDST  = "LDST"
	ADDI  = "ADDI"
	LDF   = "LDF"
	LDB   = "LDB"
	LDIV  = "LDIV"
	LDVI  = "LDVI"
)

type CPU struct {
	// registers
	V []byte
	/*V0    byte
	V1    byte
	V2    byte
	V3    byte
	V4    byte
	V5    byte
	V6    byte
	V7    byte
	V8    byte
	V9    byte
	V10   byte
	V11   byte
	V12   byte
	V13   byte
	V14   byte
	V15   byte*/
	I     uint // 16-bit register
	sound byte
	delay byte
	PC    uint
	SP    uint // used to point to the topmost level of the stack

	shouldDrawGraphics bool
	shouldPlaySound    bool
}

const RAM_SIZE = 4096
const GRAPHICS_MEMORY_ADDR = 0xF00

type Memory struct {
	ram []byte
}

func (memory *Memory) Init() {
	memory.zeroFill()
	memory.storeDigitSprites()
}

func (memory *Memory) zeroFill() {
	// initialize memory to all zeroes
	for i, _ := range memory.ram {
		memory.ram[i] = 0x00
	}
}

func (memory *Memory) storeDigitSprites() {
	// 0
	memory.ram[0x0000] = 0xF0
	memory.ram[0x0001] = 0x90
	memory.ram[0x0002] = 0x90
	memory.ram[0x0003] = 0x90
	memory.ram[0x0004] = 0xF0

	// 1
	memory.ram[0x0005] = 0x20
	memory.ram[0x0006] = 0x60
	memory.ram[0x0007] = 0x20
	memory.ram[0x0008] = 0x20
	memory.ram[0x0009] = 0x70

	// 2
	memory.ram[0x000A] = 0xF0
	memory.ram[0x000B] = 0x10
	memory.ram[0x000C] = 0xF0
	memory.ram[0x000D] = 0x80
	memory.ram[0x000E] = 0xF0

	// 3
	memory.ram[0x000F] = 0xF0
	memory.ram[0x0010] = 0x10
	memory.ram[0x0011] = 0xF0
	memory.ram[0x0012] = 0x10
	memory.ram[0x0013] = 0xF0

	// 4
	memory.ram[0x0014] = 0x90
	memory.ram[0x0015] = 0x90
	memory.ram[0x0016] = 0xF0
	memory.ram[0x0017] = 0x10
	memory.ram[0x0018] = 0x10

	// 5
	memory.ram[0x0019] = 0xF0
	memory.ram[0x001A] = 0x80
	memory.ram[0x001B] = 0xF0
	memory.ram[0x001C] = 0x10
	memory.ram[0x001D] = 0xF0

	// 6
	memory.ram[0x001E] = 0xF0
	memory.ram[0x001F] = 0x80
	memory.ram[0x0020] = 0xF0
	memory.ram[0x0021] = 0x90
	memory.ram[0x0022] = 0xF0

	// 7
	memory.ram[0x0023] = 0xF0
	memory.ram[0x0024] = 0x10
	memory.ram[0x0025] = 0x20
	memory.ram[0x0026] = 0x40
	memory.ram[0x0027] = 0x40

	// 8
	memory.ram[0x0028] = 0xF0
	memory.ram[0x0029] = 0x90
	memory.ram[0x002A] = 0xF0
	memory.ram[0x002B] = 0x90
	memory.ram[0x002C] = 0xF0

	// 9
	memory.ram[0x002D] = 0xF0
	memory.ram[0x002E] = 0xF0
	memory.ram[0x002F] = 0xF0
	memory.ram[0x0020] = 0xF0
	memory.ram[0x0021] = 0xF0

	// A
	memory.ram[0x0022] = 0xF0
	memory.ram[0x0023] = 0x90
	memory.ram[0x0024] = 0xF0
	memory.ram[0x0025] = 0x90
	memory.ram[0x0026] = 0x90

	// B
	memory.ram[0x0027] = 0xE0
	memory.ram[0x0028] = 0x90
	memory.ram[0x0029] = 0xE0
	memory.ram[0x002A] = 0x90
	memory.ram[0x002B] = 0xE0

	// C
	memory.ram[0x002C] = 0xF0
	memory.ram[0x002D] = 0x80
	memory.ram[0x002E] = 0x80
	memory.ram[0x002B] = 0x80
	memory.ram[0x002C] = 0xF0

	// D
	memory.ram[0x002D] = 0xE0
	memory.ram[0x002E] = 0x90
	memory.ram[0x002F] = 0x90
	memory.ram[0x0030] = 0x90
	memory.ram[0x0031] = 0xE0

	// E
	memory.ram[0x0032] = 0xF0
	memory.ram[0x0033] = 0x80
	memory.ram[0x0034] = 0xF0
	memory.ram[0x0035] = 0x80
	memory.ram[0x0036] = 0xF0

	// F
	memory.ram[0x0037] = 0xF0
	memory.ram[0x0038] = 0x80
	memory.ram[0x0039] = 0xF0
	memory.ram[0x003A] = 0x80
	memory.ram[0x003B] = 0x80
}

// Default Chip8 resolution
const CHIP_8_WIDTH int32 = 64
const CHIP_8_HEIGHT int32 = 32

type Display struct {
	graphics [][]byte
	rows     int
	cols     int
	window   *sdl.Window
	canvas   *sdl.Renderer
}

func NewDisplay(rows int, cols int) *Display {
	data := make([][]byte, rows)
	for i := range data {
		data[i] = make([]byte, cols)
	}

	var modifier int32 = 10
	window, windowErr := sdl.CreateWindow("Chip 8", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED, CHIP_8_WIDTH*modifier, CHIP_8_HEIGHT*modifier, sdl.WINDOW_SHOWN)
	if windowErr != nil {
		panic(windowErr)
	}

	// Create render surface
	c, cErr := sdl.CreateRenderer(window, -1, 0)
	if cErr != nil {
		panic(cErr)
	}

	return &Display{rows: rows, cols: cols, graphics: data, canvas: c, window: window}
}

func (display *Display) Teardown() {
	display.window.Destroy()
	display.canvas.Destroy()
	sdl.Quit()
}

func (display *Display) Render() {
	for i, row := range display.graphics {
		for j, _ := range row {
			if display.graphics[i][j] == 0 {
				display.canvas.SetDrawColor(0, 0, 0, 255)
			} else {
				display.canvas.SetDrawColor(255, 255, 255, 255)
			}

			var modifier int32 = 10
			display.canvas.FillRect(&sdl.Rect{
				Y: int32(j) * modifier,
				X: int32(i) * modifier,
				W: modifier,
				H: modifier,
			})
		}
	}

	display.canvas.Present()
}

func (display *Display) ClearScreen() {
	for i, row := range display.graphics {
		for j, _ := range row {
			display.graphics[i][j] = 0
		}
	}
}

type ROM struct {
	instructions []byte
}

func NewChip8() *Chip8 {
	if sdlErr := sdl.Init(sdl.INIT_EVERYTHING); sdlErr != nil {
		panic(sdlErr)
	}

	cpu := CPU{V: make([]byte, 16)}
	memory := NewMemory()
	display := NewDisplay(64, 32)
	return &Chip8{cpu: &cpu, memory: memory, display: display}
}

func (chip *Chip8) Disassemble(rom *ROM) []string {
	disassembled := make([]string, 0)
	for idx := 0; idx < len(rom.instructions); idx += 2 {
		low := uint(rom.instructions[idx])
		high := uint(rom.instructions[idx+1])
		var word uint = (low << 8) | high

		instruction, args := chip.cpu.parseInstruction(word)

		argsStr := make([]string, len(args))
		for i := 0; i < len(args); i++ {
			aStr := fmt.Sprint(args[i])
			argsStr = append(argsStr, aStr)
		}

		hex := fmt.Sprintf("%x", word)
		diss := instruction + " " + strings.Join(argsStr, ",") + "  ; " + hex
		disassembled = append(disassembled, diss)
	}

	return disassembled
}

func (chip *Chip8) loadRom(rom *ROM) {
	chip.rom = rom

	// load rom data into memory starting at address 0x200
	startAddr := 0x200
	for offset := 0; offset < len(rom.instructions); offset += 1 {
		chip.memory.ram[startAddr+offset] = rom.instructions[offset]
	}

	chip.cpu.Run(chip.memory, chip.display)
}

func LoadRomFromFile(filepath string) *ROM {
	data, _ := ioutil.ReadFile(filepath)
	return &ROM{instructions: data}
}

func NewMemory() *Memory {
	return &Memory{ram: make([]byte, RAM_SIZE)}
}

func (cpu *CPU) executeInstruction(instruction string, args []uint, memory *Memory, display *Display) {
	switch instruction {
	case CLS:
		display.ClearScreen()
		cpu.shouldDrawGraphics = true
	case RET:
		// the stack is 16 16-bits (2 bytes) values
		addr := uint(memory.ram[cpu.SP])<<8 | uint(memory.ram[cpu.SP-1])
		cpu.PC = addr
		cpu.SP -= 2
	case JP_1:
		addr := args[0]
		cpu.PC = addr
	case CALL:
		cpu.SP += 2
		memory.ram[cpu.SP] = byte((cpu.PC & 0xff00) >> 8)
		memory.ram[cpu.SP-1] = byte(cpu.PC & 0x00ff)
		cpu.PC = args[0]
	case SE_1:
		reg := args[0]
		data := args[1]

		if cpu.V[reg] == byte(data) {
			cpu.PC += 2
		}
	case SNE_1:
		reg := args[0]
		data := args[1]

		if cpu.V[reg] != byte(data) {
			cpu.PC += 2
		}
	case SE_2:
		reg1 := args[0]
		reg2 := args[1]

		if cpu.V[reg1] == cpu.V[reg2] {
			cpu.PC += 2
		}
	case LD_1:
		reg := args[0]
		data := args[1]
		cpu.V[reg] = byte(data)
	case ADD:
		reg := args[0]
		data := args[1]
		cpu.V[reg] += byte(data)
	case LD_2:
		reg1 := args[0]
		reg2 := args[1]
		cpu.V[reg1] = cpu.V[reg2]
	case OR:
		reg1 := args[0]
		reg2 := args[1]
		cpu.V[reg1] = cpu.V[reg1] | cpu.V[reg2]
	case AND_1:
		reg1 := args[0]
		reg2 := args[1]
		cpu.V[reg1] = cpu.V[reg1] & cpu.V[reg2]
	case XOR:
		reg1 := args[0]
		reg2 := args[1]
		cpu.V[reg1] = cpu.V[reg1] ^ cpu.V[reg2]
	case ADD_2:
		// Set Vx = Vx + Vy, set VF = carry.
		reg1 := args[0]
		reg2 := args[1]

		r := uint16(cpu.V[reg1]) + uint16(cpu.V[reg2])
		var cf byte
		if r > 0xFF {
			cf = 1
		}
		cpu.V[0xF] = cf
		cpu.V[reg1] = byte(r)
	case SUB:
		reg1 := args[0]
		reg2 := args[1]

		v1 := cpu.V[reg1]
		v2 := cpu.V[reg2]
		var cf byte
		if v1 > v2 {
			cf = 1
		}
		cpu.V[0xF] = cf
		cpu.V[reg1] = byte(v1 - v2)
	case SHR:
		reg1 := args[0]

		var cf byte
		if (cpu.V[reg1] & 0x01) == 0x01 {
			cf = 1
		}
		cpu.V[0xF] = cf

		cpu.V[reg1] = cpu.V[reg1] / 2
	case SUBN:
		reg1 := args[0]
		reg2 := args[1]

		var cf byte
		if cpu.V[reg2] > cpu.V[reg1] {
			cf = 1
		}
		cpu.V[0xF] = cf
		cpu.V[reg1] = cpu.V[reg2] - cpu.V[reg1]
	case SHL:
		reg1 := args[0]
		var cf byte
		if (cpu.V[reg1] & 0x80) == 0x80 {
			cf = 1
		}
		cpu.V[0xF] = cf
		cpu.V[reg1] = cpu.V[reg1] * 2
	case SNE_2:
		reg1 := args[0]
		reg2 := args[1]

		if cpu.V[reg1] != cpu.V[reg2] {
			cpu.PC += 2
		}
	case LDI:
		nnn := args[0]
		cpu.I = nnn
	case JPV0:
		nnn := args[0]
		cpu.PC = uint(cpu.V[0]) + nnn
	case RND:
		reg := args[0]
		kk := args[1]
		rnd := rand.Intn(256)
		cpu.V[reg] = byte(rnd) & byte(kk)
	case DRW:
		// Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision
		x := uint(cpu.V[args[0]])
		y := uint(cpu.V[args[1]])
		n := args[2]

		var cf byte
		for j := uint(0); j < n; j++ {
			pixel := memory.ram[cpu.I+j]
			for i := uint(0); i < 8; i++ {
				if pixel&(0x80>>i) != 0 {
					if display.graphics[y+j][x+i] == 1 {
						cf = 1
					}
					display.graphics[y+j][x+i] ^= 1
				}
			}
		}
		cpu.V[0xF] = cf
		cpu.shouldDrawGraphics = true
	case SKP:
		// Skips the next instruction if the key stored in VX is pressed
	}

	// move to next program instruction
	cpu.PC += 2
}

func (cpu *CPU) Run(memory *Memory, display *Display) {
	cpu.PC = 0x0200
	cpu.SP = 0x0fa0

	memory.Init()

	isRunning := true
	cpu.shouldDrawGraphics = true
	cpu.shouldPlaySound = true
	for isRunning {
		// every instruction is 2 bytes
		low := uint(memory.ram[cpu.PC])
		high := uint(memory.ram[cpu.PC+1])
		word := (high << 8) | low
		instruction, args := cpu.parseInstruction(word)
		cpu.executeInstruction(instruction, args, memory, display)

		// draw graphics
		if cpu.shouldDrawGraphics {
			display.Render()
		}

		if cpu.shouldPlaySound {

		}
	}

	display.Teardown()
}

func (cpu *CPU) parseInstruction(word uint) (string, []uint) {
	if IsClear(word, 15, 14, 13, 12) {
		addr := word & 0x0fff
		return SYS, []uint{addr}
	} else if word == 0x00E0 {
		return CLS, []uint{}
	} else if word == 0x00EE {
		return RET, []uint{}
	} else if IsSet(word, 12) && IsClear(word, 15, 14, 13) {
		addr := word & 0x0fff
		return JP_1, []uint{addr}
	} else if IsSet(word, 13) && IsClear(word, 15, 14, 12) {
		addr := word & 0x0fff
		return CALL, []uint{addr}
	} else if IsSet(word, 13, 12) && IsClear(word, 15, 14) {
		reg := (word & 0x0f00) >> 8
		value := word & 0x00ff
		return SE_1, []uint{reg, value}
	} else if IsSet(word, 14) && IsClear(word, 15, 13, 12) {
		reg := (word & 0x0f00) >> 8
		value := word & 0x00ff
		return SNE_1, []uint{reg, value}
	} else if IsSet(word, 14, 12) && IsClear(word, 0, 1, 2, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := word & 0x00f0
		return SE_2, []uint{reg1, reg2}
	} else if IsSet(word, 14, 13) && IsClear(word, 15, 12) {
		reg := (word & 0x0f00) >> 8
		value := word & 0x00ff
		return LD_1, []uint{reg, value}
	} else if IsSet(word, 14, 13, 12) && IsClear(word, 15) {
		reg := (word & 0x0f00) >> 8
		value := word & 0x00ff
		return ADD, []uint{reg, value}
	} else if IsSet(word, 15) && IsClear(word, 14, 13, 12, 0, 1, 2, 3) {
		// 8xy0 - LD Vx, Vy
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return LD_2, []uint{reg1, reg2}
	} else if IsSet(word, 15, 0) && IsClear(word, 14, 13, 12, 1, 2, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return OR, []uint{reg1, reg2}
	} else if IsSet(word, 15, 1) && IsClear(word, 14, 13, 12, 0, 2, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return AND_1, []uint{reg1, reg2}
	} else if IsSet(word, 15, 1, 0) && IsClear(word, 14, 13, 12, 2, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return XOR, []uint{reg1, reg2}
	} else if IsSet(word, 15, 2) && IsClear(word, 14, 13, 12, 0, 1, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return ADD_2, []uint{reg1, reg2}
	} else if IsSet(word, 15, 2, 0) && IsClear(word, 14, 13, 12, 1, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return SUB, []uint{reg1, reg2}
	} else if IsSet(word, 15, 2, 1) && IsClear(word, 14, 13, 12, 0, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return SHR, []uint{reg1, reg2}
	} else if IsSet(word, 15, 2, 1, 0) && IsClear(word, 14, 13, 12, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return SUBN, []uint{reg1, reg2}
	} else if IsSet(word, 15, 3, 2, 1) && IsClear(word, 14, 13, 12, 0) {
		// 8xyE - SHL Vx {, Vy}
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return SHL, []uint{reg1, reg2}
	} else if IsSet(word, 15, 12) && IsClear(word, 14, 13, 0, 1, 2, 3) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		return SNE_2, []uint{reg1, reg2}
	} else if IsSet(word, 15, 13) && IsClear(word, 14, 12) {
		addr := word & 0x0fff
		return LDI, []uint{addr}
	} else if IsSet(word, 15, 13, 12) && IsClear(word, 14) {
		addr := word & 0x0fff
		return JPV0, []uint{addr}
	} else if IsSet(word, 15, 14) && IsClear(word, 13, 12) {
		reg := (word & 0x0f00) >> 8
		and := word & 0x00ff
		return RND, []uint{reg, and}
	} else if IsSet(word, 15, 14, 12) && IsClear(word, 13) {
		reg1 := (word & 0x0f00) >> 8
		reg2 := (word & 0x00f0) >> 4
		nibble := word & 0x000f
		return DRW, []uint{reg1, reg2, nibble}
	} else if IsSet(word, 15, 14, 13, 7, 4, 3, 2, 1) && IsClear(word, 12, 6, 5, 0) {
		reg := (word & 0x0f00) >> 8
		return SKP, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 7, 5, 0) && IsClear(word, 12, 6, 4, 3, 2, 1) {
		reg := (word & 0x0f00) >> 8
		return SKNP, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 2, 1, 0) && IsClear(word, 7, 6, 5, 4, 3) {
		reg := (word & 0x0f00) >> 8
		return LDVDT, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 3, 1) && IsClear(word, 7, 6, 5, 4, 2, 0) {
		reg := (word & 0x0f00) >> 8
		return LDK, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 4, 2, 0) && IsClear(word, 7, 6, 5, 3, 1) {
		reg := (word & 0x0f00) >> 8
		return LDDTV, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 4, 3) && IsClear(word, 7, 6, 5, 2, 1, 0) {
		reg := (word & 0x0f00) >> 8
		return LDST, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 4, 3, 2, 1) && IsClear(word, 7, 6, 5, 0) {
		reg := (word & 0x0f00) >> 8
		return ADDI, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 5, 3, 0) && IsClear(word, 7, 6, 4, 2, 1) {
		reg := (word & 0x0f00) >> 8
		return LDF, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 5, 4, 1, 0) && IsClear(word, 7, 6, 3, 2) {
		reg := (word & 0x0f00) >> 8
		return LDB, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 6, 4, 2, 0) && IsClear(word, 7, 5, 3, 1) {
		reg := (word & 0x0f00) >> 8
		return LDIV, []uint{reg}
	} else if IsSet(word, 15, 14, 13, 12, 6, 5, 2, 0) && IsClear(word, 7, 4, 3, 1) {
		reg := (word & 0x0f00) >> 8
		return LDVI, []uint{reg}
	}

	return NOP, []uint{}
}

func IsClear(num uint, positions ...uint64) bool {
	for _, position := range positions {
		if GetBit(num, position) != 0 {
			return false
		}
	}

	return true
}

func IsSet(num uint, positions ...uint64) bool {
	for _, position := range positions {
		if GetBit(num, position) != 1 {
			return false
		}
	}

	return true
}

func GetBit(num uint, position uint64) uint {
	return (num >> position) & 1
}

func main() {
	chip8 := NewChip8()

	rom := LoadRomFromFile("./roms/Breakout.ch8")

	/*disassembled := chip8.Disassemble(rom)
	f, err := os.Create("Breakout.disassembled")
	if err != nil {
		fmt.Println(err)
		return
	}
	for _, instruction := range disassembled {
		f.WriteString(instruction + "\n")
	}*/

	chip8.loadRom(rom)
}
