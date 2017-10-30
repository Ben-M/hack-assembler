// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(WAITPRESS)
//if keyboard = 0  GOTO WAITPRESS
  @KBD
  D=M
  @WAITPRESS
  D;JEQ

//i =8192
  @8191
  D=A
  @i
  M=D
(DRAW)
//screen[i]  = -1
  @i
  D=M
  @SCREEN
  A=D+A
  M=-1
// i--
  @i
  M=M-1
//GOTO draw if i>0
  D=M
  @DRAW
  D;JGE

(WAITRELEASE)
//if keyboard > 0 GOTO WAITRELEASE
  @KBD
  D=M
  @WAITRELEASE
  D;JGT

//i =8192
  @8191
  D=A
  @i
  M=D
(UNDRAW)
//screen[i]  = -1
  @i
  D=M
  @SCREEN
  A=D+A
  M=0
// i--
  @i
  M=M-1
//GOTO draw if i>0
  D=M
  @UNDRAW
  D;JGE

//GOTO WAITPRESS
  @WAITPRESS
  0;JMP