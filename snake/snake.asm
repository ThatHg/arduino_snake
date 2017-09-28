/* 
	Snake? Snake! Snaaaaaaaaaaaaaaaaake!
	By: Joel Juvél, Hampus Gustafsson och Karl Zylinski
	
*/

.include "snake.inc"

.DSEG
	; 8 byte framebuffer (8x8 led matrix)
	rows: 
		row0: .byte 1
		row1: .byte 1
		row2: .byte 1
		row3: .byte 1
		row4: .byte 1
		row5: .byte 1
		row6: .byte 1
		row7: .byte 1

	bodySegments: .byte 128		; 64 coordinates (x & y)
.CSEG
.ORG 0000
	jmp init
.ORG OVF2addr
	jmp _timerInterrupt2		; Update display
.ORG OVF0addr
	jmp _timerInterrupt			; Increases counters and sets flags in rStatus
.ORG INT_VECTORS_SIZE

init:
	; Create a zero register
	eor rZero,rZero
	
	; Setup stack
	ldi rTemp0,LOW(RAMEND)
	out SPL,rTemp0
	ldi rTemp0,HIGH(RAMEND)
	out SPH,rTemp0

	; Activate row DDRs
	sbi ROW0_DDR,ROW0_BIT
	sbi ROW1_DDR,ROW1_BIT
	sbi ROW2_DDR,ROW2_BIT
	sbi ROW3_DDR,ROW3_BIT
	sbi ROW4_DDR,ROW4_BIT
	sbi ROW5_DDR,ROW5_BIT
	sbi ROW6_DDR,ROW6_BIT
	sbi ROW7_DDR,ROW7_BIT

	; Activate column DDRs
	sbi COL0_DDR,COL0_BIT
	sbi COL1_DDR,COL1_BIT
	sbi COL2_DDR,COL2_BIT
	sbi COL3_DDR,COL3_BIT
	sbi COL4_DDR,COL4_BIT
	sbi COL5_DDR,COL5_BIT
	sbi COL6_DDR,COL6_BIT
	sbi COL7_DDR,COL7_BIT

	; Initialize A/D converter
	lds rTemp0,ADMUX
	sbr rTemp0,1<<REFS0 ; REFS = Reference current
	cbr rTemp0,1<<REFS1
	sbr rTemp0,1<<ADLAR ; 8 bit mode ( 10 sucks shits )
	sts ADMUX,rTemp0
	
	; Control register, used for starting A/D conversion
	lds rTemp0,ADCSRA
	sbr rTemp0,1<<ADPS0 ; ADPS sets clock speed for conversion
	sbr rTemp0,1<<ADPS1
	sbr rTemp0,1<<ADPS2
	sbr rTemp0,1<<ADEN ; Actives A/D converter
	sts ADCSRA,rTemp0

	; Initialize timer
	in rTemp0, TCCR0B	; Set pre scaling
	andi rTemp0, 0b11111000
	ori rTemp0, 0b101	; timer/1024
	out TCCR0B, rTemp0
	
	lds rTemp0, TIMSK0	; Activate timer 0
	ori rTemp0, 1<<TOIE0
	sts TIMSK0, rTemp0

	; Initialize timer2
	lds rTemp0, TCCR2B	; Set pre scaling
	andi rTemp0, 0b11111000
	ori rTemp0, 0b100	; timer/64 (optimal value for screen refresh (flicker vs brightness))
	sts TCCR2B, rTemp0
	
	lds rTemp0, TIMSK2	; Activate timer 2
	ori rTemp0, 1<<TOIE2
	sts TIMSK2, rTemp0
	jmp main

; These 8 sub-routines each updates a specific row on the screen
_row0_update:
	call _clearCols
	cbi ROW7_PORT,ROW7_BIT			; 2 clocks
	sbi ROW0_PORT,ROW0_BIT			; 2 clocks
	call _refreshCols				; 4 clocks
	ret								; 4 clocks
_row1_update:
	call _clearCols
	cbi ROW0_PORT,ROW0_BIT
	sbi ROW1_PORT,ROW1_BIT
	call _refreshCols
	ret
_row2_update:
	call _clearCols
	cbi ROW1_PORT,ROW1_BIT
	sbi ROW2_PORT,ROW2_BIT
	call _refreshCols
	ret
_row3_update:
	call _clearCols
	cbi ROW2_PORT,ROW2_BIT
	sbi ROW3_PORT,ROW3_BIT
	call _refreshCols
	ret
_row4_update:
	call _clearCols
	cbi ROW3_PORT,ROW3_BIT
	sbi ROW4_PORT,ROW4_BIT
	call _refreshCols
	ret
_row5_update:
	call _clearCols
	cbi ROW4_PORT,ROW4_BIT
	sbi ROW5_PORT,ROW5_BIT
	call _refreshCols
	ret
_row6_update:
	call _clearCols
	cbi ROW5_PORT,ROW5_BIT
	sbi ROW6_PORT,ROW6_BIT
	call _refreshCols
	ret
_row7_update:
	call _clearCols
	cbi ROW6_PORT,ROW6_BIT
	sbi ROW7_PORT,ROW7_BIT
	call _refreshCols
	ret

_clearCols:
	; Clear columns
	cbi COL0_PORT,COL0_BIT
	cbi COL1_PORT,COL1_BIT
	cbi COL2_PORT,COL2_BIT
	cbi COL3_PORT,COL3_BIT
	cbi COL4_PORT,COL4_BIT
	cbi COL5_PORT,COL5_BIT
	cbi COL6_PORT,COL6_BIT
	cbi COL7_PORT,COL7_BIT
	ret

_refreshCols:
	; Activate columns. rArg0 contains the bits for the columns to be lit or not
	sbrc rArg0,7
	sbi COL0_PORT,COL0_BIT
	sbrc rArg0,6
	sbi COL1_PORT,COL1_BIT
	sbrc rArg0,5
	sbi COL2_PORT,COL2_BIT
	sbrc rArg0,4
	sbi COL3_PORT,COL3_BIT
	sbrc rArg0,3
	sbi COL4_PORT,COL4_BIT
	sbrc rArg0,2
	sbi COL5_PORT,COL5_BIT
	sbrc rArg0,1
	sbi COL6_PORT,COL6_BIT
	sbrc rArg0,0
	sbi COL7_PORT,COL7_BIT
	ret

; rArg0 is the axis to be read according to ADMUX documentation. rRet0 contains the value of the specified axis upon return
_readJoystickAxis:
	; Setup A/D-converter with axis to be read
	lds rTemp0,ADMUX
	andi rTemp0,0xF0
	or rTemp0,rArg0
	sts ADMUX,rTemp0

	; Start A/D-conversion
	lds rTemp0,ADCSRA
	ori rTemp0,1<<ADSC
	sts ADCSRA,rTemp0

	; Loop until A/D-conversion is complete
	_readJoystickAxis_loop:
		lds rTemp0,ADCSRA
		sbrc rTemp0,ADSC
		jmp _readJoystickAxis_loop

	; Return axis value
	lds rRet0,ADCH
	ret

_readJoystick:
	ldi rArg0,4
	call _readJoystickAxis
	mov rRet1,rRet0 ; Store Y axis value in ret1

	ldi rArg0,5
	call _readJoystickAxis ; returns X axis value in rRet0
		
	cpi rRet0,JOYSTICK_THRESHOLD_HIGH
	brlo _readJoystick_checkIfLowerX	; Branch if X axis under the upper threshold
	andi rStatus,~(1<<STATUS_DIR)		; Set direction (sign of axis) to 0 (-)
	andi rStatus,~(1<<STATUS_AXIS)		; Set axis to 0 (X)
	jmp _readJoystick_checkIfHigherY

_readJoystick_checkIfLowerX:
	cpi rRet0,JOYSTICK_THRESHOLD_LOW
	brsh _readJoystick_checkIfHigherY	; Branch if X axis is over the lower threshold (X axis is inside the deadzone)
	ori rStatus,(1<<STATUS_DIR)			; Set direction to (+)
	andi rStatus,~(1<<STATUS_AXIS)		; Set axis to 0 (X)

_readJoystick_checkIfHigherY:
	cpi rRet1,JOYSTICK_THRESHOLD_HIGH
	brlo _readJoystick_checkIfLowerY	; Branch if Y axis is under the upper threshold
	andi rStatus,~(1<<STATUS_DIR)		; Set direction to 0 (-)
	ori rStatus,(1<<STATUS_AXIS)		; Set axis to 1 (Y)
	jmp _readJoystick_end

_readJoystick_checkIfLowerY:
	cpi rRet1,JOYSTICK_THRESHOLD_LOW
	brsh _readJoystick_end				; Branch if Y axis is over the lower threshold (Y axis is inside the deadzone)
	ori rStatus,(1<<STATUS_DIR)			; Set direction to 1 (+)
	ori rStatus,(1<<STATUS_AXIS)		; Set Axis to 1 (Y)

_readJoystick_end:
	andi rStatus,~(1<<STATUS_UPDATE_JOYSTICK)	; Clear joystick update flag
	ret

_timerInterrupt:
	push rTemp0	; Save registers and flags
	push rTemp1
	in rTemp0,SREG
	
	; Changes the random registers, since these interuppt aren't completely periodic, this creates "randomness"
	dec rRandom2 
	dec rRandom2
	dec rRandom3

	; Increase timer counters
	inc rSnakeTimer
	inc rJoystickTimer

	; Set joystick update flag when rJoystickTimer reaches 5 (every fifth interuppt)
	ldi rTemp1,5
	cp rJoystickTimer,rTemp1
	brlo _timerInterrupt_Joystick_noOverflow
	clr rJoystickTimer
	ori rStatus,(1<<STATUS_UPDATE_JOYSTICK)
_timerInterrupt_Joystick_noOverflow:
	
	; Set update snake flag every 30th interuppt. This will make the main loop update the snake (speed of snake)
	ldi rTemp1,30
	cp rSnakeTimer,rTemp1
	brlo _timerInterrupt_Snake_noOverflow
	clr rSnakeTimer
	ori rStatus,(1<<STATUS_UPDATE_SNAKE)
_timerInterrupt_Snake_noOverflow:

	out SREG,rTemp0
	pop rTemp1
	pop rTemp0
	reti

; Updates screen and random-registers
_timerInterrupt2:
	push rTemp0 ; Save registers and flags
	push rTemp1
	push rArg0
	push XL
	push XH
	in rTemp0,SREG	

	; Changes the random registers, since these interuppt aren't completely periodic, this creates "randomness"
	inc rRandom1
	inc rRandom4
	inc rRandom4

	; Update one row of the screen
	ldi XL,LOW(rows)
	ldi XH,HIGH(rows)	
	clc						; Clear carry and rotate rowcounter 1 step
	rol rRowCounter
	adc rRowCounter,rZero
	mov rTemp1,rRowCounter
	ld rArg0,X+
	sbrc rTemp1,0			; Skip update if bit is clear
	call _row0_update
	ld rArg0,X+
	sbrc rTemp1,1
	call _row1_update
	ld rArg0,X+
	sbrc rTemp1,2
	call _row2_update
	ld rArg0,X+
	sbrc rTemp1,3
	call _row3_update
	ld rArg0,X+
	sbrc rTemp1,4
	call _row4_update
	ld rArg0,X+
	sbrc rTemp1,5
	call _row5_update
	ld rArg0,X+
	sbrc rTemp1,6
	call _row6_update
	ld rArg0,X	
	sbrc rTemp1,7
	call _row7_update

	out SREG,rTemp0
	pop XH
	pop XL
	pop rArg0
	pop rTemp1
	pop rTemp0
	reti

; Called once at start of a new game, inits variables etc
;
;
_gameInit:
	clr rStatus
	clr rSnakeTimer
	clr rJoystickTimer

	clr rSnakeHeadX
	clr rSnakeHeadY
	clr rSnakeHeadPtr
	clr rSnakeTailPtr

	; Set next head position to the second segment (1 segment = 2 bytes)
	ldi rTemp0,2
	mov rSnakeHeadPtr,rTemp0

	ldi rTemp0,1
	mov rRowCounter,rTemp0	; Which row to display 
	
	; Snaaaake starts at (3,4)
	ldi rTemp0,4
	mov rSnakeHeadY,rTemp0
	ldi rTemp0,1<<3
	mov rSnakeHeadX,rTemp0

	ldi ZL,LOW(bodysegments)	; Store current head pos in segments
	ldi ZH,HIGH(bodysegments)	
	st Z+,rSnakeHeadX
	st Z,rSnakeHeadY

	ori rStatus,(1<<STATUS_AXIS)	; Move up (axis=1, dir=0)

	; Clear framebuffer
	ldi XL,LOW(rows)
	ldi XH,HIGH(rows)
	st X+,rZero
	st X+,rZero
	st X+,rZero
	st X+,rZero
	st X+,rZero
	st X+,rZero
	st X+,rZero
	st X,rZero

	; Place snake head in frame buffer.
	ldi XL,LOW(rows)
	ldi XH,HIGH(rows)
	add XL,rSnakeHeadY
	adc XH,rZero
	st X,rSnakeHeadX
	
	call _placefood
	
	sei	; Activate global interrupts
	ret

; Updates game one tick.
; Moves snake, checks collision against food & itself.
; Generates new food when previous was eaten.
; Calls gameinit if head collides with itself.
_moveSnake:
	ldi ZL,LOW(bodysegments)
	ldi ZH,HIGH(bodysegments)
	ldi XL,LOW(rows)
	ldi XH,HIGH(rows)

	sbrs rStatus,STATUS_AXIS	; X or Y axis?
	jmp _moveSnake_XAxis		; Movement on X axis

	; Movement on Y axis
	mov rTemp0,rStatus			; Grab Y direction from rStatus
	andi rTemp0,(1<<STATUS_DIR)
	subi rTemp0,1				; Make it -1..1
	add rSnakeHeadY,rTemp0		; Update Y pos
	ldi rTemp0,0x7
	and rSnakeHeadY,rTemp0		; Make Y pos 3 bit (0..7)

	jmp _moveSnake_end			; Complete movement operation

_moveSnake_XAxis:
	mov rTemp1,rSnakeHeadX
	com rTemp1				; Invert x position of the head
	
	ldi rTemp2,2
	cp rSnakeHeadX,rTemp2	; Compare head x position with 2 (sets carry if < 2)
	sbrc rStatus,STATUS_DIR	; Skip instruction if moving left
	ror rSnakeHeadX			; Rotate X 1 step right (if head pos was 1, carry will be rotated into bit 7)
	
	clc						; Clear carry
	sbrs rStatus,STATUS_DIR ; Moving Left?
	rol rSnakeHeadX			; Rotate X 1 step left (if bit 7 is set it will be rotated into carry)
	adc rSnakeHeadX,rZero	; Add carry to head x position (if rol was skipped clc has cleared the carry)

_moveSnake_end:
	push ZL
	push ZH	
	add ZL,rSnakeHeadPtr	; Store new head position in body segment array
	adc ZH,rZero
	st Z+,rSnakeHeadX
	st Z,rSnakeHeadY	
	pop ZH
	pop ZL
		
	ldi rTemp0,2			; Increase headptr position and make it wrap if > 127
	add rTemp0,rSnakeHeadPtr
	andi rTemp0,127
	mov rSnakeHeadPtr,rTemp0

	; Activate new head position in frame buffer
	push XL
	push XH
	add XL,rSnakeHeadY			; New head pos
	adc XH,rZero
	ld rTemp0,X	
	mov rTemp1,rTemp0			; Save byte in rTemp1 (old frame buffer value)
	or rTemp0,rSnakeHeadX		; Set bit and store in frame buffer
	st X,rTemp0	
	pop XH
	pop XL

	; Check if new head position collides with food.
	cp rSnakeHeadY,rFoodY
	brne checkSelfCollision
	cp rSnakeHeadX,rFoodX
	brne checkSelfCollision

	; We collided with food, place new.
	call _placefood

	; Don't reset tail position (tailptr into body segments, makes snake grow one segment)
	jmp _no_reset_tail

checkSelfCollision:
	and rTemp1,rSnakeHeadX	; Mask saved row from frame buffer with headposition.
	brne _move_collision	; If a bit is set in the row at head x position we have a colllision with a body segment.

	; Read coordinates of tail position (rTemp0,rTemp1)
	add ZL,rSnakeTailPtr
	adc ZH,rZero
	ld rTemp0,Z+
	ld rTemp1,Z

	; Clear bit (tail) in framebuffer at coordinate (rTemp0, rTemp1)
	add XL,rTemp1		; Y coord
	adc XH,rZero
	ld rTemp1,X			; Load frame buffer row
	com rTemp0
	and rTemp1,rTemp0	; Mask with inverted tail x position
	st X,rTemp1			; Save to frame buffer

	ldi rTemp0,2		; Increase tail pointer and make it wrap if > 127
	add rTemp0,rSnakeTailPtr
	andi rTemp0,127
	mov rSnakeTailPtr,rTemp0
_no_reset_tail:
	andi rStatus,~(1<<STATUS_UPDATE_SNAKE)	; Clear update flag
	ret
_move_collision:	; Collision occured, reinit game
	call _gameInit 
	ret

_placefood:	
_placefood_findRow:
	call _rand		; Generate random value

	andi rRet0,7	; Clamp value between 0..7 (a random row)
	ldi XL,LOW(rows)
	ldi XH,HIGH(rows)
	add XL,rRet0
	adc XH,rZero
	ld rTemp0,X		; Read row from frame buffer
	cpi rTemp0,0xFF	; Find row that has some bits free
	breq _placefood_findRow
	
	; Row in frame buffer that contains atleast one free column
	mov rFoodY,rRet0	
	
	; Generate a random value (use an initial x position)
	call _rand
	ldi rTemp0,1
	andi rRet0,7
	breq _placefood_conversionComplete

_placefood_convertCoord: ; Convert the value(0..7) in rRet0 into the bit format we use for x positions.
	lsl rTemp0			 ; rTemp0 = (1 << rRet0)
	dec rRet0
	brne _placefood_convertCoord

_placefood_conversionComplete:

	ld rTemp1,X		; Load row from frame buffer
_placefood_findEmptyXPos:
	mov rTemp2,rTemp1
	and rTemp2,rTemp0
	breq _placefood_findEmptyXPosComplete	; If (rowvalue & xpos) == 0 the column is free
	lsl rTemp0								; Rotate rTemp0 one step left (shift + adc)
	adc rTemp0,rZero
	jmp _placefood_findEmptyXPos

_placefood_findEmptyXPosComplete:
	mov rFoodX, rTemp0		; Place new food in frame buffer
	or rTemp1, rFoodX		
	st X, rTemp1
	ret

; Initialize random seeds
;
;
_initRand:
	
	ldi rTemp0,3
	mov rRandom1,rTemp0
	ldi rTemp0,17
	mov rRandom2,rTemp0
	ldi rTemp0,189
	mov rRandom3,rTemp0
	ldi rTemp0,99
	mov rRandom4,rTemp0
	ret

; Generates a pseudo random number
;
;
_rand:
	mov rTemp1,rRandom1
	lsl rTemp1
	lsl rTemp1
	lsl rTemp1	; rTemp0 = rRandom1 ^ (rRandom1 << 3)
	mov rTemp0,rRandom1
	eor rTemp0,rTemp1

	mov rRandom1,rRandom2	; rRandom1 = rRandom2
	mov rRandom2,rRandom3	; rRandom2 = rRandom3
	mov rRandom3,rRandom4	; rRandom3 = rRandom4 

	mov rTemp2,rRandom4
	swap rTemp2
	andi rTemp2,0x0F
	lsr rTemp2
	mov rTemp1,rRandom4
	eor rTemp1,rTemp2 ; rTemp1 = rRandom4 ^ (rRandom4 >> 5)

	mov rTemp3,rTemp0
	lsr rTemp3
	lsr rTemp3
	mov rTemp2,rTemp0
	eor rTemp2,rTemp3 ; rTemp2 = rTemp0 ^ (rTemp0 >> 2)
	eor rTemp1,rTemp2 ; rRandom4 = rTemp1 ^ rTemp2
	mov rRandom4,rTemp1
	mov rRet0,rRandom4
	ret

; Main loop
;
;
main:
	call _initRand
	call _gameInit	
	main_loop:	
		
		; Update joystick if flag set
		sbrc rStatus,STATUS_UPDATE_JOYSTICK
		call _readJoystick
		
		; Update snake if flag set
		sbrc rStatus,STATUS_UPDATE_SNAKE
		call _moveSnake

		; Generate a random number
		; This will help making the random generate "better" random numbers
		call _rand		
		jmp main_loop