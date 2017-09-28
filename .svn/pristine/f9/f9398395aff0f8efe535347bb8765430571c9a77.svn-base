;;;;;; This file contains all aliases for constants, registers, ports etc

;;; Register aliases
.DEF rZero=r0

;; Current head position
.DEF rSnakeHeadX=r1
.DEF rSnakeHeadY=r2

;; Pointer to head and tail in body-segment-array
.DEF rSnakeHeadPtr=r5
.DEF rSnakeTailPtr=r6

;; Food position
.DEF rFoodX = r7
.DEF rFoodY = r8

;; Used for random generation
.DEF rRandom1=r9
.DEF rRandom2=r10
.DEF rRandom3=r11
.DEF rRandom4=r12

;; Which display row to activate
.DEF rRowCounter=r13

;; Counters, used in interuppts
.DEF rJoystickTimer=r14
.DEF rSnakeTimer=r15

.DEF rTemp0=r16
.DEF rTemp1=r17
.DEF rTemp2=r18
.DEF rTemp3=r19

;; For passing arguments to sub-routines
.DEF rArg0=r20
.DEF rArg1=r21

;; For returning from sub-routines
.DEF rRet0=r22
.DEF rRet1=r23

;; Status register
.DEF rStatus=r24

;;; Status bits
;; X or Y axis
.EQU STATUS_AXIS=0
;; Sign for current axis (+ or -)
.EQU STATUS_DIR=1
;; Booleans which are set in interuppts
.EQU STATUS_UPDATE_JOYSTICK=2
.EQU STATUS_UPDATE_SNAKE=3

;;; Axis thresholds
.EQU JOYSTICK_THRESHOLD_LOW = 50
.EQU JOYSTICK_THRESHOLD_HIGH = 200

;;; Row ports
.EQU ROW0_DDR=DDRC
.EQU ROW0_PORT=PORTC
.EQU ROW0_BIT=0
.EQU ROW1_DDR=DDRC
.EQU ROW1_PORT=PORTC
.EQU ROW1_BIT=1
.EQU ROW2_DDR=DDRC
.EQU ROW2_PORT=PORTC
.EQU ROW2_BIT=2
.EQU ROW3_DDR=DDRC
.EQU ROW3_PORT=PORTC
.EQU ROW3_BIT=3
.EQU ROW4_DDR=DDRD
.EQU ROW4_PORT=PORTD
.EQU ROW4_BIT=2
.EQU ROW5_DDR=DDRD
.EQU ROW5_PORT=PORTD
.EQU ROW5_BIT=3
.EQU ROW6_DDR=DDRD
.EQU ROW6_PORT=PORTD
.EQU ROW6_BIT=4
.EQU ROW7_DDR=DDRD
.EQU ROW7_PORT=PORTD
.EQU ROW7_BIT=5

;;; Column ports
.EQU COL0_DDR=DDRD
.EQU COL0_PORT=PORTD
.EQU COL0_BIT=6
.EQU COL1_DDR=DDRD
.EQU COL1_PORT=PORTD
.EQU COL1_BIT=7
.EQU COL2_DDR=DDRB
.EQU COL2_PORT=PORTB
.EQU COL2_BIT=0
.EQU COL3_DDR=DDRB
.EQU COL3_PORT=PORTB
.EQU COL3_BIT=1
.EQU COL4_DDR=DDRB
.EQU COL4_PORT=PORTB
.EQU COL4_BIT=2
.EQU COL5_DDR=DDRB
.EQU COL5_PORT=PORTB
.EQU COL5_BIT=3
.EQU COL6_DDR=DDRB
.EQU COL6_PORT=PORTB
.EQU COL6_BIT=4
.EQU COL7_DDR=DDRB
.EQU COL7_PORT=PORTB
.EQU COL7_BIT=5
