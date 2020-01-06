; Name:                     Zachary Hughes
; Course:                   COMP 252
; Instructor:               Dr. Conlon
; Date Started:             March 7th, 2018
; Last Modification:        May 2nd, 2018
; Purpose of the Program:   Pong Game. The game implements all the necessary
;                           subroutines to play a game of pong. Players control
;                           the paddles via keyboard input while the ball moves
;                           according to calculated directional vectors. On
;                           start, he game will print a directional statement 
;                           and begin when one of two signal keys are pressed.
;                           Pressing 'q' will start a regular pong game, up to
;                           22 points, and pressing 'z' will play to only 11.
;                           Then, the game will loop until one side accumulates
;                           enough points, ending the game. The program will
;                           then remove the ball from play and print a message
;                           to the console, congratulating the winner. At this
;                           point, the player may either press space to restart
;                           the game, which will allow them to select a new
;                           point limit, or they may press any other key to
;                           terminate the program.
;
;
;   Quick direction blurb:
;   Controls:
;   q   : left paddle up
;   z   : left paddle down
;   ]   : right paddle up
;   /   : right paddle down
; space : pause during game, reset after 
;
;
    .CR 6502            ; Assemble 6502 Language
    .LI on,toff         ; Listening on, no timings included
    .TF kilo.prg,BIN    ; Object file and format

; Define some constants
block   =   $e6
lwall   =   $f6         ;Left wall tile
rwall   =   $f5         ;Right wall tile
bwall   =   $f8         ;Bottom of screen tile
twall   =   $f9         ;Top of screen tile
lup     =   $71         ;Left up character q
ldo     =   $7a         ;Left down character z
rup     =   $5d         ;Right up character ]
rdo     =   $2f         ;Right down character /
lp      =   $61         ;Right power hit a
rp      =   $27         ;Left power hit '
iobase  =   $8800       ;Keyboard data register
iodata  =   iobase      ;Keyboard data
iostat  =   iobase+1    ;Keyboard status register
iocmd   =   iobase+2    ;Keyboard command register
ioctrl  =   iobase+3    ;Keyboard control register
space   =   $20         ;ASCII code for space
box     =   $fe         ;ASCII code for a pixel-filling box
nine    =   $39         ;ASCII 9
colon   =   $3a         ;ASCII :
zero    =   $30         ;ASCII 0
one     =   $31         ;ASCII 1
two     =   $32         ;ASCII 2
mask    =   %00011111   ;Binary bit mask
maxmask =   %00000000   ;111001111100111 111001110111111
home    =   $7000       ;Address of upper left (home) on video screen
statln  =   24*40 + home ;Because two passes, home is defined
irv     =   $fffa       ;Interrupt vector
spdpt1  =   $5          ;Flag for first speed transition
spdpt2  =   $a          ;Flag for second speed transition
spdpt3  =   $f          ;Flag for third speed transition
spd1    =   $40         ;Value for first speed
spd2    =   $33         ;Value for second speed
spd3    =   $26         ;Value for third speed
spd4    =   $20         ;Value for fourth speed
spdp    =   $15         ;Value for power hit


    .OR $0000   ;Start code at address $0000
    jmp start   ;Jump to the beginning of the program, proper

; Define zero-page storage
;Pointers for input buffer
inbuff  = *     
        .BS $20
headptr .DB 0
tailptr .DB 0
scrend  .DW $73e7       ;Address of bottom right of video screen
;Zero-Page holders for rows
row00   .DW home        ;row 0
row01   .DW $7028       ;row 1
row02   .DW $7050       ;row 2
row03   .DW $7078       ;row 3
row04   .DW $70a0       ;row 4
row05   .DW $70c8       ;row 5
row06   .DW $70f0       ;row 6
row07   .DW $7118       ;row 7
row08   .DW $7140       ;row 8
row09   .DW $7168       ;row 9
row10   .DW $7190       ;row 10
row11   .DW $71b8       ;row 11
row12   .DW $71e0       ;row 12
row13   .DW $7208       ;row 13
row14   .DW $7230       ;row 14
row15   .DW $7258       ;row 15
row16   .DW $7280       ;row 16
row17   .DW $72a8       ;row 17
row18   .DW $72d0       ;row 18
row19   .DW $72f8       ;row 19
row20   .DW $7320       ;row 20
row21   .DW $7348       ;row 21
row22   .DW $7370       ;row 22
row23   .DW $7398       ;row 23
row24   .DW $73c0       ;row 24
;Ball variables
balloc  .DW $0          ;Location of the ball
horipos .DW $14         ;value 0-39 which is the ball's horizontal position
vertpos .DW $18         ;Value 0-48 which is twice the row number
horidir .DW $1          ;Direction, 1 or -1, ball's horizontal direction
vertdir .DW $0          ;Value -3-3 which holds the ball's vertical direction
balltmp .DW $0          ;Temporary holder variable for movement
postmp  .DW $0          ;Temporary holder variable for position
pdllpos .DW $14         ;Row pointer for left paddle
pdlrpos .DW $14         ;Row pointer for right paddle
;Beginning of memory pointers for the left paddle
pdllloc .DW $7190       ;left paddle portion 1: up 2
pdlllc2 .DW $71b8       ;left paddle portion 2: up 1
pdlllc3 .DW $71e0       ;left paddle portion 3: center
pdlllc4 .DW $7208       ;left paddle portion 4: down 1
pdlllc5 .DW $7230       ;left paddle portion 5: down 2
;Beginning of memory pointers for the right paddle
pdlrloc .DW $7190       ;right paddle portion 1: up 2
pdlrlc2 .DW $71b8       ;right paddle portion 2: up 1
pdlrlc3 .DW $71e0       ;right paddle portion 3: center
pdlrlc4 .DW $7208       ;right paddle portion 4: down 1
pdlrlc5 .DW $7230       ;right paddle portion 5: down 2
scorel  .DW $3030       ;This value will hold left's score 
scorer  .DW $3030       ;This value will hold right's score
win     .DW $0000       ;Win flag: 0 = in game, -1 = left win, 1 = right win
;End Game Messages
msg     .AZ "Thanks for playing PONG!" ;End-game message
endpt1  .AZ "Winner: Player " ;Winner message
endpt2  .AZ "Space to restart, or other key to quit" ;restart prompt
pausmsg .AZ "PAUSED"    ;Pause text for pause screen
;Game speed variables
ballspd .DW $00         ;Speed variable, handles game speed-up
pongcnt .DW $00         ;Counter for reflects, used in speed-up
;Score variables
scrlmt  .DW $0000       ;Holds score limit
rowptr  .DW png1-1      ;Pointer for direction screen string in ROM
pausloc .DW $73d1       ;Location of PAUSED word on screen
rpwr    .DW $01         ;Power hits for right player
lpwr    .DW $01         ;Power hits for left player
rpuse   .DW $00         ;Flag for right power hit: 1 for true, 0 for false
lpuse   .DW $00         ;Flag for left power hit: 1 for true, 0 for false
rploc   .DW $73e2       ;Location on screen of right power indicator
lploc   .DW $73c5       ;Location on screen of left power indicator

        .BS $0300-*     ;Skip to beginning of the program
        


;
;Beginning of game proper, starts with initialization
;
start   cld             ;Set binary mode
        jsr instprt     ;print instructions
init    ldx #0
        lda #space
        jsr clrscrn     ;clear screen
        ldx #0
        jsr initio      ;initialize io
        jsr irqinit     ;initialize interrupt
        ldx #0
        ldy #0
        jsr iniscrn     ;initialize the bounds
        ldx #0
        jsr initpdl     ;initialize the paddles
        jsr initscr     ;initializes score to 0
        ;jsr tstbnd     ;load test bounds for collision testing
        ldx #0
        jsr drwscr      ;draw score at bottom of screen
        jsr poll        ;poll for input to set score limit
        jsr iniball     ;initialize ball
        jsr loop        ;jump to game loop
        jsr prtmsg      ;print end game message
        jsr restart     ;sees if you would like to restart
        cmp #1          ;if A is 1, restart is yes
        beq init        ;restart
        brk             ;And stop

;
;Sees if player would like to restart        
;
restart lda tailptr     ;read the ACIA status
        cmp headptr     ;check pointers
        beq restart     ;if equal, buffer is empty, restart poll
        tax
        lda inbuff,x    ;get the character
        tay             ;put in y
        inc tailptr     ;increment offeset
        lda tailptr
        and #mask       ;clear high 3 bits to retain circular
        sta tailptr
        cpy #space      ;is input a space?
        beq yesrest     ;it is a restart
        lda #0          ;not a space, load 0
        rts
yesrest lda #1
        rts

;        
;Polls for an input to set score limit       
;
poll    lda tailptr     ;read the ACIA status
        cmp headptr     ;check pointers
        beq poll        ;if equal, buffer is empty, poll
        tax
        lda inbuff,x    ;get the character
        tay             ;put in y
        inc tailptr     ;increment offeset
        lda tailptr
        and #mask       ;clear high 3 bits to retain circular
        sta tailptr
        cpy #lup        ;is input a q?
        beq donpoll     ;if so, done with poll
        cpy #ldo        ;is input a z?
        beq donpols     ;if so, done with poll
        jmp poll        ;not a space, poll more
donpoll lda #two        ;long game
        sta scrlmt+1    ;store 2 to tens place of score limit
        lda #two
        sta scrlmt      ;store 2 to ones place of score limit
        rts
donpols lda #one        ;short game
        sta scrlmt+1    ;store 1 to tens place of score limit
        lda #one
        sta scrlmt      ;store 1 to ones place of score limit
        rts
        
;        
;Prints instructions to console        
;
instprt ldy #0          ;initialize index to 0
reset   ldx #80         ;load column number
inslp1  clc             ;clear carry
        lda rowptr      ;increment rowptr
        adc #1
        sta rowptr
        lda rowptr+1
        adc #0
        sta rowptr+1
        lda (rowptr),Y  ;load next character
        cmp #'#'
        beq indone      ;leave if sentinal value # reached
        cmp #0
        beq inout
        sta iobase      ;store character to console
        dex             ;decrement columns left
        jmp inslp1
inout   lda #space      ;load space character
outloop cpx #0          ;fill remaining line with zeroes
        beq reset       ;end of row, reset column number
        sta iobase
        dex
        jmp outloop
indone  rts

;
;Prints the end game message to the screen
;
prtmsg  ldy #8
        ldx #0
msg1lp  lda msg,X       ;get "Thanks..." message character
        cmp #0
        beq out1        ;done after string is over
        sta (row11),Y   ;store in center of screen
        iny
        inx
        jmp msg1lp
out1    ldy #12
        ldx #0
msg2lp  lda endpt1,X    ;get "Winner: " message character
        cmp #0
        beq out2        ;done after string is over
        sta (row12),Y   ;store below "Thanks..." message
        iny
        inx
        jmp msg2lp
out2    lda win         ;see who wins
        cmp #1
        beq rwins
lwins   lda #one        ;load ASCII 1
        jmp outw
rwins   lda #two        ;load ASCII 2
outw    sta (row12),Y   ;store character
        ldx #0
        ldy #1
msg3lp  lda endpt2,X    ;get "Space to..." message character
        cmp #0
        beq out3        ;done after string is over
        sta (row13),Y   ;store below "Winner:..." message
        iny
        inx
        jmp msg3lp
out3    rts

;
;Clears the screen
;
clrscrn sta home,X      ;Start at 4 blocks on the screen, clearing all spaces.
        sta home+$100,x 
        sta home+$200,x 
        sta home+$2e8,x 
        inx
        bne clrscrn
        rts             ;end

;
;Sets bounds of screen and draws score labels
;
iniscrn lda #twall      ;fill top boundary with twall character
toplp   sta (row00),Y
        iny
        cpy #40
        bne toplp       ;loop until top row filled
        ldy #0
        lda #bwall      ;fill bottom boundary with bwall character
botlp   sta (row23),Y
        iny
        cpy #40
        bne botlp       ;loop until bottom row filled
        lda #colon      ;load colon for both sides
        ldy #2
        sta (row24),Y
        ldy #37
        sta (row24),Y
        lda #one        ;ASCII 1 for player 1
        ldy #3
        sta (row24),Y
        lda #two        ;ASCII 2 for player 2
        ldy #36
        sta (row24),Y
        rts

;
;Sets the default location for the paddles
;
initpdl lda #space      ;load space to erase left paddle
        pha
        jsr drawlp      ;erase left paddle
        lda #$14
        sta pdllpos     ;restore left paddle position
        lda #$90
        sta pdllloc     ;restore left paddle portion 1
        lda #$71
        sta pdllloc+1
        lda #$b8
        sta pdlllc2     ;restore left paddle portion 2
        lda #$71
        sta pdlllc2+1
        lda #$e0
        sta pdlllc3     ;restore left paddle portion 3
        lda #$71
        sta pdlllc3+1
        lda #$08
        sta pdlllc4     ;restore left paddle portion 4
        lda #$72
        sta pdlllc4+1
        lda #$30
        sta pdlllc5     ;restore left paddle portion 5
        lda #$72
        sta pdlllc5+1
        lda #lwall      ;load left wall to redraw left paddle
        pha
        jsr drawlp      ;redraw left paddle
        lda #space      ;load space to erase right paddle
        pha
        jsr drawrp      ;erase right paddle
        lda #$14
        sta pdlrpos     ;restore right paddle position
        lda #$90       
        sta pdlrloc     ;restore right paddle portion 1
        lda #$71
        sta pdlrloc+1
        lda #$b8
        sta pdlrlc2     ;restore right paddle portion 2
        lda #$71
        sta pdlrlc2+1
        lda #$e0
        sta pdlrlc3     ;restore right paddle portion 3
        lda #$71
        sta pdlrlc3+1
        lda #$08
        sta pdlrlc4     ;restore right paddle portion 4
        lda #$72
        sta pdlrlc4+1
        lda #$30
        sta pdlrlc5     ;restore right paddle portion 5
        lda #$72
        sta pdlrlc5+1
        lda #rwall      ;load right wall to redraw right paddle
        pha
        jsr drawrp      ;redraw right paddle
        lda #$1
        sta rpwr
        sta lpwr        ;reset power hits for each player
        lda #block      ;load power indicator
        ldy #0
        sta (rploc),Y
        sta (lploc),Y   ;draw power indicators
pdldn   rts
 
; 
;Sets test boundaries at left and right extremes of screen
;
tstbnd  lda #lwall
        ldy #0          ;Set 0 to allow for all first cells to be filled
redo    sta (row00),Y
        sta (row01),Y
        sta (row02),Y
        sta (row03),Y
        sta (row04),Y
        sta (row05),Y   ;Fills top half of screen side with what i in A
        sta (row06),Y
        sta (row07),Y
        sta (row08),Y
        sta (row09),Y
        sta (row10),Y
        sta (row11),Y   ;All of these rows fill column Y with what is in A
        sta (row12),Y
        sta (row13),Y
        sta (row14),Y
        sta (row15),Y
        sta (row16),Y
        sta (row17),Y   ;Fills bottom half of screen side with what is in A
        sta (row18),Y
        sta (row19),Y
        sta (row20),Y
        sta (row21),Y
        sta (row22),Y
        sta (row23),Y
        sta (row24),Y
        cpy #39
        beq done
        ldy #39         ;set to 39 to allow for all last cells to be filled
        lda #rwall
        jmp redo
done    rts

;
;Initializes ball location and direction vectors
;
iniball ldx #$18        ;load generic vertical location and save
        stx vertpos
        lda $27,X
        sta balloc      ;save generic ball location
        lda $28,X
        sta balloc+1
        lda #box
        ldy #$14        ;load generic horizontal position and save
        sty horipos
        sta (balloc),Y  ;draw ball
        ldy #$0
        sty vertdir     ;reset vertdir to 0
        lda #spd1
        sta ballspd     ;reset ball speed to spd1 = $40
        lda #0
        sta pongcnt     ;reset reflect count to 0
        rts
 
; 
;Initializes score to 00 for both players 
; 
initscr lda #zero
        sta scorel      ;initialize left score to 00
        sta scorel+1
        sta scorer      ;initialize right score to 00
        sta scorer+1
        lda #0
        sta win         ;initialize win to false
        rts

;        
;Draws the scores of the users at the bottom of the screen
;
drwscr  lda scorel+1    ;load MSB of left score
        ldy #0          
        sta (row24),Y   ;store at bottom left screen
        lda scorel      ;load LSB of left score
        iny
        sta (row24),Y   ;store at bottom left screen, right one
        lda scorer+1    ;load MSB of right score
        ldy #38         
        sta (row24),Y   ;store at bottom right screen, left one
        lda scorer      ;load LSB of right score
        iny
        sta (row24),Y   ;store at bottom right screen
        rts

;        
;The main game loop
;
loop    nop             ;holder op for later inputs, remove later
getchar lda tailptr     ;read the ACIA status
        cmp headptr     ;check pointers
        beq empty       ;if equal, buffer is empty, continue with program
        tax
        lda inbuff,x    ;get the character
        tay
        jsr mvpdl       ;move the paddle
        inc tailptr     ;increment offeset
        lda tailptr
        and #mask       ;clear high 3 bits to retain circular
        sta tailptr
        cpy #space      ;is y a pause character
        bne getchar     ;not pause, loop
        jsr pause       ;character is space, pause
        jmp getchar
empty   jsr movball
        jsr cpscore     ;jump to compare score
        lda win         ;load win flag
        cmp #$0         ;win = 0 iff neither side has won
        bne gmovr       ;if not equal to 0, someone won, so jump to game over
        jsr delay
        jmp loop
gmovr   rts



;;;;;;;;;Helper Subroutines;;;;;;;;;

;
;Pauses the game
;
pause   ldx #0
        ldy #0          ;load y with beginning of location
pmsglp  lda pausmsg,X   ;get character in word PAUSED
        cmp #0
        beq pauselp     ;string over, go to loop
        sta (pausloc),Y ;store character on screen
        inx
        iny
        jmp pmsglp
pauselp lda tailptr     ;read the ACIA status
        cmp headptr     ;check pointers
        beq pauselp     ;if equal, buffer is empty, loop to get next character
        tax
        lda inbuff,x    ;get the character
        tay
        inc tailptr     ;increment offeset
        lda tailptr
        and #mask       ;clear high 3 bits to retain circular
        sta tailptr
        cpy #space      ;is y a pause character
        bne pauselp     ;not space, loop
        lda #space      ;load A with space to overwrite PAUSED
        ldy #0
unplp   sta (pausloc),Y ;overwrite PAUSED
        iny
        cpy #7
        bne unplp       ;not done fixing, loop back
        rts

;        
;Subroutine for delaying the computer to slow down output
;
delay   ldy ballspd     ;count down from ff^ball speed (ff^ao - ff^40)
count2  ldx #$ff
count1  dex
        cpx #0
        bne count1      ;loop for X
        dey
        cpy #0
        bne count2      ;loop for Y
        rts

;
;Print the character to the console
;
prtchar sty iobase      ;save to home
        rts
        
;
;Logic to compare scores to see if either side has won
;
cpscore lda scorel+1    ;load tens place score for left player
        cmp scrlmt+1    ;compare to tens place score limit
        beq lscrtwo     ;left score >= 20 or >=10, check if limit
        lda scorer+1    ;load tens place score for right player
        cmp scrlmt+1    ;compare to tens place score limit
        beq rscrtwo     ;right score >= 20 or >=10, check if limit
        rts             ;neither score high enough, return
lscrtwo lda scorel      ;load ones place score for left player
        cmp scrlmt      ;compare to ones place score limit
        bne flsalrm     ;not at limit yet
        lda #-1         ;load -1 for left player win
        sta win
        rts
rscrtwo lda scorer      ;load ones place score for righ tplayer
        cmp scrlmt      ;compare to ones place score limit
        bne flsalrm     ;not at limit yet
        lda #1          ;load 1 for left player win
        sta win
        rts
flsalrm rts

        
        
;;;;;;;;;Paddle Movement;;;;;;;;;

;  
;Moves the paddle based on input char in accumulator
;
mvpdl   cpy #lup        ;character moves up left paddle
        beq lpup
        cpy #ldo        ;character moves down left paddle
        beq lpdo
        jmp ckright     ;jump to check right
lpup    ldx pdllpos
        cpx #2
        beq chrinv1     ;paddle too far up, can't move paddle
        lda #space
        pha
        jsr drawlp      ;clear left paddle
        ldx pdllpos
        dex
        dex
        stx pdllpos     ;decrement paddle pos twice per segment
        lda $27,X       ;store location to left paddle locations
        sta pdllloc     ;decrement left paddle segment 1
        lda $28,X
        sta pdllloc+1
        lda $29,X
        sta pdlllc2     ;decrement left paddle segment 2
        lda $2a,X
        sta pdlllc2+1
        lda $2b,X
        sta pdlllc3     ;decrement left paddle segment 3
        lda $2c,X
        sta pdlllc3+1
        lda $2d,X
        sta pdlllc4     ;decrement left paddle segment 4
        lda $2e,X
        sta pdlllc4+1
        lda $2f,X
        sta pdlllc5     ;decrement left paddle segment 5
        lda $30,X
        sta pdlllc5+1
        lda #lwall
        pha
        jsr drawlp      ;draw new left paddle
chrinv1 rts
lpdo    ldx pdllpos
        cpx #36
        beq chrinv2     ;paddle too far down, can't move paddle
        lda #space
        pha
        jsr drawlp      ;clear left paddle
        ldx pdllpos
        inx
        inx
        stx pdllpos     ;increment paddle pos twice per segment
        lda $27,X       ;store location to left paddle locations
        sta pdllloc     ;increments segment 1
        lda $28,X
        sta pdllloc+1
        lda $29,X
        sta pdlllc2     ;increment left paddle segment 2
        lda $2a,X
        sta pdlllc2+1
        lda $2b,X
        sta pdlllc3     ;increment left paddle segment 3
        lda $2c,X
        sta pdlllc3+1
        lda $2d,X
        sta pdlllc4     ;increment left paddle segment 4
        lda $2e,X
        sta pdlllc4+1
        lda $2f,X
        sta pdlllc5     ;increment left paddle segment 5
        lda $30,X
        sta pdlllc5+1
        lda #lwall
        pha
        jsr drawlp      ;draw new left paddle
chrinv2 rts
ckright cpy #rup        ;character moves up right paddle
        beq rpup
        cpy #rdo        ;character moves down right paddle
        beq rpdo
        jmp ckpwr       ;check if character was power flag
rpup    ldx pdlrpos
        cpx #2
        beq chrinv3     ;paddle too far up, can't move paddle
        lda #space
        pha
        jsr drawrp      ;clear right paddle
        ldx pdlrpos
        dex
        dex
        stx pdlrpos     ;decrement paddle pos twice per segment
        lda $27,X       ;store location to right paddle locations
        sta pdlrloc     ;decrement right paddle segment 1
        lda $28,X
        sta pdlrloc+1
        lda $29,X
        sta pdlrlc2     ;decrement right paddle segment 2
        lda $2a,X
        sta pdlrlc2+1
        lda $2b,X
        sta pdlrlc3     ;decrement right paddle segment 3
        lda $2c,X
        sta pdlrlc3+1
        lda $2d,X
        sta pdlrlc4     ;decrement right paddle segment 4
        lda $2e,X
        sta pdlrlc4+1
        lda $2f,X
        sta pdlrlc5     ;decrement right paddle segment 5
        lda $30,X
        sta pdlrlc5+1
        lda #rwall
        pha
        jsr drawrp      ;draw new right paddle
chrinv3 rts
rpdo    ldx pdlrpos
        cpx #36
        beq chrinv4     ;paddle too far down, can't move paddle
        lda #space
        pha
        jsr drawrp      ;clear right paddle
        ldx pdlrpos
        inx
        inx
        stx pdlrpos     ;increment paddle pos twice per segment
        lda $27,X       ;store location to right paddle locations
        sta pdlrloc     ;increment right paddle segment 1
        lda $28,X
        sta pdlrloc+1
        lda $29,X
        sta pdlrlc2     ;increment right paddle segment 2
        lda $2a,X
        sta pdlrlc2+1
        lda $2b,X
        sta pdlrlc3     ;increment right paddle segment 3
        lda $2c,X
        sta pdlrlc3+1
        lda $2d,X
        sta pdlrlc4     ;increment right paddle segment 4
        lda $2e,X
        sta pdlrlc4+1
        lda $2f,X
        sta pdlrlc5     ;increment right paddle segment 5
        lda $30,X
        sta pdlrlc5+1
        lda #rwall
        pha
        jsr drawrp      ;draw new right paddle
chrinv4 rts
ckpwr   cpy #rp         ;key was right power hit
        beq rightp
        cpy #lp         ;key was left power hit
        beq leftp
        rts             ;not valid key, return
rightp  lda #1
        sta rpuse       ;set flag for right power hit
        rts
leftp   lda #1
        sta lpuse       ;set flag for left power hit
        rts
;
;Logic for drawing left paddles, overwrite char stored in stack
;This can be used for clearing or drawing
;
drawlp  pla             ;get return address
        sta .oetadr
        pla
        sta .oetadr+1
        pla
        ldy #0          ;load leftmost column index
        sta (pdllloc),Y ;overwrite left paddle positions with char in A
        sta (pdlllc2),Y
        sta (pdlllc3),Y
        sta (pdlllc4),Y
        sta (pdlllc5),Y
        lda .oetadr+1   ;restore return address
        pha
        lda .oetadr
        pha
        rts
.oetadr .BS 2
.otradr .BS 1
 
; 
;Logic for drawing right paddles, overwrite char stored in stack
;This can be used for clearing or drawing
;
drawrp  pla             ;get return address
        sta .tetadr
        pla
        sta .tetadr+1
        pla
        ldy #39         ;load rightmost column index
        sta (pdlrloc),Y ;overwrite right paddle positions with char in A
        sta (pdlrlc2),Y
        sta (pdlrlc3),Y
        sta (pdlrlc4),Y
        sta (pdlrlc5),Y
        lda .tetadr+1   ;restore return address
        pha
        lda .tetadr
        pha
        rts
.tetadr .BS 2
.ttradr .BS 1



;;;;;;;;;Ball Movement;;;;;;;;;
        
;        
;Moves the ball, vertically and then horizontally, can be changed
;
movball jsr mvblhor     ;move ball horizontally
        ldy horipos     ;at leftmost extreme?
        cpy #$0
        beq offl
        cpy #$27        ;at rightmost extreme?
        beq offr
        ldy vertdir
        cpy #$0         ;does ball need to go vertical?
        beq donvrt
        jsr mvblvrt     ;not at extreme, move ball vertically
donvrt  rts
offl    lda #$20
        ldy horipos     ;reset ball location with space
        sta (balloc),Y
        jsr inscrr      ;increment right score
        jsr drwscr      ;update score
        jsr iniball     ;reinitialize ball
        jsr initpdl     ;reinitialize paddles
        rts
offr    lda #$20
        ldy horipos     ;reset ball location with space
        sta (balloc),Y
        jsr inscrl      ;increment left score
        jsr drwscr      ;update score
        jsr iniball     ;reinitialize ball
        jsr initpdl     ;reinitialize paddles
        rts
        
;
;Logic for incrementing score right
;
inscrr  ldx scorer      ;load right score
        cpx #nine       ;is score ASCII 9?
        beq roundr
        inx
        stx scorer      ;first place not ASCII 9, so increment it
        rts
roundr  inc scorer+1    ;increment tens place
        ldx #zero       ;reset ones place
        stx scorer
        rts
        
;
;Logic for incrementing score left
;
inscrl  ldx scorel      ;load right score
        cpx #nine       ;is score ASCII 9?
        beq roundl
        inx
        stx scorel      ;first place not ASCII 9, so increment it
        rts
roundl  inc scorel+1    ;increment tens place
        ldx #zero       ;reset ones place
        stx scorel
        rts

;        
;Moves the ball vertically, rebounding if necessary
;
mvblvrt ldy horipos
        ldx vertdir     ;handle vertical direction first
        stx balltmp
        ldx balltmp
        cpx #0
        bpl down        ;value > 0, so going down
up      ldy vertpos
        dey
        dey             ;increment y by two to check next memory cell
        cpy #$2         
        bmi fulabv      ;above is out of memory range, branch to fix
        lda #space
        ldy horipos
        sta (balloc),Y  ;reset space to empty
        ldx vertpos
        dex
        dex
        stx vertpos     ;decrement X by two
        lda $27,X
        sta balloc
        lda $28,X
        sta balloc+1
        lda #box
        sta (balloc),Y  ;store location and new tmp values
        ldx balltmp
        inx
        stx balltmp
        cpx #0
        beq dnmov
        jmp up
fulabv  clc
        lda #0          ;subtract the value of balltmp from 0 to get positive
        sbc balltmp
        clc
        adc #1
        sta balltmp
        lda #0
        clc
        sbc vertdir
        clc
        adc #1
        clc
        sta vertdir
        jmp mvblvrt
down    ldy vertpos
        iny
        iny             ;increment y by two to check next memory cell
        cpy #$2e         
        bpl fulbel      ;below is out of memory range, branch to fix
        lda #space
        ldy horipos
        sta (balloc),Y  ;reset space to empty
        ldx vertpos
        inx
        inx
        stx vertpos     ;increment X by two
        lda $27,X
        sta balloc
        lda $28,X
        sta balloc+1
        lda #box
        sta (balloc),Y  ;store location and new tmp values
        ldx balltmp
        dex
        stx balltmp
        cpx #0
        beq dnmov
        jmp down
fulbel  clc
        lda #0          ;subtract the value of baltmp from 0 to get negative
        sbc balltmp
        clc
        adc #1
        clc
        sta balltmp
        lda #0
        sbc vertdir
        clc
        adc #1
        clc
        sta vertdir
        jmp mvblvrt
dnmov   rts

;
;Moves the ball horizontally by 1, checking for rebound or off-screen instance
;
mvblhor ldy horipos
        sty postmp
        ldy postmp
        ldx horidir
        stx balltmp
        ldx balltmp
        cpx #0
        bpl right       ;horidir > 0, so going right
left    dey             ;decrement y to check one left
        lda (balloc),Y
        cmp #lwall      ;if box, go to bxleft
        beq bxleft
        iny             ;put y back to original spot
        lda #space
        sta (balloc),Y  ;overwrite with new space
        lda #box
        dey
        sta (balloc),Y  ;redraw box
        sty horipos     ;save new horizontal position
        jmp finhori
bxleft  ldx #1
        stx horidir
        iny             ;increment y to avoid confrontation loop
        sty postmp
        ldy #0          ;set power flag to 0
        lda lpwr        ;see if can use power move, jump if can't
        cmp #0
        beq nopl
        lda lpuse       ;see if have uses left
        cmp #0
        beq nopl
        dec lpwr        ;decrement lpwr
        dec lpuse       ;reset lpuse
        lda #space
        sta (lploc),Y   ;erase left power indicator
        ldy #1          ;load true flag for power hit
nopl    jsr updtspd
        jsr lychng      ;branch to change y on left paddle
        ldy postmp
        jmp right
right   iny             ;increment y to check one right
        lda (balloc),Y
        cmp #rwall      ;if box, go to bxright
        beq bxright
        dey             ;put y back to original spot
        lda #space
        sta (balloc),Y  ;overwrite with space
        lda #box
        iny
        sta (balloc),Y  ;redraw box
        sty horipos     ;save new horizontal position
        jmp finhori
bxright ldx #-1
        stx horidir
        dey             ;decrement y to avoid confrontation loop
        sty postmp
        ldy #0          ;set power flag to 0
        lda rpwr        ;see if can use power move, jump if can't
        cmp #0
        beq nopr
        lda rpuse       ;see if have uses left
        cmp #0
        beq nopr
        dec rpwr        ;decrement lpwr
        dec rpuse       ;reset rpuse
        lda #space
        sta (rploc),Y   ;erase right power indicator
        ldy #1          ;load true flag for power hit
nopr    jsr updtspd
        jsr rychng      ;branch to change y on righ tpaddle
        ldy postmp
        jmp left
finhori nop
        rts
        
;
;Changes vertdir for ball on left paddle hit
;
lychng  lda balloc
        cmp pdllloc     ;compare to topmost left paddle mem loc
        beq sbl2
        cmp pdlllc2     ;compare to next to left topmost
        beq sbl1
        cmp pdlllc4     ;compare to next to left bottommost
        beq adl1
        cmp pdlllc5     ;compare to bottommost left
        beq adl2
        rts             ;hit center, rts
adl2    ldy vertdir
        iny             ;increment vertdir
        iny
        jmp admore3     ;check if more than 3
adl1    ldy vertdir
        iny             ;increment vertdir
        jmp admore3     ;check if more than 3
admore3 cpy #3          ;compare vertdir to 3
        bpl adrest      ;vertdir > 3, reset to 3
        sty vertdir     ;vertdir <= 3, store to vertdir, rts
        rts
adrest  ldy #3
        sty vertdir     ;reset vertdir to 3
        rts
sbl1    ldy vertdir
        dey             ;decrement vertdir
        jmp sblesn3     ;check if less than -3
sbl2    ldy vertdir
        dey             ;decrement vertdir
        dey
        jmp sblesn3     ;check if less than -3
sblesn3 cpy #-3         ;compare virtdir to -3
        bmi sbrest      ;vertdir < -3, reset to -3
        sty vertdir     ;vertdir >= -3, store to vertdir, rts
        rts
sbrest  ldy #-3
        sty vertdir     ;reset vertdir to -3
        rts

;
;Changes vertdir for ball on right paddle hit
;
rychng  lda balloc
        cmp pdlrloc     ;compare to topmost right paddle mem loc
        beq sbr2
        cmp pdlrlc2     ;compare to next to right topmost
        beq sbr1
        cmp pdlrlc4     ;compare to next to right bottommost
        beq adr1
        cmp pdlrlc5     ;compare to bottommost right
        beq adr2
        rts             ;hit center, rts
adr2    ldy vertdir
        iny             ;increment vertdir
        iny
        jmp adrmre3     ;check if more than 3
adr1    ldy vertdir
        iny             ;increment vertdir
        jmp adrmre3     ;check if more than 3
adrmre3 cpy #3          ;compare vertdir to 3
        bpl adrrest     ;vertdir > 3, reset to 3
        sty vertdir     ;vertdir <= 3, store to vertdir, rts
        rts
adrrest ldy #3
        sty vertdir     ;reset vertdir to 3
        rts
sbr1    ldy vertdir
        dey             ;decrement vertdir
        jmp sbrlsn3     ;check if less than -3
sbr2    ldy vertdir
        dey             ;decrement vertdir
        dey
        jmp sbrlsn3     ;check if less than -3
sbrlsn3 cpy #-3         ;compare virtdir to -3
        bmi sbrrest     ;vertdir < -3, reset to -3
        sty vertdir     ;vertdir >= -3, store to vertdir, rts
        rts
sbrrest ldy #-3
        sty vertdir     ;reset vertdir to -3
        rts
        
;
;Updates speed based on reflect count
;
updtspd lda pongcnt     ;initial speed: $40
        cmp #spdpt3     ;compare if max speed is reached
        beq maxspd
        clc
        adc #1          ;not max, increment pongcnt
        sta pongcnt
        cpy #1          ;see if power move
        bne nopwr       ;if 0, no power hit
        clc
        adc #$f         ;jump to power speed
nopwr   cmp #spdpt1
        beq speed2      ;reflect = 5?
        bpl speed2      ;reflect > 5?
        lda #spd1
        sta ballspd
        rts
speed2  cmp #spdpt2
        beq speed3      ;reflect = 10?
        bpl speed3      ;reflect > 10?
        lda #spd2
        sta ballspd
        rts
speed3  cmp #spdpt3
        beq speed4      ;reflect = 15?
        bpl sspeed      ;reflect > 15? SUPER SPEED
        lda #spd3
        sta ballspd
        rts
speed4  lda #spd4
        sta ballspd
        rts
sspeed  lda #spdp       ;max speed > spd4
        sta ballspd
maxspd  rts

        
;;;;;;;;;ROM;;;;;;;;;
;Splash text array
png1    .AZ " _______  _______  __    _  _______         _______  _______  _     _ "
png2    .AZ "|       ||       ||  |  | ||       |       |       ||       || | _ | |"
png3    .AZ "|    _  ||   _   ||   |_| ||    ___|  ___  |    _  ||   _   || || || |"
png4    .AZ "|   |_| ||  | |  ||       ||   | __  |   | |   |_| ||  | |  ||       |"
png5    .AZ "|    ___||  |_|  ||  _    ||   ||  | |___| |    ___||  |_|  ||       |"
png6    .AZ "|   |    |       || | |   ||   |_| |       |   |    |       ||   _   |"
png7    .AZ "|___|    |_______||_|  |__||_______|       |___|    |_______||__| |__|"

;Control text array
enter1  .AZ " "         ;a single space will create an empty line
contrl1 .AZ "Authors: Zachary Hughes, Ben Mumaw."   ;Authors
contrl2 .AZ "Controls:" ;Controls
contrl3 .AZ "           UP     DOWN     PAUSE     POWER"
contrl4 .AZ "Player 1:  Q      Z        space     A"
contrl5 .AZ "Player 2:  ]      /        space     '"
enter2  .AZ " "         ;same as the first enter, this creates an empty line
contrl6 .AZ "Gameplay:" ;Gameplay instructions
contrl7 .AZ "The pong ball changes direction depending on where it hits each player's paddle."
contr18 .AZ "Hitting the end of the paddle with the ball will cause the ball to move more in"
contr19 .AZ "that direction. Hitting the center of the paddle will preserve the ball's"
contr20 .AZ "vertical direction. Press your power key for a burst of speed, 1/round!"
contr21 .AZ "If you miss, you lose the round. The ball will then reset."
contr22 .AZ "On reset, the winner gets a point and the ball moves toward the loser."
contr23 .AZ "Power status is located next to scores. It, too, resets after each round."
contr24 .AZ "The longer you go without a point, the faster the ball will travel."
contr25 .AZ "When a player scores enough points, the game is over."
contr26 .AZ "When ready, select a point limit: Q = 22, Z = 11.#"
;# is the sentinal value for the reader, end after this line

        
;;;;;;;;;IO POINT;;;;;;;;;
;
;Interrupt handler
;
irq     pha             ;Save registers.
        txa
        pha
        tya
        pha
        lda headptr     ;Get buffer head pointer.
        tax             ;Set index register value.
        sec
        sbc tailptr
        and #$1f        ;Make circular.
        cmp #$1f        ;If headptr - tailptr = 31, buffer is full.
        beq out         ;Buffer is full. Can't do anything.
        lda iodata      ;Get the character from the keyboard.
        sta inbuff,x    ;Store it into the buffer.
        inx             ;Next buffer address.
        txa
        and #mask       ;Clear high 3 bits to make buffer circular.
        sta headptr
out     pla             ;Restore registers
        tay
        pla
        tax
        pla
        cli             ;Clear interrupt mask (un-disable)
        rti             ;Return from interrupt handler.
irvec  .DW irq

;
;Initiates the interrupt vector
;
irqinit lda irvec       ;NMI vector
        sta irv
        lda irvec+1
        sta irv+1
        lda #start      ;reset vector
        sta irv+2
        lda #start+1
        sta irv+3
        lda irvec       ;interrupt vector
        sta irv+4  
        lda irvec+1
        sta irv+5
        rts

;        
;Initiates IO
;
initio  lda #%00001001  ;No parity bit (000), no echo (0), disable Tx
        sta iocmd       ;int & !RTS low (10), enable Rx int (0), 
                        ;!DTR low (1)
        lda #%00011110  ;1 stop bit (0), 8-bit word (00),
        sta ioctrl      ;Use bitrate generator (1), 9600 bps (1110)
        rts


    .En                 ;End of program


    ;This section will not be assembled
