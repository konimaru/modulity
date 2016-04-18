''
''        Author: Marko Lukat
'' Last modified: 2016/04/18
''       Version: 0.14
''
'' 20160418: initial release
''
CON
  _clkmode = client#_clkmode
  _xinfreq = client#_xinfreq

CON
  AUDIO_L  = client#AUDIO_L
  AUDIO_R  = client#AUDIO_R

  SCL      = client#SCL
  SDA      = client#SDA

OBJ
  SSD1306: "core.con.ssd1306"
   client: "core.con.client.badge"

   sidcog: "SIDcog"
     core: "6502"
     math: "umath.mod"

     draw: "coreDraw"
     plex: "badge.PLEX"
     util: "badge.UTIL"
     
VAR
  long  surface
  long  LEDs, pads

  long  xyzt, orientation, up, down
  long  stack[64]

  long  warm, selected
  
PUB selftest : n | now

  init

  now := cnt
  repeat
    repeat n from draw#res_x to -drwuro[SX]
      draw.blit(0, @drwuro, n, 0, 0, 0)
      repeat                           
      until draw.idle                  
      waitcnt(now += clkfreq/30)
      draw.cmdN(orientation, 2)
      draw.swap(0)

      updateLED

PRI updateLED : n                                       ' establish PAD/LED link(s)
  
  if pads & plex#PAD_P0                                 ' right side
    n |= plex#LED_B0
  if pads & plex#PAD_P1
    n |= plex#LED_B1
  if pads & plex#PAD_P2
    n |= plex#LED_B2     

  if pads & plex#PAD_P3                                 ' left side
    n |= plex#LED_B3
  if pads & plex#PAD_P4
    n |= plex#LED_B4
  if pads & plex#PAD_P5
    n |= plex#LED_B5     

  if pads & plex#PAD_P6                                 ' logo
    ifnot selected~~
      ifnot ++warm & 7
        ++warm                                          ' skip black
    n |= %001001*(warm & 7) << 8
  else
    selected := FALSE

  LEDs := n                                             ' final LED update

DAT                                                     ' display initialisation sequence
        byte    8
iseq    byte    SSD1306#SET_MEMORY_MODE, %111111_00     ' horizontal mode
        byte    SSD1306#SET_SEGMENT_REMAP|1             ' |
        byte    SSD1306#SET_COM_SCAN_DEC                ' rotate 180 deg
        byte    SSD1306#SET_PRECHARGE_PERIOD, $F1       ' dis/charge 1/15
        byte    SSD1306#SET_CHARGE_PUMP, %11_010100     ' no external Vcc

PRI init                                                ' driver/task initialisation

  plex.init(-1, @LEDs)                                  ' LED/PAD driver

  surface := draw.init($02_00_0000)                     ' drawing library and OLED driver

  draw.cmdN(@iseq, iseq[-1])                            ' finish setup
  draw.swap(0)                                          ' show initial screen
  draw.cmd1(SSD1306#DISPLAY_ON)                         ' display on

  SIDcog.start(AUDIO_R, AUDIO_L)                        ' 8580 softcore
  core.init(-1, @mbox{0})                               ' 6502 softcore

  util.init(SCL, SDA, @xyzt, $04040C08)                 ' accelerometer and EEPROM
  
' runtime support

  up.word[1]   := constant(SSD1306#SET_COM_SCAN_DEC << 8 | SSD1306#SET_SEGMENT_REMAP|1)
  up.word{0}   := @up.word[1]

  down.word[1] := constant(SSD1306#SET_COM_SCAN_INC << 8 | SSD1306#SET_SEGMENT_REMAP|0)
  down.word{0} := @down.word[1]

  orientation  := up                                    ' default orientation

' background tasks

  cognew(SID_task, @stack{0})                           ' SID player
  
CON
  #-3, FS, SX, SY                                       ' sprite header indices
  
DAT                                                     ' primary bitmap
        word    1920            ' frame size
        word    240, 64         ' width, height

drwuro  word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $FFFC, $FFFF, $03FF, $FFF8, $FF81, $001F, $0000, $803F, $3F3F, $F9F8, $01FF, $0FFE, $0000, $0000
        word    $0000, $FFFE, $FFFF, $03FF, $FFF8, $FF83, $003F, $0000, $803F, $3F3F, $F9F8, $03FF, $1FFF, $0000, $0000
        word    $0000, $FFFF, $FFFF, $03FF, $FFF8, $FF87, $007F, $0000, $803F, $3F3F, $F9F8, $87FF, $3FFF, $0000, $0000
        word    $8000, $FFFF, $FFFF, $03FF, $FFF8, $FF8F, $00FF, $0000, $803F, $3F3F, $F9F8, $CFFF, $7FFF, $0000, $0000
        word    $C000, $FFFF, $FFFF, $03FF, $FFF8, $FF8F, $00FF, $0000, $803F, $3F3F, $F9F8, $CFFF, $7FFF, $0000, $0000
        word    $E000, $FFFF, $FFFF, $03E7, $FFF8, $FF8F, $00FF, $0000, $843F, $3F3F, $F9F8, $CFFF, $7FFF, $0000, $0000
        word    $F000, $FFFF, $FFFF, $03E3, $C000, $000F, $00FC, $0000, $8E3F, $3F3F, $01F8, $0FC0, $7E00, $0000, $0000
        word    $F800, $FFFF, $FFFF, $03E1, $C000, $000F, $00FC, $0000, $9F3F, $3F3F, $01F8, $0FC0, $7E00, $0000, $0000
        word    $FC00, $FFFF, $FFFF, $03E0, $C000, $000F, $00FC, $0000, $9F3F, $3F3F, $01F8, $0FC0, $7E00, $0000, $0000
        word    $FE00, $FFFF, $7FFF, $03E4, $C1F8, $FF8F, $00FF, $0000, $9F3F, $3F3F, $F9F8, $CFFF, $7E0F, $0000, $0000
        word    $FF00, $FFFF, $3FFF, $03E6, $C1F8, $FF8F, $007F, $0000, $9F3F, $3F3F, $F9F8, $C7FF, $7E0F, $0000, $0000
        word    $FF80, $FFFF, $0FFF, $03E7, $C1F8, $FF8F, $003F, $0000, $9F3F, $3F3F, $F9F8, $C3FF, $7E0F, $0000, $0000
        word    $FFC0, $FFFF, $87FF, $03E7, $C1F8, $FF8F, $001F, $0000, $9F3F, $3F3F, $F9F8, $C1FF, $7E0F, $0000, $0000
        word    $FFC0, $FFFF, $C1FF, $03E7, $C1F8, $FF8F, $003F, $0000, $9F3F, $3F3F, $F9F8, $C3FF, $7E0F, $0000, $0000
        word    $FFC0, $FFFF, $E0FF, $03E7, $C1F8, $FF8F, $007F, $0000, $9F3F, $3F3F, $F9F8, $C7FF, $7E0F, $0000, $0000
        word    $FFC0, $FFFF, $C03F, $03E7, $C1F8, $FF8F, $00FF, $0000, $9F3F, $3F3F, $F9F8, $CFFF, $7E0F, $0000, $0000
        word    $FFC0, $FFFF, $C40F, $03E7, $C1F8, $1F8F, $00FC, $0000, $9F3F, $3F3F, $F9F8, $CFC1, $7E0F, $0000, $0000
        word    $FFC0, $FFFF, $C703, $03E7, $C1F8, $1F8F, $00FC, $0000, $9F3F, $3F3F, $F9F8, $CFC1, $7E0F, $0000, $0000
        word    $FFC0, $7FFF, $8FE0, $03E7, $FFF8, $1F8F, $FCFC, $0000, $FFFF, $FF3F, $F9FF, $CFC1, $7FFF, $0000, $0000
        word    $FFC0, $0FFF, $8FF0, $03E3, $FFF8, $1F8F, $FCFC, $0000, $FFFF, $FF3F, $F9FF, $CFC1, $7FFF, $0000, $0000
        word    $FFC0, $00FF, $9FF8, $03E3, $FFF8, $1F8F, $FCFC, $0000, $FFFF, $FF3F, $F9FF, $CFC1, $7FFF, $0000, $0000
        word    $FFC0, $C007, $1FF8, $03F3, $FFF8, $1F87, $FCFC, $0000, $F7FF, $FF1F, $F8FF, $8FC1, $3FFF, $0000, $0000
        word    $3FC0, $F800, $3FF9, $03F1, $FFF8, $1F83, $FCFC, $0000, $F3FF, $FF1F, $F87F, $0FC1, $1FFF, $0000, $0000
        word    $01C0, $FF00, $3FF1, $03F0, $FFF8, $1F81, $FCFC, $0000, $F1FF, $FF0F, $F83F, $0FC1, $0FFE, $0000, $0000
        word    $00C0, $FF3F, $7FF3, $03F8, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $F8C0, $FF3F, $7FF3, $03F8, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $F9C0, $FE3F, $FFE3, $03FC, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $F1C0, $FE3F, $7FE3, $03FC, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $F1C0, $FE7F, $3FE7, $03FE, $F3F8, $9F83, $1FFF, $E0FC, $FF07, $FF3F, $F9FF, $C1FF, $FFCF, $F07F, $03FF
        word    $E3C0, $FE7F, $0FC7, $03FF, $F3F8, $9F83, $3FFF, $E0FC, $FF87, $FF3F, $F9FF, $C3FF, $FFCF, $F87F, $03FF
        word    $E3C0, $FE7F, $87C7, $03FF, $F3F8, $9F87, $7FFF, $E0FC, $FFC7, $FF3F, $F9FF, $C7FF, $FFCF, $FC7F, $03FF
        word    $C7C0, $FC7F, $C38F, $03FF, $F3F8, $9F87, $FFFF, $E0FC, $FFE7, $FF3F, $F9FF, $CFFF, $FFCF, $FE7F, $03FF
        word    $8FC0, $FC7F, $E09F, $03FF, $F3F8, $9F8F, $FFFF, $E0FC, $FFE7, $FF3F, $F9FF, $CFFF, $FFCF, $FE7F, $03FF
        word    $0FC0, $FC7E, $F01F, $03FF, $F3F8, $9F8F, $FFFF, $E0FC, $FFE7, $FF3F, $F9FF, $CFFF, $FFCF, $FE7F, $03FF
        word    $3FC0, $FCFC, $FC0F, $03FF, $F3F8, $1F9F, $FC00, $E0FC, $07E7, $E000, $000F, $CFC0, $0FCF, $7E00, $0000
        word    $7FC0, $FCE0, $FF00, $03FF, $F3F8, $1F9F, $FC00, $E0FC, $07E7, $E000, $000F, $CFC0, $0FCF, $7E00, $0000
        word    $FFC0, $0001, $FFC0, $03FF, $F3F8, $1FBF, $FC00, $E0FC, $07E7, $E000, $000F, $CFC0, $0FCF, $7E00, $0000
        word    $FFC0, $0007, $FFF8, $03FF, $F3F8, $9FBF, $FC1F, $E0FC, $FFE7, $E007, $F80F, $CFFF, $FFCF, $FE0F, $007F
        word    $FFC0, $C07F, $FFFF, $03FF, $F3F8, $9FFF, $FC1F, $E0FC, $FFE7, $E00F, $F80F, $C7FF, $FFCF, $FE0F, $00FF
        word    $FFC0, $FFFF, $FFFF, $03FF, $F3F8, $9FFF, $FC1F, $E0FC, $FFE7, $E01F, $F80F, $C3FF, $FFCF, $FE0F, $01FF
        word    $FFC0, $FFFF, $FFFF, $01FF, $F3F8, $9FFF, $FC1F, $E0FC, $FFC7, $E03F, $F80F, $C1FF, $FFCF, $FC0F, $03FF
        word    $FFC0, $FFFF, $FFFF, $00FF, $F3F8, $9FFF, $FC1F, $E0FC, $FF87, $E03F, $F80F, $C3FF, $FFCF, $F80F, $03FF
        word    $FFC0, $FFFF, $FFFF, $007F, $F3F8, $9FFB, $FC1F, $E0FC, $FF07, $E03F, $F80F, $C7FF, $FFCF, $F00F, $03FF
        word    $FFC0, $FFFF, $FFFF, $003F, $F3F8, $9FFB, $FC1F, $E0FC, $0007, $E03F, $F80F, $CFFF, $0FCF, $0000, $03F0
        word    $FFC0, $FFFF, $FFFF, $001F, $F3F8, $9FF3, $FC1F, $E0FC, $0007, $E03F, $F80F, $CFC1, $0FCF, $0000, $03F0
        word    $FFC0, $FFFF, $FFFF, $000F, $F3F8, $9FF3, $FC1F, $E0FC, $0007, $E03F, $F80F, $CFC1, $0FCF, $0000, $03F0
        word    $FFC0, $FFFF, $FFFF, $0007, $F3F8, $9FE3, $FFFF, $FFFC, $FFE7, $E03F, $F80F, $CFC1, $FFCF, $FE7F, $03FF
        word    $FFC0, $FFFF, $FFFF, $0003, $F3F8, $9FE3, $FFFF, $FFFC, $FFE7, $E03F, $F80F, $CFC1, $FFCF, $FE7F, $03FF
        word    $FFC0, $FFFF, $FFFF, $0001, $F3F8, $9FC3, $FFFF, $FFFC, $FFE7, $E03F, $F80F, $CFC1, $FFCF, $FE7F, $03FF
        word    $FFC0, $FFFF, $FFFF, $0000, $F3F8, $9FC3, $7FFF, $FFFC, $FFE3, $E01F, $F80F, $CFC1, $FF8F, $FE7F, $01FF
        word    $FFC0, $FFFF, $7FFF, $0000, $F3F8, $9F83, $3FFF, $FFFC, $FFE1, $E00F, $F80F, $CFC1, $FF0F, $FE7F, $00FF
        word    $FFC0, $FFFF, $3FFF, $0000, $F3F8, $9F83, $1FFF, $FFFC, $FFE0, $E007, $F80F, $CFC1, $FE0F, $FE7F, $007F
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000
        word    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000

CON
  PAL      = trunc(sidcog#C64_CLOCK_FREQ == sidcog#PAL)
  NTSC     = trunc(sidcog#C64_CLOCK_FREQ == sidcog#NTSC)

  FREQ_CIA = round(sidcog#C64_CLOCK_FREQ / 60.0)
  FREQ_VBL = round(sidcog#C64_CLOCK_FREQ / 50.0) * PAL + FREQ_CIA * NTSC

  ESC      = 27
  
VAR
  long  mbox[core#res_m], heap[32], song[32]
  long  sidx
  
PRI SID_task : now | delta, cold                        ' audio background task

  repeat
  while mbox{0} < 0                                     ' startup complete

  delta := math.div(clkfreq >> 24, clkfreq << 8, trunc(sidcog#C64_CLOCK_FREQ))

  util.read(@song{0}, $00808000, 128)                   ' load first 32 entries
  if song{0}
    repeat                                                                      
      SID_load(song[sidx]|$8000)                        ' load song             
      cold := warm
                                                                                
      now := cnt                                                                
      repeat                                                                    
        SID_exec(@s_play)                               ' play song             
        waitcnt(now += (delta * word[$7D04]) >> 8)                              
        sidcog.updateRegisters($7F00)
      while cold == warm                   

      waitcnt(now += (delta * word[$7D04]) >> 8)        ' finish this cycle
      sidcog.resetRegisters                             ' then silence

      ifnot song[++sidx]                                ' next song available?
        sidx := 0                                       ' wrap
      
PRI SID_load(name) : load | addr, size, pcnt            ' stream loader

  size := name.word[1]                                  ' open resource

  name += util.read(@heap{0}, name, $16)                ' read minimal header
  load := SID_swap(heap.word[3])                        ' payload offset
  name += util.read(@heap.byte[$16], name, load - $16)  ' remainder

  size -= load                                          ' payload length

  ifnot load := SID_swap(heap.word[4])
    name += util.read(@load, name, 2)                   ' little endian load address
    size -= 2

  s_init[3] := heap.byte[11]
  s_init[4] := heap.byte[10]                            ' swap(data.word[5])

  ifnot heap.word[6]
    abort                                               ' played through interrupt handler

  pcnt := (load.byte{0} + size + 255) & $FF00           ' covered pages (in bytes)
  addr := $7D00 - pcnt + load.byte{0}                   ' top pages are used for SID/CIA mapping

 {name +=}util.read(addr, name, size)                   ' transfer payload

  core.bmap(load.byte[1], addr.byte[1], pcnt >> 8)      ' map payload
  core.pmap($D4, $7F)                                   ' map SID registers, shared with stack
  core.pmap($DC, $7D)                                   ' map CIA registers (#1)

' core.pmap($01, $7F)                                   ' map stack (core enforces $7F), optional
' core.pmap($00, $7E)                                   ' map zpage (core enforces $7E), optional

  bytefill($7F00, 0, 25)                                ' clear register bank
  
  word[$7D04] := lookupz(heap.byte[$15] & 1 : FREQ_VBL, FREQ_CIA)

  SID_exec(@s_init)                                     ' initialise player

  s_play[1] := heap.byte[13]    {override init}         
  s_play[2] := heap.byte[12]    {override init}         ' swap(data.word[6])

PRI SID_exec(locn)                                      ' 6502 connector

  mbox{0} := NEGX|locn
  repeat
  while mbox{0} < 0

PRI SID_swap(value)                                     ' endianness support

  return value.byte{0} << 8 | value.byte[1]
  
DAT                                                     ' player entry points

s_init  byte    $A9, $00                                ' lda #0
s_play  byte    $20, word $0000                         ' jsr init/play
        byte    ESC                                     ' invalid

DAT