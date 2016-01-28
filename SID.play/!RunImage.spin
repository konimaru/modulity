''
''        Author: Marko Lukat
'' Last modified: 2016/01/28
''       Version: 0.10
''
CON
  _clkmode = client#_clkmode
  _xinfreq = client#_xinfreq
  
CON
  AUDIO_L  = client#AUDIO_L
  AUDIO_R  = client#AUDIO_R

OBJ
  client: "core.con.client.badge"
  stream: "core.aux.stream"
  sidcog: "SIDcog"
    core: "6502"
    util: "umath.mod"
  
VAR
  long  mbox[core#res_m], data[stream#res_m]
  long  heap[32]
  
PUB main : t | delta

  sidcog.start(AUDIO_R, AUDIO_L)
  core.init(-1, @mbox{0})
  repeat while mbox{0} < 0                              ' startup complete

  processSID(string("built-in:2"))

  delta := util.div(clkfreq >> 24, clkfreq << 8, trunc(sidcog#C64_CLOCK_FREQ))

  t := cnt
  repeat
    exec(@s_play)
    waitcnt(t += (delta * word[$7D04]) >> 8)
    sidcog.updateRegisters($7F00)

PRI processSID(name) : load | addr, size, pcnt, inst

  inst := @data{0}
  size := stream.attach(inst, name)                     ' open resource

  stream.bget(inst, @heap{0}, $16)                      ' read minimal header
  load := swap(heap.word[3])                            ' payload offset
  stream.bget(inst, @heap.byte[$16], load - $16)        ' remainder

  size -= load                                          ' payload length

  ifnot load := swap(heap.word[4])
    stream.bget(inst, @load, 2)                         ' little endian load address
    size -= 2

  s_init[3] := heap.byte[11]
  s_init[4] := heap.byte[10]                            ' swap(data.word[5])

  s_play[1] := heap.byte[13]                            
  s_play[2] := heap.byte[12]                            ' swap(data.word[6])

  ifnot heap.word[6]
    abort                                               ' played through interrupt handler

  pcnt := (load.byte{0} + size + 255) & $FF00           ' covered pages (in bytes)
  addr := $7D00 - pcnt + load.byte{0}                   ' top pages are used for SID/CIA mapping

  stream.bget(inst, addr, size)                         ' transfer payload

  core.bmap(load.byte[1], addr.byte[1], pcnt >> 8)      ' map payload
  core.pmap($D4, $7F)                                   ' map SID registers, shared with stack
  core.pmap($DC, $7D)                                   ' map CIA registers (#1)

' core.pmap($01, $7F)                                   ' map stack (core enforces $7F), optional
' core.pmap($00, $7E)                                   ' map zpage (core enforces $7E), optional
  
  word[$7D04] := lookupz(heap.byte[$15] & 1 : FREQ_VBL, FREQ_CIA)

  exec(@s_init)                                         ' initialise player

CON
  PAL      = trunc(sidcog#C64_CLOCK_FREQ == sidcog#PAL)
  NTSC     = trunc(sidcog#C64_CLOCK_FREQ == sidcog#NTSC)

  FREQ_CIA = round(sidcog#C64_CLOCK_FREQ / 60.0)
  FREQ_VBL = round(sidcog#C64_CLOCK_FREQ / 50.0) * PAL + FREQ_CIA * NTSC

  ESC      = 27
  
PRI exec(locn)

  mbox{0} := NEGX|locn
  repeat
  while mbox{0} < 0

PRI swap(value)

  return value.byte{0} << 8 | value.byte[1]
  
DAT
s_init  byte    $A9, $00                                ' lda #0
        byte    $20, word $0000                         ' jsr init
        byte    ESC                                     ' invalid
s_play  byte    $20, word $0000                         ' jsr play
        byte    ESC                                     ' invalid

DAT
{{

 TERMS OF USE: MIT License

 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 associated documentation files (the "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
 following conditions:

 The above copyright notice and this permission notice shall be included in all copies or substantial
 portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
 LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

}}
DAT