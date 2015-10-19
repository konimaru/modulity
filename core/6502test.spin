''
''        Author: Marko Lukat
'' Last modified: 2015/10/19
''       Version: 0.6
''
'' ToDo: have a closer look into offsets $78/$79 (v2)
''
CON
  _clkmode = client#_clkmode
  _xinfreq = client#_xinfreq
  
CON
  AUDIO_L  = client#AUDIO_L
  AUDIO_R  = client#AUDIO_R

OBJ
  client: "core.con.client.demoboard"
  stream: "core.aux.stream"
  sidcog: "SIDcog"
    core: "6502"
  
VAR
  long  mbox[core#res_m], data[stream#res_m]
  long  heap[32]
  
PUB main : t | delta

  sidcog.start(AUDIO_R, AUDIO_L)
  core.init(-1, @mbox{0})
  repeat while mbox{0} < 0                              ' startup complete

  processSID(string("newtest2-7000.sid"))

  delta := clkfreq / 50

  t := cnt
  repeat
    exec(@s_play)
    waitcnt(t += delta)
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
  addr := $7F00 - pcnt + load.byte{0}                   ' top page is used for SID mapping

  stream.bget(inst, addr, size)                         ' transfer payload
  stream.detach(inst)                                   ' done
  
  core.bmap(load.byte[1], addr.byte[1], pcnt >> 8)      ' map payload
  core.pmap($D4, $7F)                                   ' map SID registers

  exec(@s_init)                                         ' initialise player
  
PRI exec(locn)

  mbox{0} := NEGX|locn
  repeat
  while mbox{0} < 0

PRI swap(value)

  return value.byte{0} << 8 | value.byte[1]
  
DAT     byte    $FF[256]
DAT
s_init  byte    $A9, $00                                ' lda #0
        byte    $20, word $0000                         ' jsr init
        byte    $00, $00                                ' brk #0
s_play  byte    $20, word $0000                         ' jsr play
        byte    $00, $00                                ' brk #0

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
