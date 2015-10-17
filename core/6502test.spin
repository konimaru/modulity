''
''        Author: Marko Lukat
'' Last modified: 2015/10/17
''       Version: 0.4
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
' stream: "core.aux.stream"
  sidcog: "SIDcog"
    core: "6502"
  
VAR
  long  mbox[core#res_m]
  
PUB main | t, delta

  sidcog.start(AUDIO_R, AUDIO_L)
  core.init(-1, @mbox{0})

  processSID(@sid, @sid_end - @sid)
' processSID(@sid3, @sid3_end - @sid3)

  delta := clkfreq / 50

  repeat
  while mbox{0} < 0                                     ' startup complete

  usr(@s_init)
  
  t := cnt
  repeat
    usr(@s_play)
    waitcnt(t += delta)
    sidcog.updateRegisters($7F00)

PRI usr(locn)

  mbox{0} := NEGX|locn
  repeat
  while mbox{0} < 0

PRI processSID(addr, size) : load | base, pcnt

  base := swap(word[addr][3])                           ' payload offset
  size -= base                                          ' payload length
  base += addr                                          ' absolute

  ifnot load := swap(word[addr][4])
    load := word[base]                                  ' little endian load address
    base += 2                                           ' ... to be skipped
    size -= 2

  s_init[3] := byte[addr][11]
  s_init[4] := byte[addr][10]                           ' swap(word[addr][5])

  s_play[1] := byte[addr][13]                           
  s_play[2] := byte[addr][12]                           ' swap(word[addr][6])

  ifnot word[addr][6]
    abort                                               ' played through interrupt handler

  pcnt := (load.byte{0} + size + 255) & $FF00           ' covered pages (in bytes)
  addr := $7F00 - pcnt                                  ' top page is used for SID mapping

  bytemove(addr + load.byte{0}, base, size)             ' transfer payload
  core.bmap(load.byte[1], addr.byte[1], pcnt >> 8)      ' map payload
  core.pmap($D4, $7F)                                   ' map SID registers
  
PRI swap(value)

  return value.byte{0} << 8 | value.byte[1]
  
DAT     byte    $FF[256]
DAT
s_init  byte    $A9, $00                                ' lda #0
        byte    $20, word $0000                         ' jsr init
        byte    $00, $00                                ' brk #0
s_play  byte    $20, word $0000                         ' jsr play
        byte    $00, $00                                ' brk #0

DAT     long    'newtest2-7000.sid'
sid     file    "newtest2-7000.sid"
sid_end

DAT     long    'Magic_Disk_64_1991_06.sid'
sid3    file    "Magic_Disk_64_1991_06.sid"
sid3_end
        
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