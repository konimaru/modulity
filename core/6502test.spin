''
''        Author: Marko Lukat
'' Last modified: 2015/10/11
''       Version: 0.1
''
CON
  _clkmode = XTAL1|PLL16X
  _xinfreq = 5_000_000
  
CON
  TARGET = $7000
  ENTRY  = $7003
  
OBJ
  serial: "FullDuplexSerial"
  sidcog: "SIDcog"
    core: "6502"
  
VAR
  long  mbox[core#res_m]
  
PUB main : n | t, delta

  core.init(-1, @mbox{0})
  bytemove(TARGET, @sid, @sid_end-@sid)
  longfill($5400, 0, 8)
  sidcog.start(27, 0)

  serial.start(31, 30, %0000, 115200)
  waitcnt(clkfreq*3 + cnt)
  serial.tx(0)
  waitcnt(clkfreq + cnt)

  delta := clkfreq / 50

  repeat
  while mbox{0} < 0                                     ' startup complete

  usr(@s_init)
  
  t := cnt
  repeat
    usr(@s_play)
    waitcnt(t += delta)
    dumpline($5400, 25)
  while ++n < 9
'   sidcog.updateRegisters($5400)
  dump($7200)
  dump($0000)

PRI dump(base)

  serial.tx(13)
  repeat 16
    serial.hex(base, 4)
    serial.str(string(": "))
    base := dumpline(base, 16)
    
PRI dumpline(base, bcnt) : n

  repeat bcnt
    serial.hex(byte[base][n++], 2)
    serial.tx(32)
  serial.tx(13)

  n += base
  
PRI usr(locn)

  mbox{0} := NEGX|locn
  repeat
  while mbox{0} < 0

DAT     byte    $FF[256]
DAT
s_init  byte    $A9, $00
        byte    $20, word TARGET
        byte    $00
        
s_play  byte    $20, word ENTRY
w_end   byte    $00

DAT     long    'newtest2-7000.bin'
sid     file    "newtest2-7000.bin"
sid_end
        
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