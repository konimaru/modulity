''
''        Author: Marko Lukat
'' Last modified: 2015/10/16
''       Version: 0.3
''
CON
  _clkmode = XTAL1|PLL16X
  _xinfreq = 5_000_000
  
CON
  TARGET = $7000
  ENTRY  = $7003
  
OBJ
  sidcog: "SIDcog"
    core: "6502"
  
VAR
  long  mbox[core#res_m]
  
PUB main | t, delta

  sidcog.start(27, 0)
  core.init(-1, @mbox{0})
  bytemove(TARGET, @sid, @sid_end-@sid)

  delta := clkfreq / 50

  core.pmap($D4, $7F)

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

DAT     byte    $FF[256]
DAT
s_init  byte    $A9, $00
        byte    $20, word TARGET
        byte    $00
        
s_play  byte    $20, word ENTRY
        byte    $00

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