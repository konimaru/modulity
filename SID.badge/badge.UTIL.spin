''
'' Parallax eBadge I2C high level device handler
''
''        Author: Marko Lukat
'' Last modified: 2016/02/21
''       Version: 0.8
''
'' acknowledgements
'' - MMA7660FC 3-Axis accelerometer interface, Copyright (c) 2015 Jon McPhalen
'' - I2C open-drain PASM driver 1.8od, Copyright (c) 2014 Chris Gadd
''
CON
  res_m         = T_END                                 ' UI support

  #0, T_RES, T_DST, T_SRC, T_LEN, T_END
  
OBJ
  axis: "core.con.mma7660fc"
  prom: "core.con.eeprom"
  util: "I2C PASM driver v1.8od"
  
VAR
  long  lock, transfers[8], head, tail
  long  xyzt, orientation, up, down, right, left

  long  stack[64]
  
PUB null
'' This is not a top level object.

PUB init(SCL, SDA, base, layout) : n

  util.start(SCL, SDA, 400_000)                         ' start driver

  xyzt        := base                                   ' |
  orientation := base + 4                               ' locations for raw/custom values

  repeat n from 0 to 3                                  ' |
    up[n] := base + layout.byte[n]                      ' initialise custom source(s)

  lock := locknew                                       ' reserve lock
  cognew(task, @stack{0})                               ' start helper task
  
PUB complete(transfer)

  return long[transfer][T_DST] < 0                      ' report completion status
  
PUB reada(transfer)

  long[transfer][T_RES] := 0                            ' result
  long[transfer][T_DST] &= $FFFF                        ' status

  repeat
  while lockset(lock)                                   ' acquire lock

  transfers[head++] := transfer                         ' record transfer
  head &= 7                                             ' (there is at most one transfer per client)

  lockclr(lock)                                         ' release lock
    
PUB read(dst, src, length)
                                                        ' memory layout: result, parameters, local variables
  reada(@result)                                        ' asynchronous read

  repeat
  until dst < 0                                         ' wait for completion
  
PRI task : length | mark, transfer, value

  util.writeByte(axis#ID, axis#MODE,  $00)              ' stand-by
  util.writeByte(axis#ID, axis#INTSU, $00)              ' no interrupts
  util.writeByte(axis#ID, axis#SR,    $00)              ' 120 sps
  util.writeByte(axis#ID, axis#PDET,  $6C)              ' tap detect on Z, threshold = 12
  util.writeByte(axis#ID, axis#PD,    $08)              ' tap debounce count           
  util.writeByte(axis#ID, axis#MODE,  $C1)              ' active, int pin is push-pull active-high
  
  mark := cnt

  repeat
    repeat
      util.readBytes(axis#ID, axis#XOUT, @value, 4)     ' 4 bytes starting at XOUT
    while value & axis#ALERT_XYZT                       ' monitor alert bits

    long[xyzt] := value                                 ' set acceleration and tilt

    case byte[xyzt][3] & %000_111_00                    ' |
      %000_110_00: long[orientation] := long[up]        ' |
      %000_101_00: long[orientation] := long[down]      ' custom mapping for tilt values
      %000_010_00: long[orientation] := long[right]     ' |
      %000_001_00: long[orientation] := long[left]      ' |

    repeat                                                        
      if tail <> head                                   ' transfers available
        transfer := transfers[tail]                     ' grab active transfer

        if length := long[transfer][T_LEN] <# 2048
          util.readBytes(prom#ID, long[transfer][T_SRC], long[transfer][T_DST], length)

          long[transfer][T_DST] += length               ' |       
          long[transfer][T_SRC] += length               ' update transfer
          long[transfer][T_RES] += length               ' |
        
        ifnot long[transfer][T_LEN] -= length
          tail := (tail + 1) & 7                        ' remove transfer
          long[transfer][T_DST] |= NEGX                 ' mark complete

    while (cnt - mark) < clkfreq >> 2                             

    mark += clkfreq >> 2                                          

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