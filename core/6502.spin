''
''        Author: Marko Lukat
'' Last modified: 2015/10/06
''       Version: 0.7
''
'' acknowledgements
'' - 6502 CORE (C) 2009-10-07 Eric Ball
'' - 6502 Emulator Copyright (C) Eric Ball and Darryl Biggar
''
CON
  _clkmode = XTAL1|PLL16X
  _xinfreq = 5_000_000
  
CON
  TARGET = $7000
  ENTRY  = $7003
  
OBJ
  serial: "FullDuplexSerial"
  
PUB main | mbox[res_m]

  init(-1, @mbox{0})
  serial.start(31, 30, %0000, 115200)
  bytemove(TARGET, @sid, @sidend-@sid)
  longfill($7F00, -1, 64)
  waitcnt(clkfreq*3 + cnt)
  serial.tx(0)
  waitcnt(clkfreq*1 + cnt)
  
  repeat
  while mbox{0} < 0                                     ' startup complete

  mbox{0} := NEGX|@wrapper
  repeat
  while mbox{0} < 0

  dira[16..23]~~
  outa[16..23] := mbox.byte[2]

  println(mbox{0})
  dump(mbox{0}-32)
  serial.tx(13)
  dump($7000)
  serial.tx(13)
  dump($7F00)
  
  waitpne(0, 0, 0)

PRI println(value)

  serial.hex(value, 8)
  serial.tx(13)

PRI dump(address) | x, y

  repeat y from 0 to 15
    serial.hex(address + y<<4, 4)
    serial.str(string(": "))
    repeat x from 0 to 15
      serial.hex(byte[address + y<<4 + x], 2)
      serial.tx(32)
    serial.tx(13)
    
DAT     byte    $FF[256]

wrapper byte    $6C, $33, $01, $35, $01
        byte    $20, word ENTRY
        byte    $00
        
DAT     long    'newtest2-7000.bin'
sid     file    "newtest2-7000.bin"
sidend
        
OBJ
  system: "core.con.system"
  
PUB null
'' This is not a top level object.

PUB init(ID, mailbox)

  long[mailbox]{0} := NEGX|@mapping
  
  return system.launch(ID, @core, mailbox)
  
DAT             org     0

core            jmpret  $, #setup                       '  -4

                rdlong  addr, par                       '  +0 = fetch address (bit 31 set)
                cmps    addr, #0 wz,wc                  '  +8
        if_ae   jmp     %%0                             '  -4

rd_n{ext}       rdbyte  insn, addr                      '  +0 = fetch opcode

                shl     insn, #2                        '  +8   long address
                add     insn, base                      '  -4
                rdlong  exec, insn wz                   '  +0 =
        if_nz   add     addr, #1                        '  +8   advance PC (delay slot)

exec            hubop   $, #%10000_000                  '  -4
link            shr     exec, #9 wz
        if_nz   jmp     exec                            '       potential second pass

stop            wrword  addr, par                       '       PC of unknown/invalid insn
                wrword  r_st, stat                      '       current status
                jmp     %%0


rd_w{ord}       rdbyte  tmpc, addr                      ' LSB
                add     addr, #1
                cmp     tmpc, #$FF wz                   ' $--FF -> Z
                rdbyte  oadr, addr                      ' MSB
                add     addr, #1
                shl     oadr, #8
                or      oadr, tmpc
rd_w_ret        ret


o_ind           call    #rd_w{ord}
                rdbyte  tmpc, oadr                      ' LSB
                add     oadr, #1
        if_e    sub     oadr, #$100                     ' restore page for ($--FF)
                rdbyte  oadr, oadr                      ' MSB
                shl     oadr, #8
                or      oadr, tmpc
                jmp     #link                           ' process insn
                
o_absx          call    #rd_w{ord}
                add     oadr, r_xi
                jmp     #link                           ' process insn
                
o_abs           call    #rd_w{ord}
                jmp     #link                           ' process insn

o_imm           mov     oadr, addr
                add     addr, #1
                jmp     #link                           ' process insn
                

i_rts           call    #pull
                mov     addr, tmps                      ' LSB
                add     addr, #1                        '                                       (&&)
                call    #pull
                shl     tmps, #8                        ' MSB
                add     addr, tmps
                jmp     #rd_n{ext}

i_jsr           mov     tmps, addr                      ' stack is empty/descending
                sub     tmps, #1                        '                                       (&&)
                ror     tmps, #8                        ' MSB 1st
                call    #push
                rol     tmps, #8                        ' LSB 2nd
                call    #push

i_jmp           mov     addr, oadr                      ' transfer target location
                jmp     #rd_n{ext}


i_sta           wrbyte  r_ac, oadr                      ' transfer accumulator to memory
                jmp     #rd_n{ext}

i_stx           wrbyte  r_xi, oadr                      ' transfer index register X to memory
                jmp     #rd_n{ext}


i_lda           rdbyte  r_ac, oadr wz                   ' fetch immediate value
f_upda          test    r_ac, #$80 wc
f_upd{ate}      muxz    r_st, #F_Z
                muxc    r_st, #F_N
                jmp     #rd_n{ext}
                
i_ldx           rdbyte  r_xi, oadr wz                   ' fetch immediate value
f_updx          test    r_xi, #$80 wc
                jmp     #f_upd{ate}

i_ldy           rdbyte  r_yi, oadr wz                   ' fetch immediate value
f_updy          test    r_yi, #$80 wc
                jmp     #f_upd{ate}

i_tax           mov     r_xi, r_ac wz
                jmp     #f_updx
                
i_txa           mov     r_ac, r_xi wz
                jmp     #f_upda

i_tay           mov     r_yi, r_ac wz
                jmp     #f_updy
                
i_tya           mov     r_ac, r_yi wz
                jmp     #f_upda

i_and           rdbyte  tmpc, oadr                      ' fetch mask
                and     r_ac, tmpc wz
                jmp     #f_upda

i_dix           sumnc   r_xi, #1                        ' dex(clear)/inx(set)
                and     r_xi, #$FF wz
                jmp     #f_updx

i_diy           sumnc   r_yi, #1                        ' dey(clear)/iny(set)
                and     r_yi, #$FF wz
                jmp     #f_updy

i_tsx           mov     r_xi, r_sp                      ' do flag update                        (**)
                and     r_xi, #$FF wz
                jmp     #f_updx
                
i_txs           andn    r_sp, #$FF                      ' no flag update                        (**)
                or      r_sp, r_xi
                jmp     #rd_n{ext}


i_bmi           test    r_st, #F_N wc
        if_nc   jmp     #rd_n{ext}
f_rel{ative}    rdbyte  tmpc, oadr
                shl     tmpc, #24
                sar     tmpc, #24
                add     addr, tmpc
                jmp     #rd_n{ext}

i_bpl           test    r_st, #F_N wc
        if_nc   jmp     #f_rel{ative}
                jmp     #rd_n{ext}


push            wrbyte  tmps, r_sp                      ' byte[sp--] := tmps
                sub     r_sp, #1
                or      r_sp, #$100                     ' keep page at 2n+1 (autowrap)          (##)
push_ret        ret

pull            add     r_sp, #1                        ' tmps := byte[++sp]
                test    r_sp, #$100 wc
        if_nc   sub     r_sp, #$100                     ' keep page at 2n+1 (autowrap)          (##)
                rdbyte  tmps, r_sp
pull_ret        ret

' initialised data and/or presets

stat            long    +2                              ' status location
pmap            long    -256                            ' page map

r_sp            long    $7FFF                           ' page must be 2n+1                     (##)
r_ac            long    $00
r_xi            long    $00
r_yi            long    $00
r_st            long    F_F|F_B                         ' we only have 6 effective flags

' Stuff below is re-purposed for temporary storage.

setup           rdlong  base, par wz                    '  +0 =                                 (%%)
                add     pmap, base                      '  +8
                add     stat, par                       '  -4
        if_nz   wrlong  zero, par                       '  +0 =

                jmp     %%0                             ' return

                fit

' uninitialised data and/or temporaries

                org     setup

base            res     1                               ' insn mapping table    <= setup+0      (%%)
addr            res     1
insn            res     1

oadr            res     1                               ' operand address

tmps            res     1                               ' stack operand
tmpc            res     1

tail            fit
               
CON
  F_N = %10000000
  F_V = %01000000

  F_F = %00100000
  F_B = %00010000

  F_D = %00001000
  F_I = %00000100
  F_Z = %00000010
  F_C = %00000001
  
CON
  zero  = $1F0                                          ' par (dst only)
  alias = 0

  res_m = 1                                             ' UI support
  
DAT

                byte    0[256]
                
DAT

mapping         nop                                     ' 00
                nop                                     ' 01
                nop                                     ' 02
                nop                                     ' 03
                nop                                     ' 04
                nop                                     ' 05
                nop                                     ' 06
                nop                                     ' 07

                nop                                     ' 08
                nop                                     ' 09
                nop                                     ' 0A
                nop                                     ' 0B
                nop                                     ' 0C
                nop                                     ' 0D
                nop                                     ' 0E
                nop                                     ' 0F

                jmpret  i_bpl, #o_imm nr                ' 10    relative        bpl $12
                nop                                     ' 11
                nop                                     ' 12
                nop                                     ' 13
                nop                                     ' 14
                nop                                     ' 15
                nop                                     ' 16
                nop                                     ' 17

                nop                                     ' 18
                nop                                     ' 19
                nop                                     ' 1A
                nop                                     ' 1B
                nop                                     ' 1C
                nop                                     ' 1D
                nop                                     ' 1E
                nop                                     ' 1F

                jmpret  i_jsr, #o_abs nr                ' 20    absolute        jsr $1234
                nop                                     ' 21
                nop                                     ' 22
                nop                                     ' 23
                nop                                     ' 24
                nop                                     ' 25
                nop                                     ' 26
                nop                                     ' 27

                nop                                     ' 28
                nop                                     ' 29
                nop                                     ' 2A
                nop                                     ' 2B
                nop                                     ' 2C
                nop                                     ' 2D
                nop                                     ' 2E
                nop                                     ' 2F

                jmpret  i_bmi, #o_imm nr                ' 30    relative        bmi $12
                nop                                     ' 31
                nop                                     ' 32
                nop                                     ' 33
                nop                                     ' 34
                nop                                     ' 35
                nop                                     ' 36
                nop                                     ' 37

                nop                                     ' 38
                nop                                     ' 39
                nop                                     ' 3A
                nop                                     ' 3B
                nop                                     ' 3C
                jmpret  i_and, #o_absx nr               ' 3D    absolute,x      and $4400,x
                nop                                     ' 3E
                nop                                     ' 3F

                nop                                     ' 40
                nop                                     ' 41
                nop                                     ' 42
                nop                                     ' 43
                nop                                     ' 44
                nop                                     ' 45
                nop                                     ' 46
                nop                                     ' 47

                nop                                     ' 48
                nop                                     ' 49
                nop                                     ' 4A
                nop                                     ' 4B
                jmpret  i_jmp, #o_abs nr                ' 4C    absolute        jmp $1234
                nop                                     ' 4D
                nop                                     ' 4E
                nop                                     ' 4F

                nop                                     ' 50
                nop                                     ' 51
                nop                                     ' 52
                nop                                     ' 53
                nop                                     ' 54
                nop                                     ' 55
                nop                                     ' 56
                nop                                     ' 57

                nop                                     ' 58
                nop                                     ' 59
                nop                                     ' 5A
                nop                                     ' 5B
                nop                                     ' 5C
                nop                                     ' 5D
                nop                                     ' 5E
                nop                                     ' 5F

                jmp     #i_rts                          ' 60    rts
                nop                                     ' 61
                nop                                     ' 62
                nop                                     ' 63
                nop                                     ' 64
                nop                                     ' 65
                nop                                     ' 66
                nop                                     ' 67

                nop                                     ' 68
                nop                                     ' 69
                nop                                     ' 6A
                nop                                     ' 6B
                jmpret  i_jmp, #o_ind nr                ' 6C    indirect        jmp ($1234)
                nop                                     ' 6D
                nop                                     ' 6E
                nop                                     ' 6F

                nop                                     ' 70
                nop                                     ' 71
                nop                                     ' 72
                nop                                     ' 73
                nop                                     ' 74
                nop                                     ' 75
                nop                                     ' 76
                nop                                     ' 77

                nop                                     ' 78
                nop                                     ' 79
                nop                                     ' 7A
                nop                                     ' 7B
                nop                                     ' 7C
                nop                                     ' 7D
                nop                                     ' 7E
                nop                                     ' 7F

                nop                                     ' 80
                nop                                     ' 81
                nop                                     ' 82
                nop                                     ' 83
                nop                                     ' 84
                nop                                     ' 85
                nop                                     ' 86
                nop                                     ' 87

                jmpret  exec, #i_diy wc,nr              ' 88    dey (carry clear)
                nop                                     ' 89
                jmp     #i_txa                          ' 8A    txa
                nop                                     ' 8B
                nop                                     ' 8C
                jmpret  i_sta, #o_abs nr                ' 8D    absolute        sta $4400
                jmpret  i_stx, #o_abs nr                ' 8E    absolute        stx $4400
                nop                                     ' 8F

                nop                                     ' 90
                nop                                     ' 91
                nop                                     ' 92
                nop                                     ' 93
                nop                                     ' 94
                nop                                     ' 95
                nop                                     ' 96
                nop                                     ' 97

                jmp     #i_tya                          ' 98    tya
                nop                                     ' 99
                jmp     #i_txs                          ' 9A    txs
                nop                                     ' 9B
                nop                                     ' 9C
                jmpret  i_sta, #o_absx nr               ' 9D    absolute,x      sta $1234,x
                nop                                     ' 9E
                nop                                     ' 9F

                jmpret  i_ldy, #o_imm nr                ' A0    immediate       ldy #$23
                nop                                     ' A1
                jmpret  i_ldx, #o_imm nr                ' A2    immediate       ldx #$12
                nop                                     ' A3
                nop                                     ' A4
                nop                                     ' A5
                nop                                     ' A6
                nop                                     ' A7

                jmp     #i_tay                          ' A8    tay
                jmpret  i_lda, #o_imm nr                ' A9    immediate       lda #$34
                jmp     #i_tax                          ' AA    tax
                nop                                     ' AB
                nop                                     ' AC
                nop                                     ' AD
                nop                                     ' AE
                nop                                     ' AF

                nop                                     ' B0
                nop                                     ' B1
                nop                                     ' B2
                nop                                     ' B3
                nop                                     ' B4
                nop                                     ' B5
                nop                                     ' B6
                nop                                     ' B7

                nop                                     ' B8
                nop                                     ' B9
                jmp     #i_tsx                          ' BA    tsx
                nop                                     ' BB
                nop                                     ' BC
                jmpret  i_lda, #o_absx nr               ' BD    absolute,x      lda $4400,x
                nop                                     ' BE
                nop                                     ' BF

                nop                                     ' C0
                nop                                     ' C1
                nop                                     ' C2
                nop                                     ' C3
                nop                                     ' C4
                nop                                     ' C5
                nop                                     ' C6
                nop                                     ' C7

                jmpret  zero, #i_diy wc,nr              ' C8    iny (carry set)
                nop                                     ' C9
                jmpret  exec, #i_dix wc,nr              ' CA    dex (carry clear)
                nop                                     ' CB
                nop                                     ' CC
                nop                                     ' CD
                nop                                     ' CE
                nop                                     ' CF

                nop                                     ' D0
                nop                                     ' D1
                nop                                     ' D2
                nop                                     ' D3
                nop                                     ' D4
                nop                                     ' D5
                nop                                     ' D6
                nop                                     ' D7

                nop                                     ' D8
                nop                                     ' D9
                nop                                     ' DA
                nop                                     ' DB
                nop                                     ' DC
                nop                                     ' DD
                nop                                     ' DE
                nop                                     ' DF

                nop                                     ' E0
                nop                                     ' E1
                nop                                     ' E2
                nop                                     ' E3
                nop                                     ' E4
                nop                                     ' E5
                nop                                     ' E6
                nop                                     ' E7

                jmpret  zero, #i_dix wc,nr              ' E8    inx (carry set)
                nop                                     ' E9
                jmp     #rd_n{ext}                      ' EA    nop
                nop                                     ' EB
                nop                                     ' EC
                nop                                     ' ED
                nop                                     ' EE
                nop                                     ' EF

                nop                                     ' F0
                nop                                     ' F1
                nop                                     ' F2
                nop                                     ' F3
                nop                                     ' F4
                nop                                     ' F5
                nop                                     ' F6
                nop                                     ' F7

                nop                                     ' F8
                nop                                     ' F9
                nop                                     ' FA
                nop                                     ' FB
                nop                                     ' FC
                nop                                     ' FD
                nop                                     ' FE
                nop                                     ' FF

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
