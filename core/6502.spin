''
''        Author: Marko Lukat
'' Last modified: 2015/10/11
''       Version: 0.12
''
'' acknowledgements
'' - 6502 CORE (C) 2009-10-07 Eric Ball
'' - 6502 Emulator Copyright (C) Eric Ball and Darryl Biggar
''
'' ToDo: SBC, BRK, RTI, PHA, PLA, PHP, PLP, ADC/SBC decimal mode
''
CON
  _clkmode = XTAL1|PLL16X
  _xinfreq = 5_000_000
  
CON
  TARGET = $7000
  ENTRY  = $7003
  
OBJ
  serial: "FullDuplexSerial"

PUB main : n | mbox[res_m]

  init(-1, @mbox{0})
  bytemove(TARGET, @sid, @sid_end-@sid)
  longfill($7F00, -1, 64)

  serial.start(31, 30, %0000, 115200)
  waitcnt(clkfreq*3 + cnt)
  serial.tx(0)
  waitcnt(clkfreq*1 + cnt)

  repeat
  while mbox{0} < 0                                     ' startup complete

  repeat
    mbox{0} := NEGX|@wrapper
    repeat
    while mbox{0} < 0
    n++
  until mbox.word{0} <> @w_end or n > 500{10sec}

  println(mbox{0})
  println(n)
  println(cnt)
  dump(mbox{0}-32)
  serial.tx(13)
  dump($0000)
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
DAT
wrapper byte    $20, word ENTRY
w_end   byte    $00

DAT     long    'newtest2-7000.bin'
sid     file    "newtest2-7000.bin"
sid_end
        
OBJ
  system: "core.con.system"
  
PUB null
'' This is not a top level object.

PUB init(ID, mailbox)

  long[mailbox]{0} := NEGX|@mapping
  
  return system.launch(ID, @core, mailbox)
  
DAT             org     0                               ' 6502 core

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

o_absy          call    #rd_w{ord}
                add     oadr, r_yi
                jmp     #link                           ' process insn
                
o_abs           call    #rd_w{ord}
                jmp     #link                           ' process insn

o_imm           mov     oadr, addr
                add     addr, #1
                jmp     #link                           ' process insn

o_zpg           rdbyte  oadr, addr
                add     addr, #1
                jmp     #link                           ' process insn

o_zpgx          rdbyte  oadr, addr
                add     oadr, r_xi
                and     oadr, #$FF
                add     addr, #1
                jmp     #link                           ' process insn

o_zpgy          rdbyte  oadr, addr
                add     oadr, r_yi
                and     oadr, #$FF
                add     addr, #1
                jmp     #link                           ' process insn

o_indx          rdbyte  oadr, addr                      '  +0 = (zp,x)
                add     oadr, r_xi                      '  +8
                and     oadr, #$FF                      '  -4
                rdbyte  tmpc, oadr                      '  +0 = LSB
                add     oadr, #1                        '  +8
                and     oadr, #$FF                      '  -4
                rdbyte  oadr, oadr                      '  +0 = MSB
                shl     oadr, #8
                or      oadr, tmpc
                add     addr, #1
                jmp     #link                           ' process insn

o_indy          rdbyte  oadr, addr                      '  +0 = (zp),y
                add     addr, #1                        '  +8
                shr     exec, #9                        '  -4   in-place link [1/2]
                rdbyte  tmpc, oadr                      '  +0 = LSB
                add     oadr, #1                        '  +8
                and     oadr, #$FF                      '  -4
                rdbyte  oadr, oadr                      '  +0 = MSB
                shl     oadr, #8
                or      oadr, tmpc
                add     oadr, r_yi
                jmp     exec                            '       in-place link [2/2]


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

i_sty           wrbyte  r_yi, oadr                      ' transfer index register Y to memory
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

i_ora           rdbyte  tmpc, oadr                      ' fetch mask
                or      r_ac, tmpc wz
                jmp     #f_upda

i_eor           rdbyte  tmpc, oadr                      ' fetch mask
                xor     r_ac, tmpc wz
                jmp     #f_upda

i_bit           rdbyte  tmpc, oadr                      ' fetch mask
                test    tmpc, #F_V wc                   ' V (copy)
                muxc    r_st, #F_V
                test    r_ac, tmpc wz                   ' Z
                test    tmpc, #F_N wc                   ' N (copy)
                jmp     #f_upd{ate}

                
i_cmp           rdbyte  tmpc, oadr                      ' comparison value
                mov     tmps, r_ac
f_compare       sub     tmps, tmpc wc,wz                ' C,Z
                muxnc   r_st, #F_C
                test    tmps, #$80 wc                   ' N
                jmp     #f_upd{ate}

i_cpx           rdbyte  tmpc, oadr                      ' comparison value
                mov     tmps, r_xi
                jmp     #f_compare
                
i_cpy           rdbyte  tmpc, oadr                      ' comparison value
                mov     tmps, r_yi
                jmp     #f_compare


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

i_inc           test    $, #1 wc                        ' set carry
i_dec           rdbyte  tmpc, oadr
                sumnc   tmpc, #1                        ' dec(clear)/inc(set)
                test    tmpc, #$80 wc
{NG}            wrbyte  tmpc, oadr{wz}
                test    tmpc, #$FF wz
                jmp     #f_upd{ate}

i_rla           test    r_st, #F_C wc
i_sla           rcl     r_ac, #1                        '       carry clear when used
                test    r_ac, #$100 wc                  '       C
                muxc    r_st, #F_C                      '       capture bit 7
                and     r_ac, #$FF wz                   '       Z
                jmp     #f_upda                         '       N

i_rra           test    r_st, #F_C wc                   '       |
                muxc    r_ac, #$100                     '       transfer carry
i_sra           shr     r_ac, #1 wc,wz                  '       C,Z
                muxc    r_st, #F_C                      '       capture bit 0
                jmp     #f_upda                         '       N

i_rlm           test    r_st, #F_C wc
i_slm           rdbyte  tmpc, oadr                      '  +0 = carry clear when used
                rcl     tmpc, #1 wc,wz                  '  +8   transfer carry, post clear
        if_nz   cmpsub  tmpc, #$100 wc,wz               '  -4   C,Z
{NG}            wrbyte  tmpc, oadr{wz}                  '  +0 =
                muxc    r_st, #F_C                      '       capture bit 7
                test    tmpc, #$80 wc                   '       N
                jmp     #f_upd{ate}
                
i_rrm           test    r_st, #F_C wc
i_srm           rdbyte  tmpc, oadr                      '  +0 = carry clear when used
                muxc    tmpc, #$100                     '  +8   transfer carry
                shr     tmpc, #1 wc,wz                  '  -4   C,Z
                wrbyte  tmpc, oadr                      '  +0 =
                muxc    r_st, #F_C                      '       capture bit 0
                test    tmpc, #$80 wc                   '       N
                jmp     #f_upd{ate}


i_adc           rdbyte  tmpc, oadr                      ' fetch operand
                test    r_st, #F_C                      ' fetch carry
                mov     tmps, r_ac                      ' ac(0)
                addx    r_ac, tmpc                      ' ac(1) = ac(0) + (op + carry)

                xor     tmpc, tmps                      '  ac(0) ^ op
                xor     tmps, r_ac                      '  ac(0) ^ ac(1)
                andn    tmps, tmpc                      ' (ac(0) ^ ac(1)) & !(ac(0) ^ op)
                test    tmps, #$80 wc                   ' V
                muxc    r_st, #F_V

                test    r_ac, #$100 wc                  ' C
                muxc    r_st, #F_C
                and     r_ac, #$FF wz                   ' Z
                jmp     #f_upda                         ' N
                
i_sbc           rdbyte  tmpc, oadr                      ' fetch operand
                test    r_st, #F_F|F_C wc               ' fetch (inverted) carry                (++)
                mov     tmps, r_ac                      ' ac(0)
                subx    r_ac, tmpc                      ' ac(1) = ac(0) - (op + carry)

                xor     tmpc, tmps                      '  ac(0) ^ op
                xor     tmps, r_ac                      '  ac(0) ^ ac(1)
                and     tmps, tmpc                      ' (ac(0) ^ ac(1)) & (ac(0) ^ op)
                test    tmps, #$80 wc                   ' V
                muxc    r_st, #F_V
                
                test    r_ac, #$100 wc                  ' C
                muxnc   r_st, #F_C                      '                                       (++)
                and     r_ac, #$FF wz                   ' Z
                jmp     #f_upda                         ' N


i_clc           andn    r_st, #F_C
                jmp     #rd_n{ext}

i_sec           or      r_st, #F_C
                jmp     #rd_n{ext}

i_cli           andn    r_st, #F_I
                jmp     #rd_n{ext}

i_sei           or      r_st, #F_I
                jmp     #rd_n{ext}

i_clv           andn    r_st, #F_V
                jmp     #rd_n{ext}

i_cld           andn    r_st, #F_D
                jmp     #rd_n{ext}

i_sed           or      r_st, #F_D
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

i_beq           test    r_st, #F_Z wc
        if_c    jmp     #f_rel{ative}
                jmp     #rd_n{ext}

i_bne           test    r_st, #F_Z wc
        if_nc   jmp     #f_rel{ative}
                jmp     #rd_n{ext}

i_bcs           test    r_st, #F_C wc
        if_c    jmp     #f_rel{ative}
                jmp     #rd_n{ext}

i_bcc           test    r_st, #F_C wc
        if_nc   jmp     #f_rel{ative}
                jmp     #rd_n{ext}

i_bvs           test    r_st, #F_V wc
        if_c    jmp     #f_rel{ative}
                jmp     #rd_n{ext}

i_bvc           test    r_st, #F_V wc
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

r_sp            long    $7FFF                           ' page must be 2n+1                     (##)
r_ac            long    $00
r_xi            long    $00
r_yi            long    $00
r_st            long    F_F|F_B                         ' we only have 6 effective flags

' Stuff below is re-purposed for temporary storage.

setup           rdlong  base, par wz                    '  +0 =                                 (%%)
                add     stat, par                       '  +8
        if_nz   wrlong  zero, par                       '  +0 =

                jmp     %%0                             ' return

                fit

' uninitialised data and/or temporaries

                org     setup

base            res     1                               ' insn mapping table    <= setup+0      (%%)
addr            res     1
insn            res     1

oadr            res     1                               ' operand address

tmps            res     1                               ' primarily stack operand
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

mapping         nop                                     ' 00
                jmpret  i_ora, #o_indx nr               ' 01    indirect,x      ora ($44,x)
                nop                                     ' 02
                nop                                     ' 03
                nop                                     ' 04
                jmpret  i_ora, #o_zpg nr                ' 05    zeropage        ora $44
                jmpret  i_slm, #o_zpg wc,nr             ' 06    zeropage        asl $44         (carry clear)
                nop                                     ' 07

                nop                                     ' 08
                jmpret  i_ora, #o_imm nr                ' 09    immediate       ora #$44
                jmpret  exec, #i_sla wc,nr              ' 0A                    asl a           (carry clear)
                nop                                     ' 0B
                nop                                     ' 0C
                jmpret  i_ora, #o_abs nr                ' 0D    absolute        ora $4400
                jmpret  i_slm, #o_abs wc,nr             ' 0E    absolute        asl $4400       (carry clear)
                nop                                     ' 0F

                jmpret  i_bpl, #o_imm nr                ' 10    relative        bpl $4400
                jmpret  i_ora, #o_indy nr               ' 11    indirect,y      ora ($44),y
                nop                                     ' 12
                nop                                     ' 13
                nop                                     ' 14
                jmpret  i_ora, #o_zpgx nr               ' 15    zeropage,x      ora $44,x
                jmpret  i_slm, #o_zpgx wc,nr            ' 16    zeropage,x      asl $44,x       (carry clear)
                nop                                     ' 17

                jmp     #i_clc                          ' 18                    clc
                jmpret  i_ora, #o_absy nr               ' 19    absolute,y      ora $4400,y
                nop                                     ' 1A
                nop                                     ' 1B
                nop                                     ' 1C
                jmpret  i_ora, #o_absx nr               ' 1D    absolute,x      ora $4400,x
                jmpret  i_slm, #o_absx wc,nr            ' 1E    absolute,x      asl $4400,x     (carry clear)
                nop                                     ' 1F

                jmpret  i_jsr, #o_abs nr                ' 20    absolute        jsr $4400
                jmpret  i_and, #o_indx nr               ' 21    indirect,x      and ($44,x)
                nop                                     ' 22
                nop                                     ' 23
                jmpret  i_bit, #o_zpg nr                ' 24    zeropage        bit $44
                jmpret  i_and, #o_zpg nr                ' 25    zeropage        and $44
                jmpret  i_rlm, #o_zpg nr                ' 26    zeropage        rol $44
                nop                                     ' 27

                nop                                     ' 28
                jmpret  i_and, #o_imm nr                ' 29    immediate       and #$44
                jmp     #i_rla                          ' 2A                    rol a
                nop                                     ' 2B
                jmpret  i_bit, #o_abs nr                ' 2C    absolute        bit $4400
                jmpret  i_and, #o_abs nr                ' 2D    absolute        and $4400
                jmpret  i_rlm, #o_abs nr                ' 2E    absolute        rol $4400
                nop                                     ' 2F

                jmpret  i_bmi, #o_imm nr                ' 30    relative        bmi $4400
                jmpret  i_and, #o_indy nr               ' 31    indirect,y      and ($44),y
                nop                                     ' 32
                nop                                     ' 33
                nop                                     ' 34
                jmpret  i_and, #o_zpgx nr               ' 35    zeropage,x      and $44,x
                jmpret  i_rlm, #o_zpgx nr               ' 36    zeropage,x      rol $44,x
                nop                                     ' 37

                jmp     #i_sec                          ' 38                    sec
                jmpret  i_and, #o_absy nr               ' 39    absolute,y      and $4400,y
                nop                                     ' 3A
                nop                                     ' 3B
                nop                                     ' 3C
                jmpret  i_and, #o_absx nr               ' 3D    absolute,x      and $4400,x
                jmpret  i_rlm, #o_absx nr               ' 3E    absolute,x      rol $4400,x
                nop                                     ' 3F

                nop                                     ' 40
                jmpret  i_eor, #o_indx nr               ' 41    indirect,x      eor ($44,x)
                nop                                     ' 42
                nop                                     ' 43
                nop                                     ' 44
                jmpret  i_eor, #o_zpg nr                ' 45    zeropage        eor $44
                jmpret  i_srm, #o_zpg wc,nr             ' 46    zeropage        lsr $44         (carry clear)
                nop                                     ' 47

                nop                                     ' 48
                jmpret  i_eor, #o_imm nr                ' 49    immediate       eor #$44
                jmp     #i_sra                          ' 4A                    lsr a
                nop                                     ' 4B
                jmpret  i_jmp, #o_abs nr                ' 4C    absolute        jmp $4400
                jmpret  i_eor, #o_abs nr                ' 4D    absolute        eor $4400
                jmpret  i_srm, #o_abs wc,nr             ' 4E    absolute        lsr $4400       (carry clear)
                nop                                     ' 4F

                jmpret  i_bvc, #o_imm nr                ' 50    relative        bvc $4400
                jmpret  i_eor, #o_indy nr               ' 51    indirect,y      eor ($44),y
                nop                                     ' 52
                nop                                     ' 53
                nop                                     ' 54
                jmpret  i_eor, #o_zpgx nr               ' 55    zeropage,x      eor $44,x
                jmpret  i_srm, #o_zpgx wc,nr            ' 56    zeropage,x      lsr $44,x       (carry clear)
                nop                                     ' 57

                jmp     #i_cli                          ' 58                    cli
                jmpret  i_eor, #o_absy nr               ' 59    absolute,y      eor $4400,y
                nop                                     ' 5A
                nop                                     ' 5B
                nop                                     ' 5C
                jmpret  i_eor, #o_absx nr               ' 5D    absolute,x      eor $4400,x
                jmpret  i_srm, #o_absx wc,nr            ' 5E    absolute,x      lsr $4400,x     (carry clear)
                nop                                     ' 5F

                jmp     #i_rts                          ' 60                    rts
                jmpret  i_adc, #o_indx nr               ' 61    indirect,x      adc ($44,x)
                nop                                     ' 62
                nop                                     ' 63
                nop                                     ' 64
                jmpret  i_adc, #o_zpg nr                ' 65    zeropage        adc $44
                jmpret  i_rrm, #o_zpg nr                ' 66    zeropage        ror $44
                nop                                     ' 67

                nop                                     ' 68
                jmpret  i_adc, #o_imm nr                ' 69    immediate       adc #$44
                jmp     #i_rra                          ' 6A                    ror a
                nop                                     ' 6B
                jmpret  i_jmp, #o_ind nr                ' 6C    indirect        jmp ($4400)
                jmpret  i_adc, #o_abs nr                ' 6D    absolute        adc $4400
                jmpret  i_rrm, #o_abs nr                ' 6E    absolute        ror $4400
                nop                                     ' 6F

                jmpret  i_bvs, #o_imm nr                ' 70    relative        bvs $4400
                jmpret  i_adc, #o_indy nr               ' 71    indirect,y      adc ($44),y
                nop                                     ' 72
                nop                                     ' 73
                nop                                     ' 74
                jmpret  i_adc, #o_zpgx nr               ' 75    zeropage,x      adc $44,x
                jmpret  i_rrm, #o_zpgx nr               ' 76    zeropage,x      ror $44,x
                nop                                     ' 77

                jmp     #i_sei                          ' 78                    sei
                jmpret  i_adc, #o_absy nr               ' 79    absolute,y      adc $4400,y
                nop                                     ' 7A
                nop                                     ' 7B
                nop                                     ' 7C
                jmpret  i_adc, #o_absx nr               ' 7D    absolute,x      adc $4400,x
                jmpret  i_rrm, #o_absx nr               ' 7E    absolute,x      ror $4400,x
                nop                                     ' 7F

                nop                                     ' 80
                jmpret  i_sta, #o_indx nr               ' 81    indirect,x      sta ($44,x)
                nop                                     ' 82
                nop                                     ' 83
                jmpret  i_sty, #o_zpg nr                ' 84    zeropage        sty $44
                jmpret  i_sta, #o_zpg nr                ' 85    zeropage        sta $44
                jmpret  i_stx, #o_zpg nr                ' 86    zeropage        stx $44
                nop                                     ' 87

                jmpret  exec, #i_diy wc,nr              ' 88                    dey             (carry clear)
                nop                                     ' 89
                jmp     #i_txa                          ' 8A                    txa
                nop                                     ' 8B
                jmpret  i_sty, #o_abs nr                ' 8C    absolute        sty $4400
                jmpret  i_sta, #o_abs nr                ' 8D    absolute        sta $4400
                jmpret  i_stx, #o_abs nr                ' 8E    absolute        stx $4400
                nop                                     ' 8F

                jmpret  i_bcc, #o_imm nr                ' 90    relative        bcc $4400
                jmpret  i_sta, #o_indy nr               ' 91    indirect,y      sta ($44),y
                nop                                     ' 92
                nop                                     ' 93
                jmpret  i_sty, #o_zpgx nr               ' 94    zeropage,x      sty $44,x
                jmpret  i_sta, #o_zpgx nr               ' 95    zeropage,x      sta $44,x
                jmpret  i_stx, #o_zpgy nr               ' 96    zeropage,y      stx $44,y
                nop                                     ' 97

                jmp     #i_tya                          ' 98                    tya
                jmpret  i_sta, #o_absy nr               ' 99    absolute,y      sta $4400,y
                jmp     #i_txs                          ' 9A                    txs
                nop                                     ' 9B
                nop                                     ' 9C
                jmpret  i_sta, #o_absx nr               ' 9D    absolute,x      sta $4400,x
                nop                                     ' 9E
                nop                                     ' 9F

                jmpret  i_ldy, #o_imm nr                ' A0    immediate       ldy #$44
                jmpret  i_lda, #o_indx nr               ' A1    indirect,x      lda ($44,x)
                jmpret  i_ldx, #o_imm nr                ' A2    immediate       ldx #$44
                nop                                     ' A3
                jmpret  i_ldy, #o_zpg nr                ' A4    zeropage        ldy $44
                jmpret  i_lda, #o_zpg nr                ' A5    zeropage        lda $44
                jmpret  i_ldx, #o_zpg nr                ' A6    zeropage        ldx $44
                nop                                     ' A7

                jmp     #i_tay                          ' A8                    tay
                jmpret  i_lda, #o_imm nr                ' A9    immediate       lda #$44
                jmp     #i_tax                          ' AA                    tax
                nop                                     ' AB
                jmpret  i_ldy, #o_abs nr                ' AC    absolute        ldy $4400
                jmpret  i_lda, #o_abs nr                ' AD    absolute        lda $4400
                jmpret  i_ldx, #o_abs nr                ' AE    absolute        ldx $4400
                nop                                     ' AF

                jmpret  i_bcs, #o_imm nr                ' B0    relative        bcs $4400
                jmpret  i_lda, #o_indy nr               ' B1    indirect,y      lda ($44),y
                nop                                     ' B2
                nop                                     ' B3
                jmpret  i_ldy, #o_zpgx nr               ' B4    zeropage,x      ldy $44,x
                jmpret  i_lda, #o_zpgx nr               ' B5    zeropage,x      lda $44,x
                jmpret  i_ldx, #o_zpgy nr               ' B6    zeropage,y      ldx $44,y
                nop                                     ' B7

                jmp     #i_clv                          ' B8                    clv
                jmpret  i_lda, #o_absy nr               ' B9    absolute,y      lda $4400,y
                jmp     #i_tsx                          ' BA                    tsx
                nop                                     ' BB
                jmpret  i_ldy, #o_absx nr               ' BC    absolute,x      ldy $4400,x
                jmpret  i_lda, #o_absx nr               ' BD    absolute,x      lda $4400,x
                jmpret  i_ldx, #o_absy nr               ' BE    absolute,y      ldx $4400,y
                nop                                     ' BF

                jmpret  i_cpy, #o_imm nr                ' C0    immediate       cpy #$44
                jmpret  i_cmp, #o_indx nr               ' C1    indirect,x      cmp ($44,x)
                nop                                     ' C2
                nop                                     ' C3
                jmpret  i_cpy, #o_zpg nr                ' C4    zeropage        cpy $44
                jmpret  i_cmp, #o_zpg nr                ' C5    zeropage        cmp $44
                jmpret  i_dec, #o_zpg wc,nr             ' C6    zeropage        dec $44         (carry clear)
                nop                                     ' C7

                jmpret  zero, #i_diy wc,nr              ' C8                    iny             (carry set)
                jmpret  i_cmp, #o_imm nr                ' C9    immediate       cmp #$44
                jmpret  exec, #i_dix wc,nr              ' CA                    dex             (carry clear)
                nop                                     ' CB
                jmpret  i_cpy, #o_abs nr                ' CC    absolute        cpy $4400
                jmpret  i_cmp, #o_abs nr                ' CD    absolute        cmp $4400
                jmpret  i_dec, #o_abs wc,nr             ' CE    absolute        dec $4400       (carry clear)
                nop                                     ' CF

                jmpret  i_bne, #o_imm nr                ' D0    relative        bne $4400
                jmpret  i_cmp, #o_indy nr               ' D1    indirect,y      cmp ($44),y
                nop                                     ' D2
                nop                                     ' D3
                nop                                     ' D4
                jmpret  i_cmp, #o_zpgx nr               ' D5    zeropage,x      cmp $44,x
                jmpret  i_dec, #o_zpgx wc,nr            ' D6    zeropage,x      dec $44,x       (carry clear)
                nop                                     ' D7

                jmp     #i_cld                          ' D8                    cld
                jmpret  i_cmp, #o_absy nr               ' D9    absolute,y      cmp $4400,y
                nop                                     ' DA
                nop                                     ' DB
                nop                                     ' DC
                jmpret  i_cmp, #o_absx nr               ' DD    absolute,x      cmp $4400,x
                jmpret  i_dec, #o_absx wc,nr            ' DE    absolute.x      dec $4400,x     (carry clear)
                nop                                     ' DF

                jmpret  i_cpx, #o_imm nr                ' E0    immediate       cpx #$44
                jmpret  i_sbc, #o_indx nr               ' E1    indirect,x      sbc ($44,x)
                nop                                     ' E2
                nop                                     ' E3
                jmpret  i_cpx, #o_zpg nr                ' E4    zeropage        cpx $44
                jmpret  i_sbc, #o_zpg nr                ' E5    zeropage        sbc $44
                jmpret  i_inc, #o_zpg nr                ' E6    zeropage        inc $44
                nop                                     ' E7

                jmpret  zero, #i_dix wc,nr              ' E8                    inx             (carry set)
                jmpret  i_sbc, #o_imm nr                ' E9    immediate       sbc #$44
                jmp     #rd_n{ext}                      ' EA                    nop
                nop                                     ' EB
                jmpret  i_cpx, #o_abs nr                ' EC    absolute        cpx $4400
                jmpret  i_sbc, #o_abs nr                ' ED    absolute        sbc $4400
                jmpret  i_inc, #o_abs nr                ' EE    absolute        inc $4400
                nop                                     ' EF

                jmpret  i_beq, #o_imm nr                ' F0    relative        beq $4400
                jmpret  i_sbc, #o_indy nr               ' F1    indirect,y      sbc ($44),y
                nop                                     ' F2
                nop                                     ' F3
                nop                                     ' F4
                jmpret  i_sbc, #o_zpgx nr               ' F5    zeropage,x      sbc $44,x
                jmpret  i_inc, #o_zpgx nr               ' F6    zeropage,x      inc $44,x
                nop                                     ' F7

                jmp     #i_sed                          ' F8                    sed
                jmpret  i_sbc, #o_absy nr               ' F9    absolute,y      sbc $4400,y
                nop                                     ' FA
                nop                                     ' FB
                nop                                     ' FC
                jmpret  i_sbc, #o_absx nr               ' FD    absolute,x      sbc $4400,x
                jmpret  i_inc, #o_absx nr               ' FE    absolute,x      inc $4400,x
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