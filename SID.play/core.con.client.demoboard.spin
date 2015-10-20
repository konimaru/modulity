''
'' prerequisites, dependencies & Co, DemoBoard
''
''        Author: Marko Lukat
'' Last modified: 2010/12/27
''       Version: 0.4
''
CON
  _clkmode = XTAL1|PLL16X
  _xinfreq = 5_000_000

CON
  #4,  XI_0_PRIMARY,   XI_1_PRIMARY                     ' XLINK inputs
  #6,  XO_0_PRIMARY,   XO_1_PRIMARY                     ' XLINK outputs
  #16, XO_0_SECONDARY, XO_1_SECONDARY                   ' extractor outputs

  #10, AUDIO_R, AUDIO_L                                 ' AUDIO channels
  
PUB null
'' This is not a top level object.

DAT