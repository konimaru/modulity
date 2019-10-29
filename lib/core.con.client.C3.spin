''
'' prerequisites, dependencies & Co, C3/Synapse
''
''        Author: Marko Lukat
'' Last modified: 2017/08/24
''       Version: 0.3
''
CON
  _clkmode = XTAL1|PLL16X
  _xinfreq = 5_000_000

CON
  #9, SPI_MOSI, SPI_MISO, SPI_CLK                       ' SPI bus (clk/dta)

  SPI_SEL_CLK = 8                                       ' |
  SPI_SEL_CLR = 25                                      ' SPI chip select(or)

  AUDIO_R     = -1                                      ' |
  AUDIO_L     = 24                                      ' AUDIO channel(s), mono

  VGA_MUX     = 15                                      ' VGA buffer enable
  
PUB null
'' This is not a top level object.

DAT
