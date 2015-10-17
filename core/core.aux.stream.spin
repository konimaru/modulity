''
'' cog binary stream handler - user interface
''
''        Author: Marko Lukat
'' Last modified: 2011/12/26
''       Version: 0.5
''
VAR
  long  storage[2]
  long  lcnt
  
PUB null              
'' This is not a top level object.

PUB init

  storage{0} := @payload + 44
  storage[1] := payload[10]
  
PUB bget(buffer, length) : size

  ifnot storage[1]
    if ++lcnt // 20
      init
    
  size := storage[1] <# length
  longmove(buffer, storage{0}, size >> 2)
  storage{0} += size
  storage[1] -= size

DAT payload     long    'Windows Information Bar.wav'
                file    "Windows Information Bar.wav"
PUB binary(location, name)
'' Locate and load cog binary ready for launching (overlay or cognew).
''
'' parameters
'' location: storage location for the 2K binary buffer address
''     name: cog binary identifier (e.g. "built-in:0")
''
'' result
''  boolean indicating whether the binary has been found and/or loaded
''
'' Note: Built-in binaries simply store their DAT label address in long[location]
''       to avoid unnecessary buffer copies. A 2K multi-purpose buffer is provided
''       starting at (location + 4). If it is utilised then long[location] must be
''       initialised with (location + 4).

  ifnot strncmp(name, string("built-in:"), 9)
    ifnot byte[name][10]
      case byte[name][9]
          "0": long[location] := @built_in_0            ' stereo.duty.2048.cog
          "1": long[location] := @built_in_1            ' stereo.wave.2048.cog
          "2": long[location] := @built_in_2            ' decode.adpcm.2048.cog
        other: return
      return TRUE

PRI strncmp(one, two, ccnt) | c

  repeat ccnt
    c := byte[one++]
    if (result := c - byte[two++]) or not c
      return

DAT built_in_0  long    'stereo.duty.2048.cog'          ' built-in binaries
                file    "stereo.duty.2048.cog"
DAT built_in_1  long    'stereo.wave.2048.cog'          ' |
                file    "stereo.wave.2048.cog"
DAT built_in_2  long    'decode.adpcm.2048.cog'         ' |
                file    "decode.adpcm.2048.cog"
DAT