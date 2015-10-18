''
'' cog binary stream handler - user interface
''
''        Author: Marko Lukat
'' Last modified: 2011/12/26
''       Version: 0.5
''
CON
  res_m = 2                                             ' UI support
  
PUB null              
'' This is not a top level object.

PUB attach(instance, name)

  return long[instance][1] := @built_in_0_end - (long[instance]{0} := @built_in_0)
  
PUB detach(instance)

  longfill(instance, 0, res_m)
  
PUB bget(instance, buffer, length) : size
    
  size := long[instance][1] <# length
  bytemove(buffer, long[instance]{0}, size)
  long[instance]{0} += size
  long[instance][1] -= size

PUB binary(location, name)

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

DAT built_in_0  long    'newtest2-7000.sid'             ' built-in binaries
                file    "newtest2-7000.sid"
built_in_0_end
DAT built_in_1  long    'Magic_Disk_64_1991_06.sid'     ' |
                file    "Magic_Disk_64_1991_06.sid"
built_in_1_end
DAT built_in_2  long    'Eskimonika.sid'                ' |
                file    "Eskimonika.sid"
built_in_2_end
DAT