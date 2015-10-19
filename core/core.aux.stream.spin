''
'' cog binary stream handler - user interface
''
''        Author: Marko Lukat
'' Last modified: 2015/10/19
''       Version: 0.6
''
CON
  res_m = 2                                             ' UI support
  
PUB null              
'' This is not a top level object.

PUB bget(instance, buffer, length) : size
    
  size := long[instance][1] <# length
  bytemove(buffer, long[instance]{0}, size)
  long[instance]{0} += size
  long[instance][1] -= size

PUB attach(instance, name)

  ifnot strncmp(name, string("built-in:"), 9)
    if strsize(name += 9) == 1
      case byte[name]{0}
        "0": return create(instance, @built_in_0, @built_in_0_end)
        "1": return create(instance, @built_in_1, @built_in_1_end)
        "2": return create(instance, @built_in_2, @built_in_2_end)

  abort

PRI create(instance, s, e)

  long[instance]{0} := s
  return long[instance][1] := e - s

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