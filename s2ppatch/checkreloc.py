#!/usr/bin/python3
import subprocess
import sys
import struct

(original, modified) = sys.argv[1:]

disasm = subprocess.run(['ndisasm', '-b32', original],
                        stdout=subprocess.PIPE,
                        encoding='UTF-8',
                        check=True)

with open(modified, 'rb') as m:
    for line in disasm.stdout.splitlines():
        if line[28:45] == 'mov edx,[dword 0x':
            location = int(line[:8], base=16) + 2
            value = int(line[45:-1], base=16)
        elif line[28:44] == 'mov dx,[dword 0x':
            location = int(line[:8], base=16) + 3
            value = int(line[44:-1], base=16)
        else:
            continue
        m.seek(location)
        found, = struct.unpack('<I', m.read(4))
        if found not in (0, value):
            print(line, '<=>', '%x'%found)
