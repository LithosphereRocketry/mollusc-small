#!/usr/bin/python3

import os, sys
import itertools

for fn in sys.argv[1:]:
    with open(fn, "rb") as f:
        outname = fn + ".txt"
        print(outname)
        with open(outname, "w") as o:
            words = 0
            for word in itertools.batched(f.read(), 4):
                words += 1
                n = int.from_bytes(bytes(list(word)), 'little')
                o.write(f"{n:032b}\n")
            while words < 8192:
                o.write(f"{0xdeadb105:032b}\n")
                words += 1