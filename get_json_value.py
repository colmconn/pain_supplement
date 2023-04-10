#!/usr/bin/env python3

import json
import sys

with open(sys.argv[2], 'r') as jsonFile:
    try:
        bidsSidecar=json.load(jsonFile)
    except:
        sys.exit(1)

print(bidsSidecar[sys.argv[1]])
