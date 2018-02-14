#! /usr/bin/env python3

import json
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict
import time
import math

logfile = "rterlang/json.log"

with open(logfile) as f:
    lines = f.readlines()

#nested_dict = lambda: defaultdict(nested_dict)

#taskset = nested_dict()
start = 0
elem = list()
for l in lines:
    j = json.loads(l)
    if 'diff' in j['metadata']:
        print(j['metadata']['diff'])
        elem.append(j['metadata']['diff'])

fig = plt.figure()
ax = fig.add_subplot(111)
#ax.axes.get_yaxis().set_visible(False)

plt.plot(elem)

plt.ylabel("microseconds")

plt.savefig("diff"+str(math.floor(time.time())+".pdf")
plt.show()
