#! /usr/bin/env python3

import json
import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict

logfile = "rterlang/json.log"

with open(logfile) as f:
    lines = f.readlines()

nested_dict = lambda: defaultdict(nested_dict)

taskset = nested_dict()
start = 0
elem = list()
for l in lines:
    j = json.loads(l)
    elem.append(j)
    if 'rt_start' in j['metadata']:
        start = j['metadata']['rt_start']
    if 'rt_dispatch' in j['metadata']:
        taskset[ str(j['metadata']['rt_dispatch']) ]['rt_dispatch'] = j['metadata']['now'] - start
        taskset[ str(j['metadata']['rt_dispatch']) ]['rt_deadline'] = j['metadata']['rt_deadline'] - start
    if 'rt_run' in j['metadata']:
        taskset[ str(j['metadata']['rt_run']) ]['rt_run'][ j['metadata']['now'] - start ] = True
    if 'rt_sleep' in j['metadata']:
        taskset[ str(j['metadata']['rt_sleep']) ]['rt_sleep'][ j['metadata']['now'] - start ] = True
    if 'rt_yield' in j['metadata']:
        taskset[ str(j['metadata']['rt_yield']) ]['rt_yield'][ j['metadata']['now'] - start ] = True
    if 'rt_done' in j['metadata']:
        taskset[ str(j['metadata']['rt_done']) ]['rt_done'] = j['metadata']['now'] -start

fig = plt.figure()
ax = fig.add_subplot(111)
ax.axes.get_yaxis().set_visible(False)
for task_id, task in taskset.items():
    print(task)
    for timestamp_run, rt_run in task['rt_run'].items():
        next_sleep = 0
        for timestamp_sleep, rt_sleep in task['rt_sleep'].items():
            if timestamp_sleep > timestamp_run:
                next_sleep = timestamp_sleep
                break
        x = [timestamp_run, next_sleep]
        if next_sleep == 0:
            x[1] = task['rt_done']
        y1 = np.array([int(task_id), int(task_id)])
        y2 = y1 + 1
        print(x)
        plt.fill_between(x, y1, y2=y2, color='cyan')
        plt.text(np.average(x), np.average([y1, y2]), "T"+task_id,
                 horizontalalignment='center', verticalalignment='center')

    # dispatch
    plt.scatter(task['rt_dispatch'], int(task_id) + 0.5, s=70, color='green')
    # deadline
    plt.scatter(task['rt_deadline'], int(task_id) + 0.75, s=70, marker='x', color='red')
    
    # yields
    for timestamp, rt_yield in task['rt_yield'].items():
        plt.text(timestamp, int(task_id) + 0.25, "yield()",
                 rotation=60,
                 bbox=dict(boxstyle="round", color="orange"),
                 horizontalalignment='center', verticalalignment='center')
plt.xlabel("milliseconds")
plt.ylim(1,4)
plt.savefig("schedule.pdf")
plt.show()
