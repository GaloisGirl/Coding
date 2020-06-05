#!/bin/python3

import sys
from math import sqrt, ceil

t = int(input().strip())
for a0 in range(t):
    n = int(input().strip())
    res = 1
    while n // 2 * 2 == n:
        res = 2
        n = n // 2
    i = 3
    while i <= sqrt(n):
        while n // i * i == n:
            res = i
            n = n // i
        i += 2
    print(n if n != 1 else res)
