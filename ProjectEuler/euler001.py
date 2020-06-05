#!/bin/python3

import sys

def sum_k(n, k):
    return k * ((n - 1)//k) * ((n - 1)//k + 1) // 2

t = int(input().strip())
for a0 in range(t):
    n = int(input().strip())
    print(sum_k(n, 3) + sum_k(n, 5) - sum_k(n, 15))
