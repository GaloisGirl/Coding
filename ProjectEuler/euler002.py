 #!/bin/python3

import sys

t = int(input().strip())
for _ in range(t):
    n = int(input().strip())
    x, y, sum = 1, 1, 0
    while x + y <= n:
        sum += x + y
        x, y = x + 2 * y, 2 * x + 3 * y
    print(sum)