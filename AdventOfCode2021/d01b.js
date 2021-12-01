const readline = require('readline')
const fs = require('fs')

var lines = []

const readInterface = readline.createInterface({
    input: fs.createReadStream('./AdventOfCode2021/input.txt'),
})

readInterface.on('line', function(line) {
    lines.push(parseInt(line))
})

readInterface.on('close', function() {
    const result = lines.slice(3).reduce((acc, val) => Object.assign(acc, {
        currSum: acc.currSum + val - acc.prev0,
        prev0: acc.prev1,
        prev1: acc.prev2,
        prev2: val,
        num: acc.num + (acc.currSum + val - acc.prev0  > acc.currSum ? 1 : 0) 
    }), {
        currSum: lines[0] + lines[1] + lines[2], 
        prev0: lines[0],
        prev1: lines[1],
        prev2: lines[2],
        num: 0
    })
    console.log(result)
})