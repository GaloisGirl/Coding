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
    const result = lines.reduce((acc, val) => Object.assign(acc, {prev: val, num: acc.num + (acc.prev < val ? 1 : 0) }), {prev: lines[0], num: 0})
    console.log(result)
})