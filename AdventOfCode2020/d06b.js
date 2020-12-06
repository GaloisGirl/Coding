const readline = require('readline');
const fs = require('fs');

const A_INDEX = "a".charCodeAt(0);

let lines = [];

function getClearAnswers() {
  return Array(26).fill(0)
}

const readInterface = readline.createInterface({
    input: fs.createReadStream('./example.txt'),
});

readInterface.on('line', function(line) {
  lines.push(line)
});

readInterface.on('close', function() {
  const result = lines.reduce((res, line) => {
    if (line.length == 0) {
      if (res.groupSize == 0) {
        return res
      }
      return {
        total: res.total + Object.values(res.groupAnswers).reduce((acc, x) => acc + (x == res.groupSize ? 1 : 0), 0),
        groupSize: 0,
        groupAnswers: getClearAnswers()
      }
    }
    const indices = line.split('').map(l => l.charCodeAt(0) - A_INDEX)
    return {
      total: res.total,
      groupSize: res.groupSize + 1,
      groupAnswers: indices.reduce((acc, x) => acc.fill(acc[x] + 1, x, x + 1), res.groupAnswers)
    }
  }, {
    total: 0,
    groupSize: 0,
    groupAnswers: getClearAnswers()
  });

  console.log(result.total)
})