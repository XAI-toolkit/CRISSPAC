var ranges = Array.from(document.querySelector('.irs').children);

var total = ranges.reduce(function(acc, curr) {
  return acc + Number(curr.value);
}, 0);