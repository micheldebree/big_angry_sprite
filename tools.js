function lohi(n) {
  return [n & 0xff, (n >> 8) & 0xff]
}

function loBytes(s) {
  return s.map(b => b & 0xff);
}

function hiBytes(s) {
  return s.map(b => (b >> 8) & 0xff);
}

module.exports = {
  lo: ({}, n) => {
    return n & 0xff;
  },
  hi: ({}, n) => {
    return (n >> 8)  & 0xff;
  }
}

