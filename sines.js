function sine(len, i) {
  return Math.sin(i/len * Math.PI * 2.0);
}

module.exports = {
  sine01: ({}, center, amp, len) => {
    return Array(len).fill(0).map((v,i) => center + Math.min(0, amp * sine(len, i)));
  }
}
