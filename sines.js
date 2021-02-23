function sine(len, i) {
  return Math.sin(i/len * Math.PI * 2.0);
}

module.exports = {
  sine01: ({}, center, amp, len) => {
    return Array(len).fill(0).map((v,i) => center + Math.min(0, amp * sine(len, i)));
  },
  sine02: ({}, center, amp, len) => {
    return Array(len).fill(0).map((v,i) => center + Math.max(0, amp * sine(len, i)));
  },
  sine03: ({}, center, amp, len) => {
    return Array(len).fill(0).map((v,i) => center + amp * sine(len, i));
  },
  sine04: ({}, center, amp, len) => {
    return Array(len).fill(0).map((v,i) => center - Math.abs(amp * sine(len, i)));
  },
  sine05: ({}, center, amp, len) => {
    return Array(len).fill(0).map((v,i) => center + Math.abs(amp * sine(len, i)));
  }
}
