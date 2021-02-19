function asc2int(asc) {
  return asc.charCodeAt(0)
}

function convertAsciiToScreencode(asc) {
    if (asc.length !== 1) {
        return null
    }
    if (asc >= 'a' && asc <= 'z') {
        return asc2int(asc) - asc2int('a') + 1
    }
    if (asc == 'ä') {
        return 1;
    }
    if (asc == 'ö') {
        return asc2int('o') - asc2int('a') + 1
    }
    if (asc >= 'A' && asc <= 'Z') {
        return asc2int(asc) - asc2int('A') + 0x41
    }
    if (asc >= '0' && asc <= '9') {
        return asc2int(asc) - asc2int('0') + 0x30
    }
    const otherChars = {
        '@': 0,
        ' ': 0x20,
        '!': 0x21,
        '"': 0x22,
        '#': 0x23,
        '$': 0x24,
        '%': 0x25,
        '&': 0x26,
        '(': 0x28,
        ')': 0x29,
        '*': 0x2a,
        '+': 0x2b,
        ',': 0x2c,
        '-': 0x2d,
        '.': 0x2e,
        '/': 0x2f,
        ':': 0x3a,
        ';': 0x3b,
        '<': 0x3c,
        '=': 0x3d,
        '>': 0x3e,
        '?': 0x3f
    }
    if (asc in otherChars) {
        return otherChars[asc]
    }
    return null
}

module.exports = ({}, str) => {
    const arr = [];
    const s = str.toLowerCase();
    for (let c in s) {
        arr.push(convertAsciiToScreencode(s[c]));
    }
    return arr
}
