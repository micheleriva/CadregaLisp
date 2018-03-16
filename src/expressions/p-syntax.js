const syntax = [
  {
    string: /(["'])(?:(?=(\\?))\2.)*?\1/g
  },
  {
    outerBrackets: /\(.+\)/g
  },
  {
    squareBrackets: /\[.+\]/g
  },
  {
    curlyBrackets: /\{.+\}/g
  }
];

module.exports = syntax
