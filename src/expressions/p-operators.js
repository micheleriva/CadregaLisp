const operators = [
  {
    cdg: /aggiungi/g,
    js: '+'
  },
  {
    cdg: /leva/g,
    js: '-'
  },
  {
    cdg: /moltiplica/g,
    js: '*'
  },
  {
    cdg: /dividi/g,
    js: '/'
  },
  {
    cgd: /assomiglia a/g,
    js: '=='
  },
  {
    cdg: /è spiccicato a /g,
    js: '==='
  },
  {
    cdg: /è più di/g,
    js: '>'
  },
  {
    cdg: /è meno di/g,
    js: '<'
  },
  {
    cdg: /è di più o uguale a/g,
    js: '>='
  },
  {
    cdg: /è di meno o uguale a/g,
    ks: '<='
  }
];

module.exports = operators
