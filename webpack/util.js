const path = require('path')

const Color = {
  blue: '\x1b[34m',
  red: '\x1b[31m',
  green: '\x1b[32m',
}
exports.Color = Color

const print = (msg, color = Color.blue) => { console.log(`${color}%s\x1b[0m`, msg) }
exports.print = print

const build = () => process.env.WEBPACK_ENV || 'dev'
exports.build = build
const root = (...args) => path.resolve(__dirname, '..', ...args)
exports.root = root
const src = (...args) => root('src', ...args)
exports.src = src
const dist = (...args) => root('dist', ...args)
exports.dist = dist
