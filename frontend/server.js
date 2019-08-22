const express = require('express')
const cors = require('cors');
const proxy = require('http-proxy-middleware')
const app = express()

app.use(cors());
// app.options('*', cors())

app.get('/', function (req, res) {
  res.send('Hello World')
})

app.use('/api', proxy({ target: 'http://109.228.61.185:12000', changeOrigin: true }))

app.listen(3000)