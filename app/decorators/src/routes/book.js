const { controller, get, post, required } = require('../core.js')

const books = []

@controller('/book')
class BookController {
  @get('/version')
  version(ctx) {
    ctx.body = {
      version: '2.2.0.beta',
      time: new Date()
    }
  }
  @required({ body: ['title', 'price'] })
  @post('/add')
  add(ctx) {
    const { title, price } = ctx.request.body
    books.push({
      title,
      price
    })
    ctx.body = books
  }
}

module.exports = BookController
