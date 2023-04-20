const { controller, get, log, required } = require('../core.js')

@controller('/api')
class ApiController {
  @get('/list')
  @log
  async list(ctx) {
    ctx.body = [{
      name: 'schema',
      age: 10
    }]
  }
  
  @log
  @get('/sleep')
  @required({ query: ['time'] })
  async sleep(ctx) {
    const start = new Date()
    await new Promise(resolve => setTimeout(resolve, parseInt(ctx.query.time)))
    ctx.body = {
      start,
      end: new Date()
    }
  }
}

module.exports = ApiController
