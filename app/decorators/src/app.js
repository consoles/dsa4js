const path = require('path')
const Koa = require('koa')
const bodyParser = require('koa-bodyparser');

const app = new Koa()

const { Router } = require('./core.js')

const conf = { host: 'localhost', port: 3000, routePath: path.join(__dirname, 'routes') }

async function main() {
    app.use(bodyParser())
    const router = new Router(app, conf.routePath)
    await router.init()
    // fallback response
    app.use(async (ctx, next) => {
        ctx.status = 200
        ctx.body = {
            message: 'Hello World!',
            time: new Date()
        }
    })
    app.listen(conf.port, conf.host)
    console.log('Server listening on ' + conf.host + ':' + conf.port)
}

main()
