const path = require('path')
const KoaRouter = require('koa-router')
const glob = require('glob')

const normalize = require('normalize-path')

const symbolPrefix = Symbol('prefix')

const routersMap = new Map()
const middlewareMap = new Map()

class Router {
  constructor(app, routePath) {
    this.app = app
    this.router = new KoaRouter()
    this.routePath = routePath
  }

  async init() {
    console.debug('-------init-----')
    const pattern = normalize(path.resolve(this.routePath, './*.js'))
    console.debug('scan routers at pattern:', pattern)
    for (const name of glob.sync(pattern)) {
      console.debug('require:', name)
      routersMap.clear()
      const Clazz = require(name)
      console.debug('load controller class:', Clazz)
      // 类方法的装饰器只能拿到方法上的信息，而拿不到类上的信息
      for (const [conf, controllerMethod] of routersMap) {
        const prefix = Clazz.prototype[symbolPrefix]
        const routePath = normalize(path.join(prefix, conf.path))
        console.debug(conf.method, routePath, controllerMethod)
        const middlewares = middlewareMap.get(controllerMethod) ?? []
        const controllerMethods = middlewares.concat(controllerMethod)
        this.router[conf.method](routePath, ...controllerMethods)
      }
      routersMap.clear()
      middlewareMap.clear()
    }
    this.app.use(this.router.routes())
    this.app.use(this.router.allowedMethods())
  }
}

exports.Router = Router;

exports.controller = function (routePrefix) {
  console.debug('routePrefix:', routePrefix);
  return function (controllerClass) {
    controllerClass.prototype[symbolPrefix] = routePrefix;
  };
};

exports.get = function (path) {
  return function (classMethod, ctx, ...args) {
    routersMap.set({ method: 'get', path }, classMethod);
  };
};

exports.post = function (path) {
  return function (classMethod, ctx, ...args) {
    routersMap.set({ method: 'post', path }, classMethod);
  };
};

/**
 * @param {object} rule 
 * @example `@required({ body: ['title', 'price'] })`
 */
exports.required = function (rule) {
  return convert(async (ctx, next) => {
    const errors = []
    for (const [attr, fields] of Object.entries(rule)) {
      for (const field of fields) {
        const value = ctx.request[attr][field]
        if (value === undefined || value === null) {
          errors.push(`request.${attr}.${field}`)
        }
      }
    }
    if (errors.length > 0) {
      ctx.throw(412, `${errors.join(',')} is required.`)
      return
    }
    await next()
  })
}

function convert(middleware) {
  return function (classMethod, ctx, ...args) {
    const middlewares = middlewareMap.get(classMethod) ?? []
    middlewares.push(middleware)
    middlewareMap.set(classMethod, middlewares)
  }
}

let reqId = 0
exports.log = convert(async (ctx, next) => {
  reqId++
  const start = Date.now()
  console.log(new Date(), reqId, '=>', ctx.req.method, ctx.req.url)
  await next()
  console.log(new Date(), reqId, '<=', ctx.res.statusCode, ctx.res.statusMessage, ctx.body, Date.now() - start, 'ms')
})
