// 基于 zk 的服务注册中心
// 提供服务注册、服务发现、简单的负载均衡

const ZooKeeper = require('zookeeper');
const _ = require('lodash');

/**
 * 服务注册
 */
class ServiceRegistry {
    constructor() {
        this.zkClient = new ZooKeeper({
            connect: '114.67.218.152:2181',
            timeout: 5000,
            debug_level: ZooKeeper.constants.ZOO_LOG_LEVEL_DEBUG,
            host_order_deterministic: false
        });
        this.zkClient.init();
        console.log('Starting ServiceRegistry.');
        this.registryRoot = '/registry';
    }
    async init() {
        const exists = await this.zkClient.pathExists(this.registryRoot, false);
        if (!exists) {
            await this.zkClient.create(this.registryRoot, '', ZooKeeper.constants.ZOO_PERSISTENT);
        }
    }
    /**
     * @param {string} serviceName 
     * @param {string} serviceAddr 
     */
    async register(serviceName, serviceAddr) {
        const servicePath = `${this.registryRoot}/${serviceName}`;
        const exists = await this.zkClient.pathExists(servicePath, false);
        if (!exists) {
            // `/registry/serviceA` 持久化节点
            await this.zkClient.create(servicePath, '', ZooKeeper.constants.ZOO_PERSISTENT);
        }
        const addrPath = `${servicePath}/${serviceAddr}`;
        // `/registry/serviceA/localhost:3001` 临时节点
        await this.zkClient.create(addrPath, '', ZooKeeper.constants.ZOO_EPHEMERAL);
        console.log(`service: ${serviceName} - ${addrPath} registered!`);
    }
}

class LoadBalance {
    /**
     * @param {string[]} serviceRepos 
     */
    selectAddr(serviceRepos) {
        throw new Error('interface not implemented')
    }
}

class AbstractLoadBalance extends LoadBalance {
    /**
     * @param {string[]} serviceRepos 
     * @returns {string | null}
     */
    selectAddr(serviceRepos) {
        if (!serviceRepos || serviceRepos.length === 0) {
            return null;
        }
        if (serviceRepos.length === 1) {
            return serviceRepos[0];
        }
        return this.doSelect(serviceRepos);
    }
    /**
     * @protected
     */
    doSelect(serviceRepos) {
        throw new Error('abstract method not implemented')
    }
}

class RandomLoadBalance extends AbstractLoadBalance {
    /**
     * @param {string[]} serviceRepos
     */
    doSelect(serviceRepos) {
        return _.sample(serviceRepos);
    }
}

/**
 * 服务发现
 */
class ServiceDiscovery {
    constructor() {
        this.zkClient = new ZooKeeper({
            connect: '114.67.218.152:2181',
            timeout: 3000,
            debug_level: ZooKeeper.constants.ZOO_LOG_LEVEL_INFO,
            host_order_deterministic: false
        });
        this.zkClient.init();
        this.registryRoot = '/registry';
        this.serviceRepos = [];
    }
    /**
     * @param {string} serviceName 
     */
    async init(serviceName) {
        const servicePath = `${this.registryRoot}/${serviceName}`;
        this.serviceRepos = await this.zkClient.get_children(servicePath);
        this.registerWatcher(servicePath);
    }
    getServiceEndpoint() {
        const loadBalance = new RandomLoadBalance();
        return loadBalance.selectAddr(this.serviceRepos);
    }
    /**
     * @private
     * @param {string} path 
     */
    async registerWatcher(path) {
        // 节点上只能注册一次监听器，这里在回调函数中再次对节点注册监听器
        // 参考 node_modules/zookeeper/examples/addlistener.js
        const children = await this.zkClient.w_get_children(path, (type, state, path) => this.registerWatcher(path));
        console.log('path:', path, 'children:', children);
        this.serviceRepos = children;
    }
}

async function main() {
    const serviceRegistry = new ServiceRegistry();
    await serviceRegistry.init();
    await serviceRegistry.register('product-service', 'localhost:3001');

    const serviceDiscovery = new ServiceDiscovery();
    await serviceDiscovery.init('product-service');
    // 添加一个服务节点，看看能否实现自动发现
    setInterval(() => {
        console.log('service addr:', serviceDiscovery.getServiceEndpoint())
    }, 2000)
    setTimeout(async () => {
        await serviceRegistry.register('product-service', 'localhost:3002');
    }, 3000)
    setTimeout(async () => {
        await serviceRegistry.register('product-service', 'localhost:3003');
    }, 5000)
}

main().then(() => {
    console.log('done')
}).catch(e => {
    console.error('error:', e)
})
