// 基于 zookeeper 的分布式 ID 解决方案

const ZooKeeper = require('zookeeper');
const _ = require('lodash');

class IDGenerator {
    constructor() {
        this.zkClient = new ZooKeeper({
            connect: '114.67.218.152:2181',
            timeout: 5000,
            debug_level: ZooKeeper.constants.ZOO_LOG_LEVEL_DEBUG,
            host_order_deterministic: false
        });
        this.zkClient.init();
        this.root = '/root';
        this.nodeName = 'node1';
        this.basePath = `${this.root}/${this.nodeName}`;
    }
    async init() {
        let exists = await this.zkClient.pathExists(this.root, false);
        if (!exists) {
            await this.zkClient.create(this.root, '', ZooKeeper.constants.ZOO_PERSISTENT);
        }
        exists = await this.zkClient.pathExists(this.basePath, false);
        if (!exists) {
            await this.zkClient.create(this.basePath, '', ZooKeeper.constants.ZOO_PERSISTENT);
        }        
    }
    async generate() {
        console.log(`basePath: ${this.basePath}`);
        const nodePath = await this.zkClient.create(this.basePath + '/', '', ZooKeeper.constants.ZOO_EPHEMERAL_SEQUENTIAL);
        console.log(`nodePath: ${nodePath}`);
        return nodePath;
    }
}

async function main() {
    const idGenerator = new IDGenerator();
    await idGenerator.init();
    let id = await idGenerator.generate();
    console.log(`id: ${id}`);
    id = await idGenerator.generate();
    console.log(`id: ${id}`);
}

main().then(() => {
    console.log('done')
}).catch(e => {
    console.error('error:', e)
})
