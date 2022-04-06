const Mock = require('mockjs');
const mysql = require('mysql2/promise');
const log4js = require("log4js");

const Random = Mock.Random;
const logger = log4js.getLogger();
logger.level = "debug";

(async () => {
    const connection = await mysql.createConnection({
        host: 'localhost',
        user: 'root',
        password: '123456',
        database: 'test'
    });
    const count = 1000000;
    const batchSize = 5000;
    let promises = [];
    logger.info('start generate data');
    for (let i = 0; i < count; i++) {
        const sql = `INSERT INTO goods(\`name\`,cat_id) VALUES('${Random.cword(40)}',${Random.integer(0, 11)});`;
        promises.push(connection.query(sql));
        if (i > 0 && (i % batchSize === 0)) {
            await Promise.all(promises);
            logger.info('complate: ', i, '/', count);
            promises = [];
        }
    }
    await connection.end();
})().then(() => logger.info('done')).catch(e => logger.error(e));    