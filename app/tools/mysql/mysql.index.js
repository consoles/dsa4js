const mysql = require('mysql2/promise');
const randomstring = require('randomstring');

async function initData(conn, engineName) {
    const tblName = `tbl_${engineName}`;
    const sql = `
    CREATE table IF NOT EXISTS ${tblName}(
        id char(64) PRIMARY KEY,
        ver int not null default 0,
        str1 VARCHAR(3000),
        str2 VARCHAR(3000),
        str3 VARCHAR(3000),
        KEY idx_id_ver(id,ver)
    ) ENGINE=${engineName} charset utf8;
    `;
    await conn.execute(sql);
    const count = 10000;
    for (let i = 0; i < count; i++) {
        const sql = `INSERT INTO ${tblName}(id,ver,str1,str2,str3) VALUES('${randomstring.generate(64)}',${i + 1},'${randomstring.generate(3000)}','${randomstring.generate(3000)}','${randomstring.generate(3000)}');`;
        await conn.execute(sql);
    }
}

(async () => {
    const connection = await mysql.createConnection({
        host: 'localhost',
        user: 'root',
        password: '123456',
        database: 'test'
    });
    let start = Date.now();
    await initData(connection, 'myisam');
    let end = Date.now();
    console.log(`myisam插入1W数据耗时${end - start}ms`);
    start = Date.now();
    await initData(connection, 'innodb');
    end = Date.now();
    console.log(`innodb插入1W数据耗时${end - start}ms`);
    await connection.end();
})().then(() => {
    console.log('done');
}).catch(e => console.error(e));