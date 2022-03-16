const mysql = require('mysql2/promise');
const Mock = require('mockjs');
const Random = Mock.Random;

const sql = `
CREATE table IF NOT EXISTS tbl_url(
    id int AUTO_INCREMENT PRIMARY KEY,
    url varchar(50) not null default ''
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
`;

(async () => {
    const connection = await mysql.createConnection({
        host: 'localhost',
        user: 'root',
        password: '123456',
        database: 'test'
    });
    await connection.execute(sql);
    // init data 
    const count = 100000;
    for (let i = 0; i < count; i++) {
        const sql = `INSERT INTO tbl_url(url) VALUES('${Random.url('http')}')`;
        await connection.execute(sql);
    }
    await connection.end();
    // ALTER table tbl_url ADD crc_url int unsigned not null default 0;
    // UPDATE tbl_url SET crc_url = crc32(url);
    // ALTER table tbl_url ADD index idx_url(url(16));
    // ALTER table tbl_url ADD index idx_crc_url(crc_url);
})().then(() => console.log('done')).catch(e => console.error(e));    