// 测试mysql字符串索引的区分度

const mysql = require('mysql2/promise');
const randomstring = require('randomstring');

const sql = `
    CREATE table IF NOT EXISTS tbl_char_index(
        id int AUTO_INCREMENT PRIMARY KEY,
        word varchar(512)
    ) ENGINE=innodb charset utf8;
    `;

(async () => {
    const connection = await mysql.createConnection({
        host: 'localhost',
        user: 'root',
        password: '123456',
        database: 'test'
    });
    await connection.execute(sql);
    const count = 500000; // 插入50W行数据
    for (let i = 0; i < count; i++) {
        const sql = `INSERT INTO tbl_char_index(word) VALUES('${randomstring.generate(512)}')`;
        await connection.execute(sql);
    }
    const precision = 0.99;
    // 需要在word字段上建立索引，问题是索引长度为多少的时候可以保证区分度为99%？
    // 进行以下的实际测试
    let len = 1;
    while (true) {
        const sql = `SELECT COUNT(distinct left(word,${len})) / COUNT(*) AS p FROM tbl_char_index`;
        const [rows] = await connection.execute(sql);
        const p = rows && rows[0].p;
        console.log(len, p);
        if (p >= precision) {
            break;
        }
        len++;
    }
    // ALTER table tbl_char_index ADD INDEX idx_word(word(5));
    console.log(`字符串索引的长度取${len}，即可以达到${precision}的区分度`);
    await connection.end();
})().then(() => console.log('done')).catch(e => console.error(e));    