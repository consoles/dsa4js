// InnoDB 聚簇索引 插入数据测试
// InnoDB的聚簇索引指的是主键索引和数据存储在一起，所以乱序插入会导致索引节点的分裂（调整B+树的节点），从而导致数据的移动，性能急剧下降

// get the client
const mysql = require('mysql2/promise');
const randomstring = require('randomstring');

const tbl1Sql = `
CREATE table IF NOT EXISTS tbl_seq(
    id int,
    name1 VARCHAR(300),
    name2 VARCHAR(300),
    name3 VARCHAR(300),
    name4 VARCHAR(300),
    name5 VARCHAR(300),
    name6 VARCHAR(300),
    name7 VARCHAR(300),
    name8 VARCHAR(300),
    name9 VARCHAR(300),
    PRIMARY KEY(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
`;

const tbl2Sql = `
CREATE table IF NOT EXISTS tbl_random(
    id int,
    name1 VARCHAR(300),
    name2 VARCHAR(300),
    name3 VARCHAR(300),
    name4 VARCHAR(300),
    name5 VARCHAR(300),
    name6 VARCHAR(300),
    name7 VARCHAR(300),
    name8 VARCHAR(300),
    name9 VARCHAR(300),
    PRIMARY KEY(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
`;

(async () => {
    // 创建2张表，第一张表用于测试顺序插入，第二张表用于随机插入
    // create the connection to database
    const connection = await mysql.createConnection({
        host: 'localhost',
        user: 'root',
        password: '123456',
        database: 'test'
    });
    await connection.execute(tbl1Sql);
    await connection.execute(tbl2Sql);
    const count = 10000;
    const seqSqls = [];
    const randomSqls = [];
    for (let i = 0; i < count; i++) {
        seqSqls.push(`INSERT INTO tbl_seq(id,name1,name2,name3,name4,name5,name6,name7,name8,name9) VALUES(${i + 1},'${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}');`);
        randomSqls.push(`INSERT INTO tbl_random(id,name1,name2,name3,name4,name5,name6,name7,name8,name9) VALUES(${i + 1000},'${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}','${randomstring.generate(200)}');`);
    }
    randomSqls.sort(() => Math.random() - 0.5);
    let start = Date.now();
    for (let i = 0; i < count; i++) {
        await connection.execute(seqSqls[i]);
    }
    let end = Date.now();
    console.log(`顺序插入${count}条数据耗时${end - start}ms`);
    start = Date.now();
    for (let i = 0; i < count; i++) {
        await connection.execute(randomSqls[i]);
    }
    end = Date.now();
    console.log(`随机插入${count}条数据耗时${end - start}ms`);
})().catch(e => console.error(e));