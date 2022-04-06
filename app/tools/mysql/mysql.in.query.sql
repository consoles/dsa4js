-- 商品分类表初始化
CREATE TABLE `goods_category` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(45) DEFAULT NULL,
  `parent_id` int unsigned NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='商品分类表';

-- 向商品分类表中插入数据
INSERT INTO goods_category(name,parent_id) VALUES('手机类型',0);
INSERT INTO goods_category(name,parent_id) VALUES('CDMA手机',1);
INSERT INTO goods_category(name,parent_id) VALUES('GSM类型',1);
INSERT INTO goods_category(name,parent_id) VALUES('3G手机',1);
INSERT INTO goods_category(name,parent_id) VALUES('双模手机',1);
INSERT INTO goods_category(name,parent_id) VALUES('手机配件',0);
INSERT INTO goods_category(name,parent_id) VALUES('充电器',6);
INSERT INTO goods_category(name,parent_id) VALUES('耳机',6);
INSERT INTO goods_category(name,parent_id) VALUES('电池',6);
INSERT INTO goods_category(name,parent_id) VALUES('内存卡',6);
INSERT INTO goods_category(name,parent_id) VALUES('充值卡',0);
INSERT INTO goods_category(name,parent_id) VALUES('小灵通/固话充值卡',11);
INSERT INTO goods_category(name,parent_id) VALUES('移动充值卡',11);
INSERT INTO goods_category(name,parent_id) VALUES('联通充值卡',11);

-- 初始化商品表
CREATE TABLE goods(
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(45) DEFAULT NULL,
  `cat_id` int unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY idx_cat_id(`cat_id`),
  KEY idx_id_cat_id(`id`,`cat_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='商品表';
-- 向商品表中插入数据
INSERT INTO goods(name,cat_id) VALUES('诺基亚原装充电器',7);
INSERT INTO goods(name,cat_id) VALUES('诺基亚原装立体声耳机',8);
INSERT INTO goods(name,cat_id) VALUES('索尼5800耳机',8);
INSERT INTO goods(name,cat_id) VALUES('金士顿内存卡',10);
INSERT INTO goods(name,cat_id) VALUES('三星电池',9);
INSERT INTO goods(name,cat_id) VALUES('移动50元充值卡',11);
INSERT INTO goods(name,cat_id) VALUES('联通50元充值卡',11);
INSERT INTO goods(name,cat_id) VALUES('三星5800',1);
