# golang 从零开始开发区块链

## 单节点区块链

```bash
go run main.go
curl http://localhost:8080/chain # 查看区块链
curl -d "sender=aaaaa" -d "recipient=bbb" -d "amount=1000" http://localhost:8080/transactions/new # 生成区块
curl http://localhost:8080/mine # 挖矿
curl http://localhost:8080/chain # 查看区块链
```

## 多节点

```bash
# 启动节点
go run main.go -p 2001
go run main.go -p 2002
# 互相注册节点
curl -d '["localhost:2001","localhost:2002"]' http://localhost:2001/nodes/register -H "content-type: application/json"
curl -d '["localhost:2001","localhost:2002"]' http://localhost:2002/nodes/register -H "content-type: application/json"
# 节点1生成区块并挖矿
curl -d "sender=aaaaa" -d "recipient=bbb" -d "amount=1000" http://localhost:2001/transactions/new
curl http://localhost:2001/mine
# 查看2个节点上的链详情
curl http://localhost:2001/chain
curl http://localhost:2002/chain
# 可以发现节点1上的链更长，接下来我们解决冲突
curl http://localhost:2001/nodes/resolve # 这个链是权威链，不会被替换
curl http://localhost:2002/nodes/resolve # 由于比较短，将会被节点1上的链替换
# 再次查看2个节点上的链详情（可以发现分叉解决了）
curl http://localhost:2001/chain
curl http://localhost:2002/chain
```
