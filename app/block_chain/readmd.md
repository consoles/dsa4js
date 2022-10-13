# golang 从零开始开发区块链

```bash
go run main.go
curl http://localhost:8080/chain # 查看区块链
curl -d "sender=aaaaa" -d "recipient=bbb" -d "amount=1000" http://localhost:8080/transactions/new # 生成区块
curl http://localhost:8080/mine # 挖矿
curl http://localhost:8080/chain # 查看区块链
```
