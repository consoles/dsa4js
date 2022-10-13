package main

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/google/uuid"
	"github.com/kataras/iris/v12"
)

// -------------------- BlockChain --------------------

type BlockChain struct {
	CurrentTransactions []Transaction `json:"current_transactions"`
	Chain               []Block       `json:"chain"`
}

type Block struct {
	Index         int           `json:"index"`
	Timestamp     time.Time     `json:"timestamp"`
	Transactions  []Transaction `json:"transactions"`
	Proof         uint64        `json:"proof"`
	PrevBlockHash string        `json:"prevBlockHash"`
}

type Transaction struct {
	Sender    string `json:"sender"`
	Recipient string `json:"recipient"`
	Amount    uint64 `json:"amount"`
}

// golang 中的构造函数没有一定的名字，这种写法是约定俗成的
func NewBlockChain() *BlockChain {
	instance := new(BlockChain)
	instance.NewBlock("I am Genesis Block!", 0) // 在构造函数中生成创世区块
	return instance
}

// 产生一个新区块
func (blockChain *BlockChain) NewBlock(prevBlockHash string, proof uint64) Block {
	block := Block{
		Index:         len(blockChain.Chain) + 1,
		Timestamp:     time.Now(),
		Transactions:  blockChain.CurrentTransactions,
		Proof:         proof,
		PrevBlockHash: prevBlockHash,
	}
	blockChain.Chain = append(blockChain.Chain, block)
	blockChain.CurrentTransactions = blockChain.CurrentTransactions[:0] // 清空切片
	return block
}

func validProof(lastProof uint64, proof uint64) bool {
	guess := strconv.FormatUint(lastProof, 10) + strconv.FormatUint(proof, 10)
	guessHashBytes := sha256.Sum256([]byte(guess))
	guessHash := hex.EncodeToString(guessHashBytes[:])
	log.Println("guessHash:", guessHash)
	return guessHash[:4] == "0000"
}

// 简单的工作量证明（挖矿）
// 查找一个 pC, 使得 hash(pCpP) 以 4 个 0 开头
// pC 是当前证明, pP 是上一个块的证明
func (blockChain *BlockChain) ProofOfWork(lastProof uint64) uint64 {
	proof := 0
	start := time.Now()
	for {
		if validProof(lastProof, uint64(proof)) {
			elapsed := time.Since(start)
			log.Println("success guess hash, cost:", elapsed)
			break
		}
		proof++
	}
	return uint64(proof)
}

// 生成新交易信息，信息将加入到下一个待挖的区块中
func (blockChain *BlockChain) NewTransaction(sender string, recipient string, amount uint64) int {
	blockChain.CurrentTransactions = append(blockChain.CurrentTransactions, Transaction{
		Sender:    sender,
		Recipient: recipient,
		Amount:    amount,
	})
	return blockChain.LastBlock().Index + 1
}

func (blockChain *BlockChain) LastBlock() Block {
	return blockChain.Chain[len(blockChain.Chain)-1]
}

func hash(block map[string]interface{}) string {
	jsonStr, err := json.Marshal(block)
	if err != nil {
		log.Fatal("json stringify error:", err)
	}
	bytes := sha256.Sum256(jsonStr)
	return hex.EncodeToString(bytes[:]) // 将数组转换成切片，转换成16进制
}

// -------------------- Web --------------------

func main() {
	// 测试下挖矿
	blockChain := NewBlockChain()
	// blockChain.ProofOfWork(1)
	nodeId := strings.ReplaceAll(uuid.New().String(), "-", "")

	app := iris.New()

	app.Get("/", func(ctx iris.Context) {
		ctx.HTML("Hello <strong>%s</strong>!", "World")
	})

	// curl -d "sender=aaaaa" -d "recipient=bbb" -d "amount=1000" http://localhost:8080/transactions/new
	app.Post("/transactions/new", func(ctx iris.Context) {
		sender := ctx.PostValue("sender")
		recipient := ctx.PostValue("recipient")
		amount, err := ctx.PostValueInt64("amount")
		if err != nil || len(sender) == 0 || len(recipient) == 0 {
			ctx.StatusCode(iris.StatusBadRequest)
			ctx.JSON(iris.Map{
				"message": "Request params are not valid",
			})
			return
		}
		log.Println("sender", sender, "recipient", recipient, "amount:", amount)
		index := blockChain.NewTransaction(sender, recipient, uint64(amount))
		ctx.StatusCode(iris.StatusCreated)
		ctx.JSON(iris.Map{
			"message": "Transaction will be added to Block" + strconv.Itoa(index),
		})
	})

	// curl http://localhost:8080/mine
	app.Get("/mine", func(ctx iris.Context) {
		lastBlock := blockChain.LastBlock()
		proof := blockChain.ProofOfWork(lastBlock.Proof)
		// 给工作量证明的节点提供奖励，发送者为 "0" 表示新挖出的币
		blockChain.NewTransaction("0", nodeId, 1)
		block := blockChain.NewBlock("", proof)
		ctx.JSON(iris.Map{
			"message":         "New Block Forged",
			"index":           block.Index,
			"transactions":    block.Transactions,
			"proof":           proof,
			"prev_block_hash": block.PrevBlockHash,
		})
	})

	// curl http://localhost:8080/chain
	app.Get("/chain", func(ctx iris.Context) {
		ctx.JSON(iris.Map{
			"chain":                blockChain.Chain,
			"pending_transactions": blockChain.CurrentTransactions,
			"length":               len(blockChain.Chain),
		})
	})

	app.Listen("127.0.0.1:8080")
}
