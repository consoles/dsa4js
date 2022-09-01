# 相关框架 

## truffle

### HelloWorld

```bash
mkdir MetaCoin
cd MetaCoin
truffle unbox metacoin
```

```bash
truffle develop
```

### EncryptedToken

建立简易加密代币

```bash
mkdir EncryptedToken
cd EncryptedToken
truffle init
```

编写 contracts/EncryptedToken.sol

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.13;

contract EncryptedToken {
    uint256 INITIAL_SUPPLY = 666666;
    mapping(address => uint256) balances;

    constructor() {
        balances[msg.sender] = INITIAL_SUPPLY;
    }

    function transfer(address to, uint256 amount) public {
        assert(balances[msg.sender] >= amount);
        balances[msg.sender] -= amount;
        balances[to] += amount;
    }

    function balanceOf(address owner) public view returns (uint256) {
        return balances[owner];
    }
}
```

编写 migrations/1_deploy_contracts.js

```js
const EncryptedToken = artifacts.require("EncryptedToken");

module.exports = function (deployer) {
  deployer.deploy(EncryptedToken);
};

```

```powershell
D:\code\nodejs\dsa4js\app\solidity\EncryptedToken>truffle develop
Truffle Develop started at http://127.0.0.1:9545/

Accounts:
(0) 0x3fc0d35cdb437d369bea0da0ec6bd972622e3fff
(1) 0xed8a351f997c656bddd5e82f64c172320aea4095
(2) 0x131f04a6990e919c819bb90d1d379a8ac1ee1782
(3) 0xe71d1dff8f1355eac03db7fd957ec7c567dc7991
(4) 0x7e8207496b69c01870416af6affb3e42a180cf0f
(5) 0xaf3353408ca0674274085407b6aab2f155a3ac2a
(6) 0x5fb22b809fe7516785d3c48c616cec6b5565c0d3
(7) 0xbd683b1ae253c46e294df3565daf2f24fd8e1324
(8) 0xb0871fdbbbf6861184576ea64e5df2bce88c2086
(9) 0x3b96a0ad3d272732b5bc7a54e0cc853642cc67c9

Private Keys:
(0) 9c0c4ec6a21a04fe931df2f3ab7e45652d4156a52e3b8cdd7269c9914c5571b3
(1) e5ff4c174cceaf34afc441182e9e024ec3305d1ad073469480ea1172f22273a3
(2) 9eca4a0cffe2aa80f40efd68c15fd395c3baba909eb6c79e1726a8ebbc151ff1
(3) 0d73cbc3c23b2b17d6881a8fbde136751df82552e0cb64a38f2e78192931a262
(4) e4f5731c0ee641b82fa8923114a94e22d66349349520e6897ab8c665c6876265
(5) 6a36e47b413ff5ba9dbfb60fc608e370e6e02c107b08da73af6b3e04836daadb
(6) 2048a4294b640189dea4430d0b5ce7abf6073a10ed84588063befc8bf6b0fedd
(7) f2e2437fd56ba12e3ff12b7ce6777705528880313fde537116c03ccacda6b006
(8) a7601e5fe8d45ed477e905adf84e8b70a7968cd42d0cb2babd6ecd07ccad0711
(9) 9f2831502f03ff2eda4467e9e690f851bdabe1014279f5c3174ec7d46c363e2f

Mnemonic: point lucky popular lab immune hawk away vast box grab rebel decade

⚠️  Important ⚠️  : This mnemonic was created for you by Truffle. It is not secure.
Ensure you do not use it on production blockchains, or else you risk losing funds.
truffle(develop)> migrate
truffle(develop)> let contract
undefined
truffle(develop)> EncryptedToken.deployed().then(instance => contract = instance)
truffle(develop)> contract.balanceOf('0x3fc0d35cdb437d369bea0da0ec6bd972622e3fff')
BN {
  negative: 0,
  words: [ 666666, <1 empty item> ],
  length: 1,
  red: null
}
truffle(develop)> contract.balanceOf('0xed8a351f997c656bddd5e82f64c172320aea4095')
BN { negative: 0, words: [ 0, <1 empty item> ], length: 1, red: null }
truffle(develop)> contract.transfer('0xed8a351f997c656bddd5e82f64c172320aea4095', 111111)
{
  tx: '0xf69dc9b5ce7e8b4d8e4983d1440b4cb04e07c1cbb4e9e10a6a13740d731dfe38',
  receipt: {
    transactionHash: '0xf69dc9b5ce7e8b4d8e4983d1440b4cb04e07c1cbb4e9e10a6a13740d731dfe38',
    transactionIndex: 0,
    blockNumber: 4,
    blockHash: '0xb60f2b5dc2a81ae599533919ce6e984900710a2a7d45bae018e0d8488bbe927f',
    from: '0x3fc0d35cdb437d369bea0da0ec6bd972622e3fff',
    to: '0xa3b6c59eb090812627e51463ecbaa4fff8a6b2ae',
    cumulativeGasUsed: 50148,
    gasUsed: 50148,
    contractAddress: null,
    logs: [],
    logsBloom: '0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000',
    status: true,
    effectiveGasPrice: 3102772664,
    type: '0x2',
    rawLogs: []
  },
  logs: []
}
truffle(develop)> contract.balanceOf('0xed8a351f997c656bddd5e82f64c172320aea4095')
BN {
  negative: 0,
  words: [ 111111, <1 empty item> ],
  length: 1,
  red: null
}
truffle(develop)> contract.balanceOf('0x3fc0d35cdb437d369bea0da0ec6bd972622e3fff')
BN {
  negative: 0,
  words: [ 555555, <1 empty item> ],
  length: 1,
  red: null
}
```
