pragma solidity ^0.8.13;

contract Person {
    // 约定，属性以 _ 开头
    uint _height; // 身高
    uint _age; // 年龄
    address _owner; // 合约拥有者

    bytes9 public g = 0x123456789abcde;

    mapping (uint => string) students;

    // constructor
    function Person() {
        _height = 180;
        _age = 29;
        _owner = msg.sender; // 合约发起方的地址
        students[0] = "scott";
        students[1] = "jack";
    }

    // 返回一个元组 (tuples)
    function studentNames() constant returns (string name0, string name1) {
        name0 = students[0];
        name1 = students[1];
        return (names0, names1); // 这一句可以不要
    }

    function owner() constant returns (address) {
        return _owner;
    }

    function getBalance(address addr) constant returns (uint) {
        // 钱包转账有2个方法：transfer 和 send，其中send更为低层，transfer更加安全
        return addr.balance; // addr.balance 属性是地址余额
    }

    // 查询当前合约地址余额
    function getCurrentAddressBalance() constant returns (uint) {
        return this.balance;
    }

    // setter
    function setHeight(uint height) {
        _height = height;
    }
    // getter, constant 代表方法只读
    function heiht() constant returns (uint){
        return _height;
    }
    function setAge(uint age) {
        _age = age;
    }
    function age() constant returns (uint) {
        return _age;
    }

    function kill() {
        if (_owner == msg.sender) {
            // 析构
            selfdestruct(_owner);
        }
    }

    function gByteLength() constant returns (uint) {
        return g.length;
    }
}

// https://remix.ethereum.org
// https://faucet.egorfine.com/ 获得以太测试币的地址
// https://kovan.ethplorer.io/address/0xe27d1e50397872601be3603950d93c8ebf4c1140 区块链浏览器
// compile -> deploy
