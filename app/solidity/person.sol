// SPDX-License-Identifier: MIT
pragma solidity ^0.8.13;

contract Person {
    // 约定，属性以 _ 开头
    uint256 _height; // 身高
    uint256 _age; // 年龄
    address _owner; // 合约拥有者

    bytes9 public g = 0x111111111111111111;

    mapping(uint256 => string) students;

    // constructor
    constructor() {
        _height = 180;
        _age = 29;
        _owner = msg.sender; // 合约发起方的地址
        students[0] = "scott";
        students[1] = "jack";
    }

    // 返回一个元组 (tuples)
    function studentNames()
        public
        returns (string memory name0, string memory name1)
    {
        name0 = students[0];
        name1 = students[1];
    }

    function owner() public returns (address) {
        return _owner;
    }

    function getBalance(address addr) public returns (uint256) {
        // 钱包转账有2个方法：transfer 和 send，其中send更为低层，transfer更加安全
        return addr.balance; // addr.balance 属性是地址余额
    }

    // 查询当前合约地址余额
    function getCurrentAddressBalance() public returns (uint256) {
        return msg.sender.balance;
    }

    // setter
    function setHeight(uint256 height) public {
        _height = height;
    }

    // getter, constant 代表方法只读
    function heiht() public returns (uint256) {
        return _height;
    }

    function setAge(uint256 age) public {
        _age = age;
    }

    function age() public returns (uint256) {
        return _age;
    }

    function kill() public {
        if (_owner == msg.sender) {
            // 析构
            selfdestruct(_owner);
        }
    }

    function gByteLength() public returns (uint256) {
        return g.length;
    }
}

// https://remix.ethereum.org
// https://faucet.egorfine.com/ 获得以太测试币的地址
// https://kovan.ethplorer.io/address/0xe27d1e50397872601be3603950d93c8ebf4c1140 区块链浏览器
// compile -> deploy
