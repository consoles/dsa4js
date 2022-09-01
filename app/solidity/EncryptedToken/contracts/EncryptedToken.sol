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
