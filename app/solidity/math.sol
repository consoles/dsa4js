// SPDX-License-Identifier: MIT
pragma solidity ^0.8.13;

contract Math {
    function sum(uint256 a, uint256 b) public pure returns (uint256) {
        return a + b;
    }
}

// npm i -dg solc
// solcjs --abi math.sol #abi
// solcjs --bin math.sol #bytecode
