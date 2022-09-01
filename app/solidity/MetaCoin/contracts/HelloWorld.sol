// SPDX-License-Identifier: MIT
// Tells the Solidity compiler to compile only from v0.8.13 to v0.9.0
pragma solidity ^0.8.13;

contract HelloWorld {
    function helloWorld() public pure returns (string memory){
        return "HelloWorld";
    }
    function helloWorld2() public returns (string memory){
        return "HelloWorld2";
    }
    function echo(string memory s) public pure returns (string memory){
        return s;
    }
}
