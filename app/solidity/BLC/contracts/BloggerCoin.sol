// SPDX-License-Identifier: MIT
pragma solidity ^0.8.13;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract BloggerCoin is ERC20 {
    constructor() ERC20("BloggerCoin", "BLC") {
        _mint(msg.sender, 1000);
    }
}
