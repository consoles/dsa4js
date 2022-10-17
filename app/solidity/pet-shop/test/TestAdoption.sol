// SPDX-License-Identifier: MIT
pragma solidity ^0.8.13;

// These files are dynamically created at test time
import "truffle/Assert.sol";
import "truffle/DeployedAddresses.sol";
import "../contracts/Adoption.sol";

contract TestAdoption {
    Adoption adoption = Adoption(DeployedAddresses.Adoption());

    function testUserCanAdoptPet() public {
        uint256 retId = adoption.adopt(8);
        Assert.equal(retId, 8, "Adoption of petId 8 should be recored.");
    }

    function testGetAdopterAddressByPetid() public {
        address adopter = adoption.adopters(8);
        address expected = address(this);
        Assert.equal(adopter, expected, "Ownner of petId 8 should be recored.");
    }

    function testGetAdopterAddressByPetIdInArray() public {
        address expected = address(this);
        address[16] memory adopters = adoption.getAdopters();
        Assert.equal(
            adopters[8],
            expected,
            "Ownner of petId 8 should be recored."
        );
    }
}
