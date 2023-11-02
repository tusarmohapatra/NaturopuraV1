// SPDX-License-Identifier: MIT
pragma solidity ^0.5.2;

contract FarmerDetails {
    struct Farmer {
        string name;    
        uint256 age;
        string location;
    }

    mapping(address => Farmer) public farmers; // Mapping from address to Farmer

    event FarmerAdded(address indexed farmerAddress, string name);

    function addFarmer(string memory _name, uint256 _age, string memory _location) public {
        require(bytes(_name).length > 0, "Name cannot be empty");
        require(_age > 0, "Age must be greater than 0");

        Farmer storage newFarmer = farmers[msg.sender];
        newFarmer.name = _name;
        newFarmer.age = _age;
        newFarmer.location = _location;

        emit FarmerAdded(msg.sender, _name);
    }

    function getFarmerDetails(address _farmerAddress) public view returns (string memory name, uint256 age, string memory location) {
        Farmer storage farmer = farmers[_farmerAddress];
        require(bytes(farmer.name).length > 0, "Farmer not found");
        return (farmer.name, farmer.age, farmer.location);
    }

    function updateFarmerDetails(string memory _name, uint256 _age, string memory _location) public {
        Farmer storage farmer = farmers[msg.sender];
        require(bytes(farmer.name).length > 0, "Farmer not found");
        require(_age > 0, "Age must be greater than 0");

        farmer.name = _name;
        farmer.age = _age;
        farmer.location = _location;
    }
}
