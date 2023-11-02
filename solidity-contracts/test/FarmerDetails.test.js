const FarmerDetails = artifacts.require("FarmerDetails");

contract("FarmerDetails", (accounts) => {
  let farmerDetailsInstance;

  before(async () => {
    farmerDetailsInstance = await FarmerDetails.new();
  });

  it("should add a farmer", async () => {
    const farmerName = "John";
    const farmerAge = 30;
    const farmerLocation = "Farmville";

    const tx = await farmerDetailsInstance.addFarmer(
      farmerName,
      farmerAge,
      farmerLocation,
      { from: accounts[0] }
    );
    const { event, args } = tx.logs[0];

    assert.equal(event, "FarmerAdded", "FarmerAdded event should be emitted");
    assert.equal(
      args.farmerAddress,
      accounts[0],
      "Event should contain the correct farmer address"
    );
    assert.equal(
      args.name,
      farmerName,
      "Event should contain the correct farmer name"
    );
  });

  it("should get farmer details", async () => {
    const { name, age, location } =
      await farmerDetailsInstance.getFarmerDetails(accounts[0]);
    assert.equal(name, "John", "Name should match the added farmer's name");
    assert.equal(age.toNumber(), 30, "Age should match the added farmer's age");
    assert.equal(
      location,
      "Farmville",
      "Location should match the added farmer's location"
    );
  });

  it("should update farmer details", async () => {
    const newName = "Updated Name";
    const newAge = 40;
    const newLocation = "New Farmville";

    await farmerDetailsInstance.updateFarmerDetails(
      newName,
      newAge,
      newLocation,
      { from: accounts[0] }
    );

    const { name, age, location } =
      await farmerDetailsInstance.getFarmerDetails(accounts[0]);
    assert.equal(name, newName, "Name should match the updated name");
    assert.equal(age.toNumber(), newAge, "Age should match the updated age");
    assert.equal(
      location,
      newLocation,
      "Location should match the updated location"
    );
  });
});
