module.exports = {
  networks: {
    development: {
      host: "127.0.0.1",
      port: 8545, // Update this to match the port where Ganache is running
      network_id: "*", // Match any network id
    },
    // ... other configurations
    compilers: {
      solc: {
        version: "0.5.2", // Use the version specified in your contract
      },
    },
    dashboard: {
      port: 24012,
      host: "localhost",
      verbose: false,
    },
  },
  // ... other settings
};
