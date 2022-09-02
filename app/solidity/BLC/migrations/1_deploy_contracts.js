const BloggerCoin = artifacts.require("BloggerCoin");

module.exports = function (deployer) {
  deployer.deploy(BloggerCoin);
};
