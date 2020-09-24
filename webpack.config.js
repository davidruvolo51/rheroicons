////////////////////////////////////////////////////////////////////////////////
// FILE: webpack.config.js
// AUTHOR: David Ruvolo
// CREATED: 2020-09-24
// MODIFIED: 2020-09-24
// PURPOSE: loads dev or prod configuration based on script param `env`
// DEPENDENCIES: see common
// STATUS: working
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////

// pkgs
const { merge } = require("webpack-merge");
const commonConfig = require("./config/webpack.common");

// load config based on environment
module.exports = (env) => {
    const config = require("./config/webpack." + env);
    return merge(commonConfig, config);
}
