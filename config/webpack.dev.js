////////////////////////////////////////////////////////////////////////////////
// FILE: webpack.dev.js
// AUTHOR: David Ruvolo
// CREATED: 2020-09-24
// MODIFIED: 2020-09-24
// PURPOSE: configuration for dev environment
// DEPENDENCIES: see below + common config
// STATUS: working
// COMMENTS: The property `writeToDisk` is used as it is unclear of it is
// possible to access webpack's environment using `addResourcePath`. It's far
// easier to write files to disk.
////////////////////////////////////////////////////////////////////////////////

// config
module.exports = {
    mode: "development",
    output: {
        filename: "rheroicons.min.js",
    },
    devServer: {
        port: 1234,
        hot: true,
        writeToDisk: true // for use in shiny-dev
    }
}
