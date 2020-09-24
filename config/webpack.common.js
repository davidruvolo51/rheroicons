////////////////////////////////////////////////////////////////////////////////
// FILE: webpack.common.js
// AUTHOR: David Ruvolo
// CREATED: 2020-09-24
// MODIFIED: 2020-09-24
// PURPOSE: configuration to be used in prod and dev
// DEPENDENCIES: see below
// STATUS: working
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////

// load
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const webpack = require("webpack");
const path = require("path");

// configuration
module.exports = {
    entry: "./inst/gallery-assets/src/index.js",
    output: {
        filename: "rheroicons.min.js",
        path: path.join(__dirname, "..", "inst/gallery-assets/public"),
    }  ,
    plugins: [
        new webpack.ProgressPlugin(),
        new CleanWebpackPlugin(),
        new HtmlWebpackPlugin({
            title: "rheroicons"
        }),
        new MiniCssExtractPlugin({
            filename: "rheroicons.min.css",
            ignoreOrder: false,
        })
    ],
    module: {
        rules: [
            {
                test: /\.js$/,
                use: "babel-loader",
                exclude: /node_modules/,
            },
            {
                test: /\.s[ac]ss$/i,
                use: [
                    // write to file
                    {
                        loader: MiniCssExtractPlugin.loader,
                        options: {
                            publicPath: "./inst/gallery-assets/public/"
                        }
                    },
                    "css-loader",
                    "sass-loader",
                    "postcss-loader"
                ]
            }
        ]
    }
}
