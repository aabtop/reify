const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const path = require('path');

module.exports = {
  entry: path.join(__dirname, './src/index.ts'),
  output: {
    filename: "monaco_wrapper.js",
    library: 'monaco_wrapper',
  },
  resolve: { extensions: ['.ts', '.js'] },
  module: {
    rules: [
      {
        loader: 'ts-loader',
        exclude: [/node_modules/],
      },
      {
        test: /\.css$/,
        use: ['style-loader', 'css-loader']
      },
      {
        test: /\.ttf$/,
        use: ['base64-inline-loader']
      },
    ]
  },
  mode: 'production',
  //mode: "development",
  plugins: [
    new MonacoWebpackPlugin({ languages: ['typescript'] }),
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1,
    }),
    new CopyWebpackPlugin({
      patterns: [
        { from: path.join(__dirname, './src/static') }
      ]
    })
  ],
};
