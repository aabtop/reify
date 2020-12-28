const CopyWebpackPlugin = require('copy-webpack-plugin');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const path = require('path');

module.exports = {
  entry: path.join(__dirname, './src/index.ts'),
  output: {
    filename: 'monaco_wrapper.js',
    path: path.join(__dirname, 'dist'),
  },
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
        use: ['file-loader']
      }]
  },
  mode: 'production',
  plugins: [
    new MonacoWebpackPlugin({ languages: ['typescript'] }),
    new CopyWebpackPlugin({
      patterns: [
        { from: path.join(__dirname, './src/static') }
      ]
    })
  ],
};
