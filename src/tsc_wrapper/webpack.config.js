const path = require('path');

module.exports = {
  entry: path.join(__dirname, './src/tsc_wrapper.ts'),
  output: {
    filename: 'tsc_wrapper.js',
    path: path.join(__dirname, 'dist'),
    library: 'tsc_wrapper'
  },
  module: {
    rules: [
      {
        loader: 'ts-loader',
        exclude: [/node_modules/, /resources/],
      },
    ]
  },
  resolve: {
    extensions: ['.ts', '.js'],
    modules: ['node_modules']
  },
  mode: 'production',
  plugins: [],
};
