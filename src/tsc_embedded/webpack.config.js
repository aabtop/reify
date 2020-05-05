const path = require('path');

module.exports = {
  entry: path.join(__dirname, './src/tsc_embedded.ts'),
  output: {
    filename: 'tsc_embedded.js',
    path: __dirname + '/dist',
    library: 'tsc_embedded'
  },
  module: {
    rules: [
      {
        loader: 'ts-loader',
        exclude: /node_modules/,
      },
    ]
  },
  resolve: {extensions: ['.ts', '.js']},
  mode: 'production',
  plugins: []
};
