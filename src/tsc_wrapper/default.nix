{runCommand, nodejs}:

runCommand "reify-tsc-wrapper" { buildInputs = [nodejs]; src = ./.; } ''
  echo `pwd`
  cp -r $src/* .
  export HOME=`pwd`/home
  npm install
  npx webpack
  cp dist/tsc_wrapper.js $out
''
