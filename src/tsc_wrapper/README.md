# tsc_wrapper

This folder contains the TypeScript code that wraps the Microsoft TypeScript
Compiler and interfaces with the C++/V8 side of Reify to communicate the
results.

The yarn.lock file must be manually kept up-to-date with the package.json file, 
by running `yarn install` in this directory after changing package.json. Of
course, `yarn` needs to be installed for this to work as well:

https://classic.yarnpkg.com/en/docs/install/#debian-stable
