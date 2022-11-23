'use strict';

/* global __dirname */

const Path = require('path');

module.exports = {
  rootDir:    __dirname,
  inputDir:   Path.resolve(__dirname),
  outputDir:  Path.resolve(__dirname, '..', 'mythix-orm.wiki'),
  files: [
    {
      include:  /\/lib\/.*\.js$/,
      parser:   'typescript',
      compiler: 'typescript',
    },
    {
      include:  /\/docs\/.*\.md$/,
      parser:   'markdown',
      compiler: 'markdown',
    },
  ],
  exclude: [
    /node_modules|\/spec\//
  ],
  generatorOptions: {
    repositoryURL:  'https://github.com/th317erd/mythix-orm',
    baseURL:        './',
  },
};
