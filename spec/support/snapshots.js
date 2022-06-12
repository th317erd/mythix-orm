'use strict';

const jsDiff        = require('diff');
const colors        = require('colors/safe');
const Path          = require('path');
const FileSystem    = require('fs');

const MAX_FILE_NAME_LENGTH = 100;

function showDiff(fileName, c1, c2) {
  jsDiff.createPatch(fileName, c1 || '', c2 || '').replace(/.*/g, function(m) {
    if (!m)
      return;

    let c = m.charAt(0);
    let out = m;

    if (c === '-')
      console.log(colors.red(out));
    else if (c === '+')
      console.log(colors.green(out));
    else
      console.log(out);
  });
}

function serialize(value) {
  if (value == null)
    return ('' + value);

  if (typeof value === 'boolean' || value instanceof Boolean)
    return ('' + value);

  if (typeof value === 'number' || value instanceof Number)
    return ('' + value);

  if (typeof value === 'bigint')
    return `BigInt(${value})`;

  if (typeof value === 'string' || value instanceof String)
    return `"${value.replace(/"/g, '\\"')}"`;

  return JSON.stringify(value, undefined, 2);
}

const specFileCache = {};
const callCountCache = {};

function getSnapshotNameAndPath() {
  const getFileName = (stackLine) => {
    let fileName;

    stackLine.replace(/\(([^(]+):\d+:\d+\)$/, (_, str) => {
      fileName = str;
    });

    return fileName;
  };

  const getLineNumber = (stackLine) => {
    let number;

    stackLine.replace(/(\d+):\d+\)$/, (_, numStr) => {
      number = parseInt(numStr, 10);
    });

    return number;
  };

  const getBacktraceFile = () => {
    const matchesPatterns = (patterns, fileName) => {
      for (let i = 0, il = patterns.length; i < il; i++) {
        let pattern = patterns[i];
        if (pattern.test(fileName))
          return true;
      }

      return false;
    };

    let specFilePatterns  = [ /-spec\.js/ ];
    let stack             = (new Error()).stack.split(/\s+at\s+/g).slice(1).map((part) => part.trim());
    let file;

    for (let i = 0, il = stack.length; i < il; i++) {
      let stackLine = stack[i];
      let fileName  = getFileName(stackLine);
      if (!matchesPatterns(specFilePatterns, fileName))
        continue;

      let lineNumber = getLineNumber(stackLine);

      if (!file) {
        file = {
          lineNumbers: [ lineNumber ],
          fileName,
        };
      } else {
        file.lineNumbers.push(lineNumber);
      }
    }

    return file;
  };

  const getSpecFileStructure = (fileName) => {
    if (specFileCache[fileName])
      return specFileCache[fileName];

    let contents = FileSystem.readFileSync(fileName, 'utf8');
    let lines = contents.split(/\n/g);

    lines = lines.map((line, lineIndex) => {
      let specName;
      let indentAmount;

      line.replace(/([\s\t]*)f?(?:describe|it)\s*\(\s*(['"])((?:\\.|.)*?)\2/, (m, indent, q, name) => {
        specName = name.replace(/\\(.)/g, '$1');
        indentAmount = indent.replace(/\t/g, '  ').length;
      });

      if (!specName)
        return;

      return { specName, indentAmount, lineNumber: lineIndex + 1 };
    }).filter(Boolean);

    let nodes = [];
    let currentChildren = nodes;
    let previousNode;
    let parent = null;

    for (let i = 0, il = lines.length; i < il; i++) {
      let line = lines[i];
      let { specName, indentAmount, lineNumber } = line;

      if (previousNode) {
        if (previousNode.indentAmount < indentAmount) {
          parent = previousNode;
          currentChildren = previousNode.children;
        } else if (previousNode.indentAmount > indentAmount) {
          parent = previousNode.parent;
          while (parent && parent.indentAmount >= indentAmount)
            parent = parent.parent;

          currentChildren = (parent) ? parent.children : nodes;
        }
      }

      let node = { parent, indentAmount, name: specName, lineNumber, children: [] };
      currentChildren.push(node);

      previousNode = node;
    }

    specFileCache[fileName] = nodes;

    return nodes;
  };

  const findCorrectNode = (nodes, lineNumber, parentNode) => {
    const findChildNode = (nodes, lineNumber) => {
      for (let i = 0, il = nodes.length; i < il; i++) {
        let node = nodes[i];
        let childNode = findCorrectNode(node.children, lineNumber, node);
        if (childNode)
          return childNode;
      }
    };

    const findLargerNode = (nodes, lineNumber) => {
      for (let i = 0, il = nodes.length; i < il; i++) {
        let node = nodes[i];
        if (node.lineNumber > lineNumber)
          return i;
      }

      return -1;
    };

    let childNode = findChildNode(nodes, lineNumber);
    if (childNode) {
      while (childNode.lineNumber > lineNumber)
        childNode = childNode.parent;

      return childNode;
    }

    let largerNodeIndex = findLargerNode(nodes, lineNumber);
    if (largerNodeIndex <= 0)
      return (largerNodeIndex === 0) ? parentNode : undefined;

    return nodes[largerNodeIndex - 1];
  };

  const getLastNode = (nodes) => {
    if (!nodes.length)
      return;

    let lastNode = nodes[nodes.length - 1];
    if (lastNode.children.length > 0)
      return getLastNode(lastNode.children);

    return lastNode;
  };

  const getNodePath = (_node) => {
    if (!_node)
      return;

    let node = _node;
    let path = [ node.name ];

    while (node && node.parent) {
      node = node.parent;
      path.push(node.name);
    }

    return path.reverse().join('/');
  };

  const getCorrectNodePath = (nodes, lineNumbers) => {
    let nodePaths = lineNumbers.map((lineNumber) => {
      let node = findCorrectNode(nodes, lineNumber);
      if (!node)
        node = getLastNode(nodes);

      return getNodePath(node);
    }).filter(Boolean);

    let nodePath = nodePaths.sort((a, b) => {
      if (a.length === b.length)
        return 0;

      return (a.length < b.length) ? 1 : -1;
    })[0];

    let callCount = callCountCache[nodePath];
    if (!callCount)
      callCount = 0;

    callCount++;
    callCountCache[nodePath] = callCount;

    nodePath = nodePath.replace(/[^\w/]+/g, '_').replace(/\//g, '-');
    if (nodePath.length > MAX_FILE_NAME_LENGTH) {
      nodePath = nodePath.substring(0, MAX_FILE_NAME_LENGTH);
      nodePath = nodePath.replace(/^[^0-9a-zA-Z]+/, '').replace(/[^0-9a-zA-Z]+$/, '');
    }

    // eslint-disable-next-line no-magic-numbers
    return (`${nodePath}-${('' + callCount).padStart(3, '0')}.snapshot`);
  };

  const getSnapshotDetails = () => {
    let { fileName, lineNumbers } = getBacktraceFile();
    let nodes = getSpecFileStructure(fileName);
    let name  = getCorrectNodePath(nodes, lineNumbers);

    return { path: Path.dirname(fileName), name };
  };

  let structure = getSnapshotDetails();
  return structure;
}

function matchesSnapshot(value) {
  let { name, path }  = getSnapshotNameAndPath();
  let snapshotPath    = Path.join(path, '__snapshots__');
  let fullPath        = Path.join(snapshotPath, name);

  try {
    FileSystem.mkdirSync(snapshotPath);
  } catch (error) {
    if (error.code !== 'EEXIST')
      throw error;
  }

  let serializedValue = serialize(value);
  if (!FileSystem.existsSync(fullPath))
    FileSystem.writeFileSync(fullPath, serializedValue, 'utf8');

  let storedValue = FileSystem.readFileSync(fullPath, 'utf8');

  if (storedValue !== serializedValue) {
    showDiff(fullPath, storedValue, serializedValue);
    return false;
  }

  return true;
}

module.exports = matchesSnapshot;
