'use strict';

function sanitizeFieldString(str) {
  return ('' + str).replace(/[^\w:.]+/g, '').replace(/([:.])+/g, '$1');
}

function parseQualifiedName(str) {
  let fields = [];
  let modelName;

  let parts = sanitizeFieldString(str).split(/:/).filter(Boolean);
  if (parts.length > 1) {
    modelName = parts[0];
    fields = parts.slice(1).join('').split(/\.+/g).filter(Boolean);
  } else {
    fields = parts.join('').split(/\.+/g).filter(Boolean);

    if (fields.length === 1 && fields[0].match(/^[A-Z]/)) {
      modelName = fields[0];
      fields = [];
    }
  }

  return { modelName, fields };
}

module.exports = {
  sanitizeFieldString,
  parseQualifiedName,
};
