'use strict';

const { Model, Types } = require('../../../lib');

class BlobTest extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'data': {
      type: Types.BLOB,
    },
  };
}

module.exports = BlobTest;
