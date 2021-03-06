/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../lib/connection/sqlite-connection');
const Model = require('../../../lib/model');
const { Types } = require('../../../lib');

describe('ForeignKeyType', () => {
  let connection;
  let FKModel;

  beforeEach(async () => {
    connection = new SQLiteConnection({
      models: Object.assign({}, require('../../support/models'), {
        FKModel: class FKModel extends Model {
          static fields = {
            'id': {
              type:         Types.UUIDV4,
              defaultValue: Types.UUIDV4.Default.UUIDV4,
              allowNull:    false,
              primaryKey:   true,
            },
            'userID': {
              type:       Types.FOREIGN_KEY('User:id'),
              allowNull:  true,
            },
          };
        },
      }),
    });

    let models = connection.getModels();

    FKModel = models.FKModel;
  });

  describe('toString', () => {
    it('should be empty if a connection is provided', () => {
      let type = FKModel.fields.userID.type;
      expect(type.toString({ dialect: 'sqlite' })).toEqual('VARCHAR(36)');
    });

    it('should display field type without any arguments', () => {
      let type = FKModel.fields.userID.type;
      expect(type.toString()).toEqual('ForeignKeyType {}');
    });
  });

  it('can construct from class', () => {
    let type = new Types.ForeignKeyType('User:id');
    expect(type.toString()).toEqual('ForeignKeyType {}');
  });

  it('can construct from type helper', () => {
    let type = Types.FOREIGN_KEY('User:id');
    expect(type.toString()).toEqual('ForeignKeyType {}');
  });
});
