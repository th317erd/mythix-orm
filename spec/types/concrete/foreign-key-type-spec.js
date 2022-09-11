/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Model } = require('../../../lib/model');
const {
  Types,
  ConnectionBase,
} = require('../../../lib');

describe('ForeignKeyType', () => {
  let connection;
  let FKModel;

  beforeAll(async () => {
    try {
      connection = new ConnectionBase({
        bindModels: false,
        models:     Object.assign({}, require('../../support/models'), {
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
    } catch (error) {
      console.log('Failed in beforeAll', error);
      throw error;
    }

    let models = connection.getModels();

    FKModel = models.FKModel;
  });

  describe('toString', () => {
    it('should be empty if a connection is provided', () => {
      let type = FKModel.fields.userID.type;
      expect(type.toString(connection)).toEqual('VARCHAR(36)');
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
