/* eslint-disable max-classes-per-file */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { Types, ConnectionBase } = require('../../../src');

describe('ModelType', () => {
  it('can construct from class', () => {
    let type = new Types.ModelType('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('can construct from type helper', () => {
    let type = Types.Model('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('will throw error on attempt to cast without a type instance', () => {
    let type = Types.Model('Role:userID');
    expect(() => type.castToType({})).toThrow(new TypeError('ModelType::castToType: Type instance is required to cast.'));
  });
});

describe('ModelsType', () => {
  it('can construct from class', () => {
    let type = new Types.ModelsType('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('can construct from type helper', () => {
    let type = Types.Models('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('will throw error on attempt to cast without a type instance', () => {
    let type = Types.Models('Role:userID');
    expect(() => type.castToType({})).toThrow(new TypeError('ModelsType::castToType: Type instance is required to cast.'));
  });
});

describe('Model relations', () => {
  let connection;
  let User;
  let Role;
  let UserRole;
  let UserThing;
  let RoleThing;

  beforeEach(() => {
    connection = new ConnectionBase({
      models: require('../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
    UserRole = models.UserRole;
    UserThing = models.UserThing;
    RoleThing = models.RoleThing;
  });

  describe('getSourceField', () => {
    it('can get source field', () => {
      let user  = new User();
      let field = user.getField('primaryRole');

      let result = field.type.getSourceField(true);
      expect(result).toBe(User.fields.primaryRoleID);
    });
  });

  describe('getJoinableRelations', () => {
    it('can recursively parse target relations #1', () => {
      let user        = new User();
      let rolesField  = user.getField('roles');

      let result = rolesField.type.getJoinableRelations(connection);
      expect(result).toEqual([
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserRole',
          sourceFieldName:  'roleID',
          targetModelName:  'Role',
          targetFieldName:  'id',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserRole',
          sourceFieldName:  'userID',
          targetModelName:  'User',
          targetFieldName:  'id',
        },
      ]);
    });

    it('can recursively parse relations #2', () => {
      let user                = new User();
      let userThingRoleField  = user.getField('userThingRole');

      let result = userThingRoleField.type.getJoinableRelations(connection);
      expect(result).toEqual([
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserThing',
          sourceFieldName:  'userID',
          targetModelName:  'User',
          targetFieldName:  'id',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserThing',
          sourceFieldName:  'roleThingID',
          targetModelName:  'RoleThing',
          targetFieldName:  'id',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'RoleThing',
          sourceFieldName:  'roleID',
          targetModelName:  'Role',
          targetFieldName:  'id',
        },
      ]);
    });

    it('can recursively parse relations #3', () => {
      let user        = new User();
      let rolesField  = user.getField('roles');

      let result = rolesField.type.getJoinableRelations(connection);
      expect(result).toEqual([
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserRole',
          sourceFieldName:  'roleID',
          targetModelName:  'Role',
          targetFieldName:  'id',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserRole',
          sourceFieldName:  'userID',
          targetModelName:  'User',
          targetFieldName:  'id',
        },
      ]);
    });

    it('can recursively parse relations #4', () => {
      let user                = new User();
      let userThingRoleField  = user.getField('userThingRole');

      let result = userThingRoleField.type.getJoinableRelations(connection);
      expect(result).toEqual([
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserThing',
          sourceFieldName:  'userID',
          targetModelName:  'User',
          targetFieldName:  'id',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserThing',
          sourceFieldName:  'roleThingID',
          targetModelName:  'RoleThing',
          targetFieldName:  'id',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'RoleThing',
          sourceFieldName:  'roleID',
          targetModelName:  'Role',
          targetFieldName:  'id',
        },
      ]);
    });

    it('can recursively parse relations #5', () => {
      let user  = new User();
      let field = user.getField('primaryRole');

      let result = field.type.getJoinableRelations(connection);
      expect(result).toEqual([
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'User',
          sourceFieldName:  'primaryRoleID',
          targetModelName:  'Role',
          targetFieldName:  'id',
        },
      ]);
    });
  });
});
