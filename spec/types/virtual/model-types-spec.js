/* eslint-disable max-classes-per-file */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach, spyOn */

const UUID = require('uuid');
const { Types, ConnectionBase } = require('../../../src');
const ModelUtils = require('../../../src/utils/model-utils');

describe('ModelType', () => {
  it('can construct from class', () => {
    let type = new Types.ModelType('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('can construct from type helper', () => {
    let type = Types.Model('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('will throw error on attempt to cast without being able to fetch target model', () => {
    let type = Types.Model('Role:userID');

    spyOn(type, 'getTargetModel').and.callFake(() => null);
    expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelType::castToType: Failed when attempting to fetch the required model.'));
  });

  it('will throw error on attempt to cast without a proper value type', () => {
    let type = Types.Model('Role:userID');

    class FakeModel {}
    spyOn(type, 'getTargetModel').and.callFake(() => FakeModel);

    expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelType::castToType: Unable to cast provided value. Value must be a model instance, or a raw object.'));
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

  it('will throw error on attempt to cast without being able to fetch target model', () => {
    let type = Types.Models('Role:userID');

    spyOn(type, 'getTargetModel').and.callFake(() => null);
    expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelsType::castToType: Failed when attempting to fetch the required model.'));
  });

  it('will throw error on attempt to cast without a proper value type', () => {
    let type = Types.Models('Role:userID');

    class FakeModel {}
    spyOn(type, 'getTargetModel').and.callFake(() => FakeModel);

    expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelsType::castToType: Unable to cast provided value at index 0. Value must be a model instance, or a raw object.'));
  });
});

describe('Model relations', () => {
  let connection;
  let User;
  let Role;
  let UserThing;
  let RoleThing;

  beforeEach(() => {
    connection = new ConnectionBase({
      models: require('../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
    UserThing = models.UserThing;
    RoleThing = models.RoleThing;
  });

  describe('setRelationalValues', () => {
    it('can set relational values on a model instance', () => {
      let user  = new User();
      let role  = new Role({ id: UUID.v4(), name: 'test' });

      ModelUtils.setRelationalValues(User, user, Role, role);

      expect(user.primaryRoleID).toEqual(role.id);
    });

    it('can set relational values on a model instance when related model is null', () => {
      let user  = new User({ primaryRoleID: UUID.v4() });

      ModelUtils.setRelationalValues(User, user, Role);

      expect(user.primaryRoleID).toBe(null);
    });
  });

  describe('walkTargetRelation', () => {
    it('can walk target relation #1', () => {
      let user  = new User();
      let field = user.getField('roles');

      let result = field.type.walkTargetRelation(({ source, target }) => {
        let sourceStr = `${source.modelName}:${source.fieldName}`;
        let targetStr = `${target.modelName}:${target.fieldName}`;
        return `${sourceStr} -> ${targetStr}`;
      });

      expect(result).toEqual([
        'User:roles -> Role:id',
      ]);
    });

    it('can walk target relation #2', () => {
      let user  = new User();
      let field = user.getField('userThingRole');

      let result = field.type.walkTargetRelation(({ source, target }) => {
        let sourceStr = `${source.modelName}:${source.fieldName}`;
        let targetStr = `${target.modelName}:${target.fieldName}`;
        return `${sourceStr} -> ${targetStr}`;
      });

      expect(result).toEqual([
        'User:userThingRole -> Role:id',
      ]);
    });
  });

  describe('walkSourceRelation', () => {
    it('can walk source relation #1', () => {
      let user  = new User();
      let field = user.getField('roles');

      let result = field.type.walkSourceRelation(({ source, target }) => {
        let sourceStr = `${source.modelName}:${source.fieldName}`;
        let targetStr = `${target.modelName}:${target.fieldName}`;
        return `${sourceStr} -> ${targetStr}`;
      });

      expect(result).toEqual([
        'User:roles -> User:userRoles',
        'User:userRoles -> UserRole:role',
        'UserRole:role -> UserRole:roleID',
        'UserRole:roleID -> Role:id',
      ]);
    });

    it('can walk source relation #2', () => {
      let user  = new User();
      let field = user.getField('userThingRole');

      let result = field.type.walkSourceRelation(({ source, target }) => {
        let sourceStr = `${source.modelName}:${source.fieldName}`;
        let targetStr = `${target.modelName}:${target.fieldName}`;
        return `${sourceStr} -> ${targetStr}`;
      });

      expect(result).toEqual([
        'User:userThingRole -> User:userThing',
        'User:userThing -> UserThing:role',
        'UserThing:role -> UserThing:roleThing',
        'UserThing:roleThing -> RoleThing:role',
        'RoleThing:role -> RoleThing:roleID',
        'RoleThing:roleID -> Role:id',
      ]);
    });
  });

  describe('getSourceField', () => {
    it('can get source field', () => {
      let user  = new User();
      let field = user.getField('primaryRole');

      let result = field.type.getSourceField();
      expect(result).toBe(User.fields.primaryRoleID);
    });

    it('will skip following foreign keys if requested', () => {
      let roleThing = new RoleThing();
      let field = roleThing.getField('user');

      let result = field.type.getSourceField({ recursive: true, followForeignKeys: false });
      expect(result).toBe(UserThing.fields.userID);

      result = field.type.getSourceField({ recursive: true });
      expect(result).toBe(User.fields.id);
    });

    it('can get source field recursively', () => {
      let user  = new User();
      let field = user.getField('roles');

      let result = field.type.getSourceField({ recursive: true });
      expect(result).toBe(Role.fields.id);
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
          sourceModelName:  'User',
          sourceFieldName:  'id',
          targetModelName:  'UserRole',
          targetFieldName:  'userID',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserRole',
          sourceFieldName:  'roleID',
          targetModelName:  'Role',
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
          sourceModelName:  'User',
          sourceFieldName:  'id',
          targetModelName:  'UserThing',
          targetFieldName:  'userID',
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
          sourceModelName:  'User',
          sourceFieldName:  'id',
          targetModelName:  'UserRole',
          targetFieldName:  'userID',
        },
        {
          relationType:     'target',
          fieldIndex:       0,
          sourceModelName:  'UserRole',
          sourceFieldName:  'roleID',
          targetModelName:  'Role',
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
          sourceModelName:  'User',
          sourceFieldName:  'id',
          targetModelName:  'UserThing',
          targetFieldName:  'userID',
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
