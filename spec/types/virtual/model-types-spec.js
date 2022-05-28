/* eslint-disable max-classes-per-file */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { Model, Types, ConnectionBase } = require('../../../src');

class User extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'roles': {
      type:       Types.Models('UserRole:role', 'UserRole:user'),
    },
    'userThing': {
      type:       Types.Model('UserThing:user'),
    },
    'userThingRole': {
      type:       Types.Model('Role', 'userThing.role'),
    },
  };
}

// find source ->

// [
//   { 'UserThing', 'userID' },
//   { 'User', 'id' },
//   { 'UserThing', 'roleThingID' },
//   { 'RoleThing', 'id' },
//   { 'RoleThing', 'roleID' },
//   { 'Role', 'id' },
// ]

class Role extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  false,
      index:      true,
    },
    'user': {
      type:       Types.Model('UserRole:user', 'UserRoles:role'),
    },
  };
}

class UserRole extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'roleID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'userID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'role': {
      type:       Types.Model('Role:id', 'roleID'),
    },
    'user': {
      type:       Types.Model('User:id', 'userID'),
    },
  };
}

class UserThing extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'userID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'roleThingID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'roleThing': {
      type:       Types.Model('RoleThing', 'roleThingID'),
    },
    'role': {
      type:       Types.Model('Role', 'roleThing.role'),
    },
    'user': {
      type:       Types.Model('User:id', 'userID'),
    },
  };
}

class RoleThing extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'roleID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'userThing': {
      type:       Types.Model('UserThing', 'UserThing:roleThing'),
    },
    'role': {
      type:       Types.Model('Role:id', 'roleID'),
    },
    'user': {
      type:       Types.Model('User', 'UserThing:user'),
    },
  };
}

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
    expect(() => Types.Model.castToType({})).toThrow(new TypeError('ModelType::castToType: Type instance is required to cast.'));
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
    expect(() => Types.Models.castToType({})).toThrow(new TypeError('ModelsType::castToType: Type instance is required to cast.'));
  });
});

describe('Model relations', () => {
  let connection;

  beforeEach(() => {
    connection = new ConnectionBase({
      models: [
        User,
        Role,
        UserRole,
        UserThing,
        RoleThing,
      ],
    });
  });

  it('can recursively parse target relations #1', () => {
    let user        = new User();
    let rolesField  = user.getField('roles');

    let result = rolesField.type.getTargetRelationPath(connection);
    expect(result).toEqual([
      {
        modelName:  'UserRole',
        fieldNames: [ 'roleID' ],
      },
      {
        modelName:  'Role',
        fieldNames: [ 'id' ],
      },
    ]);
  });

  it('can recursively parse target relations #2', () => {
    let user                = new User();
    let userThingRoleField  = user.getField('userThingRole');

    let result = userThingRoleField.type.getTargetRelationPath(connection);
    expect(result).toEqual([
      {
        modelName:  'Role',
        fieldNames: [ 'id' ],
      },
    ]);
  });

  it('can recursively parse source relations #1', () => {
    let user        = new User();
    let rolesField  = user.getField('roles');

    let result = rolesField.type.getSourceRelationPath(connection);
    expect(result).toEqual([
      {
        modelName:  'User',
        fieldNames: [ 'id' ],
      },
      {
        modelName:  'UserRole',
        fieldNames: [ 'userID' ],
      },
    ]);
  });

  fit('can recursively parse source relations #2', () => {
    let user                = new User();
    let userThingRoleField  = user.getField('userThingRole');

    let result = userThingRoleField.type.getFullRelationPath(connection);
    console.log('RESULT: ', result);

    expect(result).toEqual([
      {
        modelName:  'User',
        fieldNames: [ 'id' ],
      },
      {
        modelName:  'UserRole',
        fieldNames: [ 'userID' ],
      },
    ]);
  });

  it('can recursively parse full relations', () => {
    let user        = new User();
    let rolesField  = user.getField('roles');

    let result = rolesField.type.getFullRelationPath(connection);
    expect(result).toEqual([
      {
        modelName:  'UserRole',
        fieldNames: [ 'roleID' ],
      },
      {
        modelName:  'Role',
        fieldNames: [ 'id' ],
      },
      {
        modelName:  'User',
        fieldNames: [ 'id' ],
      },
      {
        modelName:  'UserRole',
        fieldNames: [ 'userID' ],
      },
    ]);
  });
});
