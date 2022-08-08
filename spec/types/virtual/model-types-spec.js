/* eslint-disable max-classes-per-file */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, spyOn */

const UUID                      = require('uuid');
const { Types, ConnectionBase } = require('../../../lib');
const ModelUtils                = require('../../../lib/utils/model-utils');

describe('ModelType', () => {
  let connection;
  let User;
  let UserRole;
  let RoleThing;
  let UserThing;
  let Role;

  beforeAll(() => {
    try {
      connection = new ConnectionBase({
        bindModels: false,
        models:     require('../../support/models'),
      });

      let models = connection.getModels();
      User = models.User;
      UserRole = models.UserRole;
      RoleThing = models.RoleThing;
      UserThing = models.UserThing;
      Role = models.Role;
    } catch (error) {
      console.error('Error in beforeAll: ', error);
    }
  });

  it('can construct from class', () => {
    let type = new Types.ModelType('Role:userID');
    expect(type.toString()).toEqual('ModelType {}');
  });

  it('can construct from type helper', () => {
    let type = Types.Model('Role:userID');
    expect(type.toString()).toEqual('ModelType {}');
  });

  it('can walk a field relation', async () => {
    let relations = [];
    let user = new User({
      id:             '664e9071-11d9-4544-85fe-1359ce1904b1',
      firstName:      'Mary',
      lastName:       'Anne',
      primaryRoleID:  '5016a9dc-0271-41a0-937a-a0c95acd117b',
    });

    await User.fields.userThingRole.type.walkQueryRelations(connection, (context) => {
      relations.push(context);
    }, { self: user, field: User.fields.roles });

    expect(relations).toEqual([
      {
        PrimaryModel: User,
        TargetModel:  Role,
        TargetField:  Role.fields.id,
        source:       {
          Model:      RoleThing,
          modelName:  'RoleThing',
          field:      RoleThing.fields.roleID,
          fieldName:  'roleID',
        },
        target:       {
          Model:      Role,
          modelName:  'Role',
          field:      Role.fields.id,
          fieldName:  'id',
        },
      },
      {
        PrimaryModel: User,
        TargetModel:  Role,
        TargetField:  Role.fields.id,
        source:       {
          Model:      UserThing,
          modelName:  'UserThing',
          field:      UserThing.fields.roleThingID,
          fieldName:  'roleThingID',
        },
        target:       {
          Model:      RoleThing,
          modelName:  'RoleThing',
          field:      RoleThing.fields.id,
          fieldName:  'id',
        },
      },
    ]);
  });

  // it('will throw error on attempt to cast without being able to fetch target model', () => {
  //   let type = Types.Model('Role:userID');

  //   spyOn(type, 'getTargetModel').and.callFake(() => null);
  //   expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelType::castToType: Failed when attempting to fetch the required model.'));
  // });

  // it('will throw error on attempt to cast without a proper value type', () => {
  //   let type = Types.Model('Role:userID');

  //   class FakeModel {}
  //   spyOn(type, 'getTargetModel').and.callFake(() => FakeModel);

  //   expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelType::castToType: Unable to cast provided value. Value must be a model instance, or a raw object.'));
  // });
});

// describe('ModelsType', () => {
//   it('can construct from class', () => {
//     let type = new Types.ModelsType('Role:userID');
//     expect(type.toString()).toEqual('ModelsType {}');
//   });

//   it('can construct from type helper', () => {
//     let type = Types.Models('Role:userID');
//     expect(type.toString()).toEqual('ModelsType {}');
//   });

//   it('will throw error on attempt to cast without being able to fetch target model', () => {
//     let type = Types.Models('Role:userID');

//     spyOn(type, 'getTargetModel').and.callFake(() => null);
//     expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelsType::castToType: Failed when attempting to fetch the required model.'));
//   });

//   it('will throw error on attempt to cast without a proper value type', () => {
//     let type = Types.Models('Role:userID');

//     class FakeModel {}
//     spyOn(type, 'getTargetModel').and.callFake(() => FakeModel);

//     expect(() => type.castToType({ value: 20 })).toThrow(new TypeError('ModelsType::castToType: Unable to cast provided value at index 0. Value must be a model instance, or a raw object.'));
//   });
// });

// describe('Model relations', () => {
//   let connection;
//   let User;
//   let Role;
//   let UserThing;
//   let RoleThing;

//   beforeAll(() => {
//     try {
//       connection = new ConnectionBase({
//         bindModels: false,
//         models:     require('../../support/models'),
//       });

//       let models = connection.getModels();
//       User = models.User;
//       Role = models.Role;
//       UserThing = models.UserThing;
//       RoleThing = models.RoleThing;
//     } catch (error) {
//       console.error('Error in beforeAll: ', error);
//     }
//   });

//   describe('setRelationalValues', () => {
//     it('can set relational values on a model instance', () => {
//       let user  = new User(null, { connection });
//       let role  = new Role({ id: UUID.v4(), name: 'test' }, { connection });

//       ModelUtils.setRelationalValues(connection, User, user, Role, role);

//       expect(user.primaryRoleID).toEqual(role.id);
//     });

//     it('can set relational values on a model instance when related model is null', () => {
//       let user  = new User({ primaryRoleID: UUID.v4() }, { connection });

//       ModelUtils.setRelationalValues(connection, User, user, Role);

//       expect(user.primaryRoleID).toBe(null);
//     });
//   });

//   describe('walkTargetRelation', () => {
//     it('can walk target relation #1', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('roles');

//       let result = field.type.walkTargetRelation(connection, ({ source, target }) => {
//         let sourceStr = `${source.modelName}:${source.fieldName}`;
//         let targetStr = `${target.modelName}:${target.fieldName}`;
//         return `${sourceStr} -> ${targetStr}`;
//       });

//       expect(result).toEqual([
//         'User:roles -> Role:id',
//       ]);
//     });

//     it('can walk target relation #2', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('userThingRole');

//       let result = field.type.walkTargetRelation(connection, ({ source, target }) => {
//         let sourceStr = `${source.modelName}:${source.fieldName}`;
//         let targetStr = `${target.modelName}:${target.fieldName}`;
//         return `${sourceStr} -> ${targetStr}`;
//       });

//       expect(result).toEqual([
//         'User:userThingRole -> Role:id',
//       ]);
//     });
//   });

//   describe('walkSourceRelation', () => {
//     it('can walk source relation #1', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('roles');

//       let result = field.type.walkSourceRelation(connection, ({ source, target }) => {
//         let sourceStr = `${source.modelName}:${source.fieldName}`;
//         let targetStr = `${target.modelName}:${target.fieldName}`;
//         return `${sourceStr} -> ${targetStr}`;
//       });

//       expect(result).toEqual([
//         'User:roles -> User:userRoles',
//         'User:userRoles -> UserRole:role',
//         'UserRole:role -> UserRole:roleID',
//         'UserRole:roleID -> Role:id',
//       ]);
//     });

//     it('can walk source relation #2', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('userThingRole');

//       let result = field.type.walkSourceRelation(connection, ({ source, target }) => {
//         let sourceStr = `${source.modelName}:${source.fieldName}`;
//         let targetStr = `${target.modelName}:${target.fieldName}`;
//         return `${sourceStr} -> ${targetStr}`;
//       });

//       expect(result).toEqual([
//         'User:userThingRole -> User:userThing',
//         'User:userThing -> UserThing:role',
//         'UserThing:role -> UserThing:roleThing',
//         'UserThing:roleThing -> RoleThing:role',
//         'RoleThing:role -> RoleThing:roleID',
//         'RoleThing:roleID -> Role:id',
//       ]);
//     });
//   });

//   describe('getSourceField', () => {
//     it('can get source field', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('primaryRole');

//       let result = field.type.getSourceField(connection);
//       expect(result).toBe(User.fields.primaryRoleID);
//     });

//     it('will skip following foreign keys if requested', () => {
//       let roleThing = new RoleThing(null, { connection });
//       let field = roleThing.getField('user');

//       let result = field.type.getSourceField(connection, { recursive: true, followForeignKeys: false });
//       expect(result).toBe(UserThing.fields.userID);

//       result = field.type.getSourceField(connection, { recursive: true });
//       expect(result).toBe(User.fields.id);
//     });

//     it('can get source field recursively', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('roles');

//       let result = field.type.getSourceField(connection, { recursive: true });
//       expect(result).toBe(Role.fields.id);
//     });
//   });

//   describe('getJoinableRelations', () => {
//     it('can recursively parse target relations #1', () => {
//       let user        = new User(null, { connection });
//       let rolesField  = user.getField('roles');

//       let result = rolesField.type.getJoinableRelations(connection);
//       expect(result).toEqual([
//         {
//           sourceModelName:  'UserRole',
//           sourceFieldName:  'roleID',
//           targetModelName:  'Role',
//           targetFieldName:  'id',
//         },
//         {
//           sourceModelName:  'User',
//           sourceFieldName:  'id',
//           targetModelName:  'UserRole',
//           targetFieldName:  'userID',
//         },
//       ]);
//     });

//     it('can recursively parse relations #2', () => {
//       let user                = new User(null, { connection });
//       let userThingRoleField  = user.getField('userThingRole');

//       let result = userThingRoleField.type.getJoinableRelations(connection);
//       expect(result).toEqual([
//         {
//           sourceModelName:  'RoleThing',
//           sourceFieldName:  'roleID',
//           targetModelName:  'Role',
//           targetFieldName:  'id',
//         },
//         {
//           sourceModelName:  'User',
//           sourceFieldName:  'id',
//           targetModelName:  'UserThing',
//           targetFieldName:  'userID',
//         },
//         {
//           sourceModelName:  'UserThing',
//           sourceFieldName:  'roleThingID',
//           targetModelName:  'RoleThing',
//           targetFieldName:  'id',
//         },
//       ]);
//     });

//     it('can recursively parse relations #3', () => {
//       let user        = new User(null, { connection });
//       let rolesField  = user.getField('roles');

//       let result = rolesField.type.getJoinableRelations(connection);
//       expect(result).toEqual([
//         {
//           sourceModelName:  'UserRole',
//           sourceFieldName:  'roleID',
//           targetModelName:  'Role',
//           targetFieldName:  'id',
//         },
//         {
//           sourceModelName:  'User',
//           sourceFieldName:  'id',
//           targetModelName:  'UserRole',
//           targetFieldName:  'userID',
//         },
//       ]);
//     });

//     it('can recursively parse relations #4', () => {
//       let user                = new User(null, { connection });
//       let userThingRoleField  = user.getField('userThingRole');

//       let result = userThingRoleField.type.getJoinableRelations(connection);
//       expect(result).toEqual([
//         {
//           sourceModelName:  'RoleThing',
//           sourceFieldName:  'roleID',
//           targetModelName:  'Role',
//           targetFieldName:  'id',
//         },
//         {
//           sourceModelName:  'User',
//           sourceFieldName:  'id',
//           targetModelName:  'UserThing',
//           targetFieldName:  'userID',
//         },
//         {
//           sourceModelName:  'UserThing',
//           sourceFieldName:  'roleThingID',
//           targetModelName:  'RoleThing',
//           targetFieldName:  'id',
//         },

//       ]);
//     });

//     it('can recursively parse relations #5', () => {
//       let user  = new User(null, { connection });
//       let field = user.getField('primaryRole');

//       let result = field.type.getJoinableRelations(connection);
//       expect(result).toEqual([
//         {
//           sourceModelName:  'User',
//           sourceFieldName:  'primaryRoleID',
//           targetModelName:  'Role',
//           targetFieldName:  'id',
//         },
//       ]);
//     });
//   });
// });
