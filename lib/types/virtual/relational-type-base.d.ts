import ConnectionBase from '../../connection/connection-base';
import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { Model, ModelClass } from '../../model';
import { QueryEngine } from '../../query-engine';
import Type from '../type';

export declare interface QueryFactoryContext {
  userQuery: QueryEngine | GenericObject | Array<GenericObject> | undefined;
  type: Type;
  self: Model;
  connection: ConnectionBase;
  field: Field;
  [ key: string ]: ModelClass | any;
}

export declare interface QueryFactory {
  (context: QueryFactoryContext, ...args: Array<any>): QueryEngine;
}

export declare interface RelationalInfo {
  Model: ModelClass;
  modelName: string;
  field: Field | null;
  fieldName: string | null;
}

export declare interface RelationalContext {
  PrimaryModel: ModelClass;
  TargetModel: ModelClass;
  TargetField: Field;
  target: RelationalInfo;
  source: RelationalInfo;
}

declare class RelationalTypeBase extends Type {
  declare public targetModelName: string;
  declare public queryFactory: QueryFactory;
  declare public options: string;

  constructor(targetModelName: string, queryFactory: QueryFactory, options?: GenericObject);
  getOptions(): GenericObject;
  walkQueryRelations(connection: ConnectionBase, callback: (context: RelationalContext) => void, context: QueryFactoryContext, ...args: Array<any>): Promise<QueryEngine>;
  getTargetModel(connection?: ConnectionBase): ModelClass;
  prepareQuery(context: QueryFactoryContext, args: Array<any>): Promise<QueryEngine>;
}

export default RelationalTypeBase;
