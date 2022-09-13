import ConnectionBase from '../../connection/connection-base';
import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { Model, ModelClass, Models } from '../../model';
import { QueryEngine } from '../../query-engine';
import Type from '../type';

export declare interface QueryFactoryContext<T extends Model> {
  type: Type;
  self: T;
  connection: ConnectionBase;
  field: Field;
}

export declare interface QueryFactory<T extends Model, M = Models> {
  (context: QueryFactoryContext<T>, models: M, ...args: Array<any>): QueryEngine | Promise<QueryEngine>;
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

declare class RelationalTypeBase<T extends Model, M = Models> extends Type {
  declare public targetModelName: string;
  declare public queryFactory: QueryFactory<T, M>;
  declare public options: string;

  constructor(targetModelName: string, queryFactory: QueryFactory<T, M>, options?: GenericObject);
  getOptions(): GenericObject;
  walkQueryRelations(connection: ConnectionBase, callback: (context: RelationalContext) => void, context: QueryFactoryContext<T>, ...args: Array<any>): Promise<QueryEngine>;
  getTargetModel(connection?: ConnectionBase): ModelClass;
  prepareQuery(context: QueryFactoryContext<T>, args: Array<any>): Promise<QueryEngine>;
}

export default RelationalTypeBase;
