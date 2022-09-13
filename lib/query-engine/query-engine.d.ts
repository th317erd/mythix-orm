import ConnectionBase from '../connection/connection-base';
import { LiteralBase } from '../connection/literals';
import { GenericObject } from '../interfaces/common';
import { Model, ModelClass } from '../model';
import Field from '../field';

export declare interface QueryEngineOptions {
  connection: ConnectionBase;
  [ key: string ]: any;
}

export declare type QueryEngineClass = typeof QueryEngine;

export declare interface CallableInterface {
  (...args: Array<any>): QueryEngine;
  [ key: string ]: QueryEngine;
}

export declare class QueryEngine<T = ConnectionBase> {
  // QueryEngineBase
  static generateID(): number;
  static isQueryContext(value: any): boolean;
  static isQuery(value: any): boolean;
  static queryContextType(queryContext: GenericObject): { hasCondition: boolean; hasField: boolean; hasModel: boolean; };

  public getModelScopeClass(): QueryEngine;
  public getFieldScopeClass(): QueryEngine;
  public _inheritContext(context: GenericObject, name: string, ...args: Array<GenericObject>): GenericObject;
  public _fetchScope(...scopeNames: Array<string>): QueryEngine;
  public _newQueryEngineScope(): QueryEngine;
  public _newModelScope(Model: ModelClass): QueryEngine;
  public _newFieldScope(Field: Field): QueryEngine;
  public constructor(context: QueryEngineOptions);
  public _getTopContextID(): number;
  public _getRawQueryContext(): GenericObject;
  public _getRawQuery(): Array<GenericObject>;
  public _isLastPartControl(): boolean;
  public _isLastPartCondition(): boolean;
  public _queryHasConditions(): boolean;
  public _queryHasJoins(): boolean;
  public _debugQuery(): void;
  public _addToQuery(queryPart: GenericObject, context: GenericObject): void;
  public getConnection(): ConnectionBase;
  public getModel(modelName: string): ModelClass | undefined;
  public getQueryEngineScope(): QueryEngine;
  public getQueryEngineClass(): QueryEngineClass;
  public clone(): QueryEngine;

  // QueryEngine
  public getModelScopeClass(): QueryEngineClass;
  public getFieldScopeClass(): QueryEngineClass;
  public Model(modelName: string): QueryEngine;
  public unscoped(context?: GenericObject): QueryEngine;
  public toString(options?: GenericObject): string;
  public MERGE(queryEngine: QueryEngine): QueryEngine;
  public all(options?: GenericObject): Promise<Array<Model>>;
  public fetchAll(options?: GenericObject): AsyncGenerator<Model>;
  public first(limit?: number | null | undefined, options?: GenericObject): Promise<Model | undefined>;
  public last(limit?: number | null | undefined, options?: GenericObject): Promise<Model | undefined>;
  public update(attributes: Model | GenericObject, options?: GenericObject): Promise<number>;
  public destroy(options?: GenericObject): Promise<number>;
  public average(field: Field | string, options?: GenericObject): Promise<number>;
  public count(field: Field | string, options?: GenericObject): Promise<number>;
  public min(field: Field | string, options?: GenericObject): Promise<number>;
  public max(field: Field | string, options?: GenericObject): Promise<number>;
  public sum(field: Field | string, options?: GenericObject): Promise<number>;
  public pluck(fields: string | Array<string>, options?: GenericObject): Promise<Array<any>>;
  public exists(options?: GenericObject): Promise<boolean>;

  // ModelScope
  public _getField(fieldName: string): Field | undefined;
  public _getQueryEngineClass(): QueryEngineClass;
  public Field(fieldName: string): QueryEngine;
  public LIMIT(value: number): QueryEngine;
  public OFFSET(value: number): QueryEngine;
  public ORDER(...args: Array<string | Array<string>>): QueryEngine;
  public PROJECT(...args: Array<string | ModelClass | LiteralBase | Field>): QueryEngine;

  declare public NOT: {
    (): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public AND: {
    (query: QueryEngine): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public OR: {
    (query: QueryEngine): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public DISTINCT: {
    (fullyQualifiedName: string | Field): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public INNER_JOIN: {
    (): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public LEFT_JOIN: {
    (): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public RIGHT_JOIN: {
    (): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public FULL_JOIN: {
    (): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public CROSS_JOIN: {
    (): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  declare public JOIN: {
    (type: string | LiteralBase): QueryEngine;
    [ key: string ]: QueryEngine;
  };

  // FieldScope
  public _fetchOperatorValue(value: any): any;
  public EQ(value: any): QueryEngine;
  public NEQ(value: any): QueryEngine;
  public GT(value: any): QueryEngine;
  public GTE(value: any): QueryEngine;
  public LT(value: any): QueryEngine;
  public LTE(value: any): QueryEngine;
  public LIKE(value: string, options?: { caseSensitive: boolean }): QueryEngine;
  public NOT_LIKE(value: string, options?: { caseSensitive: boolean }): QueryEngine;

  [ key: string ]: any;
}

export class ModelScope extends QueryEngine { }
export class FieldScope extends QueryEngine { }