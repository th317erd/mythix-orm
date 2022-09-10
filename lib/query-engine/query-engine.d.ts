import { ConnectionBase } from '../connection/connection-base';
import { LiteralBase } from '../connection/literals';
import { GenericObject } from '../interfaces/common';
import { Model, ModelClass } from '../model';
import { Field } from '../field';

export declare interface QueryEngineOptions<T = ConnectionBase> {
  connection: T;
  [ key: string ]: any;
}

export declare interface QueryEngineClass<T = ConnectionBase> {
  new(data: GenericObject, options?: QueryEngineOptions<T>): QueryEngine<T>;
}

export declare interface CallableInterface<T> {
  (...args: Array<any>): QueryEngine<T>;
  [ key: string ]: QueryEngine<T>;
}

export declare class QueryEngine<T = ConnectionBase> {
  // QueryEngineBase
  static generateID(): number;
  static isQueryContext(value: any): boolean;
  static isQuery(value: any): boolean;
  static queryContextType(queryContext: GenericObject): { hasCondition: boolean; hasField: boolean; hasModel: boolean; };

  public getModelScopeClass(): QueryEngine<T>;
  public getFieldScopeClass(): QueryEngine<T>;
  public _inheritContext(context: GenericObject, name: string, ...args: Array<GenericObject>): GenericObject;
  public _fetchScope(...scopeNames: Array<string>): QueryEngine<T>;
  public _newQueryEngineScope(): QueryEngine<T>;
  public _newModelScope(Model: ModelClass): QueryEngine<T>;
  public _newFieldScope(Field: Field): QueryEngine<T>;
  public constructor(context: QueryEngineOptions<T>);
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
  public getQueryEngineScope(): QueryEngine<T>;
  public getQueryEngineClass(): QueryEngineClass<T>;
  public clone(): QueryEngine<T>;

  // QueryEngine
  public getModelScopeClass(): QueryEngineClass;
  public getFieldScopeClass(): QueryEngineClass;
  public Model(modelName: string): QueryEngine<T>;
  public unscoped(context?: GenericObject): QueryEngine<T>;
  public toString(options?: GenericObject): string;
  public MERGE(queryEngine: QueryEngine<T>): QueryEngine<T>;
  public all(options): Promise<Array<Model>> | AsyncGenerator<Model>;
  public first(limit: number | null | undefined, options?: GenericObject): Promise<Model | undefined>;
  public last(limit: number | null | undefined, options?: GenericObject): Promise<Model | undefined>;
  public update(attributes: Model | GenericObject, options?: GenericObject): Promise<number>;
  public destroy(options?: GenericObject): Promise<number>;
  public average(field: Field | string, options?: GenericObject): Promise<number>;
  public count(field: Field | string, options?: GenericObject): Promise<number>;
  public min(field: Field | string, options?: GenericObject): Promise<number>;
  public max(field: Field | string, options?: GenericObject): Promise<number>;
  public sum(field: Field | string, options?: GenericObject): Promise<number>;
  public pluck(fields: string | Array<string>, options?: GenericObject): Promise<Array<any> | Array<Array<any>>>;
  public exists(options?: GenericObject): Promise<boolean>;

  // ModelScope
  public _getField(fieldName: string): Field | undefined;
  public _getQueryEngineClass(): QueryEngineClass;
  public Field(fieldName: string): QueryEngine<T>;
  public LIMIT(value: number): QueryEngine<T>;
  public OFFSET(value: number): QueryEngine<T>;
  public ORDER(...args: Array<string | Array<string>>): QueryEngine<T>;
  public PROJECT(...args: Array<string | ModelClass | LiteralBase | Field>): QueryEngine<T>;

  declare public NOT: {
    (): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public AND: {
    (query: QueryEngine<T>): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public OR: {
    (query: QueryEngine<T>): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public DISTINCT: {
    (fullyQualifiedName: string | Field): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public INNER_JOIN: {
    (): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public LEFT_JOIN: {
    (): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public RIGHT_JOIN: {
    (): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public FULL_JOIN: {
    (): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public CROSS_JOIN: {
    (): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  declare public JOIN: {
    (type: string | LiteralBase): QueryEngine<T>;
    [ key: string ]: QueryEngine<T>;
  };

  // FieldScope
  public _fetchOperatorValue(value: any): any;
  public EQ(value: any): QueryEngine<T>;
  public NEQ(value: any): QueryEngine<T>;
  public GT(value: any): QueryEngine<T>;
  public GTE(value: any): QueryEngine<T>;
  public LT(value: any): QueryEngine<T>;
  public LTE(value: any): QueryEngine<T>;
  public LIKE(value: string, options?: { caseSensitive: boolean }): QueryEngine<T>;
  public NOT_LIKE(value: string, options?: { caseSensitive: boolean }): QueryEngine<T>;

  [ key: string ]: any;
}

export class ModelScope extends QueryEngine { }
export class FieldScope extends QueryEngine { }
