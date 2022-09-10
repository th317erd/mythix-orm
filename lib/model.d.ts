import { GenericObject } from './interfaces/common';
import { ConnectionBase } from './connection/connection-base';
import { Field, FieldDefinition } from './field';
import { QueryEngine, QueryEngineClass } from './query-engine/query-engine';

export class CacheKey {
  constructor(number: number);
  valueOf(): number;
}

export declare interface ModelOptions extends GenericObject {
  connection: ConnectionBase;
}

export declare interface ModelClass {
  new(data: GenericObject, options?: ModelOptions): Model;
}

export declare interface Models {
  [ key: string ]: ModelClass;
}

export declare interface HookContext {
  connection: ConnectionBase;
  Model: ModelClass;
  options: GenericObject;
}

export declare interface DirtyChanges {
  [ key: string ]: { previous: any; current: any };
}

export declare type LooseFields = Array<Field | FieldDefinition> | { [ key: string ]: Field | FieldDefinition } | Map<string, Field | FieldDefinition> | Set<Field | FieldDefinition>;
export declare type Fields = Array<Field> | { [ key: string ]: Field } | Map<string, Field> | Set<Field>;

export declare interface IterateFieldsContext {
  field: Field;
  fieldName: string;
  fields: Fields;
  index: number;
  stop: () => void;
  isStopped: () => boolean;
}

export declare type IterateFieldsCallback = (context: IterateFieldsContext) => any;

export declare class Model {
  declare public static fields: LooseFields | undefined;
  declare public static _sortedFields: Array<Field> | null;
  declare public static _isMythixModel: boolean;
  declare public static _mythixBoundConnection: ConnectionBase | null;
  declare public static where: QueryEngine;
  declare public static $: QueryEngine;

  public static isModelClass(value: any): boolean;
  public static isModel(value: any): boolean;
  public static toString(showFields: boolean): string;

  public static _getConnection(connection?: ConnectionBase): ConnectionBase;
  public _getConnection(connection?: ConnectionBase): ConnectionBase;

  public static getConnection(connection?: ConnectionBase): ConnectionBase;
  public getConnection(connection?: ConnectionBase): ConnectionBase;

  public static bindConnection(connection: ConnectionBase): ModelClass;

  public static getQueryEngineClass(connection?: ConnectionBase): QueryEngineClass;
  public getQueryEngineClass(connection?: ConnectionBase): QueryEngineClass;

  public static getUnscopedQueryEngine(connection?: ConnectionBase, options?: GenericObject): QueryEngine;
  public getUnscopedQueryEngine(connection?: ConnectionBase, options?: GenericObject): QueryEngine;

  public static defaultScope(query: QueryEngine): QueryEngine;
  public defaultScope(query: QueryEngine): QueryEngine;

  public static getQueryEngine(connection?: ConnectionBase, options?: GenericObject): QueryEngine;
  public getQueryEngine(connection?: ConnectionBase, options?: GenericObject): QueryEngine;

  public static getForeignKeyFieldsMap(connection?: ConnectionBase): Map<string, Array<{ targetFieldName: string, sourceFieldName: string }>>;
  public getForeignKeyFieldsMap(connection?: ConnectionBase): Map<string, Array<{ targetFieldName: string, sourceFieldName: string }>>;

  public static getForeignKeysTargetModels(connection?: ConnectionBase): Map<string, ModelClass>;
  public getForeignKeysTargetModels(connection?: ConnectionBase): Map<string, ModelClass>;

  public static getForeignKeysTargetModelNames(connection?: ConnectionBase): Array<string>;
  public getForeignKeysTargetModelNames(connection?: ConnectionBase): Array<string>;

  public static getForeignKeysTargetFieldNames(connection: ConnectionBase | null | undefined, modelName: string): Array<{ targetFieldName: string, sourceFieldName: string }>;
  public getForeignKeysTargetFieldNames(connection: ConnectionBase | null | undefined, modelName: string): Array<{ targetFieldName: string, sourceFieldName: string }>;

  public static getForeignKeysTargetField(connection: ConnectionBase | null | undefined, modelName: string, fieldName: string): { targetFieldName: string, sourceFieldName: string } | undefined;
  public getForeignKeysTargetField(connection: ConnectionBase | null | undefined, modelName: string, fieldName: string): { targetFieldName: string, sourceFieldName: string } | undefined;

  public static isForeignKeyTargetModel(connection: ConnectionBase | null | undefined, modelName: string): boolean;
  public isForeignKeyTargetModel(connection: ConnectionBase | null | undefined, modelName: string): boolean;

  public static getTableName(connection?: ConnectionBase): string;
  public getTableName(connection?: ConnectionBase): string;

  public static getModelName(): string;
  public getModelName(): string;

  public static getSingularName(): string;
  public getSingularName(): string;

  public static getPluralModelName(): string;
  public getPluralModelName(): string;

  public static getModel(): ModelClass;
  public getModel(): ModelClass;

  public static getFields(fieldNames?: Array<string>): Fields;
  public getFields(fieldNames?: Array<string>): Fields;

  public static getSortedFields(fieldNames?: Array<string>): Array<Field>;
  public getSortedFields(fieldNames?: Array<string>): Array<Field>;

  public static mergeFields(mergeFields?: Fields): Fields;

  public static initializeFields(fields: Fields): Fields;

  public static iterateFields(callback: IterateFieldsCallback, fields: LooseFields, sorted: boolean): Array<any>;
  public iterateFields(callback: IterateFieldsCallback, fields: LooseFields, sorted: boolean): Array<any>;

  public static hasRemoteFieldValues(): boolean;
  public hasRemoteFieldValues(): boolean;

  public static getPrimaryKeyField(): Field | undefined;
  public getPrimaryKeyField(): Field | undefined;

  public static getPrimaryKeyFieldName(): string | undefined;
  public getPrimaryKeyFieldName(): string | undefined;

  public static primaryKeyHasRemoteValue(): boolean;
  public primaryKeyHasRemoteValue(): boolean;

  public static getField(findFieldName: string): Field | undefined;
  public getField(findFieldName: string): Field | undefined;

  public static hasField(fieldName: string): boolean;
  public hasField(fieldName: string): boolean;

  public static getConcreteFieldCount(): number;
  public getConcreteFieldCount(): number;

  public static defaultOrder(options?: GenericObject): Array<string>;

  public static getWhereWithConnection(options?: { connection: ConnectionBase }): QueryEngine;
  public getWhereWithConnection(options?: { connection: ConnectionBase }): QueryEngine;

  public static create(models: Array<Model | GenericObject>, options?: GenericObject): Promise<Array<Model>>;
  public static create(models: Model | GenericObject, options?: GenericObject): Promise<Model>;
  public static count(options?: GenericObject): Promise<number>;
  public static all(options?: GenericObject): Promise<Array<Model>>;
  public static fetchAll(options?: GenericObject): AsyncGenerator<Model>;
  public static first(limit?: number, options?: GenericObject): Promise<Model | undefined>;
  public static last(limit?: number, options?: GenericObject): Promise<Model | undefined>;
  public static pluck(fields: string | Array<string>, options?: GenericObject): Promise<Array<any>>;

  public constructor(data: GenericObject, _options?: ModelOptions);
  public getOptions(): ModelOptions;
  public _constructor(data?: GenericObject): void;
  public _constructFields(): void;
  public _constructField(fieldName: string, field: Field): void;
  public _initializeModelData(data?: GenericObject): void;
  public _castFieldValue(field: Field, value: any): any;
  public _initializeFieldData(fieldName: string, field: Field, fieldValue: any, data?: GenericObject): void;
  public _getDirtyFields(options?: GenericObject): DirtyChanges;
  public _getFieldValue(fieldName: string, field: Field): any;
  public _setFieldValue(fieldName: string, field: Field, value: any): void;
  public isPersisted(): boolean;
  public updateDirtyID(): void;
  public isDirty(fieldName?: string): boolean;
  public clearDirty(fieldName?: string): void;
  public getDirtyFields(options?: GenericObject): Array<Field>;
  public getDataValue(fieldName: string): any;
  public setDataValue(fieldName: string, value: any): void;
  public getAttributes(): GenericObject;
  public setAttributes(attributes: GenericObject, noPrimaryKey?: boolean): void;
  public hasValidPrimaryKey(): boolean;
  public onValidate(context: HookContext): Promise<any>;
  public onBeforeCreate(context: HookContext): Promise<any>;
  public onBeforeUpdate(context: HookContext): Promise<any>;
  public onBeforeSave(context: HookContext): Promise<any>;
  public onAfterCreate(context: HookContext): Promise<any>;
  public onAfterUpdate(context: HookContext): Promise<any>;
  public onAfterSave(context: HookContext): Promise<any>;
  public save(options?: GenericObject): boolean;
  public reload(options?: GenericObject): void;
  public destroy(options?: GenericObject): number;
  public toString(): string;
  public toJSON(): GenericObject;

  declare public _options: ModelOptions;
  declare public _mythixModelInstance: boolean;
  declare public _connection: ConnectionBase | null;
  declare public _fieldData: GenericObject;
  declare public _dirtyFieldData: GenericObject;
  declare public _typeData: GenericObject;
  declare public dirtyID: CacheKey;
  declare public _persisted: boolean;
  declare public __order: number;
  declare public __assignedRelatedModels: Map<string, ModelClass>;
  declare public changes: DirtyChanges;
  declare public where: QueryEngine;
  declare public $: QueryEngine;
}