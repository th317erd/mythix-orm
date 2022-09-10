import EventEmitter from 'events';
import { Moment } from 'moment';
import Field from '../field';
import { GenericObject } from '../interfaces/common';
import { IterateFieldsCallback, ModelClass, Models, Model } from '../model';
import { QueryEngine, QueryEngineClass } from '../query-engine/query-engine';
import { BigIntType, BlobType, BooleanType, CharType, DateTimeType, DateType, IntegerType, NumericType, RealType, StringType, TextType, Type, UUIDV1Type, UUIDV3Type, UUIDV4Type, UUIDV5Type, XIDType } from '../types';
import { FullyQualifiedFieldDefinition } from '../utils/model-utils';
import { AverageLiteral, CountLiteral, DistinctLiteral, FieldLiteral, MaxLiteral, MinLiteral, SumLiteral } from './literals';
import LiteralBase from './literals/literal-base';
import QueryGeneratorBase from './query-generator-base';

export declare interface ConnectionBaseOptions {
  QueryEngine: QueryEngineClass;
  queryGenerator: QueryGeneratorBase;
  models: Models | Array<ModelClass>;
}

export declare type ModelCache = Map<ModelClass, Map<string, any>>;

export declare interface PreparedModels {
  models: Array<Model>;
  dirtyFields: Array<Field>;
  dirtyModels: Array<Model>;
  _mythixPreparedModels: boolean;
}

declare class ConnectionBase extends EventEmitter {
  declare public static dialect: string;
  declare public static _isMythixConnection: boolean;

  public static isConnectionClass(value: any): boolean;
  public static isConnection(value: any): boolean;

  declare public dialect: string;
  declare protected _models: Models;
  declare protected _options: ConnectionBaseOptions;
  declare protected _modelCache: ModelCache;
  declare protected queryGenerator: any; // TODO: Needs proper type

  public constructor(options?: ConnectionBaseOptions);
  public getDefaultOrder(Model: ModelClass, options?: GenericObject): Array<string>;
  public isLimitSupportedInContext(options?: GenericObject): boolean;
  public isOrderSupportedInContext(options?: GenericObject): boolean | string;
  public _getFromModelCache(Model: ModelClass, key: string, defaultValue?: any): any;
  public _setToModelCache<T>(Model: ModelClass, key: string, value: T): T;
  public getOptions(): ConnectionBaseOptions;
  public isStarted(): boolean;
  public toQueryEngine(queryEngineLike: any): QueryEngine | undefined;
  public registerModel<T = ModelClass>(Model: T): T;
  public registerModels(models: Models | Array<ModelClass>): Models | undefined;
  public findModelField(finder: IterateFieldsCallback): Array<Field>;
  public parseQualifiedName(fullyQualifiedName: string): FullyQualifiedFieldDefinition;
  public getModels(): Models;
  public getModel(modelName: string): ModelClass | undefined;
  public getField(fieldName: string, modelName?: string): Field | undefined;
  public getQueryEngineClass(): QueryEngineClass;
  public getQueryGenerator(): QueryGeneratorBase;
  public setQueryGenerator(queryGenerator: QueryGeneratorBase): void;
  public _escape(value: any): string;
  public escape(field: Field, value: any, options?: GenericObject): string;
  public _escapeID(value: string): string;
  public escapeID(value: LiteralBase | string, options?: GenericObject): string;
  public _averageLiteralToString(literal: AverageLiteral, options?: GenericObject): string;
  public _countLiteralToString(literal: CountLiteral, options?: GenericObject): string;
  public _distinctLiteralToString(literal: DistinctLiteral, options?: GenericObject): string;
  public _fieldLiteralToString(literal: FieldLiteral, options?: GenericObject): string;
  public _maxLiteralToString(literal: MaxLiteral, options?: GenericObject): string;
  public _minLiteralToString(literal: MinLiteral, options?: GenericObject): string;
  public _sumLiteralToString(literal: SumLiteral, options?: GenericObject): string;
  public literalToString(literal: LiteralBase, options?: GenericObject): string;
  public _bigintTypeToString(type: BigIntType): string;
  public _blobTypeToString(type: BlobType): string;
  public _booleanTypeToString(type: BooleanType): string;
  public _charTypeToString(type: CharType): string;
  public _dateTypeToString(type: DateType): string;
  public _datetimeTypeToString(type: DateTimeType): string;
  public _numericTypeToString(type: NumericType): string;
  public _realTypeToString(type: RealType): string;
  public _integerTypeToString(type: IntegerType);
  public _stringTypeToString(type: StringType): string
  public _textTypeToString(type: TextType): string;
  public _uuidV1TypeToString(type: UUIDV1Type): string;
  public _uuidV3TypeToString(type: UUIDV3Type): string;
  public _uuidV4TypeToString(type: UUIDV4Type): string;
  public _uuidV5TypeToString(type: UUIDV5Type): string;
  public _xidTypeToString(type: XIDType): string;
  public typeToString(type: Type, options?: GenericObject): string;
  public convertDateToDBTime(value: Date | Moment): Date;
  public ensureAllModelsAreInstances(Model: ModelClass, models: Array<Model | GenericObject> | PreparedModels, options?: GenericObject): Array<Model>;
  public prepareAllModelsForOperation(Model: ModelClass, models: Array<Model | GenericObject> | PreparedModels, options?: GenericObject): PreparedModels;
  public splitModelAndSubModels(Model: ModelClass, primaryModel: Model, relationMap?: Map<string, Set<Model>>): Map<string, Set<Model>>;
  public prepareAllModelsAndSubModelsForOperation(Model: ModelClass, models: Array<Model>, options?: GenericObject): Map<string, Array<Model>>;

  public bulkModelOperation(
    Model: ModelClass,
    models: Array<Model | GenericObject> | PreparedModels,
    options: GenericObject | null | undefined,
    beforeCallback: ((Model: ModelClass, batchModelInstances: Array<Model>, options: GenericObject, queryGenerator: QueryGeneratorBase) => Promise<void>) | null | undefined,
    callback: (Model: ModelClass, preparedModels: PreparedModels, options: GenericObject, queryGenerator: QueryGeneratorBase) => Promise<void>,
    afterCallback: ((Model: ModelClass, models: Array<Model>, options: GenericObject, queryGenerator: QueryGeneratorBase) => Promise<void>) | null | undefined,
    afterOperationCallback: ((Model: ModelClass, dirtyModels: Set<Model>, options: GenericObject, queryGenerator: QueryGeneratorBase) => Promise<void>) | null | undefined,
  ): Promise<Array<Model> | undefined>;

  public setPersisted(models: Array<Models> | PreparedModels, value: boolean): void;
  public start(): Promise<void>;
  public stop(): Promise<void>;
  public insert(Model: ModelClass, models: Array<Model | GenericObject> | Model | GenericObject, options?: GenericObject): Promise<Array<Models> | undefined>;
  public upsert(Model: ModelClass, models: Array<Model | GenericObject> | Model | GenericObject, options?: GenericObject): Promise<Array<Models> | undefined>;
  public update(Model: ModelClass, models: Array<Model | GenericObject> | Model | GenericObject, options?: GenericObject): Promise<Array<Models> | undefined>;
  public updateAll(queryEngine: QueryEngine | ModelClass, model: Model | GenericObject, options?: GenericObject): Promise<any>;
  public destroyModels(Model: ModelClass, models: Array<Model> | Model, options?: GenericObject): Promise<Array<Model>>;
  public destroy(queryEngineOrModel: QueryEngine | ModelClass, modelsOrOptions: Array<Models> | Model | GenericObject, options?: GenericObject): Promise<any>;
  public select(queryEngine: QueryEngine, options?: GenericObject): AsyncIterator<Model>;
  public aggregate(queryEngine: QueryEngine, literal: LiteralBase, options?: GenericObject): Promise<number>;
  public average(queryEngine: QueryEngine, field: Field | string, options?: GenericObject): Promise<number>;
  public count(queryEngine: QueryEngine, field: Field | string, options?: GenericObject): Promise<number>;
  public min(queryEngine: QueryEngine, field: Field | string, options?: GenericObject): Promise<number>;
  public max(queryEngine: QueryEngine, field: Field | string, options?: GenericObject): Promise<number>;
  public sum(queryEngine: QueryEngine, field: Field | string, options?: GenericObject): Promise<number>;
  public pluck(queryEngine: QueryEngine, fields: Array<Field> | Array<string> | Field | string, options?: GenericObject): Promise<Array<any> | Array<Array<any>>>;
  public exists(queryEngine: QueryEngine, options?: GenericObject): Promise<boolean>;
}

export default ConnectionBase;
