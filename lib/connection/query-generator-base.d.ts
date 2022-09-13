import Field from '../field';
import { GenericObject } from '../interfaces/common';
import { ModelClass } from '../model';
import { QueryEngine } from '../query-engine';
import ConnectionBase from './connection-base';
import { AverageLiteral, CountLiteral, DistinctLiteral, FieldLiteral, MaxLiteral, MinLiteral, SumLiteral } from './literals';
import LiteralBase from './literals/literal-base';

export declare interface GetEscapedFieldNameOptions {
  fieldNameOnly?: boolean;
}

export declare interface GetEscapedTableNameNameOptions {
  tableNamePrefix?: string;
}

export declare interface GetEscapedColumnNameOptions extends GetEscapedTableNameNameOptions {
  columnNamePrefix?: string
  columnNameOnly?: boolean;
}

export declare interface GetEscapedProjectionNameOptions extends GetEscapedColumnNameOptions, GetEscapedFieldNameOptions {
  noProjectionAliases?: boolean;
}

export declare interface GetEscapedModelFieldsOptions extends GetEscapedProjectionNameOptions {
  asProjection?: boolean;
  asColumn?: boolean;
}

export declare interface ProjectedFieldInfo {
  projectedName: string;
  Model: ModelClass;
  modelName: string;
  Field: Field;
  fieldName: string;
  direction: string;
  fullFieldName: string;
}

export declare interface JoinTableInfo {
  operator: string;
  joinType: string | LiteralBase;
  rootModelName: string;
  joinModel: ModelClass;
  joinModelName: string;
  leftSideModel: ModelClass;
  leftSideModelName: string;
  leftQueryContext: GenericObject;
  leftSideField: Field;
  rightSideModel: ModelClass;
  rightSideModelName: string;
  rightQueryContext: GenericObject;
  rightSideField: Field;
}

export declare interface FieldDirectionInfo {
  hasDirection: boolean;
  direction: string;
  fieldName: string;
}

export declare interface FieldOrderInfo {
  Model: ModelClass;
  Field: Field;
  direction: string;
}

declare class QueryGeneratorBase {
  public constructor(connection);
  public stackAssign(obj: GenericObject, ...args: Array<GenericObject>): GenericObject;
  public getOptionsCache(options: GenericObject, keyPath: string, initialValue: any): any;
  public setOptionsCache(options: GenericObject, keyPath: string, value: any): void;
  public escape(field: Field, value: any, options?: GenericObject): string;
  public escapeID(value: LiteralBase | string, options?: GenericObject): string;
  public getEscapedFieldName(Model: ModelClass | null | undefined, field: Field, options?: GetEscapedFieldNameOptions): string;
  public getEscapedColumnName(Model: ModelClass | null | undefined, field: Field, options?: GetEscapedColumnNameOptions): string;
  public getEscapedTableName(modelOrField: ModelClass | Field, options?: GetEscapedTableNameNameOptions): string;
  public getEscapedProjectionName(Model: ModelClass | null | undefined, field: Field, options?: GetEscapedProjectionNameOptions): string;
  public getEscapedModelFields(Model: ModelClass, options?: GetEscapedModelFieldsOptions): { [ key: string ]: string };
  public getAllModelsUsedInQuery(queryEngine: QueryEngine, options?: GenericObject): Array<ModelClass>;
  public getProjectionRequiredFields(queryEngine: QueryEngine, options?: GenericObject): Map<string, ProjectedFieldInfo>;
  public sortedProjectedFields(projectedFields: Array<LiteralBase | string>, options?: GenericObject): Array<LiteralBase | string>;
  public getProjectionFromQueryEngine(queryEngine: QueryEngine, options?: GenericObject): Array<LiteralBase | string | ProjectedFieldInfo>;
  public isFieldIdentifier(value: string): boolean;
  public getProjectedFields(queryEngine: QueryEngine, options?: GenericObject, asMap?: false | undefined): Array<string>;
  public getProjectedFields(queryEngine: QueryEngine, options?: GenericObject, asMap?: true): Map<string, string>;

  public getJoinTableInfoFromQueryContexts(
    leftQueryContext: GenericObject,
    rightQueryContext: GenericObject,
    joinType: string | LiteralBase,
    options?: GenericObject
  ): JoinTableInfo;

  public getFieldDirectionSpecifier(order: LiteralBase): LiteralBase;
  public getFieldDirectionSpecifier(order: string | Field): FieldDirectionInfo;

  public getOrderLimitOffset(
    queryEngine: QueryEngine,
    options?: GenericObject,
  ): {
    limit: number | undefined,
    offset: number | undefined,
    order: Array<FieldOrderInfo>,
  };

  public getQuerySliceFromQueryPart(queryPart: GenericObject): Array<GenericObject>;
  public _averageLiteralToString(literal: AverageLiteral, options?: GenericObject): string;
  public _countLiteralToString(literal: CountLiteral, options?: GenericObject): string;
  public _distinctLiteralToString(literal: DistinctLiteral, options?: GenericObject): string;
  public _fieldLiteralToString(literal: FieldLiteral, options?: GenericObject): string;
  public _maxLiteralToString(literal: MaxLiteral, options?: GenericObject): string;
  public _minLiteralToString(literal: MinLiteral, options?: GenericObject): string;
  public _sumLiteralToString(literal: SumLiteral, options?: GenericObject): string;
  public toConnectionString(queryEngine: QueryEngine, options?: GenericObject): string;

  declare public connection: ConnectionBase;
}

export default QueryGeneratorBase;
