import Field from '../field';
import { GenericObject } from '../interfaces/common';
import { QueryEngine } from '../query-engine';
import ConnectionBase from './connection-base';
import { AverageLiteral, CountLiteral, DistinctLiteral, FieldLiteral, MaxLiteral, MinLiteral, SumLiteral } from './literals';
import LiteralBase from './literals/literal-base';

declare class QueryGeneratorBase {
  public constructor(connection);
  public stackAssign(obj: GenericObject, ...args: Array<GenericObject>): GenericObject;
  public setOptionsCache(options: GenericObject, keyPath: string, value: any): void;
  public escape(field: Field, value: any, options?: GenericObject): string;
  public escapeID(value: LiteralBase | string, options?: GenericObject): string;

  public _getLiteralAlias(literal: LiteralBase, options?: GenericObject);
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
