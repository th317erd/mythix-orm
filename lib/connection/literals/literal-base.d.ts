import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { FullyQualifiedFieldDefinition } from '../../utils/model-utils';
import ConnectionBase from '../connection-base';

declare class LiteralBase {
  declare public static _isMythixLiteral: boolean;

  public static isLiteralClass(value: any): boolean;
  public static isLiteral(value: any): boolean;
  public static isLiteralType(value: any): boolean;
  public static isAggregate(): boolean;

  public constructor(literal: any, options?: GenericObject);
  public fullyQualifiedNameToDefinition(fullyQualifiedName: LiteralBase | string | Field): LiteralBase | FullyQualifiedFieldDefinition;
  public definitionToField(connection, definition): LiteralBase | Field;
  public toString(connection: ConnectionBase, options?: GenericObject): string;

  declare public literal: any;
  declare public options: GenericObject;
}

export default LiteralBase;
