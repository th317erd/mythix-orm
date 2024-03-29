import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { FullyQualifiedFieldDefinition } from '../../utils/model-utils';
import ConnectionBase from '../connection-base';
import LiteralBase from './literal-base';

declare class LiteralFieldBase extends LiteralBase {
  public static isFieldRequired(): boolean;

  public constructor(fullyQualifiedName: Field | string, options?: GenericObject);
  getFullyQualifiedFieldName(): string | undefined;
  getField(connection: ConnectionBase): Field | undefined;

  declare public definition: FullyQualifiedFieldDefinition | undefined;
}

export default LiteralFieldBase;
