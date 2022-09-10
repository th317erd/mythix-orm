import { ConnectionBase } from '../../connection/connection-base';
import { Field } from '../../field';
import { ModelClass } from '../../model';
import Type, { TypeWrapper } from '../type';

export declare interface ForeignKeyTypeWrapper extends TypeWrapper<ForeignKeyType> {
  (fullyQualifiedName: string | ForeignKeyTypeOptions, options?: ForeignKeyTypeOptions): ForeignKeyType;
}

export declare interface ForeignKeyTypeOptions {
  Field?: Field;
  modelName?: string;
  fieldName?: string;
  onDelete?: 'CASCADE' | 'SET NULL' | 'NO ACTION' | 'SET DEFAULT' | 'RESTRICT'
  onUpdate?: 'CASCADE' | 'SET NULL' | 'NO ACTION' | 'SET DEFAULT' | 'RESTRICT'
}

export declare class ForeignKeyType extends Type {
  public constructor(fullyQualifiedName: string | ForeignKeyTypeOptions, options?: ForeignKeyTypeOptions);
  public parseOptionsAndCheckForErrors(SourceModel: ModelClass, sourceField: Field, connection: ConnectionBase): { Model: ModelClass, Field: Field };
  public getOptions(): ForeignKeyTypeOptions;
  public getTargetModel(connection?: ConnectionBase): ModelClass;
  public getTargetModelName(connection?: ConnectionBase): string;
  public getTargetField(connection?: ConnectionBase): Field;
  public getTargetFieldName(connection?: ConnectionBase): string;

  declare public targetModel: ModelClass | undefined;
  declare public targetField: Field | undefined;
  declare public fullyQualifiedName: string;
}

export const FOREIGN_KEY: ForeignKeyTypeWrapper;
