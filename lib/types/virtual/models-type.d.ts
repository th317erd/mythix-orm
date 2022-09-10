import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { TypeWrapper } from '../type';
import RelationalTypeBase, { QueryFactory } from './relational-type-base';

export declare interface ModelsTypeWrapper extends TypeWrapper<ModelsType> {
  (targetModelName: string, queryFactory: QueryFactory, options?: GenericObject): ModelsType;
}

export declare class ModelsType extends RelationalTypeBase {
  public fieldNameToOperationName(field: Field, operation: string, rootMethod: boolean): string;
}

export const Models: ModelsTypeWrapper;
