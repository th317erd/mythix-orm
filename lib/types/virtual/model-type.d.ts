import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { TypeWrapper } from '../type';
import RelationalTypeBase, { QueryFactory } from './relational-type-base';
import { Model as _Model, Models } from '../../model';

export declare interface Model<T extends _Model, M = Models> extends TypeWrapper<ModelType<T, M>> {
  (targetModelName: string, queryFactory: QueryFactory<T, M>, options?: GenericObject): ModelType<T, M>;
}

export declare class ModelType<T extends _Model, M = Models> extends RelationalTypeBase<T, M> {
  public fieldNameToOperationName(field: Field, operation: string, rootMethod: boolean): string;
}

export function Model<T extends _Model, M = Models>(targetModelName: string, queryFactory: QueryFactory<T, M>, options?: GenericObject): ModelType<T, M>;
