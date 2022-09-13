import Field from '../../field';
import { GenericObject } from '../../interfaces/common';
import { Model, Models as _Models } from '../../model';
import { TypeWrapper } from '../type';
import RelationalTypeBase, { QueryFactory } from './relational-type-base';

export declare interface Models<T extends Model, M = _Models> extends TypeWrapper<ModelsType<T, M>> {
  (targetModelName: string, queryFactory: QueryFactory<T, M>, options?: GenericObject): ModelsType<T, M>;
}

export declare class ModelsType<T extends Model, M = _Models> extends RelationalTypeBase<T, M> {
  public fieldNameToOperationName(field: Field, operation: string, rootMethod: boolean): string;
}

export function Models<T extends Model, M = _Models>(targetModelName: string, queryFactory: QueryFactory<T, M>, options?: GenericObject): ModelsType<T, M>;
