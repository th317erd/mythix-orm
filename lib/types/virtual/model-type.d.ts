import { Field } from "../../field";
import { GenericObject } from "../../interfaces/common";
import { TypeWrapper } from "../type";
import RelationalTypeBase, { QueryFactory } from "./relational-type-base";

export declare interface ModelTypeWrapper extends TypeWrapper<ModelType> {
  (targetModelName: string, queryFactory: QueryFactory, options?: GenericObject): ModelType;
}

export declare class ModelType extends RelationalTypeBase {
  public fieldNameToOperationName(field: Field, operation: string, rootMethod: boolean): string;
}

export const Model: ModelTypeWrapper;
