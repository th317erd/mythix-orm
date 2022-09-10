import Type, { TypeWrapper } from '../type';

export declare interface BooleanTypeWrapper extends TypeWrapper<BooleanType> {
  (): BooleanType;
}

export declare class BooleanType extends Type {
  public constructor();
}

export const BOOLEAN: BooleanTypeWrapper;
