import Type, { TypeWrapper } from "../type";

export declare interface StringTypeWrapper extends TypeWrapper<StringType> {
  (length: number): StringType;
}

export declare class StringType extends Type {
  public constructor(length: number);
}

export const STRING: StringTypeWrapper;
