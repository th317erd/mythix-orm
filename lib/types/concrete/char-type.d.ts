import Type, { TypeWrapper } from "../type";

export declare interface CharTypeWrapper extends TypeWrapper<CharType> {
  (): CharType;
}

export declare class CharType extends Type {
  public constructor();
}

export const CHAR: CharTypeWrapper;
