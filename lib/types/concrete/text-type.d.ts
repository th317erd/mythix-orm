import Type, { TypeWrapper } from "../type";

export declare interface TextTypeWrapper extends TypeWrapper<TextType> {
  (length: number): TextType;
}

export declare class TextType extends Type {
  public constructor(length: number);
}

export const TEXT: TextTypeWrapper;
