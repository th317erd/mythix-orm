import { DefaultValueProvider } from "../../field";
import Type, { TypeWrapper } from "../type";

export declare interface IntegerTypeWrapper extends TypeWrapper<IntegerType> {
  (length?: number): IntegerType;
}

export declare class IntegerType extends Type {
  declare public static Default: {
    AUTO_INCREMENT: DefaultValueProvider;
  };

  public constructor(length?: number);
}

export const INTEGER: IntegerTypeWrapper;
