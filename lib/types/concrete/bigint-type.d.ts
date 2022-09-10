import { DefaultValueProvider } from "../../field";
import Type, { TypeWrapper } from "../type";

export declare interface BigIntTypeWrapper extends TypeWrapper<BigIntType> {
  (length?: number, options?: { strict: boolean }): BigIntType;
}

export declare class BigIntType extends Type {
  declare public static Default: {
    AUTO_INCREMENT: DefaultValueProvider;
  };

  public constructor(length?: number, options?: { strict: boolean });
}

export const BIGINT: BigIntTypeWrapper;
