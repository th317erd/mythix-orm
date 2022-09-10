import { DefaultValueProvider } from "../../field";
import { TypeWrapper } from "../type";
import UUIDBaseType from "./uuid-base";

export declare interface UUIDV1TypeWrapper extends TypeWrapper<UUIDV1Type> {
  (options?: UUIDV1TypeOptions): UUIDV1Type;
}

export declare interface UUIDV1TypeOptions {
  prefix?: string;
  node?: Array<number>;
  clockseq?: number;
  msecs?: number;
  nsecs?: number;
  random?: Array<number>;
  rng?: () => Array<number>;
  buffer?: Buffer;
  offset?: number;
}

export declare class UUIDV1Type extends UUIDBaseType {
  declare public static Default: {
    UUIDV1: DefaultValueProvider;
  }

  constructor(options?: UUIDV1TypeOptions);
  getOptions(): UUIDV1TypeOptions;
  getArgsForUUID(options: UUIDV1TypeOptions): Array<any>;
}

export const UUIDV1: UUIDV1TypeWrapper;
