import { DefaultValueProvider } from "../helpers/default-helpers";
import { TypeWrapper } from "../type";
import UUIDBaseType from "./uuid-base";

export declare interface UUIDV4TypeWrapper extends TypeWrapper<UUIDV4Type> {
  (options?: UUIDV4TypeOptions): UUIDV4Type;
}

export declare interface UUIDV4TypeOptions {
  prefix?: string;
  random?: Array<number>;
  rng?: () => Array<number>;
  buffer?: Buffer;
  offset?: number;
}

export declare class UUIDV4Type extends UUIDBaseType {
  declare public static Default: {
    UUIDV4: DefaultValueProvider;
  }

  constructor(options?: UUIDV4TypeOptions);
  getOptions(): UUIDV4TypeOptions;
  getArgsForUUID(options: UUIDV4TypeOptions): Array<any>;
}

export const UUIDV4: UUIDV4TypeWrapper;
