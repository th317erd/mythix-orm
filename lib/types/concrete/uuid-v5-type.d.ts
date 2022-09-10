import { DefaultValueProvider } from "../helpers/default-helpers";
import { TypeWrapper } from "../type";
import UUIDBaseType from "./uuid-base";

export declare interface UUIDV5TypeWrapper extends TypeWrapper<UUIDV5Type> {
  (options?: UUIDV5TypeOptions): UUIDV5Type;
}

export declare interface UUIDV5TypeOptions {
  prefix?: string;
  name?: string | Array<number>;
  namespace?: string | Array<number>;
  buffer?: Buffer;
  offset?: number;
}

export declare class UUIDV5Type extends UUIDBaseType {
  declare public static Default: {
    UUIDV5: DefaultValueProvider;
  }

  constructor(options?: UUIDV5TypeOptions);
  getOptions(): UUIDV5TypeOptions;
  getArgsForUUID(options: UUIDV5TypeOptions): Array<any>;
}

export const UUIDV5: UUIDV5TypeWrapper;
