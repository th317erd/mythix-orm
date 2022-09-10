import { DefaultValueProvider } from '../helpers/default-helpers';
import { TypeWrapper } from '../type';
import UUIDBaseType from './uuid-base';

export declare interface UUIDV3TypeWrapper extends TypeWrapper<UUIDV3Type> {
  (options?: UUIDV3TypeOptions): UUIDV3Type;
}

export declare interface UUIDV3TypeOptions {
  prefix?: string;
  name?: string | Array<number>;
  namespace?: string | Array<number>;
  buffer?: Buffer;
  offset?: number;
}

export declare class UUIDV3Type extends UUIDBaseType {
  declare public static Default: {
    UUIDV3: DefaultValueProvider;
  }

  constructor(options?: UUIDV3TypeOptions);
  getOptions(): UUIDV3TypeOptions;
  getArgsForUUID(options: UUIDV3TypeOptions): Array<any>;
}

export const UUIDV3: UUIDV3TypeWrapper;
