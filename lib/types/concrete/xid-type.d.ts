import { DefaultValueProvider } from "../helpers/default-helpers";
import { TypeWrapper } from "../type";
import UUIDBaseType from "./uuid-base";

export declare interface XIDTypeWrapper extends TypeWrapper<XIDType> {
  (): XIDType;
}

export declare class XIDType extends UUIDBaseType {
  declare public static Default: {
    XID: DefaultValueProvider;
  }

  constructor();
}

export const XID: XIDTypeWrapper;
