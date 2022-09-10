import { ConnectionBase } from "../../connection/connection-base";
import Type, { CheckDirtyContext, TypeWrapper } from "../type";

export declare interface SerializedTypeWrapper extends TypeWrapper<SerializedType> {
  (options: Type | SerializedTypeOptions): SerializedType;
}

interface SerializeCallContext {
  value: any;
  connection: ConnectionBase | undefined;
}

export declare interface SerializedTypeOptions {
  type: Type;
  serialize?: (context: SerializeCallContext) => any;
  deserialize?: (context: SerializeCallContext) => any;
  isDirty?: (context: CheckDirtyContext) => any;
}

export declare class SerializedType extends Type {
  public constructor(options: Type | SerializedTypeOptions);
  public getOptions(): SerializedTypeOptions;
}

export const SERIALIZED: SerializedTypeWrapper;
