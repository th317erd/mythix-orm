import { DefaultValueProvider } from "../../field";
import Type, { TypeWrapper } from "../type";

export declare interface DateTimeTypeWrapper extends TypeWrapper<DateTimeType> {
  (format?: string, length?: number): DateTimeType;
}

export declare interface DateTimeDefaultValueProvider extends DefaultValueProvider {
  UPDATE: DefaultValueProvider;
}

export declare class DateTimeType extends Type {
  declare public static Default: {
    NOW: DateTimeDefaultValueProvider;
  };

  public constructor(format?: string, length?: number);
}

export const DATETIME: DateTimeTypeWrapper;
