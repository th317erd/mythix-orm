import { DefaultValueProvider, UpdateDefaultValueProvider } from "../helpers/default-helpers";
import Type, { TypeWrapper } from "../type";

export declare interface DateTypeWrapper extends TypeWrapper<DateType> {
  (format?: string): DateType;
}

export declare class DateType extends Type {
  declare public static Default: {
    NOW: UpdateDefaultValueProvider;
  };

  public constructor(format?: string);
}

export const DATE: DateTypeWrapper;
