import { DefaultValueProvider } from "../../field";
import Type, { TypeWrapper } from "../type";

export declare interface DateTypeWrapper extends TypeWrapper<DateType> {
  (format?: string): DateType;
}

export declare interface DateDefaultValueProvider extends DefaultValueProvider {
  UPDATE: DefaultValueProvider;
}

export declare class DateType extends Type {
  declare public static Default: {
    NOW: DateDefaultValueProvider;
  };

  public constructor(format?: string);
}

export const DATE: DateTypeWrapper;
