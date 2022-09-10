import { DefaultValueProvider, UpdateDefaultValueProvider } from '../helpers/default-helpers';
import Type, { TypeWrapper } from '../type';

export declare interface DateTimeTypeWrapper extends TypeWrapper<DateTimeType> {
  (format?: string, length?: number): DateTimeType;
}

export declare class DateTimeType extends Type {
  declare public static Default: {
    NOW: UpdateDefaultValueProvider;
  };

  public constructor(format?: string, length?: number);
}

export const DATETIME: DateTimeTypeWrapper;
