import Type, { TypeWrapper } from "../type";

export declare interface NumericTypeWrapper extends TypeWrapper<NumericType> {
  (precision?: number, scale?: number): NumericType;
}

export declare class NumericType extends Type {
  public constructor(precision?: number, scale?: number);
}

export const NUMERIC: NumericTypeWrapper;
