import Type, { TypeWrapper } from '../type';

export declare interface RealTypeWrapper extends TypeWrapper<RealType> {
  (length?: number, scale?: number): RealType;
}

export declare class RealType extends Type {
  public constructor(length?: number, scale?: number);
}

export const REAL: RealTypeWrapper;
