import Type, { TypeWrapper } from '../type';

export declare interface BlobTypeWrapper extends TypeWrapper<BlobType> {
  (length: number): BlobType;
}

export declare class BlobType extends Type {
  public constructor(length: number);
}

export const BLOB: BlobTypeWrapper;
