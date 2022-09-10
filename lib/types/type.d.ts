import { ConnectionBase } from "../connection/connection-base";
import { Field } from "../field";
import { GenericObject } from "../interfaces/common";
import { Model, ModelClass } from "../model";
import { DefaultValueProvider } from "./helpers/default-helpers";

export declare interface TypeClass {
  new(...any: Array<any>): Type;
}

export declare interface TypeWrapper<T = Type> {
  (...any: Array<any>): T;
  Default: { [ key: string ]: DefaultValueProvider };
  _isMythixFieldType: boolean;
  clone: () => TypeClass;
  isTypeClass: (value: any) => boolean;
  isType: (value: any) => boolean;
  instantiateType: (type: Type | TypeClass) => Type;
  isVirtual: () => boolean;
  isRelational: () => boolean;
  isForeignKey: () => boolean;
  exposeToModel: () => boolean;
  getDisplayName: () => string;
}

export declare interface CastToTypeContext {
  connection: ConnectionBase;
  field: Field;
  Model: ModelClass;
  self: Model;
  value: any;
}

export declare interface CheckDirtyContext {
  value: any;
  field: Field;
  fieldName: string;
  self: Model;
  connection: ConnectionBase;
}

export declare interface SetFieldValueContext {
  value: any;
  field: Field;
  fieldName: string;
  self: Model;
}

declare class Type {
  declare public static _isMythixFieldType: boolean;
  public static clone(): TypeClass;

  public static getDisplayName(): string;
  public static isTypeClass(value: any): boolean;
  public static isType(value): boolean;
  public static isSameType(value: any): boolean;
  public static instantiateType(type: Type | TypeClass): Type;
  public static isVirtual(): boolean;
  public static isRelational(): boolean;
  public static isForeignKey(): boolean;
  public static exposeToModel(): boolean;
  public static wrapConstructor(TypeKlass: TypeClass): TypeWrapper;

  declare public _args: Array<any>;
  declare public _Model: ModelClass | undefined;
  declare public _field: Field | undefined;

  public constructor(...args: Array<any>);
  public clone(): Type;
  public isVirtual(): boolean;
  public isRelational(): boolean;
  public isForeignKey(): boolean;
  public exposeToModel(): boolean;
  public isRemote(): boolean;
  public isValidValue(value: any): boolean;
  public getField(): Field | undefined;
  public setField(field: Field): void;
  public getModel(): ModelClass | undefined;
  public setModel(Model: ModelClass): void;
  public castToType(context: CastToTypeContext): any;
  public initialize(connection: ConnectionBase, self: Model): void;
  public isDirty(context: CheckDirtyContext): any;
  public onSetFieldValue(context: SetFieldValueContext): void;
  public serialize(value: any, connection?: ConnectionBase): any;
  public deserialize(value: any, connection?: ConnectionBase): any;
  public toConnectionType(connection?: ConnectionBase, options?: GenericObject): string;
  public toString(connection?: ConnectionBase): string;
  public getDisplayName(): string;
}

export default Type;
