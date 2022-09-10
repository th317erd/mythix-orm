import { ConnectionBase } from "./connection/connection-base";
import { GenericObject } from "./interfaces/common";
import { Model, ModelClass } from "./model";
import Type from "./types/type";
import { DefaultValueProvider } from "./types/helpers/default-helpers";

export declare interface GetSetContext {
  field: Field;
  fieldName: string;
  get: () => any;
  set: (value: any) => void;
  self: Model;
  value: any;
}

export declare interface ValidateContext {
  connection: ConnectionBase;
  Model: ModelClass;
  options: GenericObject;
}

export declare interface FieldDefinition {
  type: Type;
  primaryKey: boolean;
  fieldName: string;
  columnName: string;
  allowNull: boolean;
  index: boolean | Array<boolean | string | Array<string>>;
  unique: boolean;
  defaultValue: any | DefaultValueProvider;
  get: ((context: GetSetContext) => any) | undefined;
  set: ((context: GetSetContext) => void) | undefined;
  validate: (value: any, context: ValidateContext) => Promise<any> | any;
}

export declare class Field {
  declare public static _isMythixField: boolean;

  public static isFieldClass(value: any): boolean;
  public static isField(value: any): boolean;

  public constructor(fieldDefinition?: Field | FieldDefinition);
  public clone(): Field;
  public setModel(Model: ModelClass): void;

  declare public Model: ModelClass;
  declare public type: Type;
  declare public primaryKey: boolean;
  declare public fieldName: string;
  declare public columnName: string;
  declare public allowNull: boolean;
  declare public index: boolean | Array<boolean | string | Array<string>>;
  declare public unique: boolean;
  declare public defaultValue: any | DefaultValueProvider;
  declare public get: ((context: GetSetContext) => any) | undefined;
  declare public set: ((context: GetSetContext) => void) | undefined;
  declare public validate: (value: any, context: ValidateContext) => Promise<any> | any;
}
