import { ConnectionBase } from '../../connection/connection-base';
import { Field } from '../../field';
import { GenericObject } from '../../interfaces/common';
import { Model } from '../../model';

export const FLAG_ON_INITIALIZE: number;
export const FLAG_ON_INSERT: number;
export const FLAG_ON_UPDATE: number;
export const FLAG_ON_STORE: number;
export const FLAG_LITERAL: number;
export const FLAG_REMOTE: number;

export declare interface DefaultValueContext {
  _initial: boolean;
  connection: ConnectionBase;
  data: GenericObject | undefined;
  field: Field;
  fieldName: string;
  fieldValue: any;
  self: Model;
}

export declare interface DefaultValueProvider {
  (context: DefaultValueContext): any;
  mythixFlags: number | undefined;
}

export declare interface DefaultValueFlagsOptions {
  onInitialize: boolean;
  onInsert: boolean;
  onUpdate: boolean;
  onStore: boolean;
  literal: boolean;
  remote: boolean;
}

export declare type DefaultValueFlags = number | 'onInitialize' | 'onInsert' | 'onUpdate' | 'onStore' | 'literal' | 'remote';

export declare function defaultValueFlags(func: Function, flagsObj: DefaultValueFlagsOptions): DefaultValueProvider;
export declare function getDefaultValueFlags(func: Function): number;
export declare function checkDefaultValueFlags(func: Function, checkFlags: Array<DefaultValueFlags>): boolean;

// For AUTOINCREMENT
export declare interface AutoIncrementDefaultValueProvider extends DefaultValueProvider {
  _mythixIsAutoIncrement: boolean;
}

// For DATETIME_NOW.LOCAL and DATE_NOW.LOCAL
export declare interface UpdateLocalDefaultValueProvider extends DefaultValueProvider {
  UPDATE: DefaultValueProvider;
}

// For DATETIME_NOW and DATE_NOW
export declare interface UpdateDefaultValueProvider extends DefaultValueProvider {
  UPDATE: DefaultValueProvider;
  LOCAL: UpdateLocalDefaultValueProvider;
}

export const AUTO_INCREMENT: AutoIncrementDefaultValueProvider;
export const DATETIME_NOW: UpdateDefaultValueProvider;
export const DATE_NOW: UpdateDefaultValueProvider;
