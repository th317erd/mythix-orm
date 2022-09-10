import ConnectionBase from '../connection/connection-base';
import { GenericObject } from '../interfaces/common';
import { Model, ModelClass } from '../model';
import { QueryEngine } from '../query-engine';

export declare function parseFilterFieldAndOperator(fieldName: string): { field: string, operator: string };
export declare function generateQueryFromFilter(
  connection: ConnectionBase,
  Model: ModelClass,
  filter: Array<GenericObject | Model> | GenericObject | Model,
): QueryEngine;
