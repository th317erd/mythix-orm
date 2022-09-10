import ConnectionBase from '../connection/connection-base';
import Field from '../field';
import { GenericObject } from '../interfaces/common';
import { Model, ModelClass } from '../model';
import { QueryEngine } from '../query-engine';

export declare interface FullyQualifiedFieldDefinition {
  modelName: string;
  fieldNames: Array<string>;
}

export declare interface RelationStatus {
  create?: boolean;
  instance?: Model | GenericObject;
  modelName: string;
  Model: ModelClass;
  fields: Map<string, string>;
}

export declare interface ModelsRelationStatuses {
  [ key: string ]: RelationStatus
}

export declare function isUUID(value: any): boolean;
export declare function sanitizeFieldString(value: string): string;
export declare function parseQualifiedName(value: string): FullyQualifiedFieldDefinition;
export declare function injectModelMethod(self: any, method: Function, methodName: string, fullMethodName: string): void;
export declare function fieldToFullyQualifiedName(field: Field | string, Model: ModelClass): string;

export declare function sortModelNamesByDependencyOrder(
  connection: ConnectionBase,
  modelNames: Array<string>,
  dependencyHelper: (Model: ModelClass, modelName: string) => Array<string>,
): Array<string>;

export declare function sortModelNamesByCreationOrder(connection: ConnectionBase, modelNames: Array<string>): Array<string>;

export declare function getRelationalModelStatusForField(
  connection: ConnectionBase,
  self: Model,
  field: Field,
  ...args: Array<any>,
): Promise<{
  relationalMap: ModelsRelationStatuses,
  TargetModel?: ModelClass,
  TargetField?: Field,
}>;

export declare function constructModelsForCreationFromOriginField(
  connection: ConnectionBase,
  self: Model,
  field: Field,
  attributes: Model | GenericObject,
): Promise<{
  relationalMap: ModelsRelationStatuses,
  sortedModelNames: Array<string>
} | undefined>;

export declare function createAndSaveAllRelatedModels(
  connection: ConnectionBase,
  self: Model,
  field: Field,
  allModelAttributes: Array<Model | GenericObject>,
  options?: GenericObject,
): Promise<Array<Model> | undefined>;

export declare function setRelationalValues<T = Model>(
  connection: ConnectionBase,
  TargetModel: ModelClass,
  targetModelInstance: T,
  SourceModel: ModelClass,
  sourceModelInstance: Model,
): T;

export declare function assignRelatedModels(model: Model, relatedModels: Model | Array<Model>): void;
export declare function getPrimaryKeysForModels(
  connection: ConnectionBase,
  Model: ModelClass,
  models: Model | Array<Model>,
  options?: {
    includeRelations?: boolean | Array<string>,
    skipRelations?: Array<string>,
  },
): Array<any> | { [ key: string ]: Array<any> };

export declare function buildQueryFromModelsAttributes(
  connection: ConnectionBase,
  Model: ModelClass,
  models: Model | GenericObject | Array<Model | GenericObject>
): QueryEngine;
