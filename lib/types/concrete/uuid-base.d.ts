import Type from "../type";

declare class UUIDBaseType extends Type {
  getPrefix(options?: { prefix: string; }): string;
  stripPrefix(value: string): { prefix: string; id: string };
  addPrefix(value: string): string;
  getBaseLength(): number;
  getTotalLength(): number;
  validateOptions(uuidArgs: Array<any>): void;
  getArgsForUUID(options)
}

export default UUIDBaseType;
