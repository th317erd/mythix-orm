import { GenericObject } from '../interfaces/common';

declare function collect(iterator: AsyncIterator<any>): Promise<Array<any>>;
declare function objectAssignSpecial(obj: GenericObject, proto: GenericObject | null, skipKeys: Array<string> | GenericObject): GenericObject;
