import { DateTime } from 'luxon';
import { GenericObject } from '../interfaces/common';

declare function collect(iterator: AsyncIterator<any>): Promise<Array<any>>;
declare function valueToDateTime(value: DateTime | Date | number | BigInt | string, format?: string): DateTime;
