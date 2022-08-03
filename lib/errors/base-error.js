'use strict';

class MythixORMBaseError extends Error {
  constructor(message) {
    super((message instanceof Error) ? message.message : message);

    this.name = 'MythixORMBaseError';

    if (message instanceof Error)
      this.original = message;
  }
}

module.exports = MythixORMBaseError;
