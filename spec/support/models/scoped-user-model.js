'use strict';

const User = require('./user-model');

class ScopedUser extends User {
  static fields = User.cloneFields();

  static defaultScope(queryEngine) {
    return queryEngine.firstName.EQ('Bob');
  }
}

module.exports = ScopedUser;
