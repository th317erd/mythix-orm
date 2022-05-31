'use strict';

const Database              = require('better-sqlite3');
const SQLConnectionBase     = require('../sql-connection-base');
const SQLiteQueryGenerator  = require('./sqlite-query-generator');

class SQLiteConnection extends SQLConnectionBase {
  static dialect = 'sqlite';

  constructor(_options) {
    super(_options);

    this.setQueryGenerator(new SQLiteQueryGenerator(this));

    Object.defineProperties(this, {
      'db': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
    });
  }

  async start() {
    let opts = Object.assign({
      filename: ':memory:',
    }, this.getOptions());

    this.db = new Database(opts.filename, opts);
  }

  async stop() {
    if (!this.db)
      return;

    await this.db.close();
    this.db = null;
  }

  getDefaultFieldValue(type) {
    switch (type) {
      case 'AUTO_INCREMENT':
        return '!AUTOINCREMENT';
      case 'DATETIME_NOW':
        return '(datetime(\'now\'))';
      case 'DATE_NOW':
        return '(date(\'now\'))';
      default:
        return type;
    }
  }
}

module.exports = SQLiteConnection;
