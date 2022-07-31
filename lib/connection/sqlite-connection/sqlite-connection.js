'use strict';

const Nife                  = require('nife');
const moment                = require('moment');
const Database              = require('better-sqlite3');
const SQLConnectionBase     = require('../sql-connection-base');
const SQLiteQueryGenerator  = require('./sqlite-query-generator');
const { SQLLiteral }        = require('../sql-literals');

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

  getDefaultOrder(Model, _options) {
    let options = _options || {};
    let result  = super.getDefaultOrder(Model, options);
    if (result)
      return result;

    let escapedTableName  = this.escapeID(Model.getTableName());
    let escapedFieldName  = this.escapeID('rowid');

    return [ new SQLLiteral(`${escapedTableName}.${escapedFieldName} ${(options.reverseOrder === true) ? 'DESC' : 'ASC'}`) ];
  }

  async start() {
    let options = this.getOptions();

    let opts = Object.assign({
      filename: ':memory:',
    }, this.getOptions());

    let db = this.db = new Database(opts.filename, opts);

    if (options.foreignConstraints !== false)
      await db.pragma('foreign_keys = ON');
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
        return new SQLLiteral('AUTOINCREMENT', { noDefaultStatementOnCreateTable: true });
      case 'DATETIME_NOW':
        return new SQLLiteral('(datetime(\'now\'))', { escape: false });
      case 'DATE_NOW':
        return new SQLLiteral('(date(\'now\'))', { escape: false });
      case 'DATETIME_NOW_LOCAL':
        return moment.utc().toDate();
      case 'DATE_NOW_LOCAL':
        return moment.utc().startOf('day').toDate();
      default:
        return type;
    }
  }

  async exec(sql) {
    if (!sql)
      return;

    return await this.db.exec(sql);
  }

  async query(sql, _options) {
    if (!sql)
      return;

    let options = _options || {};

    try {
      let statement   = this.db.prepare(sql);
      let methodName  = ((/^\s*SELECT\s+/i).test(sql)) ? 'all' : 'run';
      let parameters  = (Nife.isNotEmpty(options.parameters)) ? [].concat(parameters) : [];

      if (options.logger)
        console.log('QUERY: ', sql);

      if (methodName === 'all')
        statement.raw(true);

      let result = await statement[methodName](...parameters);

      if (options.formatResponse === true) {
        if ((/^INSERT\b/i).test(sql))
          result = this.formatInsertResponse(sql, result);
        else if ((/^SELECT\b/).test(sql))
          result = this.formatSelectResponse(sql, statement.columns(), result);
      }

      return result;
    } catch (error) {
      if (options.logger) {
        options.logger.error(error);
        options.logger.error('QUERY: ', sql);
      }

      throw error;
    }
  }

  async transaction(callback, _options) {
    let options       = _options || {};
    let inheritedThis = Object.create(options.transaction || this);
    let savePointName;

    if (inheritedThis.inTransaction !== true) {
      inheritedThis.inTransaction = true;
      await this.query(`BEGIN${(options.mode) ? ` ${options.mode}` : ''}`);
    } else {
      savePointName = this.generateSavePointName();
      inheritedThis.savePointName = savePointName;
      inheritedThis.isSavePoint = true;

      await this.query(`SAVEPOINT ${savePointName}`);
    }

    try {
      let result = await callback.call(inheritedThis, inheritedThis);

      if (savePointName)
        await this.query(`RELEASE SAVEPOINT ${savePointName}`);
      else
        await this.query('COMMIT');

      return result;
    } catch (error) {
      if (savePointName)
        await this.query(`ROLLBACK TO SAVEPOINT ${savePointName}`);
      else if (inheritedThis.inTransaction)
        await this.query('ROLLBACK');

      throw error;
    }
  }

  formatInsertResponse(sqlStatement, insertResponse) {
    if (!insertResponse)
      return insertResponse;

    if (insertResponse.changes === 0)
      return [];

    let ids = [];
    for (let i = (insertResponse.lastInsertRowid - insertResponse.changes), il = insertResponse.lastInsertRowid; i < il; i++)
      ids.push(i + 1);

    return ids;
  }

  formatSelectResponse(sqlStatement, columns, result) {
    return {
      rows:     result,
      columns:  columns.map((column) => column.name),
    };
  }

  async truncate(Model) {
    global.debug = true;
    let result = await this.destroy(Model);
    global.debug = false;

    // Update the autoincrement sequence for this table
    let sqlStr = `UPDATE sqlite_sequence SET seq=0 WHERE name='${Model.getTableName()}'`;
    await this.query(sqlStr);


    return result;
  }
}

module.exports = SQLiteConnection;
