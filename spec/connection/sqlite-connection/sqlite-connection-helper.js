'use strict';

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');

async function createConnection() {
  const createTable = async (connection, Model, options) => {
    return await connection.createTable(Model, options);
  };

  let connection = new SQLiteConnection({
    models: require('../../support/models'),
  });

  await connection.start();

  let models  = connection.getModels();
  let keys    = Object.keys(models);

  for (let i = 0, il = keys.length; i < il; i++) {
    let key   = keys[i];
    let model = models[key];

    await createTable(connection, model, { logger: console });
  }

  return Object.assign({}, models, { connection });
}

async function truncateTables(connection) {
  let models  = connection.getModels();
  let keys    = Object.keys(models);

  for (let i = 0, il = keys.length; i < il; i++) {
    let key   = keys[i];
    let model = models[key];

    await await connection.truncate(model);
  }
}

module.exports = {
  createConnection,
  truncateTables,
};
