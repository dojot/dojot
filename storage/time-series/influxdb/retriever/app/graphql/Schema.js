const { buildSchema } = require('graphql');

const typeDefs = require('./TypeDefs');

const schema = buildSchema(typeDefs);

module.exports = schema;
