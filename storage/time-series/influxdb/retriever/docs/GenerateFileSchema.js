// generate `schemaDoc.graphql` file based on the schemas used in the backstage service
const { writeFileSync } = require('fs');
const typeDefs = require('../app/graphql/TypeDefs');

writeFileSync(`${__dirname}/schemaDoc.graphql`, (typeDefs));
