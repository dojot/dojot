const devicesTypeDefs = require('./devices/TypeDefs');

const baseTypeDefs = `
  #Defining a type for influx's point
  type InfluxPoint {
      ts: String!
      value: String
      attr: String!
      id: String!
  }

  #Type used to return the influx data
  type FetchedData {
      data: [InfluxPoint]!
  }

  #Information for next, previous or current page
  type PageInfo {
      number: Int!
      url: String!
  }

  #The pagination links for the data
  type Paging {
      previous: PageInfo
      current: PageInfo
      next:PageInfo
  }


  #Input used to request one device
  input Device {
      id: String!
      #list of attributes will be retrieved#
      attributes: [String]!
  }

  input Range {
    start: String! #dateFrom#
    stop: String #dateTo#
  }
`;

const typeDefs = [baseTypeDefs, devicesTypeDefs,
].join(' ');

module.exports = typeDefs;
