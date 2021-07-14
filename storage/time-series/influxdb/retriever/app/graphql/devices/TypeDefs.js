const typeDefs = `
  #main query schema to request Devices from InfluxDb
  type Query {
    #Returns historical data
    getData(filter: ParamsInput): FetchedData
  }

  #Parameters to query historical device data
  input ParamsInput {
    #list of devices will be retrieved#
    devices: [Device]!
    # number of elements will be returned#
    limit: Int
    # time interval used in flux query#
    range: Range
    # list ordering (desc or asc) #
    isDesc: Boolean
  }
`;

module.exports = typeDefs;
