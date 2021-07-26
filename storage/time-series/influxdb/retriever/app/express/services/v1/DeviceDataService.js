const json2csv = require("json2csv");


module.exports = class DevicesService {
    
    /**
     * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByField
     *                               A promise that returns a result and a totalItems inside that result
     * @param {Promise<{result: object, totalItems: number}| error>>} queryDataByMeasurement
     *                               A promise that returns a result and a totalItems inside that result
    */
    constructor(queryDataByField, queryDataByMeasurement){
        this.queryDataByField = queryDataByField;
        this.queryDataByMeasurement = queryDataByMeasurement;
    }

    /**
     * 
     * @param {*} tenant    tenant to which the device belongs
     * @param {*} deviceId  device id
     * @param {*} dateFrom  search interval start date
     * @param {*} dateTo    search interval end date
     * @param {*} limit     limit of data that will be returned
     * @param {*} page      page that will be returned 
     * @param {*} order     ordering of the data that will be returned
     * @param {*} getPaging paging metadata formatting method
     * @returns the device dataset
     */
    async getDeviceData (tenant, deviceId, dateFrom, dateTo, limit, page, order, getPaging) {
        
        const filters = { dateFrom, dateTo };
        const pagination = { limit, page };

        const {
            result, totalItems,
        } = await this.queryDataByMeasurement(tenant, deviceId, filters, pagination, order);
        const paging = getPaging(totalItems);   
 
        return [result,paging];
    };

    /**
     * 
     * @param {*} deviceData device dataset
     * @returns device dataset in CSV format
     */
    parseDeviceDataToCsv(deviceData){

        const messages = deviceData.map((message) => {    
            const newMessage = {};
            for(const attr of message.attrs){
                newMessage[attr.label] = attr.value;
            }
        
            return {
                ts : message.ts,
                ...newMessage
            }
        })
        
        const csvParser = new json2csv.Parser ({defaultValue : undefined});
        
        return csvParser.parse(messages);
    };

    /**
     * 
     * @param {*} tenant    tenant to which the device belongs
     * @param {*} deviceId  device id
     * @param {*} attr      attribute to be returned
     * @param {*} dateFrom  search interval start date
     * @param {*} dateTo    search interval end date
     * @param {*} limit     limit of data that will be returned
     * @param {*} page      page that will be returned 
     * @param {*} order     ordering of the data that will be returned
     * @param {*} getPaging paging metadata formatting method
     * @returns the device attribute dataset
     */
    async getDeviceAttrData (tenant, deviceId, attr,  dateFrom, dateTo, limit, page, order, getPaging) {

        const filters = { dateFrom, dateTo };
        const pagination = { limit, page };

        const {
          result, totalItems,
        } = await this.queryDataByField(tenant, deviceId, attr, filters, pagination, order);
        const paging = getPaging(totalItems);

        return [result, paging];
    }

    /**
     * 
     * @param {*} attrData device attribute dataset
     * @returns device attribute dataset in CSV format
     */
    parseDeviceAttrDataToCsv(attrData){

        const csvParser = new json2csv.Parser ({defaultValue : undefined});        
        return csvParser.parse(attrData);
    };


}      
