const ss = require('simple-statistics');
const { logger } = require('@dojot/dojot-module-logger');

const TAG = { filename: 'Metrics' };
class Metrics {
  constructor() {
    logger.info('Init Metrics', TAG);
    this.allTimes = [];
  }

  getAllTimes() {
    return this.allTimes;
  }

  cleanAllTimes() {
    logger.debug(`Clean all time, the times before clean: ${this.getAllTimes()}`, TAG);
    this.allTimes = [];
  }

  addTime(time) {
    this.allTimes.push(time);
  }

  isNotEmptyAllTimes() {
    return (this.allTimes && this.allTimes.length > 0);
  }

  getMax() {
    return this.isNotEmptyAllTimes() ? ss.max(this.getAllTimes()) : 0;
  }

  getMin() {
    return this.isNotEmptyAllTimes() ? ss.min(this.getAllTimes()) : 0;
  }

  getAvg() {
    return this.isNotEmptyAllTimes() ? ss.mean(this.getAllTimes()) : 0;
  }

  getMedian() {
    return this.isNotEmptyAllTimes() ? ss.median(this.getAllTimes()) : 0;
  }

  getStandardDeviation() {
    return this.isNotEmptyAllTimes()
      ? ss.standardDeviation(this.getAllTimes()) : 0;
  }
}

const metrics = new Metrics();
module.exports = metrics;
