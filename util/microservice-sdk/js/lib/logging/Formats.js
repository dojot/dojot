/**
 * @overview Customized message formats for being used with Winston Logger.
 */
const winston = require('winston');

const jsonStringify = require('fast-safe-stringify');

const { sdkLevels } = require('./Levels');

// text format
// ts -- sid -- LEVEL: [rid] message (file:line) {extra}
// where:
// ts: timestamp in ISO format
// sid: service identification
// rid: request identification (optional metadata)
// level: error, warn, info or debug
// [rid]: request identifier (optional metadata)
// message: message in string format
// (file:line): file and line of the log message (optional metadata)
// extra: any additional data - user specific (optional metadada)
const textFormat = winston.format.combine(
  winston.format.timestamp(),
  winston.format.metadata({
    key: 'extra',
    fillExcept:
      [
        'level',
        'message',
        'timestamp',
        'sid',
        'rid',
        'file',
        'line',
      ],
  }),
  //  color will be applied to the message and level
  winston.format.colorize({ all: true, colors: sdkLevels.colors }),
  winston.format.printf((info) => {
    // stringfy extra metada
    const stringifiedExtra = jsonStringify(info.extra);

    // text to be logged
    const text = `${info.timestamp} -- ${info.sid} -- ${info.level}:${
      info.rid ? ` [${info.rid}]` : ''} ${
      info.message}${
      (info.file && info.line) ? ` (${info.file}:${info.line})` : ''}${
      (stringifiedExtra !== '{}') ? ` ${stringifiedExtra}` : ''}`;

    return text;
  }),
);


// json format
// {
//  level: string,
//  timestamp: <number>,
//  sid: <string>,
//  rid: <string>,
//  message: <string>,
//  file: <string>,
//  line: <number>,
//  extra: <object>,
// }
// ts: timestamp in unix format
// sid: service identification
// rid: request identification (optional metadata)
// level: error, warn, info or debug
// rid: request identifier (optional metadata)
// message: message in string format
// fine: file of the log message (optional metadata)
// line: line of the log message (optional metadata)
// extra: any additional data - user specific (optional metadada)
const jsonFormat = winston.format.combine(
  winston.format.timestamp({ format: () => Math.floor(new Date().getTime() / 1000) }),
  winston.format.metadata({
    key: 'extra',
    fillExcept:
      [
        'level',
        'message',
        'timestamp',
        'sid',
        'rid',
        'file',
        'line',
      ],
  }),
  winston.format.json({
    // remove empty extra metadata
    replacer: (key, value) => {
      if (key === 'extra' && Object.entries(value).length === 0) {
        return undefined;
      }
      return value;
    },
  }),
);

module.exports = {
  textFormat,
  jsonFormat,
};
