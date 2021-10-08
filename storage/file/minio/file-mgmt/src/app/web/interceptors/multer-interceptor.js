const multer = require('multer');
const path = require('path');

module.exports = {
  dest: path.resolve(__dirname, '..', '..', '..', 'tmp/', 'uploads'),
  storage: multer.diskStorage({
    destination: (req, file, cb) => {
      cb(null, '..', '..', '..', 'tmp/', 'uploads');
    },
    filename: (req, file, cb) => {
      cb(null, file.originalname);
    },
  }),
  limits: {
    fileSize: 25 * 1024 * 1024,
  },
};
