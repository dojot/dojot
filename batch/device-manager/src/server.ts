import app from './app'
import dotenv from 'dotenv';

dotenv.config();
const port = process.env.PORT_APP;
const host = process.env.HOST_APP;

const server = app.express.listen(port, () => {
    console.log(`⚡️[server]:  Server Batch is running at http://${host}:${port}`);
  });

  ['SIGTERM', 'SIGINT'].forEach((sig) => process.on(sig, () => {
    server.close(() => {
      process.exit(0);
    });
  }));
  