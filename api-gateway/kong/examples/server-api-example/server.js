const express = require('express');
const app = express();

class InvalidTokenError {
  constructor() {
      this.message = 'Invalid JWT token';
  }
}
class MissingJWTTokenError {
  constructor() {
      this.message = 'Missing JWT token';
  }
}

const b64decode = (data) => {
  if (typeof Buffer.from === 'function') {
      return Buffer.from(data, 'base64').toString();
  }
  return (Buffer.from(data, 'base64')).toString();
};

const tenantByToken = (rawToken) => {
  const tokenData = JSON.parse(b64decode(rawToken.split('.')[1]));
  const { iss } = tokenData;
  return iss.match(/(realms\/)(\w+)/)[2];
};

try {
  app.use((req, res, next) => {
    const rawToken = req.get('authorization');

    if (req.path == '/insecure'){
      return next();
    }

    if (rawToken === undefined) {
      console.error('Missing JWT token');
      return res.status(401).send(new MissingJWTTokenError());
    }

    const token = rawToken.split('.');
    if (token.length !== 3) {
      console.error('Invalid JWT token', rawToken);
      return res.status(401).send(new InvalidTokenError());
    }

    req.token = rawToken;
    req.tenant = tenantByToken(rawToken);

    return next();
  });

  app.get('/secure', (req, res) =>  {
    res.send('GET - secure route, tenant:' + req.tenant + '\n')
  });

  app.post('/secure', (req, res) =>  {
    res.send('POST - secure route, tenant:' + req.tenant + '\n');
  });

  app.put('/secure', (req, res) => {
    res.send('PUT - secure route, tenant:' + req.tenant + '\n');
  });

  app.delete('/secure', (req, res) => {
    res.send('DELETE - secure route, tenant:' + req.tenant + '\n');
  });

  app.get('/insecure', (req, res) =>  {
    res.send('GET - insecure route \n')
  });

  app.post('/insecure', (req, res) =>  {
    res.send('POST - insecure route \n')
  });

  app.listen(8888, () => {
    console.log('Server listen in 8888 \n');
  })

} catch (e) {
  console.error(e);
  process.kill(process.pid, 'SIGTERM');
}

