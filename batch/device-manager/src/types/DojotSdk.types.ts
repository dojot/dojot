import { Request, Response, NextFunction } from 'express';

type Middleware = (req: Request, res: Response, next: NextFunction) => unknown;

export type DojotSdkInterceptor = {
  path: string[];
  name: string;
  middleware: Middleware[];
};
