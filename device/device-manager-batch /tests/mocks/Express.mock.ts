import { Request, Response } from 'express';
import { mockDeep } from 'jest-mock-extended';

export const ExpressMock = {
  new() {
    const ErrorMock = mockDeep<Error>();
    const RequestMock = mockDeep<Request>();
    const ResponseMock = mockDeep<Response>();
    const NextFunctionMock = jest.fn();

    ResponseMock.status.mockReturnValue(ResponseMock);

    return {
      ErrorMock,
      RequestMock,
      ResponseMock,
      NextFunctionMock,
    };
  },
};
