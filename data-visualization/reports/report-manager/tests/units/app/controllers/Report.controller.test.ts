import { ReportController } from 'src/app/controllers'
import { AppMock, ExpressMock, LoggerMock } from 'tests/mocks'

describe('Report.controller', () => {
  describe('findMany', () => {
    it('should find many reports with no pagination', async () => {
      const { ReportServiceMock } = AppMock.new()

      const fakeReports = [{ id: '123' }, { id: '456' }]
      ReportServiceMock.findMany.mockResolvedValue({
        reports: fakeReports as never,
        total: fakeReports.length,
      })

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()

      RequestMock.query = {
        name: undefined,
        page: undefined,
        pageSize: undefined,
      }

      ResponseMock.status.mockReturnThis()

      await reportController.findMany(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ResponseMock.json).toBeCalledWith(
        expect.objectContaining({
          reports: fakeReports,
          pagination: {
            total: fakeReports.length,
          },
        }),
      )

      expect(ReportServiceMock.findMany).toBeCalled()
      expect(ResponseMock.status).toBeCalledWith(200)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should find many reports paginated', async () => {
      const { ReportServiceMock } = AppMock.new()

      const fakeReports = [{ id: '123' }, { id: '456' }]
      ReportServiceMock.findMany.mockResolvedValue({
        reports: fakeReports as never,
        total: fakeReports.length,
      })

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()

      RequestMock.query = {
        name: 'Report',
        page: '2',
        pageSize: '15',
      }

      ResponseMock.status.mockReturnThis()

      await reportController.findMany(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ResponseMock.json).toBeCalledWith(
        expect.objectContaining({
          reports: fakeReports,
          pagination: {
            total: fakeReports.length,
            page: Number(RequestMock.query.page),
            pageSize: Number(RequestMock.query.pageSize),
          },
        }),
      )

      expect(ReportServiceMock.findMany).toBeCalledWith(RequestMock.prisma, {
        name: RequestMock.query.name,
        page: Number(RequestMock.query.page),
        pageSize: Number(RequestMock.query.pageSize),
      })

      expect(ResponseMock.status).toBeCalledWith(200)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should call next passing the error if fails', async () => {
      const { ReportServiceMock } = AppMock.new()

      const error = new Error('Error')
      ReportServiceMock.findMany.mockRejectedValue(error)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()

      await reportController.findMany(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ReportServiceMock.findMany).toBeCalled()
      expect(NextFunctionMock).toBeCalledWith(error)
    })
  })

  describe('findById', () => {
    it('should find report by id', async () => {
      const { ReportServiceMock } = AppMock.new()
      ReportServiceMock.findById.mockResolvedValue({ id: 'id' } as never)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
      RequestMock.params.id = 'id'

      await reportController.findById(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ReportServiceMock.findById).toBeCalledWith(
        RequestMock.prisma,
        RequestMock.params.id,
      )

      expect(ResponseMock.status).toBeCalledWith(200)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should return error 404 if report was not found', async () => {
      const { ReportServiceMock } = AppMock.new()
      ReportServiceMock.findById.mockResolvedValue(null)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
      RequestMock.params.id = 'id'

      await reportController.findById(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ReportServiceMock.findById).toBeCalledWith(
        RequestMock.prisma,
        RequestMock.params.id,
      )

      expect(ResponseMock.status).toBeCalledWith(404)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should call next passing the error if fails', async () => {
      const { ReportServiceMock } = AppMock.new()

      const error = new Error('Error')
      ReportServiceMock.findById.mockRejectedValue(error)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()

      await reportController.findById(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ReportServiceMock.findById).toBeCalled()
      expect(NextFunctionMock).toBeCalledWith(error)
    })
  })

  describe('delete', () => {
    it('should delete a report by id', async () => {
      const { ReportServiceMock } = AppMock.new()
      ReportServiceMock.delete.mockResolvedValue({ id: 'id' } as never)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
      RequestMock.params.id = 'id'
      RequestMock.headers.authorization = 'Bearer 123456'

      await reportController.delete(RequestMock, ResponseMock, NextFunctionMock)

      expect(ReportServiceMock.delete).toBeCalledWith(
        RequestMock.prisma,
        RequestMock.params.id,
        RequestMock.headers.authorization,
      )

      expect(ResponseMock.status).toBeCalledWith(200)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should return error 404 if report was not found', async () => {
      const { ReportServiceMock } = AppMock.new()
      ReportServiceMock.delete.mockResolvedValue(null)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()
      RequestMock.params.id = 'id'

      await reportController.findById(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ReportServiceMock.findById).toBeCalledWith(
        RequestMock.prisma,
        RequestMock.params.id,
      )

      expect(ResponseMock.status).toBeCalledWith(404)
      expect(NextFunctionMock).toBeCalled()
    })

    it('should call next passing the error if fails', async () => {
      const { ReportServiceMock } = AppMock.new()

      const error = new Error('Error')
      ReportServiceMock.findById.mockRejectedValue(error)

      const reportController = new ReportController(
        LoggerMock.new(),
        ReportServiceMock,
      )

      const { RequestMock, ResponseMock, NextFunctionMock } = ExpressMock.new()

      await reportController.findById(
        RequestMock,
        ResponseMock,
        NextFunctionMock,
      )

      expect(ReportServiceMock.findById).toBeCalled()
      expect(NextFunctionMock).toBeCalledWith(error)
    })
  })
})
