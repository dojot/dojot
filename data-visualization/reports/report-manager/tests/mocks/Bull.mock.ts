import { Job } from 'bull'
import { mockDeep } from 'jest-mock-extended'

export const BullJobMock = {
  new<T>(data: T) {
    const mock = mockDeep<Job<T>>()
    mock.data = data as never
    return mock
  },
}
