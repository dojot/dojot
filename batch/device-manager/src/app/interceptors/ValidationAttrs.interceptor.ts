import { NextFunction, Request, Response } from 'express';
import { Schema } from 'joi';

import { CreateDevicesBatchDto } from '../dto/create-devices-batch.dto';

export abstract class ValidationAttrsInterceptor {
  static use() {
    return (req: Request, res: Response, next: NextFunction) => {
      const dto = req.body as CreateDevicesBatchDto;
      if (dto) {
        const error_validate_label = this.validate_label_repetead(dto);
        if (error_validate_label) {
          return res.status(400).json({
            message: error_validate_label,
          });
        }
      }
      return next();
    };
  }
  static validate_label_repetead(dto: CreateDevicesBatchDto) {
    let message_error = '';
    if (dto.attrs?.length != 0) {
      dto.attrs?.map((attrs_result_1) => {
        dto.attrs?.map((attrs_result_2) => {
          if (attrs_result_1.id !== attrs_result_2.id) {
            if (attrs_result_1.label === attrs_result_2.label) {
              message_error = 'A device can not have repeated attributes.';
            }
          }
        });
      });
      return message_error;
    }
  }
}
