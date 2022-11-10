import { NextFunction, Request, Response } from 'express';
import { Schema } from 'joi';
import { APP_ERRORS } from '../constants/Errors.constants';
import { CreateDevicesBatchDto } from '../dto/create-devices-batch.dto';

export abstract class ValidationAttrsInterceptor {
  static use(schema: Schema) {
    return (req: Request, res: Response, next: NextFunction) => {
      const dto = req.body as CreateDevicesBatchDto;
      if (dto) {
        const error_validate_label = this.validate_label_repetead(dto);
        if (error_validate_label) {
          return res.status(400).json({
            message: error_validate_label,
          });
        }
      } else {
        const { error } = schema.validate(req, {
          abortEarly: false,
          allowUnknown: true,
        });

        if (error) {
          return res.status(400).json({
            type: APP_ERRORS.VALIDATION,
            cause: error.cause,
            details: error.details,
            message: error.message,
          });
        }
      }

      return next();
    };
  }
  static validate_label_repetead(dto: CreateDevicesBatchDto) {
    let message_error = '';
    if (dto.attrs?.length != 0) {
      const promisse_attrs = dto.attrs?.map((attrs_result_1) => {
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
