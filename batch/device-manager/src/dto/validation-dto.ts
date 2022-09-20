import { Router, Response, Request } from "express";
import { validate, Matches, IsDefined } from "class-validator";
import { plainToClass, Expose } from "class-transformer";

export class ValidationResult
{   
    data: any;
    error: any;
}

export class ValidationResultError
{   
    name: string;
    type: string;
    message: string;

    constructor(name: string,type:string,message:string)
    {
       this.name = name;
       this.type = type; 
       this.message = message;
    }
}


export async function validateAndConvert(classToConvert: any, body: string) 
{
    const result = new ValidationResult();
    let error_field : ValidationResultError;
    result.data = plainToClass(classToConvert, body);
    await validate(result.data, { skipMissingProperties: true }).then(errors => {
        if (errors.length > 0) {
            let errorTexts:ValidationResultError[] = [];
            errorTexts.push(result.error)
            for (const errorItem of errors) 
            {
                let json_parrsed = JSON.parse(JSON.stringify(errorItem.constraints));
                error_field = new ValidationResultError(errorItem.property,Object.keys(json_parrsed).join(),Object.values(json_parrsed).join());
                errorTexts.push(error_field);
       
            }

            result.error = JSON.stringify(errorTexts);
            return result;
        }
    });
    return result;
}