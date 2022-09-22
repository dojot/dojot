import { ArrayMinSize, ArrayNotEmpty, IsArray, IsJSON, IsNotEmpty, isNumber, IsNumber, IsPositive, IsString } from 'class-validator';

export class RemoveDevicesBatchDto
{
    //@IsNumber({},{each: true})
    @ArrayNotEmpty()
    @IsString({ each: true })
    devices: string[];

}

