import { ArrayNotEmpty, IsBoolean, IsNotEmpty, IsNumber, IsPositive, IsString, Max, Min } from "class-validator";

export class CreateDevicesBatchDto
{
    @IsNotEmpty()
    @IsString()
    name_prefix: string;
    
    @IsNotEmpty()
    @IsNumber()
    @Min(1)
    @Max(999)
    @IsPositive()
    quantity: number;
    
    @IsNotEmpty()
    @IsBoolean()
    associate_certificates: boolean;

    @IsNumber({},{each: true})
    @ArrayNotEmpty()
    templates: number[];

    //attrs: attrs[];

}
