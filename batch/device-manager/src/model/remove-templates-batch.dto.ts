import { ArrayNotEmpty, IsNumber } from "class-validator";

export class RemoveTemplatesBatchDto
{
    @IsNumber({},{each: true})
    @ArrayNotEmpty()
    templates: number[];
}
