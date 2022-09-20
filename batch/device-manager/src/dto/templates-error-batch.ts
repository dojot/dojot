import { Devices } from "./device";

export class TemplatesErrorBatch
{   
    id: string;
    label: string;
    type: string;
    message: string;
    associates: Devices[];
}
