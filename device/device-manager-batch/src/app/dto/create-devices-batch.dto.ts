export interface CreateDevicesBatchDto {
  name_prefix: string;
  quantity: number;
  start_sufix: number;
  associate_certificates: boolean;
  templates: number[];
  attrs: Array<{
    id: number;
    label: string;
    type: string;
    valueType: string;
    staticValue: string;
    templateId: string;
  }>;
}
