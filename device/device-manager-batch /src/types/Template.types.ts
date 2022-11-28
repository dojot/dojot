import { Devices } from './Device.types';

export interface RemoveTemplatesBatchDto {
  templates: number[];
}

export interface Templates {
  id: number;
  label: string;
}

export interface Templates_Not_Found {
  id: string;
  type: string;
  message: string;
}

export interface Templates_Associate_Devices {
  id: string;
  label: string;
  type: string;
  message: string;
  associate_devices: [Devices];
}
