export interface RemoveDevicesBatchDto {
  devices: Array<{}>;
}

export interface Devices {
  id: string;
  label: string;
}

export interface Devices_Not_Found {
  id: string;
  type: string;
  message: string;
}
