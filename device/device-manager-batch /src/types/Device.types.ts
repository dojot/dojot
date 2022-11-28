export interface RemoveDevicesBatchDto {
  // eslint-disable-next-line @typescript-eslint/ban-types
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
