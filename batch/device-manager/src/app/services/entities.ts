export type DeviceResultBatch = {
  id: string,
  label: string,
};

export type DeviceNotFoundBatch = {
  id: string,
  message: string,
  type: string,
}

export type TemplatesBatch = {
  id: number,
  label: string,
}

export type TemplatesNotFoundBatch = {
  id: number,
  message: string,
  type: string,
}

export type TemplatesAssociatedDevicesBatch = {
  id: number,
  label: string,
  type: string,
  message: string,
  associated_devices: Array<DeviceResultBatch>,
}

export type AttrBatch = {

}