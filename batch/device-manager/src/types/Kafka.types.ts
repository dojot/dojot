import { string, number } from 'joi';

export type KafkaPayload = {
  value: string;
};

export type KafkaParsedPayloadValue = {
  type: string;
  tenant: string;
};

export interface KafkaEventData {
  id: string;
  label: string;
  created: any;
  templates: number[];
  attrs: any;
}
