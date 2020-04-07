const config = {

  // When set to true, the producer will ensure that messages
  // are successfully produced exactly once and in the original produce order.
  // when idempotence is enabled  (if not modified by the user)
  // retries=INT32_MAX,  acks=all,  queuing.strategy=fifo
  // and max.in.flight.requests.per.connection=5
  'enable.idempotence': true,

  // The backoff time in milliseconds before retrying a protocol request.
  'retry.backoff.ms': 100,

  // Enable TCP keep-alives (SO_KEEPALIVE) on broker sockets
  'socket.keepalive.enable': true,

  // Maximum total message size sum allowed on
  // the producer queue.This queue is shared by all topics and partitions.
  'queue.buffering.max.kbytes': 1048576, // 1GB

  // Delay in milliseconds to wait for messages in the producer
  // queue to accumulate before constructing message batches (MessageSets) to transmit to brokers.
  'queue.buffering.max.ms': 0.5,

  // Maximum number of messages batched in one MessageSet.
  'batch.num.messages': 10000,

  // Client identifier. It must be <service.instance>.
  'client.id': 'sample-producer',

  // Initial list of brokers as a CSV list of broker host or host:port.
  'metadata.broker.list': 'kafka:9092',

  // Compression codec to use for compressing message sets.
  'compression.codec': 'none',

  // This field indicates whether or not a delivery-report will be generated.
  dr_cb: true,

};

module.exports = config;
