import * as amqp from 'amqplib';

export async function publishUserRegisteredEvent(address:any) {
  try {
    const connection = await amqp.connect('amqp://rabbitmq');
    const channel = await connection.createChannel();

    const exchangeName = 'user_events';
    const routingKey = 'user.registered';
    const message = JSON.stringify(address);

    await channel.assertExchange(exchangeName, 'direct', { durable: true });
    await channel.publish(exchangeName, routingKey, Buffer.from(message));

    console.log('User registered event published');
    
    await channel.close();
    await connection.close();
  } catch (error) {
    console.error('Failed to publish user registered event:', error);
  }
}

