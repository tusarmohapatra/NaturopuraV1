import * as amqp from "amqplib";
import Address from "../model/address.model";
import { colorLog } from "../utility/helper/helper";
export async function consumeUserRegisteredEvent() {
  return new Promise<string>(async (resolve, reject) => {
    try {
      const connection = await amqp.connect("amqp://rabbitmq");
      const channel = await connection.createChannel();

      const exchangeName = "user_events";
      const queueName = "order_service_queue";
      const routingKey = "user.registered";

      await channel.assertExchange(exchangeName, "direct", { durable: true });
      await channel.assertQueue(queueName, { durable: true });
      await channel.bindQueue(queueName, exchangeName, routingKey);

      await channel.consume(queueName, (message: any) => {
        const address = JSON.parse(message.content.toString());
        console.log("User registered event received");

        const AddressTable = Address.create({
          userId: address.user_id,
          address_line1: address.addressLine1,
          city: address.city,
          state: address.state,
          postal_code: address.zipCode,
          country: address.country,
          address_type: "user",
        });

        channel.ack(message);
      });

      console.log(colorLog("Waiting for user registered events...", "FgGreen"));
      resolve("Waiting for user registered events...");
    } catch (error) {
      console.log(
        colorLog("Failed to consume user registered event:", "FgRed")
      );

      console.error(error);
      reject("Failed to consume user registered event:");
    }
  });
}

export async function consumeEvent(): Promise<Array<any>> {
  return Promise.all([consumeUserRegisteredEvent()]);
}
