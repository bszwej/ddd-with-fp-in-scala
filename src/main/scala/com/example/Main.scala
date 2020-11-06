package com.example

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import com.example.example.OrderCreationRequest
import com.example.example.OrderHttpController

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.unit

      // Successful order creation
      orderCreationRequest = OrderCreationRequest(customerId = "42", amount = BigDecimal(10), currency = "PLN")
      _            <- IO.delay(println(s"Creating order: $orderCreationRequest"))
      httpResponse <- OrderHttpController.create(orderCreationRequest)
      _            <- IO.delay(println(httpResponse))

      // Error order creation
      orderCreationRequest = OrderCreationRequest(
        customerId = "",
        amount = BigDecimal(-100),
        currency = "invalid currency"
      )
      _            <- IO.delay(println(s"Creating order: $orderCreationRequest"))
      httpResponse <- OrderHttpController.create(orderCreationRequest)
      _            <- IO.delay(println(httpResponse))
    } yield ExitCode.Success

}
