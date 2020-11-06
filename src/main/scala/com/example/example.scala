package com.example

import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import enumeratum.Enum
import enumeratum.EnumEntry
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import eu.timepit.refined.types.all.NonNegBigDecimal
import eu.timepit.refined.types.string.NonEmptyString

object example {

  /** DTOs.
    * They can be used to e.g. model a raw request or an even in the infrastructure layer of the application.
    */
  case class OrderCreationRequest(customerId: String, amount: BigDecimal, currency: String) {
    val toDomain: EitherNel[ValidationError, Order] =
      Order.create(customerId: String, amount: BigDecimal, currency: String)
  }

  /** Domain Model.
    * The business logic belongs here. This where we want to use more types, perform validation and check for invariants etc.
    */
  sealed trait ValidationError
  case object EmptyCustomerId extends ValidationError
  case object NegativeAmount  extends ValidationError
  case object InvalidCurrency extends ValidationError

  case class CustomerId(value: NonEmptyString)
  object CustomerId {
    def create(value: String): Either[ValidationError, CustomerId] =
      refineV[NonEmpty](value).bimap(_ => EmptyCustomerId, CustomerId(_))
  }

  sealed trait Currency extends EnumEntry
  object Currency extends Enum[Currency] {
    case object PLN extends Currency
    case object EUR extends Currency
    case object GBP extends Currency

    val values = findValues
  }

  case class Money(amount: NonNegBigDecimal, currency: Currency)
  object Money {

    def create(amount: BigDecimal, currency: String): Either[NonEmptyList[ValidationError], Money] =
      (
        refineV[NonNegative](amount)
          .leftMap(_ => NegativeAmount)
          .toEitherNel,
        Currency
          .withNameInsensitiveEither(currency)
          .leftMap(_ => InvalidCurrency)
          .toEitherNel
      ).parMapN(Money(_, _))

  }

  case class Order(customerId: CustomerId, amount: Money)
  object Order {
    def create(customerId: String, amount: BigDecimal, currency: String): EitherNel[ValidationError, Order] =
      (
        CustomerId.create(customerId).toEitherNel,
        Money.create(amount, currency)
      ).parMapN(Order(_, _))
  }

  /** HTTP controller.
    * This can be for example tapir's server logic, http4s or akka routing DSL.
    * This is where we convert DTOs to the core domain model along with executing the validation logic.
    * We know exactly what to do with validation errors here.
    */
  object OrderHttpController {
    def create(order: OrderCreationRequest): IO[HttpResponse] = IO {
      order.toDomain.fold(
        errors => HttpResponse(400, s"Errors found: ${errors.toList.mkString("(", ", ", ")")}"),
        _ => HttpResponse(200, "Order created")
      )
    }

    case class HttpResponse(errorCode: Int, payload: String)
  }

  /** Order domain service.
    * Contains the main business logic and models business processes like Order creation.
    * It should accept only valid entities.
    */
  object OrderService {
    def create(order: Order): IO[Either[OrderCreationError, Unit]] = {
      // If order could have been created, it's valid.
      // That means only valid orders can be processed here.
      IO.pure(Right(()))
    }

    sealed trait OrderCreationError
  }

}
