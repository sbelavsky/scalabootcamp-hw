package com.evolution.bootcamp.error_handling

import cats.data._
import cats.implicits._

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object Homework {

  case class PaymentCard(
                          name: String,
                          number: String,
                          expirationDate: String,
                          securityCode: String
                        )

  sealed trait ValidationError

  object ValidationError {

    final case object NotLettersOnly extends ValidationError

    final case object NonNumeric extends ValidationError

    final case object InvalidDate extends ValidationError

    final case object InvalidSecurityCode extends ValidationError

  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validateName(name: String): AllErrorsOr[String] = {
      if (name.matches("^[A-Za-z]+$")) name.validNec
      else NotLettersOnly.invalidNec
    }

    def validateNumber(number: String): AllErrorsOr[String] = {
      if (number.matches("^[0-9]+$")) number.validNec
      else NonNumeric.invalidNec
    }

    def validateExpirationDate(expirationDate: String): AllErrorsOr[String] = {
      if (expirationDate.matches("^\\d{2}\\/\\d[2]$")) expirationDate.validNec
      else InvalidDate.invalidNec
    }

    def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
      if (securityCode.matches("^\\d[3]$")) securityCode.validNec
      else InvalidSecurityCode.invalidNec
    }

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String
                ): AllErrorsOr[PaymentCard] =
      (
        validateName(name),
        validateNumber(number),
        validateExpirationDate(expirationDate),
        validateSecurityCode(securityCode)
        ).mapN(PaymentCard)
  }

}
