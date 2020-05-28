package org.biobank.query

import org.biobank.validation.Validation._

package db {

  final case class SequenceNumber(persistenceId: String, sequenceNumber: Long)

  trait DbOperation

}

package object db {

  type DbOperationResult = FutureValidationResult[DbOperation]

}
