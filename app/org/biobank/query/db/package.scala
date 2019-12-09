package org.biobank.query

import org.biobank._

package db {

  final case class SequenceNumber(persistenceId: String, sequenceNumber: Long)

  trait DbOperation

}

package object db {

  type DbOperationResult = FutureValidation[DbOperation]

}
