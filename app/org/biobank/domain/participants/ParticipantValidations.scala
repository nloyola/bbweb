package org.biobank.domain.participants

import org.biobank.domain.ValidationKey

trait ParticipantValidations {

  case object UniqueIdInvalid extends ValidationKey

  case object UniqueIdRequired extends ValidationKey

  case object ParticipantIdRequired extends ValidationKey

  case object CollectinEventTypeIdRequired extends ValidationKey

  case object AmountInvalid extends ValidationKey

  case object OriginLocationIdInvalid extends ValidationKey

  case object LocationIdInvalid extends ValidationKey

  case object ContainerIdInvalid extends ValidationKey

  case object PositionInvalid extends ValidationKey

  case object VisitNumberInvalid extends ValidationKey

}
