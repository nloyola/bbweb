package org.biobank.domain.containers

import play.api.libs.json._

class ContainerStorageType(val id: String) extends AnyVal {
  override def toString: String = id
}

object ContainerStorageType {

  implicit val containerStorageTypeFormat: Format[ContainerStorageType] = new Format[ContainerStorageType] {

    override def writes(storageType: ContainerStorageType): JsValue = JsString(storageType.id)

    override def reads(json: JsValue): JsResult[ContainerStorageType] =
      Reads.StringReads.reads(json).map(new ContainerStorageType(_))
  }

}
