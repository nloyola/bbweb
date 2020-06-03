package org.biobank

import java.time.OffsetDateTime
import javax.inject.{Inject, Singleton}
import org.biobank.domain._
import org.biobank.domain.annotations._
import org.biobank.domain.access._
import org.biobank.domain.access.RoleId._
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.participants._
import org.biobank.domain.studies._
import org.biobank.domain.users._
import org.biobank.dto.centres.{CentreLocationInfo, ShipmentDto, ShipmentSpecimenDto}
import org.biobank.services.PasswordHasher
import play.api.{Configuration, Environment, Logger, Mode}
import scalaz.Scalaz._
import org.biobank.query.db.DatabaseSchema
import play.api.db.slick.DatabaseConfigProvider

/**
 * Provides initial data to test with.
 */
object TestData {

  val centreCBSRName    = "CBSR"
  val centreCalgaryName = "100-Calgary AB"
  val centreLondonName  = "101-London ON"

  val centreCBSRId:    CentreId = CentreId(centreCBSRName + "_id")
  val centreCalgaryId: CentreId = CentreId(centreCalgaryName + "_id")
  val centreLondonId:  CentreId = CentreId(centreLondonName + "_id")

  val centreData: List[Tuple2[String, String]] =
    List(("CL1-Foothills", "CL1-Foothills"),
         ("CL1-Heritage", "CL1-Heritage"),
         ("CL1-Sunridge", "CL1-Sunridge"),
         ("CL2-Children Hosp", "CL2-Alberta's Children's Hospital"),
         ("ED1-UofA", "ED1-UofA"),
         ("OT2-Children Hosp", "OT2-Children's Hospital of Eastern Ontario"),
         ("QB1-Enfant-Jesus", "QB1-Hopital Enfant-Jesus"),
         ("RD1-Red Deer Hosp", "RD1-Red Deer Regional Hospital"),
         ("SB1-St John NB Hosp", "SB1-Saint Johns NB Regional Hospital"),
         ("SD1-Sudbury Hosp", "SD1-Sudbury Regional Hospital"),
         ("SF1-Health NFLD", "SF1-Health Science Center"),
         ("SP1-St Therese Hosp", "SP1-St Therese Hospital"),
         ("SS1-Royal Hosp", "SS1-Royal University Hospital"),
         ("TH1-Regional Hosp", "TH1-Thunder Bay Regional Hospital"),
         ("TR1-St Mikes", "TR1-St Michael's Hospital"),
         ("VN1-St Paul", "VN1-St Paul's Hospital"),
         ("VN2-Childrens Hosp", "VN2-BC Women and Children's Hospital"),
         ("WL1-Westlock Hosp", "WL1-Westlock Health Care Center"),
         ("WN1-Cancer Care", "WN1-Cancer Care Manitoba"),
         ("CL1-Foothills TRW", "CL1-Foothills TRW"),
         (centreCBSRName, "Canadian BioSample Repository"),
         ("Calgary-F", "Calgary Foothills"),
         (centreCalgaryName, "100-Calgary Alberta Refine"),
         ("College Plaza UofA", "College Plaza UofA"),
         (centreLondonName, "101-London Ontario Refine"),
         ("102-Newmarket ON", "102-Newmarket Ontario Refine"),
         ("103-Montreal QC", "103-Montreal Quebec Refine"),
         ("104-Montreal QC", "104-Montreal Quebec Refine"),
         ("105-Victoria BC", "105-Victoria British Columbia refine"),
         ("106-Quebec City QC", "106-Quebec City Quebec Refine"),
         ("CaRE ED1", "CaRE ED1"),
         ("200-Spokane WA", "200-Spokane Washington REFINE"),
         ("201-Erie PA", "201-Erie Pennsylvania REFINE"),
         ("202-Cherry Hill NJ", "202-Cherry Hill New Jersey REFINE"),
         ("203-West Des Moines IA", "203-West Des Moines Iowa REFINE"),
         ("204-Evansville IN", "204-Evansville Indiana REFINE"),
         ("205-Southfield MI", "205-Southfield Michigan REFINE"),
         ("206 A -Nashville TN", "206 A -Nashville Tennessee REFINE"),
         ("207-Amarillo TX", "207-Amarillo Texas REFINE"),
         ("300-Oulu FI", "300-Oulu Finland REFINE"),
         ("ED1-GNH", "ED1-GNH"),
         ("KIDNI CL1", "KIDNI Calgary"))

  val studyData: List[Tuple2[String, String]] =
    List(("AHFEM", "Acute Heart Failure-Emergency Management"),
         ("AKI", "Acute Kidney Injury"),
         ("Asthma", "Asthma"),
         ("BBPSP", "Blood Borne Pathogens Surveillance Project"),
         ("CCCS", "Critical Care Cohort Study"),
         ("CCSC Demo", "CCSC Demo"),
         ("CDGI", "Crohn's Disease Genetic Inflizimab"),
         ("CEGIIR", "Centre of Excellence for Gastrointestinal Inflammation and Immunity Research"),
         ("CHILD", "Canadian Health Infant Longitudinal Development Study"),
         ("CITP", "Clinical Islet Transplant Program"),
         ("CRM", "Diagnostic Marker for a Colorectal Cancer Blood Test"),
         ("CSF", "CSF"),
         ("CaRE", "CaRE"),
         ("Caspase", "CITP Caspase"),
         ("DBStudy", "DBTestStudy"),
         ("DDPS", "Double Dialysate Phosphate Study"),
         ("DG Study", "Delta Genomics Study"),
         ("ERCIN",
          "Exploring the Renoprotective effects of fluid prophylaxis strategies for Contrast Induced Nephropathy (Study)"),
         ("FABRY",
          "Enzyme replacement therapy in patients with Fabry disease: differential impact on Heart Remodeling and Vascular Function"),
         ("FALLOT", "FALLOT"),
         ("FIDS", "Fedorak Iron Deficiency Study"),
         ("HAART", "Randomized Controlled Pilot Study of Highly Active Anti-Retroviral Therapy"),
         ("HEART", "Heart failure Etiology and Analysis Research Team"),
         ("JB", "Bradwein"),
         ("KDCS", "Kidney Disease Cohort Study"),
         ("KIDNI", "KIDNI"),
         ("KMS", "Kingston Merger Study"),
         ("LCS", "Laboratory Controls Study"),
         ("MPS", "Man-Chui Poon Study"),
         ("NEC", "Necrotizing Enterocolitis Study"),
         ("NHS", "Novartis Hepatitis C Study"),
         ("Novel - ESRD", "Novel - ESRD"),
         ("PG1", "Phenomic Gap"),
         ("PROBE", "PROBE"),
         ("PSS", "(Dr.) Parent Scoliosis Study"),
         ("QPCS", "Quebec Pancreas Cancer Study"),
         ("REFINE", "REFINE ICD"),
         ("REIM",
          "Resilience Enhancement in Military Populations Through Multiple Health Status Assessments"),
         ("RVS", "Retroviral Study"),
         ("SPARK",
          "A phase II randomized blinded controlled trial of the effect of furoSemide in cricially ill Patients with eARly acute Kidney injury"),
         ("Spinal Stiffness", "Spinal Stiffness"),
         ("TCKS", "Tonelli Chronic Kidney Study"),
         ("TMIC", "TMIC"),
         ("VAS", "Vascular Access Study"),
         ("ZEST", "ZEST"),
         ("iGenoMed", "iGenoMed"))

  val userData: List[Tuple2[String, String]] =
    List(("Nelson Loyola", "loyola@ualberta.ca"),
         ("Luisa Franco", "lfrancor@ucalgary.ca"),
         ("Corazon Oballo", "coballo@ucalgary.ca"),
         ("Amie Lee", "amie1@ualberta.ca"),
         ("Lisa Tanguay", "lisa.tanguay@ualberta.ca"),
         ("Darlene Ramadan", "ramadan@ucalgary.ca"),
         ("Juline Skripitsky", "Jskrip@biosample.ca"),
         ("Leslie Jackson Carter", "jacksola@ucalgary.ca"),
         ("Thiago Oliveira", "toliveir@ucalgary.ca"),
         ("Rozsa Sass", "rsas@ucalgary.ca"),
         ("Margaret Morck", "mmorck@ucalgary.ca"),
         ("Kristan Nagy", "nagy1@ualberta.ca"),
         ("Bruce Ritchie", "bruce.ritchie@ualberta.ca"),
         ("Matthew Klassen", "mwklasse@ualberta.ca"),
         ("Marleen Irwin", "mirwin@ualberta.ca"),
         ("Millie Silverstone", "millie.silverstone@me.com"),
         ("Trevor Soll", "tsoll@ualberta.ca"),
         ("Stephanie Wichuk", "stephaniewichuk@med.ualberta.ca"),
         ("Deborah Parfett", "dparfett@catrials.org"),
         ("Samantha Taylor", "samantha.taylor@albertahealthservices.ca"),
         ("Martine Bergeron", "martine.bergeron@crchum.qc.ca"),
         ("Isabelle Deneufbourg", "isabelle.deneufbourg@criucpq.ulaval.ca"),
         ("Colin Coros", "coros@ualberta.ca"),
         ("Suzanne Morissette", "suzanne.morissette.chum@ssss.gouv.qc.ca"),
         ("Francine Marsan", "francine.marsan.chum@ssss.gouv.qc.ca"),
         ("Jeanne Bjergo", "jeannebjergo@hcnw.com"),
         ("Larissa Weeks", "larissaweeks@hcnw.com"),
         ("Sharon Fulton", "sharonfulton@hcnw.com"),
         ("Mirjana Maric Viskovic", "maric@ucalgary.ca"),
         ("Paivi Kastell", "paivi.kastell@ppshp.fi"),
         ("Paivi Koski", "paivi.koski@ppshp.fi"))

  val ahfemDescription: String =
    s"""|Magnis turpis mollis. Duis commodo libero. Turpis magnis massa morbi cras non mollis, maecenas
        |dictumst venatis augue, rhoncus id non eros nec odio. Ut wisi ullamcorper elit parturient,
        |venenatis libero et, pellentesque sed, purus erat nonummy diam. Hendrerit porro lobortis.
        |
        |Eget proin ligula blandit ante magna aenean. Purus et maecenas, venenatis nonummy dolor quam
        |dictumst. Auctor etiam, ligula eu, senectus iaculis ante. Sed urna, viverra pellentesque
        |scelerisque libero vel, vitae neque nascetur nibh turpis, ridiculus pede maecenas rutrum per
        |cubilia ultrices. Lacus sapien odio per ac nulla lectus. Morbi vitae a laoreet vehicula lectus,
        |rutrum convallis diam, arcu ipsum egestas facilis eleifend, tellus neque rutrum ut wisi in. Sit
        |velit sociis placerat neque id, imperdiet ut a urna ac, sed accumsan, fusce nunc dolor, et donec
        |orci quis. Magnis vestibulum dapibus leo consectetuer blandit, ac eget, porta tempor semper urna
        |tempor diam.
        |""".stripMargin

  val accessUserData: List[Tuple2[String, String]] =
    List(("study-administrator", "Study Administrator"),
         ("study-user", "Study User"),
         ("specimen-collector", "Specimen Collector"),
         ("shipping-admin", "Shipping Admin"),
         ("shipping-user", "Shipping User"))

  val EventTypeIdPrefix      = "test-data-cevent-types"
  val ProcessingTypeIdPrefix = "test-data-processing-types"
  val SpecimenDefIdPrefix    = "test-data-specimen-specs"
  val SpecimenIdPrefix       = "test-data-specimen-specs"
}

object BbpspTestData {
  import TestData._

  val BbpspStudyId:        StudyId        = StudyId("BBPSP_id")
  val CentreNames:         List[String]   = List(TestData.centreCalgaryName, TestData.centreLondonName)
  val CentreIds:           List[CentreId] = List(TestData.centreCalgaryId, TestData.centreLondonId)
  val NumParticipants:     Int            = 3
  val EventTypeNames:      List[String]   = List("1 - Default Event", "2 - Second Event")
  val ProcessingTypeNames: List[String]   = List("1 - Step 1", "2 - Step 2")

  val ParticipantAnnotationTypeIdPrefix = "bbpsp-participant-annotation-types"
  val EventTypeAnnotationTypeIdPrefix   = "bbpsp-collection-event-annotation-types"
  val EventIdPrefix                     = "bbpsp-collection-events"
  val ParticipantIdPrefix               = "bbpsp-participants"

  // the slug is temporarily assigned an empty value, once the set is created, the slug is then derived
  // from the name below.
  val CollectedSpecimenDefinitions: Set[CollectedSpecimenDefinition] =
    Set(
      CollectedSpecimenDefinition(id                      = SpecimenDefinitionId(SpecimenDefIdPrefix + "1"),
                                  slug                    = Slug(""),
                                  name                    = "10 mL Lavender top EDTA tube",
                                  description             = None,
                                  units                   = "mL",
                                  anatomicalSourceType    = AnatomicalSourceType.Blood,
                                  preservationType        = PreservationType.FreshSpecimen,
                                  preservationTemperature = PreservationTemperature.RoomTemperature,
                                  specimenType            = SpecimenType.WholeBloodEdta,
                                  maxCount                = 2, // set to 2 for testing form, set back to 1 for demo
                                  amount                  = 10.0),
      CollectedSpecimenDefinition(id                      = SpecimenDefinitionId(SpecimenDefIdPrefix + "2"),
                                  slug                    = Slug(""),
                                  name                    = "10 mL Orange top PAXgene tube",
                                  description             = None,
                                  units                   = "mL",
                                  anatomicalSourceType    = AnatomicalSourceType.Blood,
                                  preservationType        = PreservationType.FreshSpecimen,
                                  preservationTemperature = PreservationTemperature.RoomTemperature,
                                  specimenType            = SpecimenType.Paxgene,
                                  maxCount                = 1,
                                  amount                  = 10.0),
      CollectedSpecimenDefinition(id                      = SpecimenDefinitionId(SpecimenDefIdPrefix + "3"),
                                  slug                    = Slug(""),
                                  name                    = "3mL Lavender top EDTA tube",
                                  description             = None,
                                  units                   = "mL",
                                  anatomicalSourceType    = AnatomicalSourceType.Blood,
                                  preservationType        = PreservationType.FreshSpecimen,
                                  preservationTemperature = PreservationTemperature.RoomTemperature,
                                  specimenType            = SpecimenType.WholeBloodEdta,
                                  maxCount                = 1,
                                  amount                  = 3),
      CollectedSpecimenDefinition(id                      = SpecimenDefinitionId(SpecimenDefIdPrefix + "4"),
                                  slug                    = Slug(""),
                                  name                    = "4ml lavender top EDTA tube",
                                  description             = None,
                                  units                   = "mL",
                                  anatomicalSourceType    = AnatomicalSourceType.Blood,
                                  preservationType        = PreservationType.FreshSpecimen,
                                  preservationTemperature = PreservationTemperature.RoomTemperature,
                                  specimenType            = SpecimenType.WholeBloodEdta,
                                  maxCount                = 1,
                                  amount                  = 4),
      CollectedSpecimenDefinition(id                      = SpecimenDefinitionId(SpecimenDefIdPrefix + "5"),
                                  slug                    = Slug(""),
                                  name                    = "9ml CPDA yellow top tube",
                                  description             = None,
                                  units                   = "mL",
                                  anatomicalSourceType    = AnatomicalSourceType.Blood,
                                  preservationType        = PreservationType.FreshSpecimen,
                                  preservationTemperature = PreservationTemperature.RoomTemperature,
                                  specimenType            = SpecimenType.WholeBloodEdta,
                                  maxCount                = 1,
                                  amount                  = 9),
      CollectedSpecimenDefinition(id                      = SpecimenDefinitionId(SpecimenDefIdPrefix + "6"),
                                  slug                    = Slug(""),
                                  name                    = "Urine cup",
                                  description             = None,
                                  units                   = "mL",
                                  anatomicalSourceType    = AnatomicalSourceType.Urine,
                                  preservationType        = PreservationType.FreshSpecimen,
                                  preservationTemperature = PreservationTemperature.Plus4celcius,
                                  specimenType            = SpecimenType.CdpaPlasma,
                                  maxCount                = 1,
                                  amount                  = 15)
    ).map { sd =>
      sd.copy(slug = Slug(sd.name))
    }

  val EventTypeAnnotationTypes: Set[AnnotationType] =
    Set(
      AnnotationType(id            = AnnotationTypeId(EventTypeAnnotationTypeIdPrefix + "1"),
                     slug          = Slug("Phlebotomist"),
                     name          = "Phlebotomist",
                     description   = None,
                     valueType     = AnnotationValueType.Text,
                     maxValueCount = None,
                     options       = Seq.empty[String],
                     required      = true),
      AnnotationType(id            = AnnotationTypeId(EventTypeAnnotationTypeIdPrefix + "2"),
                     slug          = Slug("Consent"),
                     name          = "Consent",
                     description   = None,
                     valueType     = AnnotationValueType.Select,
                     maxValueCount = Some(2),
                     options =
                       Seq("Surveillance", "Genetic Predisposition", "Previous Samples", "Genetic Mutation"),
                     required = true)
    ).map { at =>
      at.copy(slug = Slug(at.name))
    }

  val ProcessingTypeAnnotationTypes: Set[AnnotationType] =
    Set(
      AnnotationType(id            = AnnotationTypeId(EventTypeAnnotationTypeIdPrefix + "3"),
                     slug          = Slug("PBMC Count"),
                     name          = "PBMC Count",
                     description   = None,
                     valueType     = AnnotationValueType.Number,
                     maxValueCount = None,
                     options       = Seq.empty[String],
                     required      = true)
    ).map { at =>
      at.copy(slug = Slug(at.name))
    }

  val ParticipantAnnotationTypes: Set[AnnotationType] =
    Set(
      AnnotationType(id            = AnnotationTypeId(ParticipantAnnotationTypeIdPrefix + "1"),
                     slug          = Slug(""),
                     name          = "Date of birth",
                     description   = None,
                     valueType     = AnnotationValueType.DateTime,
                     maxValueCount = None,
                     options       = Seq.empty[String],
                     required      = true),
      AnnotationType(id            = AnnotationTypeId(ParticipantAnnotationTypeIdPrefix + "2"),
                     slug          = Slug(""),
                     name          = "Gender",
                     description   = None,
                     valueType     = AnnotationValueType.Select,
                     maxValueCount = Some(1),
                     options       = Seq("Female", "Male"),
                     required      = true)
    ).map { at =>
      at.copy(slug = Slug(at.name))
    }
}

object CbsrTestData {

  val schemaData: List[String] =
    List("96 Well Microplate", "384 Well Microplate")

}

/**
 * Provides initial data to test with. Ideally these methods should only be called for development builds.
 */
@SuppressWarnings(
  Array("org.wartremover.warts.NonUnitStatements",
        "org.wartremover.warts.ImplicitParameter",
        "org.wartremover.warts.StringPlusAny",
        "org.wartremover.warts.Var")
)
@Singleton
class TestData @Inject()(
    private val config:             Configuration,
    private val env:                Environment,
    private val passwordHasher:     PasswordHasher,
    protected val dbConfigProvider: DatabaseConfigProvider)
    extends DatabaseSchema {

  import TestData._

  case class ParticipantData(participant: Participant, eventData: List[CollectionEventData])

  case class CollectionEventData(event: CollectionEvent, specimens: List[Specimen])

  val log: Logger = Logger(this.getClass)

  lazy val participantData: List[ParticipantData] = createParticipants

  /**
   * When TRUE, the system will load test data.
   */
  private val loadTestDataMode = (env.mode == Mode.Dev) || (env.mode == Mode.Prod)

  private val loadTestData = {
    loadTestDataMode && config.get[Boolean]("application.testData.load")
  }

  private val loadSpecimenTestData =
    loadTestDataMode && loadTestData && config.get[Boolean]("application.testData.loadSpecimens")

  private val loadShipmentTestData =
    loadTestDataMode && loadSpecimenTestData && config.get[Boolean]("application.testData.loadShipments")

  private val loadAccessTestData =
    loadTestDataMode && config.get[Boolean]("application.testData.loadAccessData")

  def testUsers(): List[User] = {
    if (!loadTestData) {
      List.empty[User]
    } else {
      log.debug("testUsers")

      val plainPassword = "testuser"
      val salt          = passwordHasher.generateSalt
      val userIdPrefix  = "test-data-users"

      userData.zipWithIndex.map {
        case ((name, email), index) =>
          ActiveUser(id           = UserId(userIdPrefix + index.toString),
                     version      = 0L,
                     timeAdded    = Global.StartOfTime,
                     timeModified = None,
                     slug         = Slug(name),
                     name         = name,
                     email        = email,
                     password     = passwordHasher.encrypt(plainPassword, salt),
                     salt         = salt,
                     avatarUrl    = None)
      } ++ List[User](
        RegisteredUser(id           = UserId("registered-user"),
                       version      = 0L,
                       timeAdded    = Global.StartOfTime,
                       timeModified = None,
                       slug         = Slug("registered-user"),
                       name         = "Registered User",
                       email        = "registered@admin.com",
                       password     = passwordHasher.encrypt(plainPassword, salt),
                       salt         = salt,
                       avatarUrl    = None),
        LockedUser(id               = UserId("locked-user"),
                   version          = 0L,
                   timeAdded        = Global.StartOfTime,
                   timeModified     = None,
                   slug             = Slug("locked-user"),
                   name             = "Locked User",
                   email            = "locked@admin.com",
                   password         = passwordHasher.encrypt(plainPassword, salt),
                   salt             = salt,
                   avatarUrl        = None)
      )
    }
  }

  def testCentres(): List[Centre] =
    if (!loadTestData) {
      List.empty[Centre]
    } else {
      log.debug("testCentres")

      centreData.map {
        case (name, description) =>
          val locations = {
            if (name == centreCalgaryName) {
              Set(
                Location(id             = LocationId(s"${centreCalgaryId.id}:Primary"),
                         slug           = Slug(""),
                         name           = "Primary",
                         street         = "1403 29 St NW",
                         city           = "Calgary",
                         province       = "Alberta",
                         postalCode     = "T2N 2T9",
                         poBoxNumber    = None,
                         countryIsoCode = "CA")
              )
            } else if (name == centreLondonName) {
              Set(
                Location(id   = LocationId(s"${centreLondonId.id}:Primary"),
                         slug = Slug(""),
                         name = "Primary",
                         street =
                           "London Health Sciences Center, University Hospital, Rm A3-222B, 339 Windermere Road",
                         city           = "London",
                         province       = "Ontario",
                         postalCode     = "N0L 1W0",
                         poBoxNumber    = None,
                         countryIsoCode = "CA")
              )
            } else {
              Set.empty[Location]
            }
          }.map { l =>
            l.copy(slug = Slug(l.id.id))
          }

          if ((name == centreCalgaryName) || (name == centreLondonName)) {
            val id = if (name == centreCalgaryName) centreCalgaryId else centreLondonId
            EnabledCentre(id           = id,
                          version      = 0L,
                          timeAdded    = Global.StartOfTime,
                          timeModified = None,
                          slug         = Slug(name),
                          name         = name,
                          description  = Some(description),
                          studyIds     = Set(BbpspTestData.BbpspStudyId),
                          locations    = locations).asInstanceOf[Centre]
          } else {
            DisabledCentre(id           = CentreId(s"${name}_id"),
                           version      = 0L,
                           timeAdded    = Global.StartOfTime,
                           timeModified = None,
                           slug         = Slug(name),
                           name         = name,
                           description  = Some(description),
                           studyIds     = Set(BbpspTestData.BbpspStudyId),
                           locations    = locations).asInstanceOf[Centre]
          }
      }
    }

  def testStudies(): List[Study] =
    if (!loadTestData) {
      List.empty[Study]
    } else {
      log.debug("testStudies")

      studyData.map {
        case (name, description) =>
          val descMaybe =
            if (name == "AHFEM") Some(s"$description\n\n$ahfemDescription")
            else Some(description)

          if (name == "BBPSP") {
            EnabledStudy(id              = StudyId(s"${name}_id"),
                         version         = 0L,
                         timeAdded       = Global.StartOfTime,
                         timeModified    = None,
                         slug            = Slug(name),
                         name            = name,
                         description     = descMaybe,
                         annotationTypes = BbpspTestData.ParticipantAnnotationTypes).asInstanceOf[Study]
          } else {
            DisabledStudy(id              = StudyId(s"${name}_id"),
                          version         = 0L,
                          timeAdded       = Global.StartOfTime,
                          timeModified    = None,
                          slug            = Slug(name),
                          name            = name,
                          description     = descMaybe,
                          annotationTypes = BbpspTestData.ParticipantAnnotationTypes).asInstanceOf[Study]
          }
      }
    }

  def testEventTypes(): List[CollectionEventType] =
    if (!loadTestData) {
      List.empty[CollectionEventType]
    } else {
      log.debug("testEventTypes")

      BbpspTestData.EventTypeNames.zipWithIndex.map {
        case (name, index) =>
          val id = CollectionEventTypeId(EventTypeIdPrefix + index.toString)
          val annotationTypes =
            if (name == BbpspTestData.EventTypeNames(0)) BbpspTestData.EventTypeAnnotationTypes
            else Set.empty[AnnotationType]

          val specimenDefinitions = BbpspTestData.CollectedSpecimenDefinitions
            .map { spcdef =>
              val spcdefId = id.id + "_" + spcdef.id.id
              spcdef.copy(id = SpecimenDefinitionId(spcdefId))
            }

          CollectionEventType(studyId             = BbpspTestData.BbpspStudyId,
                              id                  = id,
                              version             = 0L,
                              timeAdded           = Global.StartOfTime,
                              timeModified        = None,
                              slug                = Slug(name),
                              name                = name,
                              description         = None,
                              recurring           = true,
                              specimenDefinitions = specimenDefinitions,
                              annotationTypes     = annotationTypes)
      }
    }

  def testProcessingTypes(): List[ProcessingType] =
    if (!loadTestData) {
      List.empty[ProcessingType]
    } else {
      log.debug("testProcessingTypes")

      BbpspTestData.ProcessingTypeNames.zipWithIndex.map {
        case (name, index) =>
          val id = ProcessingTypeId(ProcessingTypeIdPrefix + index.toString)
          val annotationTypes =
            if (name == BbpspTestData.ProcessingTypeNames(0)) BbpspTestData.ProcessingTypeAnnotationTypes
            else Set.empty[AnnotationType]

          val input =
            if (index == 0)
              InputSpecimenProcessing(expectedChange       = BigDecimal(1.0),
                                      count                = 1,
                                      containerTypeId      = None,
                                      definitionType       = ProcessingType.collectedDefinition,
                                      entityId             = CollectionEventTypeId(EventTypeIdPrefix + "0").id,
                                      specimenDefinitionId = SpecimenDefinitionId(SpecimenDefIdPrefix + "1"))
            else
              InputSpecimenProcessing(expectedChange       = BigDecimal(1.0),
                                      count                = 1,
                                      containerTypeId      = None,
                                      definitionType       = ProcessingType.processedDefinition,
                                      entityId             = ProcessingTypeId(ProcessingTypeIdPrefix + "0").id,
                                      specimenDefinitionId = SpecimenDefinitionId(SpecimenDefIdPrefix + "1"))
          val specimenDefinition =
            ProcessedSpecimenDefinition(id                      = SpecimenDefinitionId(id.id),
                                        slug                    = Slug(name),
                                        name                    = name,
                                        description             = None,
                                        units                   = "mL",
                                        anatomicalSourceType    = AnatomicalSourceType.Blood,
                                        preservationType        = PreservationType.FreshSpecimen,
                                        preservationTemperature = PreservationTemperature.Minus80celcius,
                                        specimenType            = SpecimenType.BuffyCoat)
          val output =
            OutputSpecimenProcessing(expectedChange     = BigDecimal(1.0),
                                     count              = 1,
                                     containerTypeId    = None,
                                     specimenDefinition = specimenDefinition)

          ProcessingType(studyId         = BbpspTestData.BbpspStudyId,
                         id              = id,
                         version         = 0L,
                         timeAdded       = Global.StartOfTime,
                         timeModified    = None,
                         slug            = Slug(s"$name specimen"),
                         name            = name,
                         description     = None,
                         enabled         = true,
                         input           = input,
                         output          = output,
                         annotationTypes = annotationTypes,
                         inUse           = index == 0)
      }
    }

  def testParticipants(): List[Participant] =
    participantData.map { pd =>
      pd.participant
    }

  def testEvents(): List[CollectionEvent] =
    participantData.flatMap(_.eventData.map(_.event))

  def testSpecimens(): List[Specimen] =
    participantData.flatMap(_.eventData.flatMap(_.specimens))

  def testCeventSpecimens(): List[CeventSpecimen] =
    participantData.flatMap(_.eventData.flatMap { eventData =>
      eventData.specimens.map { specimen =>
        CeventSpecimen(eventData.event.id, specimen.id)
      }
    })

  @SuppressWarnings(
    Array("org.wartremover.warts.MutableDataStructures",
          "org.wartremover.warts.Var",
          "org.wartremover.warts.TraversableOps")
  )
  def testShipments(): List[Shipment] = {
    if (!loadShipmentTestData) {
      List.empty[Shipment]
    } else {
      log.debug(s"testShipments")

      /*
       * - creates mutliple shipments, each in a different state
       */
      val originCentreId        = centreCalgaryId
      val originLocationId      = LocationId(s"${originCentreId.id}:Primary")
      val destinationCentreId   = centreLondonId
      val destinationLocationId = LocationId(s"${destinationCentreId.id}:Primary")

      List[Shipment](
        CreatedShipment(id                     = ShipmentId(s"test-shipment-created"),
                        version                = 0,
                        timeAdded              = Global.StartOfTime,
                        timeModified           = None,
                        courierName            = "FedEx",
                        trackingNumber         = "TN1",
                        originCentreId         = originCentreId,
                        originLocationId       = originLocationId,
                        destinationCentreId    = destinationCentreId,
                        destinationLocationId  = destinationLocationId,
                        timePacked             = None,
                        timeSent               = None,
                        timeReceived           = None,
                        timeUnpacked           = None,
                        timeCompleted          = None),
        CreatedShipment(id                     = ShipmentId(s"test-shipment-created-2"),
                        version                = 0,
                        timeAdded              = Global.StartOfTime,
                        timeModified           = None,
                        courierName            = "UPS",
                        trackingNumber         = "TN2",
                        originCentreId         = originCentreId,
                        originLocationId       = originLocationId,
                        destinationCentreId    = destinationCentreId,
                        destinationLocationId  = destinationLocationId,
                        timePacked             = None,
                        timeSent               = None,
                        timeReceived           = None,
                        timeUnpacked           = None,
                        timeCompleted          = None),
        PackedShipment(id                      = ShipmentId(s"test-shipment-packed"),
                       version                 = 0,
                       timeAdded               = Global.StartOfTime,
                       timeModified            = None,
                       courierName             = "FedEx",
                       trackingNumber          = "TN3",
                       originCentreId          = originCentreId,
                       originLocationId        = originLocationId,
                       destinationCentreId     = destinationCentreId,
                       destinationLocationId   = destinationLocationId,
                       timePacked              = Some(Global.StartOfTime),
                       timeSent                = None,
                       timeReceived            = None,
                       timeUnpacked            = None,
                       timeCompleted           = None),
        PackedShipment(id                      = ShipmentId(s"test-shipment-packed-2"),
                       version                 = 0,
                       timeAdded               = Global.StartOfTime,
                       timeModified            = None,
                       courierName             = "UPS",
                       trackingNumber          = "TN4",
                       originCentreId          = originCentreId,
                       originLocationId        = originLocationId,
                       destinationCentreId     = destinationCentreId,
                       destinationLocationId   = destinationLocationId,
                       timePacked              = Some(Global.StartOfTime),
                       timeSent                = None,
                       timeReceived            = None,
                       timeUnpacked            = None,
                       timeCompleted           = None),
        UnpackedShipment(id                    = ShipmentId(s"test-shipment-unpacked"),
                         version               = 0,
                         timeAdded             = Global.StartOfTime,
                         timeModified          = None,
                         courierName           = "FedEx",
                         trackingNumber        = "TN5",
                         originCentreId        = originCentreId,
                         originLocationId      = originLocationId,
                         destinationCentreId   = destinationCentreId,
                         destinationLocationId = destinationLocationId,
                         timePacked            = Some(Global.StartOfTime),
                         timeSent              = Some(Global.StartOfTime),
                         timeReceived          = Some(Global.StartOfTime),
                         timeUnpacked          = Some(Global.StartOfTime),
                         timeCompleted         = None),
        UnpackedShipment(id                    = ShipmentId(s"test-shipment-unpacked-2"),
                         version               = 0,
                         timeAdded             = Global.StartOfTime,
                         timeModified          = None,
                         courierName           = "UPS",
                         trackingNumber        = "TN6",
                         originCentreId        = originCentreId,
                         originLocationId      = originLocationId,
                         destinationCentreId   = destinationCentreId,
                         destinationLocationId = destinationLocationId,
                         timePacked            = Some(Global.StartOfTime),
                         timeSent              = Some(Global.StartOfTime),
                         timeReceived          = Some(Global.StartOfTime),
                         timeUnpacked          = Some(Global.StartOfTime),
                         timeCompleted         = None)
      )
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def testShipmentDtos(): List[ShipmentDto] = {
    val centres = testCentres

    val result = for {
      origin              <- centres.find(_.name == centreCalgaryName)
      originLocation      <- origin.locations.headOption
      destination         <- centres.find(_.name == centreLondonName)
      destinationLocation <- destination.locations.headOption
    } yield {
      val oInfo = CentreLocationInfo(origin, originLocation)
      val dInfo = CentreLocationInfo(destination, destinationLocation)
      testShipments.map { shipment =>
        ShipmentDto(shipment, oInfo, dInfo, 0, 0, 0)
      }
    }

    result match {
      case None       => throw new Error("cannot create shipment DTOs")
      case Some(dtos) => dtos
    }
  }

  def testShipmentSpecimenDtos(): List[ShipmentSpecimenDto] = {
    ???
  }

  def testShipmentSpecimens(): List[ShipmentSpecimen] =
    if (!loadShipmentTestData) {
      List.empty[ShipmentSpecimen]
    } else {
      log.debug("testShipmentSpecimens")

      /*
       * - takes all the specimens at the first centre, splits them in two and assigns the first half to the
       *   shipment in CREATED state, and the second half to the shipment in UNPACKED state
       *
       * - takes all the specimens at the second centre and assigns them to the shipment in PACKED state
       */
      val originCentreId    = centreCalgaryId
      val originLocationId  = LocationId(s"${originCentreId.id}:Primary")
      val specimens         = testSpecimens
      val halfSpecimenCount = specimens.filter(_.locationId == originLocationId).size / 2

      specimens.zipWithIndex.map {
        case (specimen, index) =>
          val shipmentId =
            if (specimen.locationId == originLocationId) {
              if (index < halfSpecimenCount) ShipmentId("test-shipment-created")
              else ShipmentId("test-shipment-unpacked")
            } else {
              ShipmentId("test-shipment-packed")
            }

          ShipmentSpecimen(id                  = ShipmentSpecimenId(specimen.id.id),
                           version             = 0L,
                           timeAdded           = Global.StartOfTime,
                           timeModified        = None,
                           shipmentId          = shipmentId,
                           specimenId          = specimen.id,
                           state               = ShipmentSpecimen.presentState,
                           shipmentContainerId = None)
      }
    }

  private def createParticipants(): List[ParticipantData] =
    if (!loadSpecimenTestData) {
      List.empty[ParticipantData]
    } else {
      log.debug("createParticipants")

      (0 to BbpspTestData.NumParticipants).map { index =>
        val id       = ParticipantId(BbpspTestData.ParticipantIdPrefix + index.toString)
        val uniqueId = f"P$index%05d"
        val participant = Participant(id = id,
                                      studyId      = BbpspTestData.BbpspStudyId,
                                      version      = 0L,
                                      timeAdded    = Global.StartOfTime,
                                      timeModified = None,
                                      slug         = Slug(uniqueId),
                                      uniqueId     = uniqueId,
                                      annotations  = Set.empty[Annotation])
        ParticipantData(participant, createCeventData(participant))
      }.toList
    }

  private def createCeventData(participant: Participant): List[CollectionEventData] = {
    log.debug(s"createCevents")

    BbpspTestData.EventTypeNames.zipWithIndex.map {
      case (eventTypeName, eventTypeIndex) =>
        val eventTypeId = CollectionEventTypeId(EventTypeIdPrefix + eventTypeIndex.toString)
        val id          = CollectionEventId(BbpspTestData.EventIdPrefix + participant.id.id + eventTypeId.id)

        val event = CollectionEvent(id = id,
                                    participantId         = participant.id,
                                    collectionEventTypeId = eventTypeId,
                                    version               = 0L,
                                    timeAdded             = Global.StartOfTime,
                                    timeModified          = None,
                                    slug                  = Slug(id.id),
                                    timeCompleted         = OffsetDateTime.now.minusDays(1),
                                    visitNumber           = eventTypeIndex + 1,
                                    annotations           = Set.empty[Annotation])

        CollectionEventData(event, createSpecimens(event))
    }
  }

  private var specimenIndex = 0L

  /*
   * Creates two specimens per collection event specimen description, one from each centre associated with
   * BBPSP.
   */
  private def createSpecimens(event: CollectionEvent): List[Specimen] =
    if (!loadSpecimenTestData) {
      List.empty[Specimen]
    } else {
      BbpspTestData.CentreIds.zipWithIndex.flatMap {
        case (centreId, centreIndex) =>
          val locationId = LocationId(s"${centreId.id}:Primary")

          BbpspTestData.CollectedSpecimenDefinitions.map { specimenDefinition =>
            val spcdefId    = event.collectionEventTypeId.id + "_" + specimenDefinition.id.id
            val uniqueId    = 1000L * centreIndex + specimenIndex
            val inventoryId = f"A$uniqueId%05d"
            val id          = SpecimenId(SpecimenIdPrefix + uniqueId.toString)

            specimenIndex = specimenIndex + 1L
            UsableSpecimen(id                   = id,
                           version              = 0L,
                           timeAdded            = Global.StartOfTime,
                           timeModified         = None,
                           slug                 = Slug(inventoryId),
                           inventoryId          = inventoryId,
                           specimenDefinitionId = SpecimenDefinitionId(spcdefId),
                           originLocationId     = locationId,
                           locationId           = locationId,
                           containerId          = None,
                           schemaLabel          = None,
                           timeCreated          = OffsetDateTime.now.minusDays(1),
                           amount               = BigDecimal(0.1))
          }
      }
    }

  /**
   * This is only to demo the User Access / Permissions. It should be removed for production servers.
   */
  def accessUsers(): List[User] =
    if (loadAccessTestData) {
      accessUserData.map {
        case (id, name) =>
          ActiveUser(id           = UserId(id),
                     version      = 0L,
                     timeAdded    = Global.StartOfTime,
                     timeModified = None,
                     slug         = Slug(name),
                     name         = name,
                     email        = s"$id@admin.com",
                     password     = "$2a$10$Kvl/h8KVhreNDiiOd0XiB.0nut7rysaLcKpbalteFuDN8uIwaojCa",
                     salt         = "$2a$10$Kvl/h8KVhreNDiiOd0XiB.",
                     avatarUrl    = None)
      }
    } else {
      List.empty[User]
    }

  def testRoles(): List[Tuple2[UserId, RoleId]] =
    if (loadAccessTestData) {
      List((UserId("study-administrator"), RoleId.StudyAdministrator),
           (UserId("study-user"), RoleId.StudyUser),
           (UserId("specimen-collector"), RoleId.SpecimenCollector),
           (UserId("shipping-admin"), RoleId.ShippingAdministrator),
           (UserId("shipping-user"), RoleId.ShippingUser))
    } else {
      List.empty[Tuple2[UserId, RoleId]]
    }

  def testMemberships(): List[Membership] =
    if (!loadAccessTestData) {
      List.empty[Membership]
    } else {
      val studyUserIds = Set("study-administrator", "study-user", "specimen-collector").map(UserId(_))

      val centreUserIds = Set("shipping-admin", "shipping-user").map(UserId(_))

      List(
        Membership(id           = MembershipId("all-studies-membership"),
                   version      = 0L,
                   timeAdded    = Global.StartOfTime,
                   timeModified = None,
                   slug         = Slug(""),
                   name         = "All studies",
                   description  = None,
                   userIds      = studyUserIds,
                   studyData    = MembershipEntitySet(true, Set.empty[StudyId]),
                   centreData   = MembershipEntitySet(false, Set.empty[CentreId])),
        Membership(id           = MembershipId("all-centres-membership"),
                   version      = 0L,
                   timeAdded    = Global.StartOfTime,
                   timeModified = None,
                   slug         = Slug(""),
                   name         = "All centres",
                   description  = None,
                   userIds      = centreUserIds,
                   studyData    = MembershipEntitySet(false, Set.empty[StudyId]),
                   centreData   = MembershipEntitySet(true, Set.empty[CentreId]))
      ).map(m => m.copy(slug = Slug(m.name)))
    }

  def testContainerSchemas(): List[ContainerSchema] =
    if (loadTestData) {
      CbsrTestData.schemaData.map { name =>
        val labels = name match {
          case "96 Well Microplate"  => microplateLabels96Wells
          case "384 Well Microplate" => microplateLabels384Wells
          case _                     => Set.empty[String]
        }
        addContainerSchema(name, Some(name), labels)
      }
    } else {
      List.empty[ContainerSchema]
    }

  def testContainerTypes(): List[ContainerType] =
    ???

  def testContainers(): List[Container] =
    ???

  private def addContainerSchema(
      name:        String,
      description: Option[String],
      labels:      Set[String]
    ): ContainerSchema = {
    val slug = Slug(name)
    ContainerSchema(id           = ContainerSchemaId(slug.id),
                    version      = 0L,
                    timeAdded    = Global.StartOfTime,
                    timeModified = None,
                    slug         = slug,
                    name         = name,
                    description  = description,
                    shared       = true,
                    centreId     = centreCBSRId,
                    labels       = labels)
  }

  private def microplateLabels96Wells(): Set[String] =
    "ABCDEFGH".flatMap { row =>
      (1 to 12).map { col =>
        row.toString + col.toString
      }
    }.toSet

  private def microplateLabels384Wells(): Set[String] =
    "ABCDEFGHIJKLMNOP".flatMap { row =>
      (1 to 24).map { col =>
        row.toString + col.toString
      }
    }.toSet

  log.debug(s"""|TEST DATA:
                |  mode:                 ${env.mode}
                |  loadTestData:         $loadTestData,
                |  loadSpecimenTestData: $loadSpecimenTestData,
                |  loadShipmentTestData: $loadShipmentTestData,
                |  loadAccessTestData:   $loadAccessTestData""".stripMargin)

}
