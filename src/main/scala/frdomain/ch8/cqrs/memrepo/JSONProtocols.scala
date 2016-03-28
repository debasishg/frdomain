package frdomain.ch8
package cqrs
package memrepo

import spray.json._
import DefaultJsonProtocol._ 

import org.joda.time.DateTime
import org.joda.time.format.{ISODateTimeFormat, DateTimeFormatter}

object JSONProtocols {

  implicit val balanceFormat = jsonFormat1(Balance)

  implicit object DateTimeFormat extends RootJsonFormat[DateTime] {

    val formatter = ISODateTimeFormat.basicDateTimeNoMillis

    def write(obj: DateTime): JsValue = {
      JsString(formatter.print(obj))
    }

    def read(json: JsValue): DateTime = json match {
      case JsString(s) => try {
        formatter.parseDateTime(s)
      }
      catch {
        case t: Throwable => error(s)
      }
      case _ =>
        error(json.toString())
    }

    def error(v: Any): DateTime = {
      val example = formatter.print(0)
      deserializationError(f"'$v' is not a valid date value. Dates must be in compact ISO-8601 format, e.g. '$example'")
    }
  }

  implicit val accountFormat = jsonFormat5(Account.apply)

  implicit object OpenedFormat extends RootJsonFormat[Opened] {
    def write(o: Opened) = JsObject(
      "no" -> JsString(o.no),
      "name" -> JsString(o.name),
      "openingDate" -> o.openingDate.map(d => d.toJson).getOrElse(JsNull),
      "at" -> o.at.toJson,
      "type" -> JsString("Opened")
    )

    def read(j: JsValue) = j.asJsObject.getFields("no", "name", "openingDate", "at") match {
      case Seq(JsString(n), JsString(m), od, a) => 
        Opened(n, m, if (od == JsNull) None else Some(od.convertTo[DateTime]), a.convertTo[DateTime])
      case _ => deserializationError(f"'$j' is not a valid Opened value")
    }
  }

  implicit object ClosedFormat extends RootJsonFormat[Closed] {
    def write(o: Closed) = JsObject(
      "no" -> JsString(o.no),
      "closeDate" -> o.closeDate.map(d => d.toJson).getOrElse(JsNull),
      "at" -> o.at.toJson,
      "type" -> JsString("Closed")
    )

    def read(j: JsValue) = j.asJsObject.getFields("no", "closeDate", "at") match {
      case Seq(JsString(n), od, a) => 
        Closed(n, if (od == JsNull) None else Some(od.convertTo[DateTime]), a.convertTo[DateTime])
      case _ => deserializationError(f"'$j' is not a valid Closed value")
    }
  }

  implicit object DebitedFormat extends RootJsonFormat[Debited] {
    def write(o: Debited) = JsObject(
      "no" -> JsString(o.no),
      "amount" -> JsNumber(o.amount),
      "at" -> o.at.toJson,
      "type" -> JsString("Debited")
    )

    def read(j: JsValue) = j.asJsObject.getFields("no", "amount", "at") match {
      case Seq(JsString(n), JsNumber(m), a) => 
        Debited(n, m, a.convertTo[DateTime])
      case _ => deserializationError(f"'$j' is not a valid Debited value")
    }
  }

  implicit object CreditedFormat extends RootJsonFormat[Credited] {
    def write(o: Credited) = JsObject(
      "no" -> JsString(o.no),
      "amount" -> JsNumber(o.amount),
      "at" -> o.at.toJson,
      "type" -> JsString("Credited")
    )

    def read(j: JsValue) = j.asJsObject.getFields("no", "amount", "at") match {
      case Seq(JsString(n), JsNumber(m), a) => 
        Credited(n, m, a.convertTo[DateTime])
      case _ => deserializationError(f"'$j' is not a valid Credited value")
    }
  }

  implicit object EventFormat extends RootJsonFormat[Event[_]] {
    def write(e: Event[_]) = e match {
      case o @ Opened(_, _, _, _)   => OpenedFormat.write(o)
      case c @ Closed(n, d, _)      => ClosedFormat.write(c)
      case d @ Debited(_, _, _)     => DebitedFormat.write(d)
      case c @ Credited(_, _, _)    => CreditedFormat.write(c)
    }

    def read(j: JsValue) = j.asJsObject.getFields("type") match {
      case Seq(JsString(c)) => c match {
        case "Opened"   => OpenedFormat.read(j) 
        case "Closed"   => ClosedFormat.read(j) 
        case "Debited"  => DebitedFormat.read(j) 
        case "Credited" => CreditedFormat.read(j) 
      }
      case _ => deserializationError(f"'$j' is not a valid Event value")
    }
  }
}
