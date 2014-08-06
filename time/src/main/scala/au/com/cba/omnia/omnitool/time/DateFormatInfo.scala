//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.omnitool.time

import java.util.Locale

import org.joda.time.{Chronology, DateTime, DateTimeField, DateTimeFieldType}
import org.joda.time.chrono.ISOChronology
import org.joda.time.format.{DateTimeFormatter, DateTimeParserBucket}

/** Query [[org.joda.time.format.DateTimeFormat]] for more info */
object DateFormatInfo {

  /**
    * Get the date time field types expected by the [[org.joda.time.format.DateTimeFormatter]]
    *
    * This method will not always succeed in finding the fields. It will fail if
    * the formatter cannot parse a date time string which it printed. As far as
    * I know, this will only happen with formatters which cannot always
    * unambiguusly parse valid timeStamps. We should avoid formatters like this.
    */
  def fields(format: DateTimeFormatter): Option[List[DateTimeFieldType]] = {
    // The DateTimeFormat does not reveal much info about the stucture of the pattern
    // but we can use our own custom DateTimeParserBucket to observe what happens when it parses a datetime

    val bucket  = new LoquaciousDateTimeParserBucket(format)
    val dateStr = format.print(new DateTime(0))
    val newPos  = format.getParser.parseInto(bucket, dateStr, 0)
    val fields  = bucket.fieldTypes

    if (newPos != dateStr.length) None
    else                          Some(fields)
  }
}

/**
  * Overrides DateTimeParserBucket, allowing us to see what fields are set by [[org.joda.time.format.DateTimeFormatter]]
  *
  * WARNING: mutable and non thread-safe. Also, this class does not reset the
  * list of field types after parsing, so the list of field types is good for
  * one parse only.
  */
class LoquaciousDateTimeParserBucket(format: DateTimeFormatter)
    extends DateTimeParserBucket(0, format.getChronology, format.getLocale,
      format.getPivotYear, format.getDefaultYear) {

  /** After the first parse, this variable will contain the field types used during parsing */
  var fieldTypes = List.empty[DateTimeFieldType]

  override def saveField(field: DateTimeField, value: Int): Unit = {
    fieldTypes = field.getType :: fieldTypes
    super.saveField(field, value)
  }

  override def saveField(fieldType: DateTimeFieldType, value: Int): Unit = {
    fieldTypes = fieldType +: fieldTypes
    super.saveField(fieldType, value)
  }

  override def saveField(fieldType: DateTimeFieldType, text: String, locale: Locale): Unit = {
    fieldTypes = fieldType +: fieldTypes
    super.saveField(fieldType, text, locale)
  }
}
