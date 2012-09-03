/*
*************************************************************************************
* Copyright 2011 Normation SAS
*************************************************************************************
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU Affero GPL v3, the copyright holders add the following
* Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU Affero GPL v3
* licence, when you create a Related Module, this Related Module is
* not considered as a part of the work and may be distributed under the
* license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>.
*
*************************************************************************************
*/

package com.normation.cfclerk.domain

import com.normation.utils.Utils
import org.joda.time.format.ISODateTimeFormat
import com.normation.utils.HashcodeCaching

class ConstraintException(val msg: String) extends Exception(msg)

trait Constraint {
  val typeName: String
  val default: Option[String]
  val mayBeEmpty: Boolean
  val regex: RegexConstraint

  def check(varValue: String, varName: String) : Unit
}

object Constraint {
  val sizeTypes = "size-b" :: "size-kb" :: "size-mb" :: "size-gb" :: "size-tb" :: Nil
  val regexTypes = "mail" :: "ip" :: Nil
  val stringTypes = "string" :: "perm" :: "textarea" :: Nil ::: regexTypes ::: sizeTypes
  val validTypes = "integer" :: "uploadedfile" :: "destinationfullpath" :: "date" :: "datetime" :: "time" ::
    "boolean" :: Nil ::: stringTypes

  def apply(typeName: String = "string", default: Option[String] = None, mayBeEmpty: Boolean = false, regex: RegexConstraint = RegexConstraint()): Constraint = {

    if (!validTypes.contains(typeName))
      throw new ConstraintException("'%s' is an invalid type.\n A type may be one of the next list : %s".format(
        typeName, validTypes.mkString(", ")))

    if (regexTypes.contains(typeName) && regex != RegexConstraint())
      throw new ConstraintException("type '%s' already has a predifined regex, you can't define a regex with these types : %s.".format(regexTypes.mkString(",")))

    typeName match {
      case "ip" => ConstraintImp(typeName, default, mayBeEmpty, IpRegex)
      case "mail" => ConstraintImp(typeName, default, mayBeEmpty, MailRegex)
      case _ => ConstraintImp(typeName, default, mayBeEmpty, regex)
    }
  }
}

case class ConstraintImp(typeName: String, default: Option[String], mayBeEmpty: Boolean, regex: RegexConstraint) extends Constraint with HashcodeCaching {

  def check(varValue: String, varName: String) = {
    //only check for non-empty variable
    if(varValue == null || varValue.length < 1) {
      if(mayBeEmpty) {
        //OK
      } else {
        throw new ConstraintException("'%s' field must not be empty".format(varName))
      }
    } else {
      checkType(varValue, varName)
      regex.check(varValue, varName)
    }
  }

  private[this] def checkType(varValue: String, fieldName: String): Any = {
    val msgErr = "Wrong value " + varValue + " for field '" + fieldName + "'"
    typeName match {
      case "datetime" =>
        try
          ISODateTimeFormat.dateTimeParser.parseDateTime(varValue)
        catch {
          case _:Exception =>
            throw new ConstraintException(msgErr + " : expecting a datetime")
        }
      case "integer" => try
        varValue.toInt
      catch {
        case _:Exception =>
          throw new ConstraintException(msgErr + " : expecting an integer")
      }
      case "boolean" => try
        varValue.toBoolean
      catch {
        case _:Exception =>
          throw new ConstraintException(msgErr + " : expecting a boolean")
      }
      case _ =>
    }
  }
}
