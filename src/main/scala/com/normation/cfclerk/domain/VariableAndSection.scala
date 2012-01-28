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

import scala.collection._
import com.normation.cfclerk.exceptions._
import scala.xml._
import org.joda.time._
import org.joda.time.format._
import com.normation.utils.XmlUtils._
import net.liftweb.common._
import mutable.Buffer
import com.normation.utils.Control.bestEffort
import com.normation.utils.HashcodeCaching

/* A SectionChild is either a Variable or a Section*/
sealed trait SectionChild

case class Section(val spec: SectionSpec) extends SectionChild with HashcodeCaching

/**
 *
 * Variable class, to describe what must be replaced in the template files
 * @author nicolas
 *
 */
trait Variable extends Loggable {

  //define in sub classes
  type T <: VariableSpec

  val spec: T

  override def clone = Variable.matchCopy(this)

  // selected values
  protected val defaultValues: Seq[String] // override in subclasses
  protected val internalValues: Buffer[String] = defaultValues.toBuffer // this is the internal representation of the data 

  override def toString() = "%s %s : %s".format(spec.name, spec.description, internalValues)

  /**
   * *********************************
   * new variable part
   */

  def values: Seq[String] = internalValues.toSeq
  def values_=(x: Seq[String]) = saveValues(x)


  def getTypedValues(): Box[Seq[Any]] = {
    bestEffort(internalValues) { x =>
      castValue(x)
    }
  }

  // the comments below contains implementations and should be reused in SelectVariable and SelectOne variable

  /**
   * Only deals with the first entry
   */
  def saveValue(s: String): Unit = {
    spec match {
      case vl: ValueLabelVariableSpec =>
          if (!(vl.valueslabels.map(x => x.value).contains(s)))
            throw new VariableException("Wrong value for variable " + vl.name + "  : " + s)
      case _ => //OK
    }
    Variable.setUniqueValue(this, s)
  }

  /**
   * Save the whole seq as value
   */
  def saveValues(seq: Seq[String]): Unit = {
    spec match {
      case vl: ValueLabelVariableSpec =>
        if ((null != vl.valueslabels) && (vl.valueslabels.size > 0)) {
          for (item <- seq)
            if (!(vl.valueslabels.map(x => x.value).contains(item)))
              throw new VariableException("Wrong value for variable " + vl.name + "  : " + item)
        }
      case _ =>
    }
    Variable.setValues(this, seq)
  }

  /**
   * Append the seq to the values
   */
  def appendValues(seq: Seq[String]): Unit = {
    spec match {
      case vl: ValueLabelVariableSpec =>
        if ((null != vl.valueslabels) && (vl.valueslabels.size > 0)) {
          for (item <- seq)
            if (!(vl.valueslabels.map(x => x.value).contains(item)))
              throw new VariableException("Wrong value for variable " + vl.name + "  : " + item)
        }
      case _ =>
    }
    Variable.appendValues(this, seq)
  }

  def getValuesLength() = {
    internalValues.size
  }
  
  protected def castValue(x: String) : Box[Any] = {
    val typeName = spec.constraint.typeName.toLowerCase
    
    //we don't want to check constraint on empty value
    // when the variable is optionnal
    if(this.spec.constraint.mayBeEmpty && x.length < 1) Full("")
    else if(Constraint.stringTypes.contains(typeName)) Full(x)
    else typeName match {
      case "datetime" =>
        try
          Full(ISODateTimeFormat.dateTimeParser.parseDateTime(x))
        catch {
          case e:Exception => Failure("Wrong variable value " + x + " for variable name " + spec.name + " : expecting a datetime")
        }
      case "integer" => try
        Full(x.toInt)
      catch {
        case e:Exception => Failure("Wrong variable value " + x + " for variable name " + spec.name + " : expecting an integer")
      }
      case "boolean" => try
        Full(x.toBoolean)
      catch {
        case e:Exception => Failure("Wrong variable value " + x + " for variable name " + spec.name + " : expecting a boolean")
      }
      case _ =>
        logger.error("Wrong variable type %s for variable name %s".format(typeName, spec.name))
        Failure("Wrong variable type " + typeName + " for variable name " + spec.name)
    }
  }
}

case class SystemVariable(
  override val spec: SystemVariableSpec,
  protected val defaultValues: Seq[String] = Seq()) extends Variable with HashcodeCaching {
  type T = SystemVariableSpec
}

case class TrackerVariable(
  override val spec: TrackerVariableSpec,
  protected val defaultValues: Seq[String] = Seq()) extends Variable with HashcodeCaching {
  type T = TrackerVariableSpec
}

trait SectionVariable extends Variable with SectionChild

case class InputVariable(
  override val spec: InputVariableSpec,
  protected val defaultValues: Seq[String] = Seq()) extends SectionVariable with HashcodeCaching {
  type T = InputVariableSpec
}

case class SelectVariable(
  override val spec: SelectVariableSpec,
  protected val defaultValues: Seq[String] = Seq()) extends SectionVariable with HashcodeCaching {
  type T = SelectVariableSpec
}

case class SelectOneVariable(
  override val spec: SelectOneVariableSpec,
  protected val defaultValues: Seq[String] = Seq()) extends SectionVariable with HashcodeCaching {
  type T = SelectOneVariableSpec
}


object Variable {
  // define our own alternatives of matchCopy because we want v.values to be the default 
  // values
  def matchCopy(v: Variable): Variable = matchCopy(v, false)
  def matchCopy(v: Variable, setMultivalued: Boolean): Variable = matchCopy(v, v.values, setMultivalued)

  def matchCopy(v: Variable, values: Seq[String], setMultivalued: Boolean = false): Variable = {

    val bVals = values.toBuffer
    v match {
      case iv: InputVariable =>
        val newSpec = if (setMultivalued) iv.spec.cloneSetMultivalued else iv.spec
        iv.copy(defaultValues = bVals, spec = newSpec)
      case sv: SelectVariable =>
        val newSpec = if (setMultivalued) sv.spec.cloneSetMultivalued else sv.spec
        sv.copy(defaultValues = bVals, spec = newSpec)
      case s1v: SelectOneVariable =>
        val newSpec = if (setMultivalued) s1v.spec.cloneSetMultivalued else s1v.spec
        s1v.copy(defaultValues = bVals, spec = newSpec)
      case systemV: SystemVariable =>
        val newSpec = if (setMultivalued) systemV.spec.cloneSetMultivalued else systemV.spec
        systemV.copy(defaultValues = bVals, spec = newSpec)
      case policyInstance: TrackerVariable =>
        val newSpec = if (setMultivalued) policyInstance.spec.cloneSetMultivalued else policyInstance.spec
        policyInstance.copy(defaultValues = bVals, spec = newSpec)
    }
  }

  def variableParsing(variable: Variable, elt: Node): Unit = {
    variable.internalValues ++= valuesParsing((elt \ "internalValues"))
  }

  private def valuesParsing(elt: NodeSeq): Seq[String] = {
    val returnedValue = mutable.Buffer[String]()
    for (value <- elt \ "value") {
      returnedValue += value.text
    }
    returnedValue
  }

  /**
   * Set the first value
   */
  def setUniqueValue(variable: Variable, value: String): Unit = {
    if (value != null) {

      if (!variable.spec.checked) {
        variable.internalValues(0) = value
      } else if (checkValue(variable, value)) {
        if (variable.internalValues.size > 0)
          variable.internalValues(0) = value
        else
          variable.internalValues += value
      }
    }
  }

  /**
   * Replace all values with the ones in argument
   */
  def setValues(variable: Variable, values: Seq[String]): Unit = {
    if (values != null) {
      if (!variable.spec.checked) {
        variable.internalValues.clear
        variable.internalValues ++= values
      } else if (!variable.spec.multivalued && values.size > 1) {
        throw new VariableException("Wrong variable length for " + variable.spec.name)
      } else if (values.map(x => checkValue(variable, x)).contains(false)) {
        throw new VariableException("Wrong variable value for " + variable.spec.name) // this should really not be thrown
      } else {
        variable.internalValues.clear
        variable.internalValues ++= values
      }
    }
  }

  /**
   * Append values in argument to the value list
   */
  def appendValues(variable: Variable, values: Seq[String]): Unit = {
    if (values != null) {
      if (!variable.spec.checked) {
        variable.internalValues ++= values
      } else if (!variable.spec.multivalued && (values.size + variable.internalValues.size) > 1) {
        throw new VariableException("Wrong variable length for " + variable.spec.name)
      } else if (values.map(x => checkValue(variable, x)).contains(false)) {
        throw new VariableException("Wrong variable value for " + variable.spec.name) // this should really not be thrown
      } else {
        variable.internalValues ++= values
      }
    }
  }

  /**
   * Check the value we intend to put in the variable
   */
  def checkValue(variable: Variable, value: String): Boolean = {
    variable.castValue(value).isDefined
  }
}

