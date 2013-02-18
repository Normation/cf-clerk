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

import org.springframework.test.context.ContextConfiguration
import com.normation.cfclerk.xmlparsers._
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.context.junit4._
import org.junit.Test
import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.BlockJUnit4ClassRunner
import scala.collection.mutable._
import scala.xml._
import java.io.FileInputStream
import org.xml.sax.SAXParseException
import java.io.FileNotFoundException
import com.normation.cfclerk.exceptions._
import org.joda.time.DateTime
import org.joda.time.format._
import org.junit.runner._
import org.specs2.mutable._
import org.specs2.runner._
import net.liftweb.common._

@RunWith(classOf[JUnitRunner])
class VariableTest extends Specification {
  def variableSpecParser = new VariableSpecParser()

  val nbVariables = 11
  val refName = "name"
  val refDescription = "description"
  val refValue = "value"
  val dateValue = "2010-01-16T12:00:00.000+01:00"
  val listValue = "value1;value2"

  val rawValue = """This is a test \ " \\ """
  val escapedTo = """This is a test \\ \" \\\\ """

  val refItem = Seq(ValueLabel("value", "label"), ValueLabel("value2", "label2"))

  val unvalidValue = "bobby"

  val simpleName = "$SIMPLEVARIABLE";
  val itemName = "$VARIABLEITEM";
  val varName = "$VARMACHINE"
  val varDate = "$VARDATE"

  val regexCardNumberVar = "$CARDNUMBER"
  val sizeVar = "$SIZE"
  val mailVar = "$MAIL"
  val ipVar = "$IP"
  val varList = "varlist"
  val rawVar = "raw_type"

  val variables = {
    val doc =
      try {
        XML.load(ClassLoader.getSystemResourceAsStream("testVariable.xml"))
      } catch {
        case e: SAXParseException => throw new Exception("Unexpected issue (unvalid xml?) with testVariable.xml ")
        case e: java.net.MalformedURLException => throw new FileNotFoundException("testVariable.xml file not found ")
      }
    if (doc.isEmpty) {
      throw new Exception("Unexpected issue (unvalido xml?) with the testvariable file ")
    }

    var variables = Map[String, Variable]()
    for {
      elt <- (doc \\ "VARIABLES")
      specNode <- elt.nonEmptyChildren
      if(!specNode.isInstanceOf[Text])
    } {
      val spec = variableSpecParser.parseSectionVariableSpec(specNode).open_!
      variables += spec.name -> spec.toVariable()
    }
    variables
  }

  "SYSTEM_VARIABLE tag" should {
    "lead to an exception" in {
      val sysvar = (for {
        elt <- (XML.load(ClassLoader.getSystemResourceAsStream("testSystemVariable.xml")) \\ "VARIABLES")
        specNode <- elt.nonEmptyChildren
        if(!specNode.isInstanceOf[Text])
      } yield {
        variableSpecParser.parseSectionVariableSpec(specNode)
      })
      (sysvar.size === 1) and (sysvar.head.isEmpty === true)
    }
  }

  "variables map" should {
    "be so that" in {
      variables.size mustEqual nbVariables
      variables must haveKey(itemName)
      variables must haveKey(simpleName)
      variables must haveKey(varName)
      variables must haveKey(varDate)
      variables must haveKey(regexCardNumberVar)
      variables must haveKey(sizeVar)
      variables must haveKey(mailVar)
      variables must haveKey(ipVar)
      variables must haveKey(rawVar)
    }
  }

  "Unsetted Variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription))
    haveName()
    haveDescription()
  }

  "Variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription))
    variable.saveValue(refValue)

    haveName()
    haveDescription()
    haveValue()
  }

  "Multivalued variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription,
      multivalued = true))
    variable.values = listValue.split(";")

    haveName()
    haveDescription()
    haveNbValues(2)
  }

  "Select variable" should {
    implicit val variable = new SelectVariable(SelectVariableSpec(refName, refDescription,
      valueslabels = refItem))
    variable.saveValue(refValue)

    haveName()
    haveDescription()
    haveValue()
  }

  "Input variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription))
    variable.saveValue(refValue)

    haveName()
    haveDescription()
    haveValue()
  }

  "Nulled variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription))
    variable.saveValue(null)

    haveName()
    haveDescription()
    haveNbValues(0)
  }

  "Valid variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription))
    variable.saveValue(refValue)

    haveName()
    haveDescription()
    haveValue()
  }

  "Boolean variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription,
      constraint = Constraint("boolean")))

    variable.saveValue("true")
    haveName()
    haveDescription()
    haveValue(true)
  }

  "Boolean variable" should {
    implicit val variable = new InputVariable(InputVariableSpec(refName, refDescription,
      constraint = Constraint("boolean")))

    variable.saveValue("false")
    haveValue(false)
  }

  "Invalid variable" should {
    implicit val variable = new SelectVariable(SelectVariableSpec(refName, refDescription,
      valueslabels = refItem))

    "throw a VariableException" in {
      variable.saveValue(unvalidValue) must throwA[VariableException]
    }

    haveName()
    haveDescription()
  }

  "Parsed variable" should {
    implicit val simpleVariable = variables(simpleName)
    saveHaveValue()

    val constrainedVariable = variables(itemName)
    saveHaveValue()(constrainedVariable)
  }

  "Date variable" should {
    implicit val dateVariable = variables(varDate)
    haveType("datetime")

    dateVariable.saveValue(dateValue)
    haveValue(ISODateTimeFormat.dateTimeParser.parseDateTime(dateValue))
  }

  "Parsed variable having unvalid value" should {
    implicit val constrainedVariable = variables(itemName)

    "throw a VariableException" in {
      constrainedVariable.saveValue(unvalidValue) must throwA[VariableException]
    }

    haveValue()
  }

  "varlist" should {
    implicit val listVariable = variables(varList)
    haveType("string")
    notBeMultivalued
  }

  "Raw variable" should {
    implicit val rawVariable = variables(rawVar)
    haveType("raw")
    rawVariable.saveValue(rawValue)
    rawVariable.getTypedValues.openOrThrowException("Invalid content for the raw variable") must containTheSameElementsAs(Seq(rawValue))
  }

  "Simple variable " should {
    implicit val simpleVariable = variables(simpleName)
    haveType("string")
    simpleVariable.saveValue(rawValue)
    simpleVariable.getTypedValues.openOrThrowException("Invalid content for the escaped variable") must containTheSameElementsAs(Seq(escapedTo))
  }

  checkType("size-kb", sizeVar)
  checkType("mail", mailVar)
  checkType("ip", ipVar)

  def checkType(typeName: String, varName: String) = {
    "An input with a type '%s'".format(typeName) should {
      val input = variables(varName)
      "have a constraint with a type '%s'".format(typeName) in {
        input.spec.constraint.typeName mustEqual typeName
      }
    }
  }

  "An input with a 'regex'" should {
    val input = variables(regexCardNumberVar)

    "have its regex field correctly set" in {
      input.spec.constraint.regex mustEqual RegexConstraint("""\d\d\d\d-\d\d\d\d-\d\d\d\d-\d\d\d\d""", "must resemble 1234-1234-1234-1234")
    }
  }

  def testSpecVarFields(spec: VariableSpec, longDescription: String = "",
    defaultValue: Option[String] = None) = {
    spec.longDescription === longDescription &&
      spec.constraint.default === defaultValue
  }

  "Variables which may be empty" should {
    // varlist can be empty, the others can't
    val listVariable = variables(varList)
    "have mayBeEmpty set to true" in {
      listVariable.spec.constraint.mayBeEmpty
    }
  }

  "Variables which may not be empty" should {
    "have mayBeEmpty set to false" in {
      val vars = variables(itemName) :: variables(simpleName) :: variables(varName) :: variables(varDate) :: Nil
      vars forall (!_.spec.constraint.mayBeEmpty) must beTrue
    }
  }

  def haveDescription(description: String = refDescription)(implicit variable: Variable) = {
    "have description '%s'".format(description) in {
      variable.spec.description mustEqual description
    }
  }

  def haveName(name: String = refName)(implicit variable: Variable) = {
    "have name '%s'".format(name) in {
      variable.spec.name mustEqual name
    }
  }

  def haveValue(value: Any = refValue)(implicit variable: Variable) = {
    "have value '%s'".format(value) in {
      variable.getTypedValues.get.head mustEqual value
    }
  }

  def haveNotValue(value: Any = refValue)(implicit variable: Variable) = {
    "have not value '%s'".format(value) in {
      variable.getTypedValues.get.head mustNotEqual value
    }
  }

  def saveHaveValue(value: String = refValue)(implicit variable: Variable) = {
    variable.saveValue(value)
    haveValue(value)
  }

  def haveNbValues(nbValues: Int)(implicit variable: Variable) = {
    "have %d values".format(nbValues) in {
      variable.values.length mustEqual nbValues
    }
  }

  def haveType(typeName: String)(implicit variable: Variable) = {
    "have type " + typeName in {
      variable.spec.constraint.typeName mustEqual typeName
    }
  }

  def notBeMultivalued(implicit variable: Variable) = {
    "not be multivalued (because it is not a valid tag of variable spec)" in {
      !variable.spec.multivalued
    }
  }

  def beSystemVar(implicit variable: Variable) = {
    "be systemVar" in {
      variable.spec.isSystem
    }
  }

  def beBounding(implicit policyVar: TrackerVariable) = {
    "be a bounding variable" in {
      policyVar.spec.boundingVariable mustEqual "Bounding"
    }
  }
}
