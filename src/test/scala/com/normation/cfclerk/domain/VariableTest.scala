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
import net.liftweb.common.Failure
import com.normation.exceptions.TechnicalException

@RunWith(classOf[JUnitRunner])
class VariableTest extends Specification {
  def variableSpecParser = new VariableSpecParser()

  val nbVariables = 18
  val refName = "name"
  val refDescription = "description"
  val refValue = "value"
  val dateValue = "2010-01-16T12:00:00.000+01:00"
  val listValue = "value1;value2"

  val refItem = Seq(ValueLabel("value", "label"), ValueLabel("value2", "label2"))

  val unvalidValue = "bobby"

  val simpleName = "$SIMPLEVARIABLE";
  val select1 = "SELECT1_TEST"
  val itemName = "$VARIABLEITEM";
  val regexCardNumberVar = "$CARDNUMBER"
  val sizeVar = "$SIZE"
  val mailVar = "$MAIL"
  val ipVar = "$IP"
  val varName = "$VARMACHINE"
  val varDate = "$VARDATE"
  val varList = "varlist"
  val gui_only = "gui_only"

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
      val spec = variableSpecParser.parseSectionVariableSpec(specNode).openOrThrowException("I'm a failing test!")
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
      variables must haveKeys(
          simpleName, select1, itemName,
          regexCardNumberVar, sizeVar, mailVar,
          ipVar, varName, varDate, varList, gui_only
      )
      variables must haveKeys( (1 to 6).map( "password" + _):_*)
    }
  }

  ///////////////// generic tests about variable construction, do not use files /////////////////

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

  ///////////////// test parsed variables /////////////////

  "Parsed variable" should {
    implicit val simpleVariable = variables(simpleName)
    beAnInput
    saveHaveValue()

    val constrainedVariable = variables(itemName)
    saveHaveValue()(constrainedVariable)
    beASelect(constrainedVariable)
  }

  "Date variable" should {
    implicit val dateVariable = variables(varDate)
    beAnInput
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
    beAnInput
    haveType("string")
    notBeMultivalued
  }


  "select 1" should {
    implicit val v = variables(select1)
    beASelect1
    haveName("SELECT1_TEST")

  }

  checkType("size-kb", sizeVar)
  checkType("mail", mailVar)
  checkType("ip", ipVar)

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

  "password1" should {
    implicit val v = variables("password1")
    beAPassword
    haveNoAlgo
  }

  "password2" should {
    implicit val v = variables("password2")
    beAPassword
    haveAlgo(PLAIN)
  }

  "password3" should {
    implicit val v = variables("password3")
    beAPassword
    haveAlgo(MD5)
  }

  "password4" should {
    implicit val v = variables("password4")
    beAPassword
    haveAlgo(MD5)
  }

  "password5" should {
    implicit val v = variables("password5")
    beAPassword
    haveAlgo(SHA1)
  }

  "password6" should {
    implicit val v = variables("password6")
    beAPassword
    haveAlgo(SHA256)
  }

  "password7" should {
    implicit val v = variables("password7")
    beAPassword
    haveNoAlgo
  }

  "unvalide password algo" should {
    val p =
      <PASSWORD>
        <NAME>pwd</NAME>
        <DESCRIPTION>Some password</DESCRIPTION>
        <HASH>NotAnAlgo</HASH>
      </PASSWORD>

    "throw a parsing error" in {
       variableSpecParser.parseSectionVariableSpec(p) must throwA[TechnicalException]
    }
  }


  ///
  /// Utility methods
  ///

  private[this] def checkType(typeName: String, varName: String) = {
    "An input with a type '%s'".format(typeName) should {
      val input = variables(varName)
      "have a constraint with a type '%s'".format(typeName) in {
        input.spec.constraint.typeName mustEqual typeName
      }
    }
  }

  private[this] def beAnInput(implicit variable: Variable) = {
    "Be an input variable" in {
      variable.spec.isInstanceOf[InputVariableSpec]
    }
  }

  private[this] def beASelect1(implicit variable: Variable) = {
    "Be an input select 1" in {
      variable.spec.isInstanceOf[SelectOneVariableSpec]
    }
  }

  private[this] def beASelect(implicit variable: Variable) = {
    "Be an input select" in {
      variable.spec.isInstanceOf[SelectVariableSpec]
    }
  }

  private[this] def beAPassword(implicit variable: Variable) = {
    "Be an input password input" in {
      variable.spec.isInstanceOf[PasswordVariableSpec]
    }
  }

  private[this] def haveDescription(description: String = refDescription)(implicit variable: Variable) = {
    "have description '%s'".format(description) in {
      variable.spec.description mustEqual description
    }
  }

  private[this] def haveName(name: String = refName)(implicit variable: Variable) = {
    "have name '%s'".format(name) in {
      variable.spec.name mustEqual name
    }
  }

  private[this] def haveValue(value: Any = refValue)(implicit variable: Variable) = {
    "have value '%s'".format(value) in {
      variable.getTypedValues.get.head mustEqual value
    }
  }

  private[this] def haveNotValue(value: Any = refValue)(implicit variable: Variable) = {
    "have not value '%s'".format(value) in {
      variable.getTypedValues.get.head mustNotEqual value
    }
  }

  private[this] def saveHaveValue(value: String = refValue)(implicit variable: Variable) = {
    variable.saveValue(value)
    haveValue(value)
  }

  private[this] def haveNbValues(nbValues: Int)(implicit variable: Variable) = {
    "have %d values".format(nbValues) in {
      variable.values.length mustEqual nbValues
    }
  }

  private[this] def haveType(typeName: String)(implicit variable: Variable) = {
    "have type " + typeName in {
      variable.spec.constraint.typeName mustEqual typeName
    }
  }

  private[this] def notBeMultivalued(implicit variable: Variable) = {
    "not be multivalued (because it is not a valid tag of variable spec)" in {
      !variable.spec.multivalued
    }
  }

  private[this] def beSystemVar(implicit variable: Variable) = {
    "be systemVar" in {
      variable.spec.isSystem
    }
  }

  private[this] def beBounding(implicit policyVar: TrackerVariable) = {
    "be a bounding variable" in {
      policyVar.spec.boundingVariable mustEqual "Bounding"
    }
  }

  private[this] def haveNoAlgo(implicit variable: Variable) = {
    s"Have an user defined hash algorithme (and so none constrained)" in {
      variable match {
        case p:PasswordVariable => p.spec.hashAlgo must beEqualTo(None)
        case _ => failure("Variable is not a password input")
      }
    }
  }

  private[this] def haveAlgo(algo:HashAlgoConstraint)(implicit variable: Variable) = {
    s"Have hash algorithme of type ${algo.prefix}" in {
      variable match {
        case p:PasswordVariable => p.spec.hashAlgo must beEqualTo(Some(algo))
        case _ => failure("Variable is not a password input")
      }
    }
  }
}
