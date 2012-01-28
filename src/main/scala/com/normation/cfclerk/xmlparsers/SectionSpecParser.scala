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

package com.normation.cfclerk.xmlparsers

import com.normation.cfclerk.domain._
import scala.xml._
import net.liftweb.common._
import com.normation.utils.XmlUtils._
import com.normation.cfclerk.exceptions._
import CfclerkXmlConstants._
import com.normation.utils.Control.{sequence,bestEffort}

class SectionSpecParser(variableParser:VariableSpecParser) extends Loggable {

  
  def parseSectionsInPolicy(policy: Node, id: PolicyPackageId, policyName: String):SectionSpec = {
    val sections = policy \\ SECTIONS_ROOT
    if (sections.size > 1) {
      val err = "In %s -> %s : Only one <sections> marker is allowed in the entire file".format(id, policyName)
      logger.error(err)
      throw new ParsingException(err)
    }

    if (sections.isEmpty)
      SectionSpec(SECTION_ROOT_NAME)
    else {
      val root = SectionSpec(SECTION_ROOT_NAME, children = parseChildren(sections.head, id, policyName))
      
      /*
       * check that all section names and all variable names are unique
       */
      val variableNames = root.getAllVariables.map( _.name )
      
      /*
       * check that all variable and seciont names are unique 
       */
      checkUniqueness(variableNames) {
        "At least two variables have the same name (case unsensitive), what is forbiden: "
      }
      
      checkUniqueness(root.getAllSections.map(_.name)) {
        "At least two sections have the same name (case unsensitive), what is forbiden: "
      }
          
      /*
       * Check that all section with defined component key reference existing
       * variables
       */
      root.getAllSections.foreach { section => section.componentKey match {
        case None => //OK
        case Some(key) => if(variableNames.find(key == _).isEmpty) {
            throw new ParsingException("Section '%s' reference as component key variable '%s' that was not found. Know variables are: %s".format(
              section.name, key, variableNames.mkString("[", "," , "]")
             ))
          }
      } }

      /*
       * check that root section only hold sub-section (and no variables)
       */
      bestEffort(root.children) { child => child match {
        case v : SectionVariableSpec => Failure("Variable declaration '%s' is not allowed here".format(v.name))
        case _ => Full("OK")
      } } match {
        case f:Failure => throw new ParsingException("<%s> must contain only <%s> children : %s".format(SECTIONS_ROOT, SECTION, f.messageChain))
        case _ => //OK
      }
      
      root
    }
  }  
  
  // utility method that check duplicate elements in a string sequence case-unsensitive
  private[this] def checkUniqueness(seq:Seq[String])(errorMsg:String) : Unit = {
    val duplicates = seq.groupBy( _.toLowerCase ).collect { 
      case(k, x) if x.size > 1 => x.mkString("(",",",")")
    }
    
    if(duplicates.nonEmpty) {
      throw new ParsingException(errorMsg + duplicates.mkString("[", "," , "]") )
    }
  }
    
  //method that actually parse a <SECTIONS> or <SECTION> tag
  private[this] def parseSection(root: Node, id: PolicyPackageId, policyName: String): Box[SectionSpec] = {

    val name = {
      val n = getAttributeText(root, "name", "")
      if(root.label == SECTIONS_ROOT) {
        if(n.size > 0) throw new ParsingException("<%s> can not have a 'name' attribute.".format(SECTIONS_ROOT))
        else SECTION_ROOT_NAME
      } else {
        if(n.size > 0) n
        else throw new ParsingException("Section must have name. Missing name for: " + root)
      }
    }
    
    val isMultivalued = "true" == getAttributeText(root, SECTION_IS_MULTIVALUED, "false").toLowerCase
    val foldable = "true" == getAttributeText(root, SECTION_IS_FOLDABLE, "false").toLowerCase
    val description = getUniqueNodeText(root, SECTION_DESCRIPTION, "")
    
    val isComponent = "true"  == getAttributeText(root, SECTION_IS_COMPONENT, "false").toLowerCase
    val componentKey = (root \ ("@" + SECTION_COMPONENT_KEY)).headOption.map( _.text) match {
      case null | Some("") => None
      case x => x
    }
    
    /**
     * A key must be define if and only if we are in a multivalued, component section.
     */
    if(isMultivalued && isComponent && componentKey.isEmpty) {
      throw new ParsingException("Section '%s' is multivalued and is component. A componentKey attribute must be specified".format(name))
    }

    val children = parseChildren(root, id, policyName)
    val sectionSpec = SectionSpec(name, isMultivalued, isComponent, componentKey, foldable, description, children)
    if (isMultivalued)
      Full(sectionSpec.cloneVariablesInMultivalued)
    else
      Full(sectionSpec)
  }

  private[this] def parseChildren(node: Node, id: PolicyPackageId, policyName: String): Seq[SectionChildSpec] = {
    assert(node.label == SECTIONS_ROOT || node.label == SECTION)

    def parseOneVariable(node: Node) = {
      variableParser.parseSectionVariableSpec(node) match {
        case Full(x) => x
        case Empty =>
          val err = "In %s -> %s, couldn't parse variable %s, no error message".format(id, policyName, node)
          logger.error(err)
          throw new ParsingException(err)
        case Failure(m, _, _) =>
          val err = "In %s -> %s, couldn't parse variable %s, error message: %s".format(id, policyName, node, m)
          logger.error(err)
          throw new ParsingException(err)
      }
    }
    
    def parseOneSection(node: Node, id: PolicyPackageId, policyName: String) : SectionSpec = {
      parseSection(node, id, policyName) match {
        case Full(section) => section
        case Failure(m, _, _) =>
          val errWithMessage = "Couldn't parse Section, error message:" + m
          logger.error(errWithMessage)
          throw new ParsingException(errWithMessage)
        case Empty =>
          logger.error("Couldn't parse Section")
          throw new ParsingException("Couldn't parse Section")
      }
    }
      
    for {
      child <- node.child
      if !child.isEmpty && child.label != "#PCDATA"
    } yield child.label match {
      case v if SectionVariableSpec.isVariable(v) => parseOneVariable(child)
      case s if SectionSpec.isSection(s) => parseOneSection(child,id,policyName)
      case x => throw new ParsingException("Unexpected <%s> child element in policy package %s: %s".format(SECTIONS_ROOT,id, x))
    }
  }
}