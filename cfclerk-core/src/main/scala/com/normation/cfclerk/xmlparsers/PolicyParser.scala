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

import CfclerkXmlConstants._
import com.normation.cfclerk.domain._
import com.normation.cfclerk.services.SystemVariableSpecService
import com.normation.cfclerk.exceptions.ParsingException
import com.normation.utils.Utils._
import com.normation.utils.Control.{sequence,bestEffort}
import scala.xml._
import net.liftweb.common._

/**
 * Parse a Policy Package (policy.xml file)
 */

class PolicyParser(
    variableSpecParser: VariableSpecParser
  , sectionParser:SectionSpecParser
  , tmlParser : TmlParser
  , systemVariableSpecService : SystemVariableSpecService
) extends Loggable {
  
  def parseXml(node: Node, id: PolicyPackageId): PolicyPackage = {
    //check that node is <POLICY> and has a name attribute
    if (node.label.toUpperCase == POLICY_ROOT) {
      node.attribute(POLICY_NAME) match {
        case Some(nameAttr) if (PolicyParser.isValidId(id.name.value) && nonEmpty(nameAttr.text)) =>

          val name = nameAttr.text
          val compatible = try Some(CompatibleParser.parseXml((node \ COMPAT_TAG).head)) catch { case _ => None }

          val policyPackage = PolicyPackage(
              id
            , name
            , rootSection = sectionParser.parseSectionsInPolicy(node, id, name)
            , description = ??!((node \ POLICY_DESCRIPTION).text).getOrElse(name)
            , compatible = compatible
            , templates = (node \ PROMISE_TEMPLATES_ROOT \\ PROMISE_TEMPLATE).map(xml => tmlParser.parseXml(id, xml) )
            , bundlesequence = (node \ BUNDLES_ROOT \\ BUNDLE_NAME).map(xml => Bundle(xml.text) )
            , trackerVariableSpec = parsePolicyInfo(node)
            , systemVariableSpecs = parseSysvarSpecs(node,id)
            , isMultiInstance = ((node \ POLICY_IS_MULTIINSTANCE).text.equalsIgnoreCase("true") )
            , longDescription = ??!((node \ POLICY_LONG_DESCRIPTION).text).getOrElse("")
            , isSystem = ((node \ POLICY_IS_SYSTEM).text.equalsIgnoreCase("true"))
          )
          
          /*
           * Check that if the policy info variable spec has a bounding variable, that
           * variable actually exists
           */
          policyPackage.trackerVariableSpec.boundingVariable.foreach { bound =>
            if(
                policyPackage.rootSection.getAllVariables.exists { v => v.name == bound } ||
                systemVariableSpecService.getAll.exists { v => v.name == bound }
            ) {
              //ok
            } else {
              throw new ParsingException("The bouding variable '%s' for policy info variable does not exists".format(bound))
            }
          }
                    
          policyPackage

        case _ => throw new ParsingException("Not a policy node, missing 'name' attribute: %s".format(node))
      }
    } else {
      throw new ParsingException("Not a policy node, bad node name. Was expecting <%s>, got: %s".format(POLICY_ROOT,node))
    }
  }
  
  private[this] def checkUniqueness(seq:Seq[String])(errorMsg:String) : Unit = {
    if(seq.distinct.size != seq.size) {
      throw new ParsingException(errorMsg + seq.groupBy(identity).collect { 
                  case(k, x) if x.size > 1 => k 
      }.mkString("[", "," , "]") )
    }
  }

  private[this] def parsePolicyInfo(node: Node): TrackerVariableSpec = {
    val trackerVariableSpecs = (node \ TRACKINGVAR)
    if(trackerVariableSpecs.size == 0) { //default trackerVariable variable spec for that package
      TrackerVariableSpec()
    } else if(trackerVariableSpecs.size == 1) {
      variableSpecParser.parseTrackerVariableSpec(trackerVariableSpecs.head) match {
        case Full(p) => p
        case e:EmptyBox =>
          throw new ParsingException( (e ?~! "Error when parsing <%s> tag".format(TRACKINGVAR)).messageChain )
      }
    } else throw new ParsingException("Only one <%s> tag is allowed the the document, but found %s".format(TRACKINGVAR,trackerVariableSpecs.size))
  }
  
  /*
   * Parse the list of system vars used by that policy package.
   * 
   */
  private[this] def parseSysvarSpecs(node: Node, id:PolicyPackageId) : Set[SystemVariableSpec] = {
    (node \ SYSTEMVARS_ROOT \ SYSTEMVAR_NAME).map(x => systemVariableSpecService.get(x.text)).toSet
  }
  
}

object PolicyParser {

  val authorizedCharInId = """([a-zA-Z0-9\-_]+)""".r

  def isValidId(s: String): Boolean = s match {
    case authorizedCharInId(_) => true
    case _ => false
  }

}