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

package com.normation.cfclerk.services

import com.normation.cfclerk.domain._
import java.io.InputStream
import net.liftweb.common._
import com.normation.utils.Control.sequence
import scala.collection.SortedSet

/**
 * A service that provides information about the policy packages
 *
 *
 */
/**
 * @author vincent
 *
 */
trait PolicyPackageService {

  /**
   * Retrieve the template path for templateName relative to
   * the root of the policy package category tree
   */
  def getTemplateContent(templateName: TmlId)(useIt: Option[InputStream] => Unit): Unit

  //  def packageDirectory : File

  /**
   * Return all the policies available
   */
  def getAllPolicies(): Map[PolicyPackageId, PolicyPackage]

  /**
   * Return a policy by its
   * @param policyName
   * @return
   */
  def getPolicy(policyId: PolicyPackageId): Option[PolicyPackage]

  /**
   * Return a policy found by its name.
   * If several versions of that policy are available,
   * the most recent version is used
   */
  def getLastPolicyByName(policyName: PolicyPackageName): Option[PolicyPackage]

  /**
   * Retrieve a the list of policies corresponding to the names
   * @param policiesName : the names of the policies
   * @return : the list of policy objects
   */
  def getPolicies(policyIds: Seq[PolicyPackageId]): Seq[PolicyPackage]

  /**
   * For the given PolicyPackageName, retrieve all available 
   * versions. 
   * If the policyName is unknown, the returned collection will
   * be empty. 
   */
  def getVersions(name:PolicyPackageName) : SortedSet[PolicyVersion]

  ////////////////// method for categories //////////////////

  def getReferencePolicyTemplateLibrary: RootPolicyPackageCategory

  def getPolicyTemplateCategory(id: PolicyPackageCategoryId): Box[PolicyPackageCategory]

  def getParentPolicyTemplateCategory_forTemplate(id: PolicyPackageId): Box[PolicyPackageCategory]

  final def getPolicyTemplateBreadCrump(id: PolicyPackageId): Box[Seq[PolicyPackageCategory]] = {
    for {
      cat <- getParentPolicyTemplateCategory_forTemplate(id)
      path <- sequence(cat.id.getIdPathFromRoot) { currentCatId =>
        getPolicyTemplateCategory(currentCatId) ?~! "'%s' category was not found but should be a parent of '%s'".format(currentCatId, cat.id)
      }
    } yield {
      path
    }
  }
}
