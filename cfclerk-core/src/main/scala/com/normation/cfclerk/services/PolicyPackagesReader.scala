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
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{ Map => MutMap }


case class PackagesInfo(
    rootCategory: RootPolicyPackageCategory
    //the PolicyPackageCategoryId is a path from the point of view of a tree
  , packagesCategory: Map[PolicyPackageId, PolicyPackageCategoryId]
  , packages: Map[PolicyPackageName, SortedMap[PolicyVersion, PolicyPackage]]
    //head of categories is the root category
  , subCategories: Map[SubPolicyPackageCategoryId, PolicyPackageCategory]
)

//a mutable version of PackagesInfo, for internal use only !
private[services] class InternalPackagesInfo(
    var rootCategory: Option[RootPolicyPackageCategory] = None
  , val packagesCategory: MutMap[PolicyPackageId, PolicyPackageCategoryId] = MutMap()
  , val packages: MutMap[PolicyPackageName, MutMap[PolicyVersion, PolicyPackage]] = MutMap()
  , val subCategories: MutMap[SubPolicyPackageCategoryId, SubPolicyPackageCategory] = MutMap()
)

/**
 * This class is in charge to maintain a map of
 * available policy package.
 * A package is composed of a policy identified by its name
 * and all the relevant information like its templates.
 *
 */
trait PolicyPackagesReader {

  /**
   * read the policies from the source directory.
   * return the policy package and the the full path to its
   * root directory for the "current" revision of the
   * reference library. "current" pointer update is 
   * implementation dependent, some implementation doesn't
   * have any notion of version, other using getModifiedPolicyPackages
   * for updating the available "next" state. 
   */
  def readPolicies(): PackagesInfo

  /**
   * Read the content of a template, if the template is known by that
   * PolicyPackageReader.
   * If the template exists, then a Some(input stream), open at the
   * beginning of the template is given to the caller. 
   * If not, a None is given. 
   * The implementation must take care of correct closing of the input
   * stream and any I/O exception. 
   */
  def getTemplateContent[T](templateName: TmlId)(useIt : Option[InputStream] => T) : T
 
  /**
   * An indicator that the underlying policy template library changed and that the content 
   * should be read again.
   * If the sequence is empty, then nothing changed. Else, the list of PolicyPackage with 
   * *any* change will be given
   */
  def getModifiedPolicyPackages : Seq[PolicyPackageId]
}

