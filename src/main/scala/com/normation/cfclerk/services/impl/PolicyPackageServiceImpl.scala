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

package com.normation.cfclerk.services.impl

import com.normation.cfclerk.services._
import com.normation.cfclerk.domain._
import org.slf4j.{ Logger, LoggerFactory }
import net.liftweb.common._
import Box._

import com.normation.cfclerk.exceptions._
import java.io.{ File, InputStream }
import scala.collection.SortedSet
import scala.collection.mutable

class PolicyPackageServiceImpl(
    policyPackagesReader: PolicyPackagesReader
  , refLibCallbacks     : Seq[ReferenceLibraryUpdateNotification]
) extends PolicyPackageService with UpdatePolicyTemplateLibrary with Loggable {

  /**
   * Callback to call on PTLib update
   */
  private[this] val callbacks = scala.collection.mutable.Buffer(refLibCallbacks:_*)

  
  /*
   * PackagesInfo:
   * - packagesCategory: Map[PolicyPackageId, PolicyPackageCategoryId]
   * - packages: Map[PolicyPackageName, SortedMap[PolicyVersion, PolicyPackage]] 
   * - categories: SortedMap[PolicyPackageCategoryId, PolicyPackageCategory]
   */
  private[this] var packageInfosCache: PackagesInfo = {
    /*
     * readPolicies result is updated only on 
     * policyPackagesReader.getModifiedPolicyPackages,
     * so we don't call that method at boot time. 
     */
    try {
      policyPackagesReader.readPolicies
    } catch {
      case e:Exception => 
        logger.error("Error when loading the previously saved policy template library. Trying to update to last library available to overcome the error")
        this.update
        policyPackagesReader.readPolicies
    }
  }
    
  ////// end constructor /////
  
  
  /**
   * Register a new callback
   */
  override def registerCallback(callback:ReferenceLibraryUpdateNotification) : Unit = {
    callbacks.append(callback)
  }
  
  override def update : Box[Seq[PolicyPackageId]] = {
    try {
      val modifiedPackages = policyPackagesReader.getModifiedPolicyPackages
      if (modifiedPackages.nonEmpty || /* first time init */ null == packageInfosCache) {
        logger.info("Reloading policy template library, " + {
          if (modifiedPackages.isEmpty) "no modified packages found"
          else "found modified policy package(s): " + modifiedPackages.mkString(", ")
        })
        packageInfosCache = policyPackagesReader.readPolicies
  
        callbacks.foreach { callback =>
          try {
            callback.updatedPolicyPackage(modifiedPackages)
          } catch {
            case e: Exception => logger.error("Error when executing callback '%s' with updated policy templates: '%s'".format(callback.name, modifiedPackages.mkString(", ")), e)
          }
        }
  
      } else {
        logger.debug("Not reloading policy template library as nothing changed since last reload")
      }
      Full(modifiedPackages)
    } catch {
      case e:Exception => Failure("Error when trying to read policy package library", Full(e), Empty)
    }
  }
  



  override def getTemplateContent(templateName: TmlId)(useIt: Option[InputStream] => Unit): Unit =
    policyPackagesReader.getTemplateContent(templateName)(useIt)

  /**
   * Return all the policies available
   * @return
   */
  override def getAllPolicies: Map[PolicyPackageId, PolicyPackage] = {
    (for {
      (id, versions) <- packageInfosCache.packages
      (v, p) <- versions
    } yield {
      (PolicyPackageId(id, v), p)
    }).toMap
  }

  override def getVersions(name: PolicyPackageName): SortedSet[PolicyVersion] = {
    SortedSet[PolicyVersion]() ++ packageInfosCache.packages.get(name).toSeq.flatMap(_.keySet)
  }

  /**
   * Retrieve the list of policies corresponding to the ids
   * @param policyIds : identifiers of the policies
   * @return : the list of policy objects
   * Throws an error if one policy ID does not match any known policy
   */
  override def getPolicies(policyIds: Seq[PolicyPackageId]): Seq[PolicyPackage] = {
    policyIds.map(x => packageInfosCache.packages(x.name)(x.version))
  }

  /**
   * Return a policy by its name
   * @param policyName
   * @return
   */
  override def getPolicy(policyId: PolicyPackageId): Option[PolicyPackage] = {
    val result = packageInfosCache.packages.get(policyId.name).flatMap(versions => versions.get(policyId.version))
    if(!result.isDefined) {
      logger.debug("Required policy package '%s' was not found".format(policyId))
    }
    result
  }

  override def getLastPolicyByName(policyName: PolicyPackageName): Option[PolicyPackage] = {
    packageInfosCache.packages.get(policyName).map { versions => versions.last._2 }
  }

  //////////////////////////////////// categories /////////////////////////////

  private def fileBreadCrump(target: File, current: File, stack: List[File]): List[File] = {

    if (current.getParentFile == target) target :: stack
    else fileBreadCrump(target, current.getParentFile, current :: stack)

  }

  override def getReferencePolicyTemplateLibrary: RootPolicyPackageCategory = packageInfosCache.rootCategory

  override def getPolicyTemplateCategory(id: PolicyPackageCategoryId): Box[PolicyPackageCategory] = {
    id match {
      case RootPolicyPackageCategoryId => Full(this.packageInfosCache.rootCategory)
      case sid: SubPolicyPackageCategoryId => this.packageInfosCache.subCategories.get(sid)
    }
  }

  override def getParentPolicyTemplateCategory_forTemplate(id: PolicyPackageId): Box[PolicyPackageCategory] = {
    for {
      cid <- this.packageInfosCache.packagesCategory.get(id)
      cat <- cid match {
        case RootPolicyPackageCategoryId => Some(this.packageInfosCache.rootCategory)
        case sid: SubPolicyPackageCategoryId => this.packageInfosCache.subCategories.get(sid)
      }
    } yield {
      cat
    }
  }
}
