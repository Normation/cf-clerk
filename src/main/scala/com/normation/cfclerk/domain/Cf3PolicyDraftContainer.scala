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

import scala.collection.mutable.{ Map => MutMap }
import net.liftweb.common._

/**
 * Container for policy instances and the path where we want to write them
 * We put directives in them, as well as an outPath (relative to the base path)
 *
 * @author Nicolas CHARLES
 *
 */
class Cf3PolicyDraftContainer(val outPath: String) extends Loggable {

  protected val cf3PolicyDrafts = MutMap[Cf3PolicyDraftId, Cf3PolicyDraft]() /* the target policies (the one we wish to have) */

  /**
   * Add a policy instance
   * @param policy
   * @return Full(the added policy) in case of success, Fail in case of error. 
   */
  def add(cf3PolicyDraft: Cf3PolicyDraft) : Box[Cf3PolicyDraft] = {
    cf3PolicyDrafts.get(cf3PolicyDraft.id) match {
      case None =>
        logger.trace("Adding cf3PolicyDraft " + cf3PolicyDraft.toString)
        updateAllUniqueVariables(cf3PolicyDraft)
        cf3PolicyDrafts += (cf3PolicyDraft.id -> cf3PolicyDraft.clone)
        Full(cf3PolicyDraft)
      case Some(x) => Failure("An instance of the cf3PolicyDraft with the same identifier already exists")
    }
  }

  /** 
   * Update a policyinstance, returns log entry only if variables have been modified (in this cf3PolicyDraft or another)
   * @param cf3PolicyDraft
   * @return Full(the updated cf3PolicyDraft) in case of success, the error else. 
   */
  def update(cf3PolicyDraft: Cf3PolicyDraft) : Box[Cf3PolicyDraft] = {
    cf3PolicyDrafts.get(cf3PolicyDraft.id) match {
      case None => Failure("No instance of the cf3PolicyDraft with the given identifier '%s' exists".format(cf3PolicyDraft.id))
      case Some(x) => 
        x.updateCf3PolicyDraft(cf3PolicyDraft)
        updateAllUniqueVariables(cf3PolicyDraft)
        Full(x)
    }
  }

  /**
   * Returns a cf3PolicyDraft by its name (not its id)
   * @param policyName
   * @return
   */
  def findById(techniqueId: TechniqueId) = {
    cf3PolicyDrafts.filter(x => x._2.techniqueId == techniqueId).map(x => (x._1, x._2.clone()))
  }

  /**
   * Returns all the cf3PolicyDraft ids defined in this container
   * @return
   */
  def getAllIds(): Seq[TechniqueId] = {
    // toSet to suppress duplicates
    cf3PolicyDrafts.values.map(_.techniqueId).toSet.toSeq
  }

  /**
   * Removes a cf3PolicyDraft instance by its id
   * @param techniqueId
   * @return
   */
  def remove(cf3PolicyDraftId: Cf3PolicyDraftId) = {
    cf3PolicyDrafts.get(cf3PolicyDraftId) match {
      case None => throw new Exception("No instance of the policy with the given identifier exists")
      case Some(x) => cf3PolicyDrafts.remove(cf3PolicyDraftId)
    }
  }

  /**
   * Returns a policy instance by its id
   * @param techniqueId
   * @return
   */
  def get(cf3PolicyDraftId: Cf3PolicyDraftId): Option[Cf3PolicyDraft] = {
    cf3PolicyDrafts.get(cf3PolicyDraftId).map(x => x.clone)
  }

  /**
   * Returns all the policy instances
   * @return
   */
  def getAll(): MutMap[Cf3PolicyDraftId, Cf3PolicyDraft] = {
    cf3PolicyDrafts.map(x => (x._1, x._2.clone))
  }

  override def toString() = "Container : %s".format(outPath)

  /**
   * Go through each cf3PolicyDraft and update the unique variable
   * Called when we add a cf3PolicyDraft
   * @param policy
   */
  private def updateAllUniqueVariables(policy: Cf3PolicyDraft) : Cf3PolicyDraft = {
    for {
      uniqueVariable <- policy.getVariables.filter(x => (x._2.spec.isUniqueVariable))
      instance <- cf3PolicyDrafts.filter(x => (x._2.getVariable(uniqueVariable._1) != None))
    } {
      instance._2.setVariable(uniqueVariable._2)
    }
    policy
  }

  override def clone(): Cf3PolicyDraftContainer = {
    val copy = new Cf3PolicyDraftContainer(outPath)
    copy.cf3PolicyDrafts ++= cf3PolicyDrafts.map(x => (x._1, x._2.clone))
    copy
  }

}
