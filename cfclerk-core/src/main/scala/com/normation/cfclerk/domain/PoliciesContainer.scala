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
 * We put policyInstances in them, as well as an outPath (relative to the base path)
 *
 * @author Nicolas CHARLES
 *
 */
class PoliciesContainer(val outPath: String) extends Loggable {

  protected val policiesInstances = MutMap[CFCPolicyInstanceId, CFCPolicyInstance]() /* the target policies (the one we wish to have) */

  /**
   * Add a policy instance
   * @param policy
   * @return Full(the added policy) in case of success, Fail in case of error. 
   */
  def addPolicyInstance(policy: CFCPolicyInstance) : Box[CFCPolicyInstance] = {
    policiesInstances.get(policy.id) match {
      case None =>
        logger.trace("Adding policy instance " + policy.toString)
        updateAllUniqueVariables(policy)
        policiesInstances += (policy.id -> policy.clone)
        Full(policy)
      case Some(x) => Failure("An instance of the policy with the same identifier already exists")
    }
  }

  /** 
   * Update a policyinstance, returns log entry only if variables have been modified (in this policy or another)
   * @param policy
   * @return Full(the updated policy) in case of success, the error else. 
   */
  def updatePolicyInstance(policy: CFCPolicyInstance) : Box[CFCPolicyInstance] = {
    policiesInstances.get(policy.id) match {
      case None => Failure("No instance of the policy with the given identifier '%s' exists".format(policy.id))
      case Some(x) => 
        x.updatePolicy(policy)
        updateAllUniqueVariables(policy)
        Full(x)
    }
  }

  /**
   * Returns a policy instance by its name (not its id)
   * @param policyName
   * @return
   */
  def findPolicyInstanceByPolicyId(policyId: PolicyPackageId) = {
    policiesInstances.filter(x => x._2.policyId == policyId).map(x => (x._1, x._2.clone()))
  }

  /**
   * Returns all the policy ids defined in this container
   * @return
   */
  def getAllPoliciesIds(): Seq[PolicyPackageId] = {
    // toSet to suppress duplicates
    policiesInstances.values.map(_.policyId).toSet.toSeq
  }

  /**
   * Removes a policy instance by its id
   * @param policyId
   * @return
   */
  def removePolicyInstance(policyInstanceId: CFCPolicyInstanceId) = {
    policiesInstances.get(policyInstanceId) match {
      case None => throw new Exception("No instance of the policy with the given identifier exists")
      case Some(x) => policiesInstances.remove(policyInstanceId)
    }
  }

  /**
   * Returns a policy instance by its id
   * @param policyId
   * @return
   */
  def getPolicyInstance(policyInstanceId: CFCPolicyInstanceId): Option[CFCPolicyInstance] = {
    policiesInstances.get(policyInstanceId).map(x => x.clone)
  }

  /**
   * Returns all the policy instances
   * @return
   */
  def getPolicyInstances(): MutMap[CFCPolicyInstanceId, CFCPolicyInstance] = {
    policiesInstances.map(x => (x._1, x._2.clone))
  }

  override def toString() = "Container : %s".format(outPath)

  /**
   * Go through each policyinstance and update the unique variable
   * Called when we add a policyInstance
   * @param policy
   */
  private def updateAllUniqueVariables(policy: CFCPolicyInstance) : CFCPolicyInstance = {
    for {
      uniqueVariable <- policy.getVariables.filter(x => (x._2.spec.isUniqueVariable))
      instance <- policiesInstances.filter(x => (x._2.getVariable(uniqueVariable._1) != None))
    } {
      instance._2.setVariable(uniqueVariable._2)
    }
    policy
  }

  override def clone(): PoliciesContainer = {
    val copy = new PoliciesContainer(outPath)

    copy.policiesInstances ++= policiesInstances.map(x => (x._1, x._2.clone))

    copy
  }

}
