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

import scala.collection._
import com.normation.cfclerk.domain._
import com.normation.cfclerk.exceptions._

import net.liftweb.common._
import com.normation.cfclerk.services._
import com.normation.utils.Control.sequence

class ContainerServiceImpl(policyTranslator : PromiseWriterService, 
    policyService : PolicyPackageService
) extends ContainerService with Loggable {

 /**
   * Create a container
   * @param identifier
   * @param policiesInstancesBeans
   * @return
   */
  def createContainer(identifier: String, policiesInstances: Seq[CFCPolicyInstance]) : Box[PoliciesContainer] = {
  
      if (policiesInstances.size==0) {
        logger.error("There should be at least one policy instance to configure the container")
        return ParamFailure[Seq[CFCPolicyInstance]]("No policy instance", Full(new NotFoundException("No policy instance defined")), Empty, policiesInstances)
      }
    
      val container = new PoliciesContainer(identifier)      
      
      for {
       res <- sequence(policiesInstances) { policytoAdd =>
         addPolicy(container, policytoAdd)
       }
      } yield {
        container
      }
  }

  
  /**
   * Add a policy instance to a container
   * @param container
   * @param policyInstanceBean
   * @return
   */
  def addPolicyInstance(container: PoliciesContainer, policyInstance : CFCPolicyInstance) : Box[CFCPolicyInstance] = {
    addPolicy(container, policyInstance)
  }  
  
  
  /**
   * Adding a policy to a container
   */
  private def addPolicy(container: PoliciesContainer, policyInstance : CFCPolicyInstance) : Box[CFCPolicyInstance] = {
     
    // check the legit character of the policy
    if (container.getPolicyInstance(policyInstance.id) != None) {
      logger.warn("Cannot add a policy instance with the same id than an already existing one " + policyInstance.id)
      return ParamFailure[CFCPolicyInstance]("Duplicate policy instance", Full(new PolicyException("Duplicate policy instance " +policyInstance.id)), Empty, policyInstance)
    }
    
    val policy = policyService.getPolicy(policyInstance.policyId).getOrElse(return Failure("Error, can not find policy with name '%s' with policy service".format(policyInstance.policyId)))
    if (container.findPolicyInstanceByPolicyId(policyInstance.policyId).filter(x => policy.isMultiInstance==false).size>0) {
      logger.warn("Cannot add a policy instance from the same non duplicable policy than an already existing one " + policyInstance.policyId)
      return ParamFailure[CFCPolicyInstance]("Duplicate unique policy", Full(new PolicyException("Duplicate unique policy " +policyInstance.policyId)), Empty, policyInstance)
    }
    
    container.addPolicyInstance(policyInstance) flatMap { pi =>
      //check that pi really is in container
      if(container.getPolicyInstance(pi.id).isDefined) {
        logger.info("Successfully added policy instance %s to container %s".format(policyInstance, container.outPath))
        Full(pi)
      } else {
        logger.error("An error occured while adding policy %s for container %s : it is not present".format( policyInstance.id, container.outPath))
        Failure("Something bad happened, the policy '%s' should have been added in container with out path '%s'".format(pi.id, container.outPath))
        }
    }
  }
}