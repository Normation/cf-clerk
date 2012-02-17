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


import scala.collection._
import com.normation.cfclerk.exceptions.NotFoundException
import org.joda.time.DateTime
import org.joda.time.format._

import scala.xml._
import com.normation.utils.HashcodeCaching

import net.liftweb.common._

/**
 * Unique identifier for a CFClerk policy instance
 *
 */
case class Cf3PolicyDraftId(value: String) extends HashcodeCaching

/**
 * That policy instance object is an instance of a policy applied (bound)
 * to a particular node, so that its variable can be specialized given the node
 * context. 
 * 
 * That object is part of a node configuration and is the last abstraction used
 * before actual promises are generated. 

 * Please note that a PolicyInstance should really have a Variable of the type TrackerVariable,
 * that will hold the id of the policyInstance to be written in the template
 *
 */
class Cf3PolicyDraft(
    val id: Cf3PolicyDraftId
  , val techniqueId: TechniqueId
  , val __variableMap: Map[String, Variable]
  , val TrackerVariable : TrackerVariable
  , val priority: Int
  , val serial: Int
) extends Loggable {

  // TODO : do we want to keep this ?
  private var _modificationDate: DateTime = DateTime.now()
  private val variableMap: mutable.Map[String, Variable] = mutable.Map[String, Variable]() ++ __variableMap

  private def updateConftime() {
    _modificationDate = DateTime.now()
  }

  def modificationDate = _modificationDate
  
  /**
   * Add (or update) a Variable in the list of expected variable
   * Return the new value if it is added/updated, usefull for loging modification
   * If the variable value is null or "", then the variable will NOT be removed from the map
   * @param variable
   * @return None if the variable wasn't changed, Some(variable) otherwise.
   */
  def setVariable(variable: Variable): Option[Variable] = {
    variableMap.get(variable.spec.name) match {
      case None => variableMap += (variable.spec.name -> variable.clone); updateConftime; Some(variable.clone)
      case Some(value) => if (variable.values != value.values) {
        variableMap += (variable.spec.name -> variable.clone);
        updateConftime
        Some(variable.clone)
      } else {
        None
      }
    }
  }

  def getVariable(key: String): Option[Variable] = {
    variableMap.get(key) match {
      case None => None
      case Some(x) => Some(x.clone)
    }
  }

  /**
   * Return a map of all the variable
   * @return
   */
  def getVariables(): Map[String, Variable] = {
    variableMap.map(x => (x._1, x._2.clone))
  }

  /**
   * Return a map of all the non system variable
   * @return
   */
  def getNonSystemVariables(): Map[String, Variable] = {
    variableMap.filter(_._2.spec.isSystem).map(x => (x._1, x._2.clone))
  }

  /**
   * Add a Variable in the list of expected variable
   * If the variable value is empty, then the variable will be removed from the map
   * @param variable : Two strings
   */
  def addVariable(variable: Variable) = {
    // check the the value is not null or empty
    if (variable.values.size == 0) {
      removeVariable(variable.spec.name)
    } else {
      setVariable(variable)
    }
  }

  def removeVariable(key: String) = {
    variableMap -= key
    updateConftime
  }

  /**
   * Update a policy based on another policy. It will check for var to remove and add, and update the time accordingly
   * Does not check for systemvar.
   * 
   * Returned the diff
   */
  def updateCf3PolicyDraft(other: Cf3PolicyDraft): ModifyCf3PolicyDraftDiff = {
    if (this.id != other.id)
      throw new Exception("the identifiers for the update don't match")

    val varToRemove = mutable.Map[String, Seq[String]]() // the variable in the old, but not in the new
    val varToAdd = mutable.Map[String, Seq[String]]() // the variable only in the new
    val varToUpdate = mutable.Map[String, Seq[String]]() // the variable in both, but changed

    for (variable <- getNonSystemVariables) {
      if (other.getVariable(variable._1) == None) {
        varToRemove += variable._1 -> variable._2.values
      }
    }

    for (variable <- varToRemove) {
      this.removeVariable(variable._1)
    }

    for (variable <- other.getNonSystemVariables) {
      variableMap.get(variable._1) match {
        case None => variableMap += (variable._1 -> variable._2.clone); updateConftime; varToAdd += (variable._1 -> variable._2.values)
        case Some(value) => if (variable._2.values != value.values) {
          variableMap += (variable._1 -> variable._2.clone);
          updateConftime
          varToUpdate += (variable._1 -> variable._2.values)
        }
      }
    }

    ModifyCf3PolicyDraftDiff(this.id, varToRemove, varToAdd, varToUpdate)
    
  }

  /**
   * Search in the variables of the policy for the TrackerVariable (that should be unique for the moment),
   * and retrieve it, along with bounded variable (or itself if it's bound to nothing)
   * Can throw a lot of exceptions if something fails
   */
  def getPolicyInstanceVariable(): (TrackerVariable, Variable) = {
      TrackerVariable.spec.boundingVariable match {
        case None | Some("") | Some(null) => (TrackerVariable.copy(), TrackerVariable.copy())
        case Some(value) =>
          variableMap.get(value) match {
            case None => throw new NotFoundException("No valid bounding found for TrackerVariable " + TrackerVariable.spec.name + " found in policyInstance " + id)
            case Some(variable) => (TrackerVariable.copy(), Variable.matchCopy(variable))
          }
      }
  }

  override lazy val toString = "%s %s".format(id, techniqueId)

  override lazy val hashCode = 37 * id.hashCode

  override def equals(other: Any) = other match {
    case that: Cf3PolicyDraft => this.id == that.id
    case _ => false
  }

  /**
   * Compare this policy instance with another and check if they are the
   * same, what means:
   * - same uuid / policy name ;
   * - same variables (an empty and a non existent variable are the same, and the variables are trimmed)
   *
   * @param that
   */
  def equalsWithSameValues(that: Cf3PolicyDraft): Boolean = {
    this.id == that.id &&
      this.techniqueId == that.techniqueId &&
      this.serial == that.serial &&
      this.variableMap.filter(x => x._2.values.size > 0).keySet == that.variableMap.filter(x => x._2.values.size > 0).keySet &&
      variableMap.filter(x => x._2.values.size > 0).keySet.forall { k =>
        this.variableMap.filter(x => x._2.values.size > 0)(k).values.map(_.trim) == that.variableMap(k).values.map(_.trim)
      }
  }

  override def clone(): Cf3PolicyDraft = {
    val returnedPolicy = new Cf3PolicyDraft(id, techniqueId, Map(), TrackerVariable, priority, serial)
    returnedPolicy.variableMap ++= this.variableMap.map(x => (x._1 -> x._2.clone))
    returnedPolicy._modificationDate = this._modificationDate
    returnedPolicy
  }
  
  def copy(serial : Int): Cf3PolicyDraft = {
    val returnedPolicy = new Cf3PolicyDraft(id, techniqueId, Map(), TrackerVariable, priority, serial)
    returnedPolicy.variableMap ++= this.variableMap.map(x => (x._1 -> x._2.clone))
    returnedPolicy._modificationDate = this._modificationDate
    returnedPolicy
  }

  
}

case class ModifyCf3PolicyDraftDiff(
    Cf3PolicyDraftId: Cf3PolicyDraftId
  , removedVariables: Map[String, Seq[String]] = Map()
  , addedVariables: Map[String, Seq[String]] = Map()
  , changedVariables: Map[String, Seq[String]] = Map()
) extends HashcodeCaching
