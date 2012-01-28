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
import java.io.{ InputStream, File }
import net.liftweb.common._
import scala.collection.SortedSet

class DummyPolicyService(policies: Seq[PolicyPackage] = Seq()) extends PolicyPackageService {

  var returnedVariable = collection.mutable.Set[VariableSpec]()
  val policy1 = PolicyPackage(PolicyPackageId(PolicyPackageName("policy1"), PolicyVersion("1.0")), "policy1", "", Seq(), Seq(Bundle("one")), TrackerVariableSpec(), SectionSpec(name="root", children=Seq(InputVariableSpec("$variable1", "a variable1"))))

  val sections = SectionSpec(name="root", children=Seq(InputVariableSpec("$variable2", "a variable2", multivalued = true), InputVariableSpec("$variable22", "a variable22")))
  val policy2 = PolicyPackage(PolicyPackageId(PolicyPackageName("policy2"), PolicyVersion("1.0")), "policy2", "", Seq(), Seq(Bundle("two")), TrackerVariableSpec(), sections)

  val sections3 = SectionSpec(name="root", children=Seq(InputVariableSpec("$variable3", "a variable3")))
  val policy3 = PolicyPackage(PolicyPackageId(PolicyPackageName("policy3"), PolicyVersion("1.0")), "policy3", "", Seq(), Seq(Bundle("three")), TrackerVariableSpec(), sections3)

  val sections4 = SectionSpec(name="root", children=Seq(InputVariableSpec("$variable4", "an variable4")))
  val policy4 = PolicyPackage(PolicyPackageId(PolicyPackageName("policy4"), PolicyVersion("1.0")), "policy4", "", Seq(), Seq(Bundle("four")), TrackerVariableSpec(), sections4)

  val sectionsFoo = SectionSpec(name="root", children=Seq(InputVariableSpec("$bar", "bar")))
  val foo = PolicyPackage(PolicyPackageId(PolicyPackageName("foo"), PolicyVersion("1.0")), "foo", "", Seq(), Seq(Bundle("foo")), TrackerVariableSpec(), sectionsFoo)

  val policyMap = Map(policy1.id -> policy1,
    policy2.id -> policy2,
    policy3.id -> policy3,
    policy4.id -> policy4,
    foo.id -> foo) ++ policies.map(p => (p.id, p))

  def this() = this(Seq()) //Spring need that...

  def packageDirectory: File = new File("/")

  def getPolicyPackagePath(id: PolicyPackageId): Option[String] = getPolicy(id).map(_ => id.name.value)

  def getTemplateContent(templateName: TmlId)(useIt: Option[InputStream] => Unit): Unit = {}

  def getAllPolicies(): Map[PolicyPackageId, PolicyPackage] = { policyMap }

  def getPolicy(policyName: PolicyPackageId): Option[PolicyPackage] = {
    policyMap.get(policyName)
  }

  def getPolicies(policiesName: Seq[PolicyPackageId]): Seq[PolicyPackage] = {
    policiesName.map(x => policyMap(x))
  }

  def getLastPolicyByName(policyName: PolicyPackageName): Option[PolicyPackage] = {
    policyMap.get(PolicyPackageId(policyName, PolicyVersion("1.0")))
  }

//  def getVariables(policyName: PolicyPackageId, includeSystemVar: Boolean = false): Seq[VariableSpec] =
//    policyName.name.value match {
//      case "policy1" =>
//        List(InputVariableSpec("$variable1", "a variable1"))
//      case "policy2" =>
//        val variable = InputVariableSpec("$variable2", "a variable2", multivalued = true)
//        List(variable, InputVariableSpec("$variable22", "a variable22"))
//      case "policy3" =>
//        List(InputVariableSpec("$variable3", "a variable3"))
//      case "policy4" =>
//        List(InputVariableSpec("$variable4", "an variable4"))
//      case "foo" =>
//        List(InputVariableSpec("$bar", "bar"))
//    }
//  
  override def getVersions(name:PolicyPackageName) : SortedSet[PolicyVersion] = SortedSet.empty[PolicyVersion]

  def manageDependencies(chosenTemplate: Seq[TmlId] , includeExternalDependencies : Boolean = true) : Seq[TmlId] = {
    Seq()
  }

  def getReferencePolicyTemplateLibrary: RootPolicyPackageCategory = null
  def getPolicyTemplateCategory(id: PolicyPackageCategoryId): Box[PolicyPackageCategory] = null
  def getParentPolicyTemplateCategory(id: PolicyPackageCategoryId): Box[PolicyPackageCategory] = null
  def getParents_PolicyTemplateCategory(id: PolicyPackageCategoryId): Box[List[PolicyPackageCategory]] = null
  def getParentPolicyTemplateCategory_forTemplate(id: PolicyPackageId): Box[PolicyPackageCategory] = null

}