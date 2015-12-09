/*
*************************************************************************************
* Copyright 2011 Normation SAS
*************************************************************************************
*
* This file is part of Rudder.
* 
* Rudder is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
* 
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU General Public License version 3, the copyright holders add
* the following Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU General
* Public License version 3, when you create a Related Module, this
* Related Module is not considered as a part of the work and may be
* distributed under the license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
* 
* Rudder is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Rudder.  If not, see <http://www.gnu.org/licenses/>.

*
*************************************************************************************
*/

package com.normation.cfclerk.services

import junit.framework.TestSuite
import org.junit.Test
import org.junit._
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.BlockJUnit4ClassRunner
import scala.collection._
import com.normation.cfclerk.domain._
import com.normation.cfclerk.xmlparsers.TechniqueParser
import org.springframework.context.{ ApplicationContext, ApplicationContextAware }
import org.springframework.test.context.ContextConfiguration
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner
import java.io.File
import org.apache.commons.io.IOUtils
import com.normation.cfclerk.services.impl._

@RunWith(classOf[SpringJUnit4ClassRunner])
@ContextConfiguration(Array("file:src/test/resources/spring-config-test.xml"))
class TestPackageReader {

  @Autowired
  val policyParser: TechniqueParser = null

  lazy val reader = new FSTechniqueReader(
    policyParser,
    "src/test/resources/techniquesRoot",
    "metadata.xml",
    "category.xml",
    "expected_reports.csv")

  @Test
  def testReadPackage() {
    val infos = reader.readTechniques
    assertEquals(3, infos.subCategories.size)

    val rootDir = new File("src/test/resources/techniquesRoot")
    val rootCatId = RootTechniqueCategoryId
    val rootCat = infos.rootCategory
    assertEquals("Root category", rootCat.name)
    assertEquals("", rootCat.description)

    assertEquals(1, rootCat.packageIds.size)
    assertEquals("p_root_1", rootCat.packageIds.head.name.value)
    assertEquals(TechniqueVersion("1.0"), rootCat.packageIds.head.version)

    assertEquals(1, rootCat.subCategoryIds.size)

    val subCatIds = ("cat1" :: Nil).map(x => rootCatId / x)
    assertTrue(subCatIds.forall(s => rootCat.subCategoryIds.exists(x => x == s)))

      def forAllSubDirs[B](root: File, p: File => Boolean): Boolean = {
        root.listFiles.filter(_.isDirectory).forall(dir => { forAllSubDirs(dir, p); p(dir) })
      }

      // dir has to be a directory
      def isValidTechniqueVersionDir(dir: File): Boolean = {
        val isVersionDir = dir.listFiles.exists(f => f.getName == reader.techniqueDescriptorName)
        if (isVersionDir) {
          try {
            TechniqueVersion(dir.getName)
            true
          } catch {
            case e: Exception => false
          }
        } else { // ignore others
          true
        }
      }

    // test that if a directory contains policy.xml files, its name is a valid policy version name
    assert(forAllSubDirs(new File(reader.techniqueDirectoryPath), isValidTechniqueVersionDir))

    //cat 1 : a sub cat, and only one package with two revision
    //(the second package is ignored)
    //simple name, category.xml is broken
    val cat1 = infos.subCategories(subCatIds(0))
    assertEquals("cat1", cat1.name)
    assertEquals("", cat1.description)

    val cat1packages = cat1.packageIds.toSeq
    assertEquals(2, cat1packages.size)
    assertEquals("p1_1", cat1packages(0).name.value)
    assertEquals(TechniqueVersion("1.0"), cat1packages(0).version)
    assertEquals("p1_1", cat1packages(1).name.value)
    assertEquals(TechniqueVersion("2.0"), cat1packages(1).version)

    val tmlId = Cf3PromisesFileTemplateId(cat1packages(0), "theTemplate")
    reader.getTemplateContent(tmlId) {
      case None => assertTrue("Can not open an InputStream for " + tmlId.toString, false)
      case Some(is) =>
        assertEquals("Bad content for the template", IOUtils.toString(is), "The template content\non two lines.")
    }

    val subCatIds2 = cat1.id / "cat1_1"
    assertEquals(1, cat1.subCategoryIds.size)
    assertEquals(subCatIds2, cat1.subCategoryIds.head)

    //cat 1_1 : has package info, and one sub cat
    val cat1_1 = infos.subCategories(subCatIds2)
    assertEquals("Category 1.1 name", cat1_1.name)
    assertEquals("Category 1.1 description", cat1_1.description)

    val subCatIds3 = cat1_1.id / "cat1_1_1"
    assertEquals(1, cat1_1.subCategoryIds.size)
    assertEquals(subCatIds3, cat1_1.subCategoryIds.head)

    val cat1_1_1 = infos.subCategories(subCatIds3)
    assertEquals("cat1_1_1", cat1_1_1.name)
    assertEquals("", cat1_1_1.description)

    assertEquals(0, cat1_1_1.subCategoryIds.size)

    assertEquals(2, cat1_1_1.packageIds.size)
    assertTrue(Seq("p1_1_1_1", "p1_1_1_2").forall { p =>
      cat1_1_1.packageIds.exists(id => id.name.value == p)
    })
    assertTrue(cat1_1_1.packageIds.forall(id => id.version == TechniqueVersion("1.0")))

  }
}