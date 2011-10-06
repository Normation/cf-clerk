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

import scala.xml._
import com.normation.cfclerk.domain._
import java.io.FileNotFoundException
import org.xml.sax.SAXParseException
import com.normation.cfclerk.exceptions._
import org.slf4j.{ Logger, LoggerFactory }
import java.io.File
import org.apache.commons.io.FilenameUtils
import com.normation.cfclerk.xmlparsers.PolicyParser
import net.liftweb.common._
import scala.collection.mutable.{ Map => MutMap }
import scala.collection.SortedSet
import com.normation.utils.Utils
import scala.collection.immutable.SortedMap
import java.io.InputStream
import java.io.FileInputStream
import com.normation.cfclerk.services._

/**
 *
 * A PolicyPackageReader that reads policy packages from
 * a directory tree.
 *
 *
 * Conventions used:
 *
 * - all directories which contains a policy.xml file is
 *   considered to be a policy package.
 *
 * - template files are looked in the directory
 *
 * - all directory without a policy.xml are considered to be
 *   a category directory.
 *
 * - if a category directory contains a category.xml file,
 *   information are look from it, else file name is used.
 *
 *  Category description information are stored in XML files with the expected
 *  structure:
 *  <xml>
 *    <name>Name of the category</name>
 *    <description>Description of the category</description>
 *  </xml>
 *
 *  In that implementation, the name of the directory of a category
 *  is used for the PolicyCategoryName.
 *
 */
class FSPolicyPackagesReader(
  policyParser: PolicyParser,
  val packageDirectoryPath: String,
  val policyDescriptorName: String, //full (with extension) conventional name for policy descriptor
  val categoryDescriptorName: String //full (with extension) name of the descriptor for categories
  ) extends PolicyPackagesReader with Loggable {

  reader =>

  private def checkPackageDirectory(dir: File): Unit = {
    if (!dir.exists) {
      throw new RuntimeException("Directory %s does not exists, how do you want that I read policy package in it?".format(dir))
    }
    if (!dir.canRead) {
      throw new RuntimeException("Directory %s is not readable, how do you want that I read policy package in it?".format(dir))
    }
  }

  private[this] val packageDirectory: File = {
    val dir = new File(packageDirectoryPath)
    checkPackageDirectory(dir)
    dir
  }

  //we never know when to read again
  override def getModifiedPolicyPackages : Seq[PolicyPackageId] = Seq()

  /**
   * Read the policies
   * @param doc : the xml representation of the knowledge file
   * @return a map of policy
   */
  override lazy val readPolicies: PackagesInfo = {
    reader.synchronized {
      val packageInfos = new InternalPackagesInfo()

      processDirectory(
        null, //we have to ignore it, so if we don't, a NPE is a good idea.
        reader.packageDirectory,
        packageInfos)
      
      PackagesInfo(
        rootCategory = packageInfos.rootCategory.getOrElse(throw new RuntimeException("No root category was found")),
        packagesCategory = packageInfos.packagesCategory.toMap,
        packages = packageInfos.packages.map { case (k, v) => (k, SortedMap.empty[PolicyVersion, PolicyPackage] ++ v) }.toMap,
        subCategories = packageInfos.subCategories.toMap)
    }
  }

  override def getTemplateContent[T](templateName: TmlId)(useIt: Option[InputStream] => T): T = {
    var is: InputStream = null
    try {
      useIt {
        readPolicies.packagesCategory.get(templateName.policyPackageId).map { catPath =>
          is = new FileInputStream(packageDirectory.getAbsolutePath + "/" + catPath + "/" + templateName.toString + Tml.templateExtension)
          is
        }
      }
    } catch {
      case ex: FileNotFoundException =>
        logger.debug(() => "Template %s does not exists".format(templateName), ex)
        useIt(None)
    } finally {
      if (null != is) {
        is.close
      }
    }
  }

  /**
   * Process the directory to find if it's a category
   * or a package.
   * f must be a directory
   */
  //  private def processDirectory(f: File, packagesInfo: InternalPackagesInfo): Unit = {
  //    val children = f.listFiles
  //    val childrenName = children.map(_.getName)
  //    (childrenName.exists(c => c == policyDescriptorName), childrenName.exists(c => c == categoryDescriptorName)) match {
  //      case (true, true) =>
  //        throw new RuntimeException("Directory %s contains both file %s and %s: can not know if it is a category or a policy package".
  //          format(f.getAbsolutePath, policyDescriptorName, categoryDescriptorName))
  //      case (true, false) => processPolicyPackage(f, packagesInfo)
  //      case (false, _) => processCategory(f, packagesInfo)
  //    }
  //  }

  private def processDirectory(parentCategoryId: PolicyPackageCategoryId, f: File, packagesInfo: InternalPackagesInfo): Unit = {
    val children = f.listFiles
    val childrenName = children.map(_.getName)
    val versionsDir = children.filter(_.isDirectory).flatMap(_.listFiles).filter(f => f.getName == policyDescriptorName).map(_.getParentFile)
    
    if (versionsDir.size > 0) {
      for (d <- versionsDir) {
        processPolicyPackage(parentCategoryId, d, packagesInfo)
      }
    } else {
      processCategory(parentCategoryId, f, packagesInfo)
    }
    
  }

  /**
   * Load a PolicyPackage contains in the directory packageRootDirectory.
   * By convention, packageRootDirectory is the version of the PolicyPackage,
   * and the parent's name directory is the PolicyPackage Name.
   */
  private def processPolicyPackage(parentCategoryId: PolicyPackageCategoryId, packageRootDirectory: File, packagesInfo: InternalPackagesInfo): Unit = {
    val name = PolicyPackageName(packageRootDirectory.getParentFile.getName)
    val id = PolicyPackageId(name, PolicyVersion(packageRootDirectory.getName))
    val pack = policyParser.parseXml(loadDescriptorFile(new File(packageRootDirectory, policyDescriptorName)), id)
        
      def updateParentCat() {
        parentCategoryId match {
          case RootPolicyPackageCategoryId =>
            val cat = packagesInfo.rootCategory.getOrElse(
              throw new RuntimeException("Can not find the parent (root) caterogy %s for package %s".format(packageRootDirectory.getParentFile.getAbsolutePath, pack.id)))
            packagesInfo.rootCategory = Some(cat.copy(packageIds = cat.packageIds + pack.id))

          case sid: SubPolicyPackageCategoryId =>
            val cat = packagesInfo.subCategories.get(sid).getOrElse(
              throw new RuntimeException("Can not find the parent caterogy %s for package %s".format(packageRootDirectory.getParentFile.getAbsolutePath, pack.id)))
            packagesInfo.subCategories(sid) = cat.copy(packageIds = cat.packageIds + pack.id)

        }
      }

    //check that that package is not already know, else its an error (by id ?)
    packagesInfo.packages.get(pack.id.name) match {
      case None => //so we don't have any version yet, and so no id
        updateParentCat()
        packagesInfo.packages(pack.id.name) = MutMap(pack.id.version -> pack)
        packagesInfo.packagesCategory(pack.id) = parentCategoryId
      case Some(versionMap) => //check for the version
        versionMap.get(pack.id.version) match {
          case None => //add that version
            updateParentCat()
            packagesInfo.packages(pack.id.name)(pack.id.version) = pack
            packagesInfo.packagesCategory(pack.id) = parentCategoryId
          case Some(v) => //error, policy package version already exsits
            logger.error("Ignoring package for policy with ID %s and root directory %s because an other policy is already defined with that id and root path %s".format(
              pack.id, packageRootDirectory.getAbsolutePath, packagesInfo.packagesCategory(pack.id).toString))
        }
    }
    
  }

  /**
   * Process the categoryRootDirectory which must be a category directory.
   * - if the file categoryDescriptorName exists, use it to find name, description,
   *   else use file name
   * - process all sub-directories
   * - ignore all other files
   *
   * @param packageRootDirectory
   * @param packagesInfo
   */
  private def processCategory(parentCategoryId: PolicyPackageCategoryId, categoryRootDirectory: File, packagesInfo: InternalPackagesInfo): Unit = {

    //build a category without children from file name and path
    //    def categoryFromFile(f: File)(name: String = f.getName, desc: String = "", system: Boolean = false) = PolicyPackageCategory(
    //      id = parentCategoryId / f.getName, name = name, description = desc, subCategoryIds = SortedSet(), packageIds = SortedSet(), isSystem = system)

    val categoryDescriptor = new File(categoryRootDirectory, categoryDescriptorName)

    //built the category
    var (name, desc, system) = {
      if (categoryDescriptor.exists && categoryDescriptor.isFile && categoryDescriptor.canRead) {
        logger.debug("Reading package category information from %s".format(categoryDescriptor.getAbsolutePath))
        try {
          val xml = loadDescriptorFile(categoryDescriptor)
          val name = Utils.??!((xml \\ "name").text).getOrElse(categoryRootDirectory.getName)
          val desc = Utils.??!((xml \\ "description").text).getOrElse("")
          val system = (Utils.??!((xml \\ "system").text).getOrElse("false")).equalsIgnoreCase("true")

          (name, desc, system)

        } catch {
          case e: Exception =>
            logger.error("Error when processing category descriptor %s, fail back to simple information".format(categoryDescriptor.getAbsolutePath))
            (categoryRootDirectory.getName, "", false)
        }
      } else {
        logger.info("No package category descriptor '%s' for directory '%s', using path for category information".format(categoryDescriptorName, categoryRootDirectory.getAbsolutePath))
        (categoryRootDirectory.getName, "", false)
      }
    }

    //add the category as a child to its parent (if not root) and as new category
    val newParentId = {
      if (null == parentCategoryId) {
        packagesInfo.rootCategory = Some(RootPolicyPackageCategory(name, desc, isSystem = system))
        RootPolicyPackageCategoryId
      } else {
        val category = SubPolicyPackageCategory(
          id = parentCategoryId / categoryRootDirectory.getName, name = name, description = desc, isSystem = system)
        parentCategoryId match {
          case RootPolicyPackageCategoryId =>
            val parent = packagesInfo.rootCategory.getOrElse(throw new RuntimeException("Missing root policy category"))
            packagesInfo.rootCategory = Some(parent.copy(subCategoryIds = parent.subCategoryIds + category.id))
          case sid: SubPolicyPackageCategoryId =>
            val parent = packagesInfo.subCategories(sid)
            packagesInfo.subCategories(parent.id) = parent.copy(subCategoryIds = parent.subCategoryIds + category.id)
        }

        packagesInfo.subCategories(category.id) = category
        category.id
      }
    }

    //process sub-directories
    categoryRootDirectory.listFiles.foreach { f =>
      if (f.isDirectory) {
        processDirectory(newParentId, f, packagesInfo)
      }
    }
  }

  /**
   * Load a descriptor document.
   * @param file : the full filename
   * @return the xml representation of the file
   */
  private def loadDescriptorFile(file: File): Elem = {
    val doc =
      try {
        XML.loadFile(file)
      } catch {
        case e: SAXParseException =>0
          throw new ParsingException("Unexpected issue with the descriptor file %s: %s".format(file,e.getMessage))
        case e: java.net.MalformedURLException =>
          throw new ParsingException("Descriptor file not found: " + file)
      }

    if (doc.isEmpty) {
      throw new ParsingException("Error when parsing descriptor file: '%s': the parsed document is empty".format(file))
    }

    doc
  }
}