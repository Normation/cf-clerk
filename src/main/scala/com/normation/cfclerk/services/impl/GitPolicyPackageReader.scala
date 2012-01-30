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
import net.liftweb.common._
import scala.collection.mutable.{ Map => MutMap }
import scala.collection.SortedSet
import com.normation.utils.Utils
import scala.collection.immutable.SortedMap
import java.io.InputStream
import java.io.FileInputStream
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.lib.RepositoryBuilder
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.{Constants => JConstants}
import org.eclipse.jgit.revwalk.RevTree
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.treewalk.filter.TreeFilter
import scala.collection.mutable.{ Map => MutMap }
import org.eclipse.jgit.errors.StopWalkException
import org.eclipse.jgit.events.RefsChangedListener
import org.eclipse.jgit.events.RefsChangedEvent
import scala.collection.mutable.Buffer
import com.normation.cfclerk.xmlparsers.PolicyParser
import com.normation.cfclerk.services._
import com.normation.exceptions.TechnicalException
import org.eclipse.jgit.treewalk.FileTreeIterator
import org.eclipse.jgit.diff.DiffEntry
import scala.collection.JavaConversions._
import org.eclipse.jgit.diff.DiffFormatter

/**
 * 
 * A PolicyPackageReader that reads policy packages from
 * a git repository.  
 * 
 * The root directory on the git repos is assumed to be
 * a parent directory of the root directory of the policy
 * template library. For example, if "policy-template" is
 * the root directory of the PT lib:
 * - (1) /some/path/policy-templates/.git [=> OK]
 * - (2) /some/path/
 *             |- policy-templates
 *             ` .git  [=> OK]
 * - (3) /some/path/
 *             |-policy-templates
 *             ` sub/dirs/.git [=> NOT OK]
 * 
 * The relative path from the parent of .git to ptlib root is given in
 * the "relativePathToGitRepos" parameter. 
 * 
 * The convention used about policy packages and categories are the
 * same than for the FSPolicyPackageReader, which are:
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
 * @parameter relativePathToGitRepos
 *   The relative path from the root directory of the git repository to
 *   the root directory of the policy template library. 
 *   If the root directory of the git repos is in the PT lib root dir, 
 *   as in example (1) above, None ("") must be used. 
 *   Else, the relative path without leading nor trailing "/" is used. For
 *   example, in example (2), Some("policy-templates") must be used. 
 */

class GitPolicyPackagesReader(
  policyParser: PolicyParser,
  revisionProvider:GitRevisionProvider,
  repo:GitRepositoryProvider,
  val policyDescriptorName: String, //full (with extension) conventional name for policy descriptor
  val categoryDescriptorName: String, //full (with extension) name of the descriptor for categories
  val relativePathToGitRepos: Option[String]
  ) extends PolicyPackagesReader with Loggable {

  reader =>

  //denotes a path for a package, so it starts by a "/"
  //and is not prefixed by relativePathToGitRepos
  private[this] case class PackagePath(path:String)
    
  //the path of the PT lib relative to the git repos
  //withtout leading and trailing /. 
  val canonizedRelativePath = relativePathToGitRepos.flatMap { path => 
      val p1 = path.trim
      val p2 = if(p1(0) == '/') p1.tail else p1
      val p3 = if(p2(p2.size -1) == '/') p2.substring(0, p2.size-1) else p2
      
      if(p3.size == 0) { //can not have Some("/") or Some("")
        None
      } else {
        Some(p3)
      }
  }

  /*
   * Change a path relative to the Git repository to a path relative
   * to the root of policy template library. 
   * As it is required that the git repository is in  a parent of the
   * ptLib, it's just removing start of the string.
   */
  private[this] def toPolicyTemplatePath(path:String) : PackagePath = {
    canonizedRelativePath match {
      case Some(relative) if(path.startsWith(relative)) => 
        PackagePath(path.substring(relative.size, path.size))
      case _ => PackagePath("/" + path)
    }
  }  
  
  
  private[this] var currentPackagesInfoCache : PackagesInfo = processRevTreeId(revisionProvider.currentRevTreeId)
  private[this] var nextPackagesInfoCache : (ObjectId,PackagesInfo) = (revisionProvider.currentRevTreeId, currentPackagesInfoCache)
  //a non empty list IS the indicator of differences between current and next
  private[this] var modifiedPolicyPackagesCache : Seq[PolicyPackageId] = Seq()

  override def getModifiedPolicyPackages : Seq[PolicyPackageId] = {
    val nextId = revisionProvider.getAvailableRevTreeId
    if(nextId == nextPackagesInfoCache._1) modifiedPolicyPackagesCache
    else reader.synchronized { //update next and calculate diffs      
      val nextPackagesInfo = processRevTreeId(nextId)
      
      //get the list of ALL valid package infos, both in current and in next version,
      //so we have both deleted package (from current) and new one (from next)
      val allKnownPackagePaths = getPolicyPackagePath(currentPackagesInfoCache) ++ getPolicyPackagePath(nextPackagesInfo)

      val diffFmt = new DiffFormatter(null)
      diffFmt.setRepository(repo.db)
      val diffPathEntries : Set[PackagePath] = 
        diffFmt.scan(revisionProvider.currentRevTreeId,nextId).flatMap { diffEntry => 
          Seq(toPolicyTemplatePath(diffEntry.getOldPath), toPolicyTemplatePath(diffEntry.getNewPath))
        }.toSet
      diffFmt.release
      
      val modifiedPackagePath = scala.collection.mutable.Set[PackagePath]()
      /*
       * now, group diff entries by policyPackageId to find which were updated
       * we take into account any modifications, as anything among a 
       * delete, rename, copy, add, modify must be accepted and the matching
       * datetime saved. 
       */
      diffPathEntries.foreach { path =>
        allKnownPackagePaths.find { packagePath =>
          path.path.startsWith(packagePath.path)
        }.foreach { packagePath =>
          modifiedPackagePath += packagePath
        } //else nothing
      }
 
      //Ok, now rebuild PolicyPackage !
      modifiedPolicyPackagesCache = modifiedPackagePath.map { s => 
        val parts = s.path.split("/")
        PolicyPackageId(PolicyPackageName(parts(parts.size - 2)), PolicyVersion(parts(parts.size - 1)))
      }.toSeq
      nextPackagesInfoCache = (nextId, nextPackagesInfo)
      modifiedPolicyPackagesCache
    }
  }


  override def getTemplateContent[T](templateName: TmlId)(useIt : Option[InputStream] => T) : T = {
    //build a treewalk with the path, given by TmlId.toString
    val path = templateName.toString + Tml.templateExtension
    //has package id are unique among the whole tree, we are able to find a
    //template only base on the packageId + name. 
    
    var is : InputStream = null
    try {
      useIt { 
        //now, the treeWalk
        val tw = new TreeWalk(repo.db)
        tw.setFilter(new TemplateFileFilter(path))
        tw.setRecursive(true)
        tw.reset(revisionProvider.currentRevTreeId)
        var ids = List.empty[ObjectId]
        while(tw.next) {
          ids = tw.getObjectId(0) :: ids
        }
        ids match {
          case Nil => 
            logger.error("Template with id %s was not found".format(templateName))
            None
          case h :: Nil =>
            is = repo.db.open(h).openStream
            Some(is)
          case _ => 
            logger.error("More than exactly one ids were found in the git tree for template %s, I can not know which one to choose. IDs: %s".format(templateName,ids.mkString(", ")))
            None
      } } 
    } catch {
      case ex:FileNotFoundException =>
        logger.debug( () => "Template %s does not exists".format(path),ex)
        useIt(None)
    } finally {
      if(null != is) {
        is.close
      }
    }    
  }

  /**
   * Read the policies from the last available tag. 
   * The last available tag state is given by modifiedPolicyPackages
   * and is ONLY updated by that method.
   * Two subsequent call to readPolicies without a call
   * to modifiedPolicyPackages does nothing, even if some 
   * commit were done in git repository. 
   */
  override def readPolicies : PackagesInfo = {    
    reader.synchronized {
      if(modifiedPolicyPackagesCache.nonEmpty) {
        currentPackagesInfoCache = nextPackagesInfoCache._2
        revisionProvider.setCurrentRevTreeId(nextPackagesInfoCache._1)
        modifiedPolicyPackagesCache = Seq()
      }
      currentPackagesInfoCache
    }
  }
  

  
  private[this] def processRevTreeId(id:ObjectId, parseDescriptor:Boolean = true) : PackagesInfo = {
    /*
     * Global process : the logic is completly different
     * from a standard "directory then subdirectoies" walk, because 
     * we have access to the full list of path in that RevTree. 
     * So, we are just looking for:
     * - paths which end by categoryDescriptorName:
     *   these paths parents are category path if and only
     *   if their own parent is a category
     * - paths which end by policyDescriptorName 
     *   these paths are policy version directories if and only
     *   if:
     *   - their direct parent name is a valid version number
     *   - their direct great-parent is a valid category
     *   
     * As the tree walk, we are sure that:
     * - A/B/cat.xml < A/B/C/cat.xml (and so we can say that the second is valid if the first is)
     * - A/B/cat.xml < A/B/0.1/pol.xml (and so we can say that the second is an error);
     * - A/B/cat.xml < A/B/P/0.1/pol.xml (and so we can say that the second is valid if the first is)
     * 
     * We know if the first is valid because:
     * - we are always looking for a category
     * - and so we have to found the matching catId in the category map. 
     */
      val packageInfos = new InternalPackagesInfo()
      //we only want path ending by a descriptor file

      //start to process all categories related information
      processCategories(id,packageInfos, parseDescriptor)
      
      //now, build packages
      processPolicyPackages(id,packageInfos,parseDescriptor)
    
      //ok, return the result in its immutable format 
      PackagesInfo(
        rootCategory = packageInfos.rootCategory.get,
        packagesCategory = packageInfos.packagesCategory.toMap,
        packages = packageInfos.packages.map { case(k,v) => (k, SortedMap.empty[PolicyVersion,PolicyPackage] ++ v)}.toMap,
        subCategories = Map[SubPolicyPackageCategoryId, SubPolicyPackageCategory]() ++ packageInfos.subCategories
      )
  }
  
  private[this] def processPolicyPackages(revTreeId: ObjectId, packageInfos : InternalPackagesInfo, parseDescriptor:Boolean) : Unit = {
      //a first walk to find categories
      val tw = new TreeWalk(repo.db)
      tw.setFilter(new FileTreeFilter(canonizedRelativePath, policyDescriptorName))
      tw.setRecursive(true)
      tw.reset(revTreeId)
      
      //now, for each potential path, look if the cat or policy
      //is valid
      while(tw.next) {
        val path = toPolicyTemplatePath(tw.getPathString) //we will need it to build the category id
        processPolicyPackage(repo.db.open(tw.getObjectId(0)).openStream, path.path, packageInfos, parseDescriptor)
      }
  }
  
  
  private[this] def processCategories(revTreeId: ObjectId, packageInfos : InternalPackagesInfo, parseDescriptor:Boolean) : Unit = {
      //a first walk to find categories
      val tw = new TreeWalk(repo.db)
      tw.setFilter(new FileTreeFilter(canonizedRelativePath, categoryDescriptorName))
      tw.setRecursive(true)
      tw.reset(revTreeId)
      
      
      val maybeCategories = MutMap[PolicyPackageCategoryId, PolicyPackageCategory]()
      
      //now, for each potential path, look if the cat or policy
      //is valid
      while(tw.next) {
        val path = toPolicyTemplatePath(tw.getPathString) //we will need it to build the category id
        registerMaybeCategory(tw.getObjectId(0), path.path, maybeCategories, parseDescriptor)
      }
    
      val toRemove = new collection.mutable.HashSet[SubPolicyPackageCategoryId]()
      maybeCategories.foreach { 
        case (sId:SubPolicyPackageCategoryId,cat:SubPolicyPackageCategory) =>
          recToRemove(sId,toRemove, maybeCategories)
          
        case _ => //ignore
      }
      
      //now, actually remove things
      maybeCategories --= toRemove
      
      //update packageInfos
      packageInfos.subCategories ++= maybeCategories.collect { case (sId:SubPolicyPackageCategoryId, cat:SubPolicyPackageCategory) => (sId -> cat) }
    
      var root = maybeCategories.get(RootPolicyPackageCategoryId) match {
          case None => sys.error("Missing root category")
          case Some(sub:SubPolicyPackageCategory) => sys.error("Bad type for root category, found: " + sub)
          case Some(r:RootPolicyPackageCategory) => r
        }
      
      //update subcategories
      packageInfos.subCategories.toSeq.foreach { 
        case(sId@SubPolicyPackageCategoryId(_,RootPolicyPackageCategoryId) , _ ) => //update root
          root = root.copy( subCategoryIds = root.subCategoryIds + sId )
        case(sId@SubPolicyPackageCategoryId(_,pId:SubPolicyPackageCategoryId) , _ ) =>
          val cat = packageInfos.subCategories(pId)
          packageInfos.subCategories(pId) = cat.copy( subCategoryIds = cat.subCategoryIds + sId )
      }

      //finally, update root !
      packageInfos.rootCategory = Some(root)
 
  }
  

  /**
   * We remove each category for which parent category is not defined. 
   */
  private[this] def recToRemove(
      catId:SubPolicyPackageCategoryId
    , toRemove:collection.mutable.HashSet[SubPolicyPackageCategoryId]
    , maybeCategories: MutMap[PolicyPackageCategoryId, PolicyPackageCategory]
  ) : Boolean = {
      catId.parentId match {
        case RootPolicyPackageCategoryId => false
        case sId:SubPolicyPackageCategoryId =>        
          if(toRemove.contains(sId)) {
            toRemove += catId
            true
          } else if(maybeCategories.isDefinedAt(sId)) {
            recToRemove(sId, toRemove, maybeCategories ) 
          } else {
            toRemove += catId
            true
          }
      }
  }
  
  private[this] val dummyPolicyPackage = PolicyPackage(
      PolicyPackageId(PolicyPackageName("dummy"),PolicyVersion("1.0"))
    , "dummy", "dummy", Seq(), Seq(), TrackerVariableSpec(), SectionSpec("ROOT"))
    
  private[this] def processPolicyPackage(
      is:InputStream
    , filePath:String
    , packagesInfo:InternalPackagesInfo
    , parseDescriptor:Boolean // that option is a pure optimization for the case diff between old/new commit
  ): Unit = {
    try {
      val descriptorFile = new File(filePath)
      val policyVersion = PolicyVersion(descriptorFile.getParentFile.getName)
      val policyName = PolicyPackageName(descriptorFile.getParentFile.getParentFile.getName)
      val parentCategoryId = PolicyPackageCategoryId.buildId(descriptorFile.getParentFile.getParentFile.getParent )
  
      val policyPackageId = PolicyPackageId(policyName,policyVersion)
      
      val pack = if(parseDescriptor) policyParser.parseXml(loadDescriptorFile(is, filePath), policyPackageId)
                 else dummyPolicyPackage

      def updateParentCat() : Boolean = {
        parentCategoryId match {
          case RootPolicyPackageCategoryId => 
            val cat = packagesInfo.rootCategory.getOrElse(
                throw new RuntimeException("Can not find the parent (root) caterogy %s for package %s".format(descriptorFile.getParent, policyPackageId))
            )
            packagesInfo.rootCategory = Some(cat.copy(packageIds = cat.packageIds + policyPackageId ))
            true
            
          case sid:SubPolicyPackageCategoryId =>
            packagesInfo.subCategories.get(sid) match {
              case Some(cat) =>
                packagesInfo.subCategories(sid) = cat.copy(packageIds = cat.packageIds + policyPackageId )
                true
              case None =>
                logger.error("Can not find the parent caterogy %s for package %s".format(descriptorFile.getParent, policyPackageId))
                false
            }
        }
      }
  
      //check that that package is not already know, else its an error (by id ?)
      packagesInfo.packages.get(policyPackageId.name) match {
        case None => //so we don't have any version yet, and so no id
          if(updateParentCat) {
            packagesInfo.packages(policyPackageId.name) = MutMap(policyPackageId.version -> pack)
            packagesInfo.packagesCategory(policyPackageId) = parentCategoryId
          }
        case Some(versionMap) => //check for the version
          versionMap.get(policyPackageId.version) match {
            case None => //add that version
              if(updateParentCat) {
                packagesInfo.packages(policyPackageId.name)(policyPackageId.version) = pack
                packagesInfo.packagesCategory(policyPackageId) = parentCategoryId
              }
            case Some(v) => //error, policy package version already exsits
              logger.error("Ignoring package for policy with ID %s and root directory %s because an other policy is already defined with that id and root path %s".format(
                  policyPackageId, descriptorFile.getParent, packagesInfo.packagesCategory(policyPackageId).toString)
              )
          }
      }
    } catch {
      case e : VersionFormatException => logger.error("Ignoring policy package '%s' because the version format is incorrect".format(filePath),e)
      case e : ParsingException => logger.error("Ignoring policy package '%s' because the descriptor file is malformed".format(filePath),e)
      case e : Exception =>
        logger.error("Error when processing policy package '%s'".format(filePath),e)
        throw e
    }
  }

  /**
   * Register a category, but whithout checking that its parent
   * is legal. 
   * So that will lead to an unconsistant Map of categories
   * which must be normalized before use ! 
   * 
   * If the category descriptor is here, but incorrect,
   * we assume that the directory should be considered
   * as a category, but the user did a mistake: signal it, 
   * but DO use the folder as a category. 
   */
  private[this] def registerMaybeCategory(
      descriptorObjectId:ObjectId
    , filePath:String
    , maybeCategories:MutMap[PolicyPackageCategoryId, PolicyPackageCategory]
    , parseDescriptor:Boolean // that option is a pure optimization for the case diff between old/new commit
  ) : Unit = {
    
    val catPath = filePath.substring(0, filePath.size - categoryDescriptorName.size - 1 ) // -1 for the trailing slash
    
    val catId = PolicyPackageCategoryId.buildId(catPath)
    //built the category
    val (name, desc, system ) = {
      if(parseDescriptor) {
        try {
          val xml = loadDescriptorFile(repo.db.open(descriptorObjectId).openStream, filePath)
            val name = Utils.??!((xml \\ "name").text).getOrElse(catId.name.value)
            val description = Utils.??!((xml \\ "description").text).getOrElse("")
            val isSystem = (Utils.??!((xml \\ "system").text).getOrElse("false")).equalsIgnoreCase("true")
            (name, description, isSystem)
        } catch {
          case e: Exception =>
            logger.error("Error when processing category descriptor %s, fail back to simple information. Exception message: %s".
                format(filePath,e.getMessage))
            (catId.name.value, "", false)
        }
      } else { (catId.name.value, "", false) }
    } 
    
    val cat = catId match {
      case RootPolicyPackageCategoryId => RootPolicyPackageCategory(name, desc, isSystem = system)
      case sId:SubPolicyPackageCategoryId => SubPolicyPackageCategory(sId, name, desc, isSystem = system)
    }
    
    maybeCategories(cat.id) = cat
        
  }

  /**
   * Load a descriptor document.
   * @param file : the full filename
   * @return the xml representation of the file
   */
  private[this] def loadDescriptorFile(is: InputStream, filePath : String ) : Elem = {
    val doc =
      try {
        XML.load(is)
      } catch {
        case e: SAXParseException =>0
          throw new ParsingException("Unexpected issue with the descriptor file %s: %s".format(filePath,e.getMessage))
        case e: java.net.MalformedURLException =>
          throw new ParsingException("Descriptor file not found: " + filePath)
      }

    if (doc.isEmpty) {
      throw new ParsingException("Error when parsing descriptor file: '%s': the parsed document is empty".format(filePath))
    }

    doc
  }
  
  /**
   * Output the set of path for all policy packages. 
   * Root is "/", so that a package "P1" is denoted
   * /P1, a package P2 in sub category cat1 is denoted 
   * /cat1/P2, etc. 
   */
  private[this] def getPolicyPackagePath(packageInfos:PackagesInfo) : Set[PackagePath] = {
   var set = scala.collection.mutable.Set[PackagePath]()
   packageInfos.rootCategory.packageIds.foreach { p => set += PackagePath( "/" + p.toString) }
   packageInfos.subCategories.foreach { case (id,cat) =>
     val path = id.toString
     cat.packageIds.foreach { p => set += PackagePath(path + "/" + p.toString) }
   }
   set.toSet
  }
}