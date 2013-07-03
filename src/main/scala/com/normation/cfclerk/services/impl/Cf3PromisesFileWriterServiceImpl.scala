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
import com.normation.cfclerk.exceptions._
import com.normation.stringtemplate.language._
import com.normation.stringtemplate.language.formatter._
import java.io._
import net.liftweb.common._
import org.antlr.stringtemplate._
import org.antlr.stringtemplate.language._
import org.apache.commons.io.{IOUtils,FileUtils}
import org.joda.time._
import org.xml.sax.SAXParseException
import scala.io.Source
import scala.xml._

/**
 * The class that handles all the writing of templates
 *
 */
class Cf3PromisesFileWriterServiceImpl(
    techniqueRepository      : TechniqueRepository
  , systemVariableSpecService: SystemVariableSpecService
  ) extends Cf3PromisesFileWriterService with Loggable {

  logger.trace("Repository loaded")

  private[this] val generationTimestampVariable = "GENERATIONTIMESTAMP"

  /**
   * Compute the TMLs list to be written
   * @param container : the container of the policies we want to write
   * @param extraVariables : optional : extra system variables that we could want to add
   * @return
   */
  def prepareCf3PromisesFileTemplate(container: Cf3PolicyDraftContainer, extraSystemVariables: Map[String, Variable]): Map[TechniqueId, PreparedTemplates] = {
    val systemVars = prepareBundleVars(container)

    val techniques = techniqueRepository.getByIds(container.getAllIds)

    val tmlsByTechnique : Map[TechniqueId,Set[Cf3PromisesFileTemplateCopyInfo]] = techniques.map{ technique =>
      (
          technique.id
        , technique.templates.map(tml => Cf3PromisesFileTemplateCopyInfo(tml.id, tml.outPath)).toSet
      )
    }.toMap

    val variablesByTechnique = prepareVariables(container, systemVars ++ extraSystemVariables, techniques)

    techniques.map {technique =>
      (
          technique.id
        , PreparedTemplates(tmlsByTechnique(technique.id), variablesByTechnique(technique.id))
      )
    }.toMap
  }


  /**
   * Move the generated promises from the new folder to their final folder, backuping previous promises in the way
   * @param folder : (Container identifier, (base folder, new folder of the policies, backup folder of the policies) )
   */
  def movePromisesToFinalPosition(folders: Seq[PromisesFinalMoveInfo]): Seq[PromisesFinalMoveInfo] = {
    val newFolders = scala.collection.mutable.Buffer[PromisesFinalMoveInfo]()
    try {
      // Folders is a map of machine.uuid -> (base_machine_folder, backup_machine_folder, machine)
      for (folder @ PromisesFinalMoveInfo(containerId, baseFolder, newFolder, backupFolder) <- folders) {
        // backup old promises
        logger.debug("Backuping old promises from %s to %s ".format(baseFolder, backupFolder))
        backupNodeFolder(baseFolder, backupFolder)
        try {
          newFolders += folder

          logger.debug("Copying new promises into %s ".format(baseFolder))
          // move new promises
          moveNewNodeFolder(newFolder, baseFolder)

        } catch {
          case ex: Exception =>
            logger.error("Could not write promises into %s, reason : ".format(baseFolder), ex)
            throw ex
        }
      }
      folders
    } catch {
      case ex: Exception =>

        for (folder <- newFolders) {
          logger.info("Restoring old promises on folder %s".format(folder.baseFolder))
          try {
            restoreBackupNodeFolder(folder.baseFolder, folder.backupFolder);
          } catch {
            case ex: Exception =>
              logger.error("could not restore old promises into %s ".format(folder.baseFolder))
              throw ex
          }
        }
        throw ex
    }

  }

  /**
   * Write the current seq of template file a the path location, replacing the variables found in variableSet
   * @param fileSet : the set of template to be written
   * @param variableSet : the set of variable
   * @param path : where to write the files
   */
  override def writePromisesFiles(
      fileSet    : Set[Cf3PromisesFileTemplateCopyInfo]
    , variableSet: Seq[STVariable]
    , outPath    : String
  ): Unit = {
    try {
      val generationVariable = getGenerationVariable()

      for (fileEntry <- fileSet) {
        techniqueRepository.getTemplateContent(fileEntry.source) { optInputStream =>
          optInputStream match {
            case None => throw new RuntimeException("Error when trying to open template '%s%s'. Check that the file exists and is correctly commited in Git, or that the metadata for the technique are corrects.".
                format(fileEntry.source.toString, Cf3PromisesFileTemplate.templateExtension))
            case Some(inputStream) =>
              logger.trace("Loading template %s (from an input stream relative to %s".format(fileEntry.source, techniqueRepository))
              //string template does not allows "." in path name, so we are force to use a templateGroup by polity template (versions have . in them)
              val template = new StringTemplate(IOUtils.toString(inputStream, "UTF-8"), classOf[NormationAmpersandTemplateLexer]);
              template.registerRenderer(classOf[DateTime], new DateRenderer());
              template.registerRenderer(classOf[LocalDate], new LocalDateRenderer());
              template.registerRenderer(classOf[LocalTime], new LocalTimeRenderer());

              for (variable <- variableSet++generationVariable) {
                if ( (variable.mayBeEmpty) && ((variable.values.size == 0) || (variable.values.size ==1 && variable.values.head == "") ) ) {
                  template.setAttribute(variable.name, null)
                } else if(variable.values.size == 0) {
                  throw new VariableException("Mandatory variable %s is empty, can not write %s".format(variable.name, fileEntry.destination))
                } else {
                  logger.trace("Adding variable %s : %s values %s".format(
                      outPath + "/" + fileEntry.destination, variable.name, variable.values.mkString("[",",","]")))
                  variable.values.foreach { value => template.setAttribute(variable.name, value)
                  }
                }
              }

              // write the files to the new promise folder
              logger.debug("Create promises file %s %s".format(outPath, fileEntry.destination))
              FileUtils.writeStringToFile(new File(outPath, fileEntry.destination), template.toString)
          }
        }
      }

    } catch {
      case ex: IOException => logger.error("Writing promises error : ", ex); throw new IOException("Could not create new promises", ex)
      case ex: NullPointerException => logger.error("Writing promises error : ", ex); throw new IOException("Could not create new promises", ex)
      case ex: VariableException => logger.error("Writing promises error in fileSet " + fileSet, ex); throw ex
      case ex: Exception => logger.error("Writing promises error : ", ex); throw ex
    }

  }

  /**
   * Returns variable relative to a specific promise generation
   * For the moment, only the timestamp
   */
  private[this] def getGenerationVariable() : Seq[STVariable]= {
    // compute the generation timestamp
    val promiseGenerationTimestamp = DateTime.now()

    Seq(STVariable(generationTimestampVariable, false, Seq(promiseGenerationTimestamp)))
  }

  private[this] def prepareVariables(
      container: Cf3PolicyDraftContainer
    , systemVars: Map[String, Variable]
    , techniques: Seq[Technique]
  ) : Map[TechniqueId,Seq[STVariable]] = {

    logger.debug("Preparing the PI variables for container %s".format(container.outPath))
    val variablesValues = prepareAllCf3PolicyDraftVariables(container)

    // fill the variable
    (for {
      technique <- techniques
    } yield {
      val ptValues = variablesValues(technique.id)

      val variables:Seq[Variable] = (for {
        variableSpec <- technique.getAllVariableSpecs
      } yield {
        variableSpec match {
          case x : TrackerVariableSpec => Some(x.toVariable(ptValues(x.name).values))
          case x : SystemVariableSpec => systemVars.get(x.name) match {
              case None =>
                if(x.constraint.mayBeEmpty) { //ok, that's expected
                  logger.debug("Variable system named %s not found in the extended variables environnement ".format(x.name))
                } else {
                  logger.warn("Mandatory variable system named %s not found in the extended variables environnement ".format(x.name))
                }
                None
              case Some(sysvar) => Some(x.toVariable(sysvar.values))
          }
          case x : SectionVariableSpec => Some(x.toVariable(ptValues(x.name).values))
        }
      }).flatten

      //return STVariable in place of Rudder variables
      val stVariables = variables.map { v => STVariable(
          name = v.spec.name
        , mayBeEmpty = v.spec.constraint.mayBeEmpty
        , values = v.getTypedValues match {
            case Full(seq) => seq
            case e:EmptyBox => throw new VariableException("Wrong type of variable " + v)
          }
      ) }
      (technique.id,stVariables)
    }).toMap
  }

  /**
   * Compute the TMLs list to be written and their variables
   * @param container : the container of the policies we want to write
   * @param extraVariables : optional : extra system variables that we could want to add
   * @return
   */
  private[this] def prepareBundleVars(container: Cf3PolicyDraftContainer) : Map[String,Variable] = {
  //prepareCf3PromisesFileTemplateAndBundleVars

    logger.trace("Preparing bundle list and input list for container : %s ".format(container))

    val inputs = scala.collection.mutable.Buffer[String]() // all the include file

    // Fetch the policies configured, with the system policies first
    val policies =  techniqueRepository.getByIds(container.getAllIds).sortWith((x,y) => x.isSystem)

    for {
      tml <- policies.flatMap(p => p.templates)
    } {
//      files += Cf3PromisesFileTemplateCopyInfo(tml.name, tml.outPath)
      if (tml.included) inputs += tml.outPath
    }

    Map[String, Variable](
        // Add the built in values for the files to be included and the bundle to be executed
        {
          val variable = SystemVariable(systemVariableSpecService.get("INPUTLIST"), Seq(inputs.distinct.mkString("\"", "\",\"", "\"")))
          (variable.spec.name, variable)
        }
      , {
          val bundleSet = policies.flatMap(x => x.bundlesequence.map(x =>x.name))
          val variable = SystemVariable(systemVariableSpecService.get("BUNDLELIST"), Seq(bundleSet.mkString(", \"", "\",\"", "\"")))

//          if(value.length == 0) variable.saveValue(value)
//          else variable.saveValue(", " + value)

          (variable.spec.name, variable)
        }
    )
  }

  /**
   * Move the machine promises folder  to the backup folder
   * @param machineFolder
   * @param backupFolder
   */
  private[this] def backupNodeFolder(nodeFolder: String, backupFolder: String): Unit = {
    val src = new File(nodeFolder)
    if (src.isDirectory()) {
      val dest = new File(backupFolder)
      if (dest.isDirectory)
        // force deletion of previous backup
        FileUtils.forceDelete(dest)

      FileUtils.moveDirectory(src, dest)
    }
  }
  /**
   * Move the newly created folder to the final location
   * @param newFolder : where the promises have been written
   * @param nodeFolder : where the promises will be
   */
  private[this] def moveNewNodeFolder(sourceFolder: String, destinationFolder: String): Unit = {
    val src = new File(sourceFolder)

    logger.debug("Moving folders from %s to %s".format(src, destinationFolder))

    if (src.isDirectory()) {
      val dest = new File(destinationFolder)

      if (dest.isDirectory)
        // force deletion of previous promises
        FileUtils.forceDelete(dest)

      FileUtils.moveDirectory(src, dest)
    } else {
      logger.error("Could not find freshly created promises at %s".format(sourceFolder))
      throw new IOException("Created promises not found !!!!")
    }
  }
  /**
   * Restore (by moving) backup folder to its original location
   * @param machineFolder
   * @param backupFolder
   */
  private[this] def restoreBackupNodeFolder(nodeFolder: String, backupFolder: String): Unit = {
    val src = new File(backupFolder)
    if (src.isDirectory()) {
      val dest = new File(nodeFolder)
      // force deletion of invalid promises
      FileUtils.forceDelete(dest)

      FileUtils.moveDirectory(src, dest)
    } else {
      logger.error("Could not find freshly backup promises at %s".format(backupFolder))
      throw new IOException("Backup promises could not be found, and valid promises couldn't be restored !!!!")
    }
  }

  /**
   * Concatenate all the variables for each policy Instances.
   *
   * The serialization is done
   */
  override def prepareAllCf3PolicyDraftVariables(cf3PolicyDraftContainer: Cf3PolicyDraftContainer): Map[TechniqueId, Map[String, Variable]] = {
      /**
       * Create the value of the policyinstancevariable from the Id of the Cf3PolicyDraft and
       * the serial
       */
      def createValue(cf3PolicyDraft: Cf3PolicyDraft): String = {
        cf3PolicyDraft.id.value + "@@" + cf3PolicyDraft.serial
      }


    (for {
      // iterate over each policyName
      activeTechniqueId <- cf3PolicyDraftContainer.getAllIds
    } yield {
      val technique = techniqueRepository.get(activeTechniqueId).getOrElse(
          throw new RuntimeException("Error, can not find policy with id '%s' and version ".format(activeTechniqueId.name.value) +
              "'%s' in the policy service".format(activeTechniqueId.name.value)))
      val cf3PolicyDraftVariables = scala.collection.mutable.Map[String, Variable]()

      for {
        // over each cf3PolicyDraft for this name
        (directiveId, cf3PolicyDraft) <- cf3PolicyDraftContainer.findById(activeTechniqueId)
      } yield {
        // start by setting the directiveVariable
        val (directiveVariable, boundingVariable) = cf3PolicyDraft.getDirectiveVariable

        cf3PolicyDraftVariables.get(directiveVariable.spec.name) match {
          case None =>
              //directiveVariable.values = scala.collection.mutable.Buffer[String]()
              cf3PolicyDraftVariables.put(directiveVariable.spec.name, directiveVariable.copy(values = Seq()))
          case Some(x) => // value is already there
        }

        // Only multi-instance policy may have a policyinstancevariable with high cardinal
        val size = if (technique.isMultiInstance) { boundingVariable.values.size } else { 1 }
        val values = Seq.fill(size)(createValue(cf3PolicyDraft))
        val variable = cf3PolicyDraftVariables(directiveVariable.spec.name).copyWithAppendedValues(values)
        cf3PolicyDraftVariables(directiveVariable.spec.name) = variable

        // All other variables now
        for (variable <- cf3PolicyDraft.getVariables) {
          variable._2 match {
            case newVar: TrackerVariable => // nothing, it's been dealt with already
            case newVar: Variable =>
              if ((!newVar.spec.checked) || (newVar.spec.isSystem)) {} else { // Only user defined variables should need to be agregated
                val variable = cf3PolicyDraftVariables.get(newVar.spec.name) match {
                  case None =>
                    Variable.matchCopy(newVar, setMultivalued = true) //asIntance is ok here, I believe
                  case Some(existingVariable) => // value is already there
                    // hope it is multivalued, otherwise BAD THINGS will happen
                    if (!existingVariable.spec.multivalued) {
                      logger.warn("Attempt to append value into a non multivalued variable, bad things may happen")
                    }
                    existingVariable.copyWithAppendedValues(newVar.values)
                }
                cf3PolicyDraftVariables.put(newVar.spec.name, variable)
              }
          }
        }
      }
      (activeTechniqueId, cf3PolicyDraftVariables.toMap)
    }).toMap
  }

}
