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

import com.normation.cfclerk.domain.{SystemVariableSpec,Constraint}
import com.normation.cfclerk.services.SystemVariableSpecService
import scala.collection.mutable.ArrayBuffer
import net.liftweb.common._


class SystemVariableSpecServiceImpl extends SystemVariableSpecService {
  
  private[this] val varSpecs : Seq[SystemVariableSpec] = Seq(
      SystemVariableSpec("ALLOWCONNECT"   , "List of ip allowed to connect to the node (policyserver + children if any)"
                                          , multivalued = true
      )
    , SystemVariableSpec("BASE_FOLDER"    , "Base folder for the installation")
    , SystemVariableSpec("CLIENTSLIST"    , "List of agent to contact via runagent"
                                          , multivalued = true
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec("CLIENTSFOLDERS" , "List of agent to contact via runagent"
                                          , multivalued = true
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec("CMDBENDPOINT"   , "The cmdb endpoint"
                                          , multivalued  = false
      )
    , SystemVariableSpec("COMMUNITYPORT"  , "The port used by the community edition"
                                          , multivalued  = false
      )
    , SystemVariableSpec("LICENSESPAID"   , "Number of Nova licences paid"
                                          , constraint = Constraint(typeName = "integer",mayBeEmpty=true)
      )
    , SystemVariableSpec("NODEROLE"       , "List of nodeConfiguration roles")
    , SystemVariableSpec("TOOLS_FOLDER"   , "Tools folder")
    , SystemVariableSpec("BASE_FOLDER"    , "Base folder for the installation")
    , SystemVariableSpec("TOOLS_FOLDER"   , "Tools folder")
    , SystemVariableSpec("CMDBENDPOINT"   , "The cmdb endpoint"
                                          , multivalued = false
      )
    , SystemVariableSpec("DAVUSER"   	  , "Username for webdav user"
                                          , multivalued = false
      )
    , SystemVariableSpec("DAVPASSWORD"    , "Password for webdav user"
                                          , multivalued = false
      )
    , SystemVariableSpec("INPUTLIST"      , "Input list")
    , SystemVariableSpec("BUNDLELIST"     , "Bundle list")
    , SystemVariableSpec("NOVA"           , "The Cfengine Nova agent"
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec("COMMUNITY"      , "The Cfengine Community agent"
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec(
        			"SHARED_FILES_FOLDER"       , "The path to the shared files folder"
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    
    
  )
  
  private[this] val varSpecsMap = varSpecs.map(x => (x.name -> x)).toMap
  
  override def get(varName : String) : SystemVariableSpec = varSpecsMap(varName)
  override def getAll() : Seq[SystemVariableSpec] = varSpecs
}
