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
import com.normation.cfclerk.domain.IntegerVType
import com.normation.cfclerk.domain.BooleanVType


class SystemVariableSpecServiceImpl extends SystemVariableSpecService {

  private[this] val varSpecs : Seq[SystemVariableSpec] = Seq(
      SystemVariableSpec("ALLOWCONNECT"   , "List of ip allowed to connect to the node (policyserver + children if any)"
                                          , multivalued = true
      )
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
                                          , constraint = Constraint(typeName = IntegerVType(), mayBeEmpty=true)
      )
    , SystemVariableSpec("NODEROLE"       , "List of nodeConfiguration roles")
    , SystemVariableSpec("TOOLS_FOLDER"   , "Tools folder")
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
    , SystemVariableSpec("SYSLOGPORT"     , "Port to use for rsyslog (used by reports)"
                                          , multivalued = false
                                          , isUniqueVariable = true
                                          , constraint = Constraint(typeName = IntegerVType())
      )
    , SystemVariableSpec("MANAGED_NODES_NAME", "Hostname of nodes managed by the policy server"
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec("MANAGED_NODES_ID", "UUID of nodes managed by the policy server"
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec("AUTHORIZED_NETWORKS", "Networks authorized to connect to the policy server"
                                          , constraint = Constraint(mayBeEmpty=true)
      )
      // this variable may be empty, has it is not filled by rudder, but by cf-clerk
    , SystemVariableSpec("GENERATIONTIMESTAMP"
                                          , "Timestamp of the promises generation"
                                          , multivalued = false
                                          , isUniqueVariable = true
                                          , constraint = Constraint(mayBeEmpty=true)
      )
    , SystemVariableSpec("DENYBADCLOCKS"  , "Should CFEngine server accept connection from agent with a desynchronized clock?"
                                          , multivalued = false
                                          , isUniqueVariable = true
                                          , constraint = Constraint(typeName = BooleanVType, default=Some("true"))

      )
    , SystemVariableSpec("SKIPIDENTIFY"  , "Should CFEngine server skip the forward DNS lookup for node identification?"
                                          , multivalued = false
                                          , isUniqueVariable = true
                                          , constraint = Constraint(typeName = BooleanVType, default=Some("false"))

      )

  )

  private[this] val varSpecsMap = varSpecs.map(x => (x.name -> x)).toMap

  override def get(varName : String) : SystemVariableSpec = varSpecsMap(varName)
  override def getAll() : Seq[SystemVariableSpec] = varSpecs
}
