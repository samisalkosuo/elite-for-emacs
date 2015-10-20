package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
fluct
*/
public class Command_fluct extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    return String.valueOf(Fluct.getInstance().getFluct());

  }

}
