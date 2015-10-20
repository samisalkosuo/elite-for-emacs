package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
save  market command
*/
public class Command_savelocalmarket extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    String localMarket=Util.getTagValue(commandXml,"localmarket");
    String galaxy=Util.getTagValue(commandXml,"galaxy");
    String planet=Util.getTagValue(commandXml,"planet");

    Connection conn=Util.getDatabaseConnection();

    PreparedStatement pstmt = conn.prepareStatement("insert into markets values("+galaxy+","+planet+",'"+localMarket+"')");
    pstmt.executeUpdate();

    conn.close();
    return "OK";
  }

}
