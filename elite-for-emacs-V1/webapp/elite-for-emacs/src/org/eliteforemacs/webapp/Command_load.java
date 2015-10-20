package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
Load commander command
*/
public class Command_load extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //get commander
    //String commanderData=new String(Base64.decode(Util.getTagValue(commandXml,"commander")));
    String commanderName=Util.getTagValue(commandXml,"commander-name");
    //get commander from database
    Connection conn=Util.getDatabaseConnection();

    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select data from commanders where name='"+commanderName+"'");
    if(!rst.next())
      {
	conn.close();
	return "NOT_OK";
      }

    Object obj=rst.getObject(1);
    //System.out.println(obj.getClass().getName());    

    conn.close();
    return obj.toString();
  }

}
