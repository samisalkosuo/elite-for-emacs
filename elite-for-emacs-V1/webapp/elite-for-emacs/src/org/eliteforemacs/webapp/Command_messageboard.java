package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
get system messages
*/
public class Command_messageboard extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //get commander
    //String commanderData=new String(Base64.decode(Util.getTagValue(commandXml,"commander")));
    String galaxy=Util.getTagValue(commandXml,"galaxy");
    String planet=Util.getTagValue(commandXml,"planet");
    //get commander from database
    Connection conn=Util.getDatabaseConnection();

    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select commander,message,sent from messages where galaxy="+galaxy+" and planet="+planet+" order by sent asc");
    if(!rst.next())
      {
	//no market data
	//return fluct for so emacs can generate market info
	conn.close();
	return "NO_MESSAGES";
      }
    rst.beforeFirst();
    StringBuffer sb=new StringBuffer("(");
    while (rst.next())
      {
	sb.append(" (");
	sb.append("\"");
	sb.append(rst.getTimestamp(3).toString());
	sb.append("\"");
	sb.append(" ");
	sb.append("\"");
	sb.append(rst.getString(1));
	sb.append("\"");
	sb.append(" ");
	sb.append("\"");
	sb.append(rst.getString(2));
	sb.append("\"");
	sb.append(")");
	
      } // end of while ()
    sb.append(" )");
    
    
    System.out.println(getClass().getName()+": "+sb.toString());    
    
    conn.close();
    return sb.toString();
  }
  
}
