package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
get commanders
*/
public class Command_commanders extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //get commanders

    //get commander from database
    Connection conn=Util.getDatabaseConnection();

    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select data from commanders order by created asc");
    if(!rst.next())
      {
	//no market data
	//return fluct for so emacs can generate market info
	conn.close();
	return "NO_COMMANDERS";
      }
    rst.beforeFirst();
    StringBuffer sb=new StringBuffer("(");
    while (rst.next())
      {
	sb.append(' ');
	sb.append(rst.getObject(1).toString());
	sb.append(' ');
	
      } // end of while ()
    sb.append(" )");
      
    
    conn.close();
    return sb.toString();
  }
  
}
