package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
New commander command
*/
public class Command_new extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //get name and check it against database
    //if not ok, return not ok
    //if ok, save to database
    //System.out.println(getClass().getName());
    //get commander struct
    //String commanderData=new String(Base64.decode(Util.getTagValue(commandXml,"commander")));
    String commanderData=Util.getTagValue(commandXml,"commander");

    //System.out.println(commanderData);
    String name=Util.replace(Util.getStructValue(commanderData,2),"\"","");;
    //System.out.println(name);
    Connection conn=Util.getDatabaseConnection();
    if ( conn==null)
      {
	System.out.println("Connection is null.");
	return null;
      } // end of if ()
    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select name from commanders where name='"+name+"'");
    if(rst.next())
      {
	conn.close();
	return "NOT_OK";
      }

    PreparedStatement pstmt = conn.prepareStatement("insert into commanders values('"+name+"','"+commanderData+"',now())");

    pstmt.executeUpdate();


    conn.close();

    return "OK";
  }

}
