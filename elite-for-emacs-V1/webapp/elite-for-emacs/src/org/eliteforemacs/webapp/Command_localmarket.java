package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
generate market command
*/
public class Command_localmarket extends CommandBase implements CommandInterface
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
    ResultSet rst = stmt.executeQuery("select data from markets where galaxy="+galaxy+" and planet="+planet);
    if(!rst.next())
      {
	//no market data
	//return fluct for so emacs can generate market info
	conn.close();
	return "RANDBYTE "+String.valueOf(Fluct.getInstance().getFluct());
      }

    Object obj=rst.getObject(1);
    //System.out.println(obj.getClass().getName());    

    conn.close();
    return obj.toString();
  }

}
