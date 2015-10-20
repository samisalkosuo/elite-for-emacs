package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
Save commander command
*/
public class Command_save extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //get commander
    //String commanderData=new String(Base64.decode(Util.getTagValue(commandXml,"commander")));
    String commanderData=Util.getTagValue(commandXml,"commander");
    //System.out.println(commanderData);
    String name=Util.replace(Util.getStructValue(commanderData,2),"\"","");;
    
    //update database
    Connection conn=Util.getDatabaseConnection();

    PreparedStatement pstmt = conn.prepareStatement("update commanders set data='"+commanderData+"' where name='"+name+"'");          
    pstmt.executeUpdate();

    conn.close();
    return "OK";
  }

}
