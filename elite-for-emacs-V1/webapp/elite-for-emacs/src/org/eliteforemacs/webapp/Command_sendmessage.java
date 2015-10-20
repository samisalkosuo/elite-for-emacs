package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
send new message
*/
public class Command_sendmessage extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //get name and check it against database
    //if not ok, return not ok
    //if ok, save to database
    //System.out.println(getClass().getName());
    //get commander struct
    //String commanderData=new String(Base64.decode(Util.getTagValue(commandXml,"commander")));
    String galaxy=Util.getTagValue(commandXml,"galaxy");
    String planet=Util.getTagValue(commandXml,"planet");
    String commanderName=Util.getTagValue(commandXml,"commander-name");
    String message=Util.getTagValue(commandXml,"message");
    if ( message.length()>160)      
      {
	message=message.substring(0,160);
      } // end of if ()
    
    //System.out.println(name);
    Connection conn=Util.getDatabaseConnection();
    if ( conn==null)
      {
	System.out.println("Connection is null.");
	return null;
      } // end of if ()

    //if count >=20
    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select count(*) from messages where galaxy="+galaxy+" and planet="+planet);
    int messages=0;

    if(rst.next())
      {
	messages=Integer.parseInt(rst.getObject(1).toString());
      }
    
    PreparedStatement pstmt;
    if (messages>=20)
      {
	//select min timestamp
	stmt = conn.createStatement();
	rst = stmt.executeQuery("select min(unix_timestamp(sent)) from messages where galaxy="+galaxy+" and planet="+planet);
	rst.next();
	String str=rst.getObject(1).toString();
	//mysql> ) from messages where planet=7 and galaxy=0;
	//del mintimestamp
	pstmt = conn.prepareStatement("delete from messages where galaxy="+galaxy+" and planet="+planet+" and unix_timestamp(sent)="+str);
	pstmt.executeUpdate();
      } // end of if ()
        

     pstmt = conn.prepareStatement("insert into messages values("+galaxy+","+planet+",'"+commanderName+"','"+message+"',now())");

    pstmt.executeUpdate();

    conn.close();

    return "OK";
  }

}
