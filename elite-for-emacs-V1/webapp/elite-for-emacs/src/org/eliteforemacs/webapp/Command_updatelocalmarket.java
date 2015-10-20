package org.eliteforemacs.webapp;

import java.util.*;
import java.sql.*;

import org.eliteforemacs.util.*;
/*
update market command
*/
public class Command_updatelocalmarket extends CommandBase implements CommandInterface
{
  public String doCommand(String commandXml) throws Exception
  {
    //emacs sends: command(buy/sell), item index and amount
    //gets localmarket from database and add/decrease quantities on that
    //avoids inconsistency if two commanders simultaneosly buy something in the same
    //system. however, this means that end amount is negative (not too bad..)
    System.out.println(getClass().getName());
    
    String command=Util.getTagValue(commandXml,"type");
    String galaxy=Util.getTagValue(commandXml,"galaxy");
    String planet=Util.getTagValue(commandXml,"planet");
    int item=Integer.parseInt(Util.getTagValue(commandXml,"item"));
    String amount=Util.getTagValue(commandXml,"amount");

    Connection conn=Util.getDatabaseConnection();

    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select data from markets where galaxy="+galaxy+" and planet="+planet);
    if(!rst.next())
      {
	conn.close();
	return "RANDBYTE "+String.valueOf(Fluct.getInstance().getFluct());
      }
    String localMarket=(String)rst.getObject(1);

    //get amount from market struct
    String quantities=Util.getStructValue(localMarket,1);
    System.out.println("All quantities:"+quantities);
    String quantity=Util.getStructValue(quantities,item);
    System.out.println("Quantity:"+quantity);

    String newQuantity=quantity;
    if (command.equals("buy"))
      {
	//calculate new quantity
	newQuantity=String.valueOf(Integer.parseInt(quantity)-Integer.parseInt(amount));
	
	//update market quantities
	//update database

      } // end of if ()

    if (command.equals("sell"))
      {
	newQuantity=String.valueOf(Integer.parseInt(quantity)+Integer.parseInt(amount));
	
      } // end of if ()


	System.out.println("newQuantity: "+newQuantity);
	String newQuantities=Util.updateArray(quantities,item,newQuantity);
	System.out.println("new quantities: "+newQuantities);
	StringBuffer sb=new StringBuffer();
	sb.append('[');
	sb.append(Util.getStructValue(localMarket,0));
	sb.append(' ');
	sb.append('[');
	sb.append(newQuantities);	
	sb.append(']');
	sb.append(' ');
	sb.append(' ');
	sb.append('[');
	sb.append(Util.getStructValue(localMarket,2));
	sb.append(']');
	sb.append(' ');
	sb.append(']');
	System.out.println(sb.toString());
	
	PreparedStatement pstmt = conn.prepareStatement("update markets set data='"+sb.toString()+"' where galaxy="+galaxy+" and planet="+planet);
	pstmt.executeUpdate();
    
    /*
    PreparedStatement pstmt = conn.prepareStatement("insert into markets values('"+galaxy+"','"+planet+"','"+localMarket+"')");
    pstmt.executeUpdate();
    */

    conn.close();
    return "OK";
  }

}
