package org.eliteforemacs.util;

import java.sql.*;
import javax.sql.*;
import javax.naming.*;
import java.util.*;

public class Util 
{
  public static String getTagValue(String xmlString,String tagName)
  {
    try
      {
	int index=xmlString.indexOf("<"+tagName+">");
	if (index==-1)
	  {
	    index=xmlString.indexOf("<"+tagName+" ");
	  } // end of if ()

	int tagLength=index+2+tagName.length();

	return xmlString.substring(tagLength,xmlString.indexOf("</"+tagName+">"));

      } catch (StringIndexOutOfBoundsException e)
	{
	  return null;
	} // end of try-catch
  }

  public static Connection getDatabaseConnection() throws Exception
  {
    Context ctx = new InitialContext();
    if (ctx==null)
      {
	System.out.println("InitialContext is null");	
	return null;
      } // end of if ()
    
    DataSource ds1 = (DataSource)ctx.lookup("java:comp/env/jdbc/EliteForEmacsDB");
    if (ds1 == null) 
      {
	System.out.println("DataSource java:comp/env/jdbc/EliteForEmacsDB is null");	
	return null;
      }
    return ds1.getConnection();

  }


  public static String replace (String body, String findString, String replaceString)
  {
    return replace(body,findString,replaceString,false);
  }


  public static String replace (String body, String findString, String replaceString,boolean once)
  {
    //StringBuffer newBodyBuffer = new StringBuffer (findString.length()*2);
    StringBuffer newBodyBuffer = new StringBuffer ();
    char[] chars = body.toCharArray();
    int start = 0;
    while (true) {
      int index = body.indexOf(findString, start);
      if (index == -1) break;
      newBodyBuffer.append (chars, start, index-start);
      newBodyBuffer.append (replaceString);
      start = index + findString.length();
      if(once) break;

    }
    newBodyBuffer.append (chars, start, chars.length-start);
    return newBodyBuffer.toString();
  }


  //get value from emacs struct index is zero based index of
  //struct element
  public static String getStructValue(String struct, int index)
  {
    Vector vr=splitStruct(struct);

    String value=(String)vr.elementAt(index);
    /*
      if (value.startsWith("["))
      {
      int i=index+1;
      StringBuffer sb=new StringBuffer(value);
      while (!value.endsWith("]"))
      {
      value=(String)vr.elementAt(i);
      if (value.startsWith("["))
      {
      value="]";
      continue;
      } // end of if ()
	    
      sb.append(' ');
      sb.append(value);
      i++;
      } // end of while ()
      value=sb.toString();
      } // end of if ()
    */
    value=replace(value,"[","");
    value=replace(value,"]","");
    return value;

  }

  public static Vector splitStruct(String struct)
  {
    Vector vr=new Vector();
    if (struct.startsWith("[") && struct.endsWith("]"))
      {
	struct=struct.substring(1,struct.length()-1);	
      } // end of if ()
   
    StringTokenizer stok=new StringTokenizer(struct," ");
    while (stok.hasMoreTokens())
      {
	String tok=stok.nextToken();
	if (tok.startsWith("["))
	  {
	    StringBuffer sb=new StringBuffer();


	    while (!tok.endsWith("]"))
	      {
		sb.append(tok);
		sb.append(' ');
		tok=stok.nextToken();		
	      } // end of while ()
	    sb.append(replace(tok,"]",""));//add last element

	   
	    tok=sb.toString();
	  } // end of if ()
	
	vr.addElement(tok);
      } // end of while ()

    return vr;

  }

  public static Vector splitString(String string)
  {
    Vector vr=new Vector();
    StringTokenizer stok=new StringTokenizer(string," ");
    while (stok.hasMoreTokens())
      {
	vr.addElement(stok.nextToken());
      } // end of while ()

    return vr;

  }

  //update string representation of emacs vector
  public static String updateArray(String array,int indexToUpdate, String newValue)
  {
    StringBuffer sb=new StringBuffer();
    Vector vr=splitString(array);
    for (int i=0;i<vr.size();i++)
      {
	if (i==indexToUpdate)
	  {
	    sb.append(newValue);
	  } // end of if ()
	else
	  {
	    sb.append((String)vr.elementAt(i));
	  } // end of else
	
	sb.append(' ');
      } // end of for ()
    return sb.toString().trim();
  }

}
