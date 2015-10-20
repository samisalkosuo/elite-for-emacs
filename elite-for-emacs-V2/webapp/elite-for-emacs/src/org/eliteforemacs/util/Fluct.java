package org.eliteforemacs.util;

import java.sql.*;
import javax.sql.*;
import javax.naming.*;
import java.util.*;

//Fluct for elite for emacs
public class Fluct  implements Runnable
{
  private static Fluct fluctInstance=null;

  private int lastrand=0;//internally
  private int fluct=0;//given to  commanders

  private boolean threadActive=true;
  private boolean nativeRandom=false;

  private Random rnd=new Random();
  private long interval=60000;


  private Fluct()
  {
    Thread t=new Thread(this);
    t.start();
  }
  
  public static Fluct getInstance()
  {
    if (fluctInstance==null)
      {
	fluctInstance=new Fluct();
      } // end of if ()

    return fluctInstance;
  }

  public void setInterval(int interval)
  {
    this.interval=interval;
  }

  public int getFluct()
  {
    //System.out.println("Returned fluct: "+fluct);    
    return fluct;
  }


  public void stopThread()
  {
    threadActive=false;
  }

  public void run()
  {
    try
      {
	fluct=getFluctDB();
	lastrand=getLastrandDB();

      }
    catch (Exception e)
      {
	e.printStackTrace();
	fluct=0;
	lastrand=0;
      } // end of try-catch

    //System.out.println(new java.util.Date()+" Fluct: "+fluct);
    while (threadActive)
      {
	try
	  {
	    Thread.currentThread().sleep(interval);

	    try
	      {
		fluct=randbyte();
		store();

	      }
	    catch (Exception e)
	      {
		e.printStackTrace();
	      } // end of try-catch


	    //System.out.println(new java.util.Date()+" Fluct: "+fluct);

	  }
	catch (InterruptedException e)
	  {
	  
	  } // end of try-catch
      
      } // end of while ()
  
  }

  private int randbyte()
  {
    int r;
    if (nativeRandom)
      {
	r=rnd.nextInt();
      } // end of if ()
    else
      {
	r = (((((((((((lastrand << 3) - lastrand) << 3)
		    + lastrand) << 1) + lastrand) << 4)
		- lastrand) << 1) - lastrand) + 0xe60)
      & 0x7fffffff;

      } // end of else
    

    lastrand=r-1;

    return r & 0xFF;

  }


  private int getFluctDB() throws Exception
  {
    //get commander from database
    Connection conn=Util.getDatabaseConnection();

    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select fluct from fluct");

    Object obj=new Integer(0);
    if (rst.next())
      {
	obj=rst.getObject(1);
	
      } // end of if ()
    //System.out.println(obj.getClass().getName());

    conn.close();
    return ((Integer)obj).intValue();
  }

  private int getLastrandDB() throws Exception
  {
    //get commander from database
    Connection conn=Util.getDatabaseConnection();

    Statement stmt = conn.createStatement();
    ResultSet rst = stmt.executeQuery("select lastrand from fluct");

    Object obj=new Integer(0);
    if (rst.next())
      {
	obj=rst.getObject(1);
	
      } // end of if ()
    
    //System.out.println(obj.getClass().getName());

    conn.close();
    return ((Integer)obj).intValue();
  }

  private void store() throws Exception
  {
    Connection conn=Util.getDatabaseConnection();

    PreparedStatement pstmt = conn.prepareStatement("update fluct set fluct="+String.valueOf(fluct)+",lastrand="+String.valueOf(lastrand));
    pstmt.executeUpdate();
    //delete market info so that markets gets updated the next time someone buys/sells/views market
    pstmt = conn.prepareStatement("delete from markets");
    pstmt.executeUpdate();

    conn.close();

  }

}
