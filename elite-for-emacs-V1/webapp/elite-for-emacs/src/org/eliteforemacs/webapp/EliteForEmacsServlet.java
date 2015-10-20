package org.eliteforemacs.webapp;

import java.util.*;
import java.text.*;
import java.net.*;
import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import org.eliteforemacs.util.*;

/**
 * Elite for EMACS server.


   version 0.1

 */
public class EliteForEmacsServlet extends HttpServlet
{

  public void init(ServletConfig config)  throws ServletException
  {
    super.init(config);

    try
    {
      Fluct.getInstance();
    }
    catch(Exception e)
    {
      e.printStackTrace();
    }
  }

  public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException  {
    
    response.setContentType("text/plain");
    PrintWriter pw=response.getWriter();
    try
    {
      //command is xml formatted
      String command=request.getParameter("command");
      
      //System.out.println(command);
      //System.out.println(Base64.decode(command));
      //String commandXml=new String(Base64.decode(command));
      String commandXml=command;
      //System.out.println(commandXml);
      command=Util.getTagValue(commandXml,"command");
      //System.out.println(new Date()+": "+command);
      
      //act on command, command must be in another class named:
      //org.eliteforemacs.webapp.command_<command>
      //System.out.println("command: "+command);
      String output=((CommandInterface)Class.forName("org.eliteforemacs.webapp.Command_"+command).newInstance()).doCommand(commandXml);

      pw.print(output);
      //do not use println as the last print
    }
    catch(Exception e)
    {
      e.printStackTrace();
      response.sendError(response.SC_INTERNAL_SERVER_ERROR,e.toString());
    }
  }

  public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException  {
      doGet(request,response);

  }


  public long getLastModified(HttpServletRequest req)
  {
    return -1;
  }
}
