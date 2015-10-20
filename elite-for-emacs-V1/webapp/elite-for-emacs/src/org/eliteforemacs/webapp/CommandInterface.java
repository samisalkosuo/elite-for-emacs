package org.eliteforemacs.webapp;


import java.util.*;
/*
Elite online command interface. Input is xml and output is string to be returned to 
emacs client.  

Command xml. command must be 3 chars (possible bug in emacs base64 encode)
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
      "<elite-for-emacs>"
      "<command>"
      command
      "</command>"
      other xml start
      "<commander>"
      tmp
      "</commander>"
      other xml end
      "</elite-for-emacs>"    

*/
public interface CommandInterface
{
  //commandXml can be any XML
  public String doCommand(String commandXml) throws Exception;
}
