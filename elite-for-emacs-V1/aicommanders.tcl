#Tcl/Tk program for AI commanders
#include <tcl.h>



proc setCommanderPath {commanderName cnvs} {
  
  global coordList currentIndex
  set commanderList [getCommanderInfo $commanderName]
  set coordList {}
  #puts [llength commanderList]
  foreach i $commanderList {
    #puts $i
    set line [split  $i ,]
    lappend coordList [expr 2 * [lindex $line 3]] [expr 2 * [lindex $line 4]]
  }
  #puts [llength $coordList]
  #$cnvs create line 0 0 100 100  -arrow last
  set currentIndex 0

  $cnvs create line 510 0 510 510
  $cnvs create line 0 510 510 510

  return 0

}

proc showCommanderPath {commanderName cnvs} {
  
  global coordList
  set ind 0
  for {set x 0} {$x<=[expr [llength $coordList] /2 ]} {set x [expr $x+2]} {
    $cnvs create line [lindex $coordList $x] [lindex $coordList [expr $x + 1]] [lindex $coordList [expr $x + 2]] [lindex $coordList [expr $x + 3]] -arrow last
   set ind [expr $ind + 2]
 }

#

}

proc continueCommanderPath {commanderName cnvs} {
  global coordList currentIndex

  if {$currentIndex>=[expr [llength $coordList] / 2]} {
    setCommanderPath $commanderName $cnvs
  } else {
    $cnvs create line [lindex $coordList $currentIndex]  [lindex $coordList [expr $currentIndex + 1]]  [lindex $coordList [expr $currentIndex + 2]]  [lindex $coordList [expr $currentIndex + 3]] -arrow last
#  $cnvs create line [lindex $coordList $currentIndex] [lindex $coordList [expr $currentIndex + 1]] [lindex $coordList [expr $currentIndex + 2]] [lindex $coordList [expr $currentIndex + 3]] -arrow last
  set currentIndex [expr $currentIndex + 2]
  }
}


proc getCommanderInfo {commanderName} {
  set commanderList {}
  set in [open "~/commanders.csv"]
  while {![eof $in]} {
    set commanderInfo [gets $in]
    if { [string first $commanderName $commanderInfo] >-1} {
      lappend commanderList $commanderInfo
      #puts $commanderInfo
    }
  }
  close $in
  
  return $commanderList
}

proc clearCanvas {cnvs} {
  $cnvs delete all
}


#main part, executes here
set w .
wm title $w "Elite for EMACS - AI commanders"
set c [canvas .aicommanders]
pack $c

$c configure -width 510 -height 550

set nameField [entry .nameField -textvariable commanderName]
set setCommanderPathButton [button .setButton -text "Set commander path" -command "setCommanderPath \$commanderName \$c"]

set continueCommanderPathButton [button .continuepathButton -text "Continue commander path" -command "continueCommanderPath \$commanderName \$c"]

set showCommanderPathButton [button .pathButton -text "Show commander path" -command "showCommanderPath \$commanderName \$c"]

set clearButton [button .clearButton -text "Clear canvas" -command "clearCanvas \$c"]

#set f [entry .textField -text "text"]
# label $w.label -text "File name:" -width 13 -anchor w
# entry $w.entry -width 40 -textvariable fileName

pack $nameField -side left
pack $setCommanderPathButton -side left
pack $continueCommanderPathButton -side left
pack $showCommanderPathButton -side left
pack $clearButton -side left

# foreach commanderName $argv {
#   showCommanderPath $commanderName $c
# #  gets stdin
# }
