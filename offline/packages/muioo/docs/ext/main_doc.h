/*! 

  \mainpage
  \section intro Introduction

  \section interface Interface Objects

  <a href="group__interface.html">Interface Objects</a>
  provide data communication between analysis modules. The motivation for 
  defining such objects is to facilitate modularity in the analysis thus
  enabling different developers to simultaneously contribute to the project 
  as a whole.

  \section container Containers

  <a href="group__container.html">Interface Objects Containers</a>
  manage collections of interface objects.  The general idea is that these 
  classes provide safe access to interface objects and an easy way to get
  at subsets of Interface Objects associated with a given portion of the 
  detector.

  \section modules Analysis Modules

  <a href="group__modules.html">Analysis Modules</a> execute a specific task
  in the analysis software framework.  Analysis Modules have an Analysis Module 
  Interface (AMI) specification that describes what Interface Object Containers
  (IOCs) a module interacts with and with what privilege. 

  \section classes Class Library 

  The <a href="group__classes.html">Class Library</a> contains 
  classes with MUID specific utility. More general 
  classes can be found in mutoo.  
  Non detector specific classes are prefixed with a
  PH, Muon identifier specific classes are prefixed with a TMui. 


*/
