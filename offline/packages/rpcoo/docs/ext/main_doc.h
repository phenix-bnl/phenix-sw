/*! 

	\mainpage 
	\section  interface Interface Objects

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

  \section event display classes

  <a href="group__display.html">Display classes</a> are root 3D objects interfaced 
	with RPC offline objects for 3D event display.

*/

/*! @defgroup container Interface Object Containers

Interface Objects are stored in keyed containers.  The use of keyed containers 
that support iteration (generalized looping over variable length collections) 
mitigates many of the well know problems with storing and accessing data
members in fixed length arrays.

*/

/*! @defgroup interface Interface Objects

Interface objects provide data communication between analysis modules.
Current requirements for interface objects are listed below.

<ul>
<li> Abstract base class with only pure virtual functions (for schema evolution)
<li> Interface objects publicly inherit from the PHKey class
<li> No user defined templatized data members (compatibility with ROOT I/O)
<li> Support for std::vectors of basic types only (compatibility with ROOT I/O)
</ul>

*/

/*! @defgroup modules Analysis modules*/

/*! @defgroup display 3D event display*/
