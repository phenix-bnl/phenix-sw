/*! @defgroup interface Interface Objects
  
  Interface objects provide data communication between analysis modules.
  The motivation for defining such objects is to facilitate modularity
  in the analysis software development and enable different developers
  to simultaneously contribute to the project as a whole.  Current
  requirements for MUID interface objects are listed below.
  
  <ul>
  <li> Abstract base class with only pure virtual functions (for schema evolution)
  <li> Interface objects publicly inherit from the PHKey class
  <li> No user defined templatized data members (compatibility with ROOT I/O)
  <li> Support for std::vectors of basic types only (compatibility with ROOT I/O)
  </ul>
  
*/















