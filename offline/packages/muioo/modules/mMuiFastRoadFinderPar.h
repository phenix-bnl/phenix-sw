#ifndef mMuiFastRoadFinderPar_h
#define mMuiFastRoadFinderPar_h

#include "TMuiParBase.h"

#include <iostream>
#include<string>

//!  Runtime parameter object for mMuiFastRoadFinder analysis module
/*! 
*/
class mMuiFastRoadFinderPar : public TMuiParBase
{
  
 public: 
  

  //! tigger modes
  enum Mode { Shallow, Deep, DeepShallow, DeepDeep }; 

  //! number of trigger modes
  enum{NMODES = 4};
  
  //! contructor
  mMuiFastRoadFinderPar():
    _use_hadron_trigger(false)
    {}

  //! hadron shallow trigger
  void set_use_hadron_trigger( bool value )
  { _use_hadron_trigger = value; }
    
  //! hadron shallow trigger
  bool get_use_hadron_trigger( void ) const
  { return _use_hadron_trigger; }
  
  //! print
  void print(std::ostream& os = std::cout) const 
  {
    MUIOO::PRINT( os, "mMuiFastRoadFinderPar::print" );
    os << " _use_hadron_trigger " << _use_hadron_trigger << std::endl;
    MUIOO::PRINT( os, "**" );
  }
  
  private:
  
  //! hadron trigger
  bool _use_hadron_trigger;
  
};

#endif
