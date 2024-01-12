#ifndef __PHPYEFROMDYinUnitRapTRIGGER_H__
#define __PHPYEFROMDYinUnitRapTRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyEfromDYinUnitRapTrigger.h
   \brief trigger module to select events that contains electron from 
   \brief Drell-Yan process in +- one unit of pseudorapidity
   \author Sasha Lebedev <lebedev@iastate.edu>
*/

//!  trigger module to select events that contains an electron from Drell-Yan process in +- one unit of pseudorapidity
class PHPyEfromDYinUnitRapTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyEfromDYinUnitRapTrigger( const std::string& name = "PHPyEfromDYinUnitRapTrigger" );
  
  //! destructor 
  virtual ~PHPyEfromDYinUnitRapTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  bool EfromDYinUnitRap( PHPythiaContainer *phpylist );


};

#endif	
