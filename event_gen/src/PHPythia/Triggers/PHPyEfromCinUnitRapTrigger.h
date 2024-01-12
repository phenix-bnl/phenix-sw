#ifndef __PHPYEFROMCinUnitRapTRIGGER_H__
#define __PHPYEFROMCinUnitRapTRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyEfromCinUnitRapTrigger.h
   \brief trigger module to select events that contains electron from 
   \brief heavy flavor in +- one unit of pseudorapidity
   \author Sasha Lebedev <lebedev@iastate.edu>
*/

//!  trigger module to select events that contains an electron from HF in +- one unit of pseudorapidity
class PHPyEfromCinUnitRapTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyEfromCinUnitRapTrigger( const std::string& name = "PHPyEfromCinUnitRapTrigger" );
  
  //! destructor 
  virtual ~PHPyEfromCinUnitRapTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  bool EfromCinUnitRap( PHPythiaContainer *phpylist );


};

#endif	
