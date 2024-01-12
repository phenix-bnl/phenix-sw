#ifndef __PHPYEFROMBinUnitRapTRIGGER_H__
#define __PHPYEFROMBinUnitRapTRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyEfromBinUnitRapTrigger.h
   \brief trigger module to select events that contains electron from 
   \brief heavy flavor in +- one unit of pseudorapidity
   \author Sasha Lebedev <lebedev@iastate.edu>
*/

//!  trigger module to select events that contains an electron from HF in +- one unit of pseudorapidity
class PHPyEfromBinUnitRapTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyEfromBinUnitRapTrigger( const std::string& name = "PHPyEfromBinUnitRapTrigger" );
  
  //! destructor 
  virtual ~PHPyEfromBinUnitRapTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  bool EfromBinUnitRap( PHPythiaContainer *phpylist );


};

#endif	
