#ifndef __PHPYNPARTCATRIGGER_H__
#define __PHPYNPARTCATRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyNpartCATrigger.h
   \brief trigger module to select events that contains at least 2 charged particles in the Central Arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2013/06/06 15:48:44 $
*/

 class PHAcceptParticleCentralArm;

//!  trigger module to select events that contains an electron from B in the central arm acceptance
class PHPyNpartCATrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyNpartCATrigger( const std::string& name = "PHPyNpartCATrigger" );
  
  //! destructor 
  virtual ~PHPyNpartCATrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  void set_min_pT(float a)  {min_pT=a;}
  void set_min_Npart(int a) {min_Npart=a;}

  protected:
  
  bool NpartInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

  double min_pT;
  int min_Npart;
};

#endif	
