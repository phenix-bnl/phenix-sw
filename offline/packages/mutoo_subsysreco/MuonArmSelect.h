// $Id: MuonArmSelect.h,v 1.4 2007/12/10 12:59:48 hpereira Exp $
#ifndef __MuonArmSelect_h__
#define __MuonArmSelect_h__

#include "SubsysReco.h"

#ifndef __CINT__
#include<boost/array.hpp>
#include <PHTimeServer.h>
#endif

/*!
  \file    MuonArmSelect.h  
  \ingroup supermodules
  \brief   remove all hits from map for arm wich do not match the module selection
  \author  Hugo Pereira
  \version $Revision: 1.4 $
  \date    $Date: 2007/12/10 12:59:48 $
*/

#include <MUTOO.h>

// Forward declerations
class PHCompositeNode;
class TMutHitMap;
class TMuiHitMapO;

/*!
  \class   MuonArmSelect
  \ingroup supermodules
  \brief   remove all hits from map for arm wich do not match the module selection
*/
class MuonArmSelect: public SubsysReco
{
  
  public:
  
  //! module arm selection mode
  enum MODE 
  {
    //! do nothing (default)
    NONE,
  
    //! selects only hits belonging to south arm. Remove all north arm hits
    SOUTH,  
    
    //! selects only hits belonging to north arm. Remove all south arm hits
    NORTH,
    
    //! selects only hits for the arm which fired level2 triggers. Remove all hits from the other arm
    LEVEL2
  
  };

  //! constructor
  MuonArmSelect( const char* name = "MuonArmSelect", MODE mode = NONE );

  //! destructor
  virtual ~MuonArmSelect() {}
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! end method
  int End(PHCompositeNode *topNode);
  
  //! selection mode
  void set_mode(MODE mode)
  { _mode = mode;}
  
  //! selecton mode
  MODE get_mode() const 
  { return _mode;}

  private:
  
  //! module selection mode
  MODE _mode;
  
  #ifndef __CINT__

  //! module timer
  PHTimeServer::timer _timer;
  
  //! total number of mut hits
  boost::array< int, MUTOO::NumberOfArms > _mut_hits_tot;

  //! total number of mui hits
  boost::array< int, MUTOO::NumberOfArms > _mui_hits_tot;
  
  //! number of rejected mut hits
  boost::array< int, MUTOO::NumberOfArms > _mut_hits_rejected;
  
  //! number of rejected mui hits
  boost::array< int, MUTOO::NumberOfArms > _mui_hits_rejected;
  #endif

};

#endif 
