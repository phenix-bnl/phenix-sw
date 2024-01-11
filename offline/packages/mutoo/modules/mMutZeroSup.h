// $Id: mMutZeroSup.h,v 1.3 2009/09/24 10:36:56 hpereira Exp $
#ifndef __MMUTZEROSUP_HH__
#define __MMUTZEROSUP_HH__

#include <MUTOO_FEM.h>
#include <TMutHitMap.h>
#include <PHTimeServer.h>

#include <boost/array.hpp>
#include <mMutZeroSupPar.h>
#include <iostream>

class MutCalibStrip;
class PHCompositeNode;

/*! \ingroup modules */
//! perform zero suppression on hits based on signal shape
/*! 
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutZeroSupPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutHitMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
</tr>
<tr>
</tr>
<tr>
</tr>
</table>
*/

class mMutZeroSup
{
 public: 

  //! creator
  mMutZeroSup();
  
  //! destructor 
  virtual ~mMutZeroSup(){}
  
  //! event method 
  virtual PHBoolean event(PHCompositeNode*);

  //! print summary of how-many hits have been removed
  void print_summary( std::ostream& out = std::cout ) const;
  
 private:  

  //! returns true if hit has to be removed
  bool is_hit_zero_suppressed( TMutHitMap::const_pointer hit_ptr);

  //! stores local pointers to interface objects  
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! parameter table
  const mMutZeroSupPar* _mod_par;
  
  //! hits
  TMutHitMap* _hit_map;		

  //! calibration object
  MutCalibStrip* _calib;		     

  //! Timer
  PHTimeServer::timer _timer;

  //! summary total number of submited hits
  boost::array< int, MUTOO::NumberOfArms*MUTOO::NumberOfStations > _total_hits;
  
  //! total number of rejected hits
  boost::array< int, MUTOO::NumberOfArms*MUTOO::NumberOfStations > _rejected_hits;
  
};

#endif /* __mMutZeroSup_HH__ */







