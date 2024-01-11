#ifndef __MMUIROADASSOCMODULE_HH__
#define __MMUIROADASSOCMODULE_HH__

#include<PHTimer.h>
#include<TMutTrkMap.h>
#include<TMuiRoadMapO.h>
#include<mMuiRoadAssocPar.h>

class mMuiRoadAssocPar;

/**
 * Take our track and a list of candidate roads, and pick the
 * road closest to the track.
 */

/*! \ingroup modules */

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
<td> const mMutMyModulePar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>

INSERT ADDITIONAL IOC SPECIFICATIONS HERE

</table>
*/

class mMuiRoadAssoc
{
 public: 

  mMuiRoadAssoc(); 
  virtual ~mMuiRoadAssoc(){} 
  virtual PHBoolean event(PHCompositeNode*);

 private:  

  // private methods:

  void set_interface_ptrs(PHCompositeNode* top_node);
  double distance( PHPoint p1, PHPoint p2 );

  // Interface pointers
  const mMuiRoadAssocPar* _mod_par;           // parameter table
  TMutTrkMap* _trk_map;                       // IOC
  TMuiRoadMapO* _road_map;                    // IOC

  // Timer
  PHTimer _timer;

};

#endif /* __MMUIROADASSOC_HH__ */






