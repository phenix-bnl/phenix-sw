// $Id: mMuiSlowSim.h,v 1.1 2006/04/22 01:59:14 hpereira Exp $
#ifndef __MMUISLOWSIMMODULE_HH__
#define __MMUISLOWSIMMODULE_HH__

/*!
  \file    mMuiSlowSim.h
  \brief   muid simulation module. Generate TMuiMCHit from pisa hits
  \author  C. Zhang
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:14 $
*/

#include<PHTimeServer.h>

// include IOC header.
#include<TMuiMCHitMapO.h>
#include<TMutMCTrkMap.h>

// include PISA staff table.
#include<munhitsWrapper.h>

#include<mMuiSlowSimPar.h>

class PHCompositeNode;

/**
 * MUID slow simulator module
 */

/*! \ingroup modules */
/*!

INSERT MODULE DESCRIPTION HERE 

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMuiSlowSimPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>

INSERT ADDITIONAL IOC SPECIFICATIONS HERE

</table>
*/

class mMuiSlowSim
{
 public: 

  mMuiSlowSim(); 
  virtual ~mMuiSlowSim(){}
  virtual PHBoolean event(PHCompositeNode*);

 private:  

  // private methods
  //
  void set_interface_ptrs(PHCompositeNode* top_node);

  void digitize();
  
  void associate_mctrk();

  void fill_new_mctrk(TMuiMCHitMapO::pointer mc_hit_ptr, int trackID = 0);

  // Module parameter table
  //
  const mMuiSlowSimPar* _mod_par;           

  //! Interface object container of muid MC hit.
  TMuiMCHitMapO* _mc_hit_map;

  // Interface object container of muon MC tracks
  TMutMCTrkMap* _mc_trk_map;

  // PISA STAF table
  //
  TABLE_HEAD_ST _munhits_h;                 // header struct mum hits STAF table
  MUNHITS_ST*   _munhits;                   // pointer to mumhits array

  // Timer
  //
  PHTimeServer::timer _timer;
};

#endif
