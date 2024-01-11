// $Id: mMutFindGapCoord.h,v 1.11 2006/01/14 04:29:42 pinkenbu Exp $
#ifndef __MMUTFINDGAPCOORD_H__
#define __MMUTFINDGAPCOORD_H__

/*!
   \file    mMutFindGapCoord.h
   \brief   Derives (x,y) coordinate from pairs of TMutCoord.
   \author  S. Kelly
   \version $Revision: 1.11 $
   \date    $Date: 2006/01/14 04:29:42 $
*/

#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<PHTimeServer.h>
#include<mMutFindGapCoordPar.h>

class PHCompositeNode;

/*! \ingroup modules */
//! Derives (x,y) coordinate from pairs of TMutCoord.
/*!
Matches two TMutCoord across a gap and derives an associated
TMutGapCoord object.<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutFindGapCoordPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutCoordMap*</td>
<td> IOC</td>
<td> immutable </td>
</tr>
<tr>
<td> TMutGapCoordMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
</table>
*/

class mMutFindGapCoord
{
 public: 

  //! constructor
  mMutFindGapCoord(); 

  //! Destructor
  virtual ~mMutFindGapCoord(){}

  //! event method
  virtual PHBoolean event(PHCompositeNode*);

 private:  

  //! Reset IOC and external interface pointers 
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! Associate contiguous hit strips in a given cathode plane with a TMutGapCoord object.
  void find_gap_coords();

  //! Associate coordinates in a given cathode plane with a TMutGapCoord object.
  void find_gap_coords( 
    int arm, 
    int station, 
    int octant, 
    int half, 
    int gap );

  //! check coordinate pair, make a gapcoordinate if they match
  void match_and_make(TMutCoordMap::pointer&, TMutCoordMap::pointer&);

  // Interface pointers
  //! parameter table
  const mMutFindGapCoordPar* _mod_par; 
  
  //! coordinate IOC
  TMutCoordMap* _coord_map;		 
  
  //! gap coordinate IOC
  TMutGapCoordMap* _gap_coord_map;	
  
  //! module Timer
  PHTimeServer::timer _timer;
  
};

#endif

