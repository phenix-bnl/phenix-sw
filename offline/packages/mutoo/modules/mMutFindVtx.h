// $Id: mMutFindVtx.h,v 1.9 2006/01/14 04:29:42 pinkenbu Exp $
#ifndef __MMUTFINDVTX_HH__
#define __MMUTFINDVTX_HH__

//////////////////////////////////////////////////////////////////
/*
  \file mMutFindVtx.h
  \brief associate pairs to make vertex objects
  \author S.Kelly 
  \version $Revision: 1.9 $
  \date    $Date: 2006/01/14 04:29:42 $
*/
//////////////////////////////////////////////////////////////////

#include<PHTimeServer.h>
#include<mMutFindVtxPar.h>
#include<TMutTrkMap.h>
#include<TMutVtxMap.h>
#include<boost/array.hpp>

class PHCompositeNode;

/*! \ingroup modules */
//! associate tracks to make vertex objects
/*!
associate tracks to make vertex objects

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutFindVtxPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> Track IOC</td>
<td> mutable </td>
<tr>
<td> TMutVtxMap*</td>
<td> Two track vertex IOC</td>
<td> mutable </td>
</table>
*/

class mMutFindVtx
{
 public: 

  //! constructor
  mMutFindVtx(); 
  
  //! destructor
  virtual ~mMutFindVtx(){}
  
  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  

  //! get local pointer to needed nodes
  void set_interface_ptrs(PHCompositeNode* top_node); 
  
  //! find track pairs, create vertex 
  void find_pairs();
  
  //! update vertex members from tracks
  void update_pairs();
  
  //! fit vertex using J.Nagle parametrization
  void nagle_fit();  
  
  //! calculate track momentum at vertex
  double calc_p_vertex(const boost::array<double,4>& pars, double p_trk);

  //! check track status
  bool accept_trk( TMutTrkMap::const_pointer trk_ptr ) const;

  //! module timer
  PHTimeServer::timer _timer;    
  
  //! module parameters
  const mMutFindVtxPar* _mod_par;
  
  //! track map
  TMutTrkMap* _trk_map;
  
  //! vertex map
  TMutVtxMap* _vtx_map;
  
};


#endif /* __MMUTFINDVTX_HH__ */














