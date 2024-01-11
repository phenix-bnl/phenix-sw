#ifndef __MMUTFINDCLUS_HH__
#define __MMUTFINDCLUS_HH__

// $Id: mMutFindClus.h,v 1.13 2011/12/24 04:48:29 slash Exp $

/*!
	 \file mMutFindClus.h
	 \brief Finds groups of contiguous hit strips
	 \author S.Kelly, H.Pereira
	 \version $Revision: 1.13 $
	 \date $Date: 2011/12/24 04:48:29 $
*/

#include<TMutClusMap.h>
#include<TMutHitMap.h>
#include<PHTimeServer.h>
#include<mMutFindClusPar.h>
#include<MutCalib.h>
#include<MutGeom.h>
#include<TMutGeo.h>

class PHCompositeNode;

/*! \ingroup modules */

//!Finds groups of contiguous hit strips and associates them
//! with a TMutClus object.
/*!
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutFindClusPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutHitMap*</td>
<td> IOC</td>
<td> immutable </td>
</tr>
<tr>
<td> TMutClusMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
</table>
*/

class mMutFindClus
{
 public:

  //! constructor
  mMutFindClus();
  
  //! destructor
  virtual ~mMutFindClus(){}
  
  //! event method
  virtual PHBoolean event( PHCompositeNode* );
  
  //! event method
  /*! 
    only find clusters in specified arm, station and octant.
    \warning: set_interface_ptrs is not called. Must be called externally
      */
  virtual PHBoolean event( 
    PHCompositeNode*, 
    const unsigned short& arm, 
    const unsigned short& station, 
    const unsigned short& octant );

  //! gets local pointer to usefull nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  private:

  //! find all clusters
  void find_clusters();
  
  /*!
    find clulsters for a given cathode
    returns true if at least one cluster was found
  */
  bool find_clusters(
    const int& arm,
    const int& station,
    const int& octant,
    const int& half_octant,
    const int& gap,
    const int& cathode );

  //! remove some clusters based on cuts
  void apply_cluster_cuts();

  //! remove cluster based on cuts
  void apply_cluster_cuts( TMutClusMap::pointer );

  //! propagate hit status to clusters
  void set_cluster_status();

  //! propagate hit status to clusters
  void set_cluster_status( TMutClusMap::pointer );

  /*!
    returns true if missing strip with strip number "strip"
    at cathode location "cathodeLocator"
    is to be excused.
  */
  bool excuse_strip(const MUTOO::cathode_locator& cathodeLocator, int strip);
  
  //! module parameters
  const mMutFindClusPar* _mod_par;
  
  //! hit map node
  TMutHitMap* _hit_map;
  
  //! cluster map node
  TMutClusMap* _clus_map;

  /*! 
    \brief
    true if cluster finding should be performed only 
    in specific arm, station, octant
  */
  bool _use_section;
  
  //! specific arm
  unsigned short _arm;
  
  //! specific station
  unsigned short _station;

  //! specific octant
  unsigned short _octant;
  
  //! module timer
  PHTimeServer::timer _timer;

};

#endif /* __MMUTFINDCLUS_HH__ */



