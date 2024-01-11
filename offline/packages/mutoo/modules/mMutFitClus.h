#ifndef __MMUTFITCLUS_HH__
#define __MMUTFITCLUS_HH__

// $Id: mMutFitClus.h,v 1.18 2011/12/24 04:48:30 slash Exp $

/*!
	\file mMutFitClus.h
	\brief fit clusters, create coordinates
	\author S. Kelly M. Brooks
	\version $Revision: 1.18 $
	\date $Date: 2011/12/24 04:48:30 $
*/

#include<PHTimeServer.h>
#include<mMutFitClusPar.h>
#include<TMutHitMap.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<MutCalib.h>
#include<MUTOO.h>
#include<MUTOO_FEM.h>

// GSL
#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>
#include <PHGslMatrix.h>

// BOOST
#include <boost/array.hpp>

// STL
#include <map>

class PHCompositeNode;

/*! \ingroup modules */
//! Applies selected cluster fitting scheme to the TMutClus
//! objects. From the resulting centroids, derives TMutCoord objects.
/*!
MUTOO Cluster finder.<br>

This module loops over all hits in the cluster map executes the
selected cluster fitting scheme.	Currently the module has a single
fit scheme. The hooks for additional cluster fitting schemes are part
of the design.	The fit scheme is selected via the modules run-time
parameter object.	The results of the cluster fit are stored in the
TMutClus object as a list of TMutCentroid objects.	The idea here is
that we will eventually want to associate more than one centroid with
a cluster (eg in station 1 in Au running).	In addition the module
inserts an new TMutCoord object into the associated map for each
centroid associated with the cluster. The TMutCoord objects 
are associated with the cluster from which they were derived.

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutFitClusPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
</tr>
<tr>
<td> mMutClusMap*</td>
<td> Cluster IOC </td>
<td> mutable </td>
</tr>
<tr>
<td> TMutCoordMap*</td>
<td> Coordinate IOC </td>
<td> mutable </td>
</tr>
</table>

*/

class mMutFitClus
{
 public: 

  //! constructor
  mMutFitClus(); 
  
  //! destructor
  virtual ~mMutFitClus(){}
  
  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
  //! event method
  /*! 
    only fit clusters in specified arm, station and octant.
    \warning: set_interface_ptrs is not called. Must be called externally
  */
  virtual PHBoolean event( 
    PHCompositeNode*, 
    const unsigned short& arm, 
    const unsigned short& station, 
    const unsigned short& octant );

  //! store local pointers to interfaces
  void set_interface_ptrs(PHCompositeNode* top_node);

  private:	

  //! Do the cluster fits
  void fit_clusters();
  
  //! Fill TMutCoordMap with cluster centroid data
  void fill_coord_map();

  //! module parameters
  const mMutFitClusPar* _mod_par;	

  //! pointer to clusters map
  TMutClusMap* _clus_map;	

  //! pointer to coordinates map
  TMutCoordMap* _coord_map;	

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
	 
  //! Timer
  PHTimeServer::timer _timer;

};

#endif /* __MMUTFITCLUS_HH__ */






