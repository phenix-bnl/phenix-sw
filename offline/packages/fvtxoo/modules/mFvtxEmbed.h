// $Id: mFvtxEmbed.h,v 1.4 2011/12/01 04:16:21 slash Exp $
#ifndef __mFvtxEmbed_h__
#define __mFvtxEmbed_h__

/*!
  \file mFvtxEmbed.h
  \brief fvtx hit embedding module.
  Merge hits from signal map and background map, merge them into
  main map in FVTXOO node, using clones.
  \author  X. R. Wang
  \version $Revision: 1.4 $
  \date $Date: 2011/12/01 04:16:21 $
*/

#include <phool.h>
#include <PHTimeServer.h>
#include <iostream>

class PHCompositeNode;
class TFvtxHitMap;
class mFvtxEmbedPar;

/*! \ingroup modules */
//! fvtx hit embedding module.
/*!
  Merge hits from signal map and background map, merge them into
  main map in MUTOO node, using clones.
*/
class mFvtxEmbed
{
 public:

  //! constructor
  mFvtxEmbed();

  //! destructor
  virtual ~mFvtxEmbed(){}

  //! event method
  /*! three nodes are passed: the signal node, the background node, and the main topnode */
  virtual PHBoolean event(
    PHCompositeNode*,
    PHCompositeNode*,
    PHCompositeNode*
    );

  //! print counts summary
  void print_summary( std::ostream& out = std::cout ) const;

  private:

  //! get pointers to needed working nodes
  void set_node_ptrs(PHCompositeNode* top_node);

 	//! get pointers to needed interface nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! merge fvtx hits
  void do_embedding();

  //! module parameters
  const mFvtxEmbedPar* _mod_par;

  //! signal fvtx hit map
  TFvtxHitMap* _signal_hit_map;

  //! background fvtx hit map
  TFvtxHitMap* _backgr_hit_map;

  //! main (merged) fvtx hit map
  TFvtxHitMap* _merged_hit_map;

  //! fvtx working node
  PHCompositeNode* _fvtxoo_node;

  //! signal working node
  PHCompositeNode* _ioc_signal_node;

  //! background working node
  PHCompositeNode* _ioc_backgr_node;

  //! numbers of non overlapping hits
  unsigned long _n_non_overlap_hits;

  //! number of overlapping (signal on background) hits
  unsigned long _n_overlap_hits;

  //! number of signal hits
  unsigned long _n_tot_signal_hits;

  //! number of background hits
  unsigned long _n_tot_backgr_hits;

  //! numbers of non overlapping hits
  unsigned long _n_tot_non_overlap_hits;

  //! number of overlapping (signal on background) hits
  unsigned long _n_tot_overlap_hits;

  //! module timer
  PHTimeServer::timer _timer;

};

#endif





