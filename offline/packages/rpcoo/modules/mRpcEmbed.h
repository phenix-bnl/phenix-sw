// $Id: mRpcEmbed.h,v 1.3 2009/09/24 09:11:49 hpereira Exp $
#ifndef __mRpcEmbed_h__
#define __mRpcEmbed_h__

/*!
  \file    mRpcEmbed.h
  \brief   rpc hit embedding module.
	Merge hits from signal map and background map, merge them into
	main map in RPCOO node, using clones.
  \author  H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2009/09/24 09:11:49 $
*/

#include <PHTimeServer.h>
#include <TRpcHitMap.h>
#include <mRpcEmbedPar.h>
#include <TRpcHit.h>

#include <iostream>

class PHCompositeNode;
class TRpcHit;

/*! \ingroup modules */
//! rpc hit embedding module.
/*!
	Merge hits from signal map and background map, merge them into
	main map in MUTOO node, using clones.
*/
class mRpcEmbed
{
 public:

	//! constructor
  mRpcEmbed();

	//! destructor
  virtual ~mRpcEmbed(){}

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

	//! merge rpc hits
  void do_embedding();

  //! module parameters
  const mRpcEmbedPar* _mod_par;

	//! signal rpc hit map
  TRpcHitMap* _signal_hit_map;

	//! background rpc hit map
  TRpcHitMap* _backgr_hit_map;

	//! main (merged) rpc hit map
  TRpcHitMap* _merged_hit_map;

	//! rpc working node
  PHCompositeNode* _rpcoo_node;

	//! signal working node
  PHCompositeNode* _ioc_signal_node;

	//! background working node
  PHCompositeNode* _ioc_backgr_node;

	//! numbers of non overlapping hits
  ULong_t _n_non_overlap_hits;

	//! number of overlapping (signal on background) hits
  ULong_t _n_overlap_hits;

	//! number of signal hits
	ULong_t _n_tot_signal_hits;

	//! number of background hits
	ULong_t _n_tot_backgr_hits;

	//! numbers of non overlapping hits
  ULong_t _n_tot_non_overlap_hits;

	//! number of overlapping (signal on background) hits
  ULong_t _n_tot_overlap_hits;

  //! module timer
  PHTimeServer::timer _timer;

};

#endif





