// $Id: mMutEmbed.h,v 1.9 2011/11/29 18:55:23 slash Exp $
#ifndef __mMutEmbed_h__
#define __mMutEmbed_h__

/*!
  \file mMutEmbed.h
  \brief mutr hit embedding module.
  Merge hits from signal map and background map, merge them into
  main map in MUTOO node, using clones.
  \author  S. Kelly
  \version $Revision: 1.9 $
  \date $Date: 2011/11/29 18:55:23 $
*/

#include<PHTimeServer.h>
#include<phool.h>

class TMutHitMap;
class PHCompositeNode;
class mMutEmbedPar;

/*! \ingroup modules */
//! mutr hit embedding module.
/*!
  Merge hits from signal map and background map, merge them into
  main map in MUTOO node, using clones.
*/

class mMutEmbed
{
 public:

  //! constructor
  mMutEmbed();

  //! destructor
  virtual ~mMutEmbed()
  {}

  //! event method
  /*! three nodes are passed: the signal node, the background node, and the main topnode */
  virtual PHBoolean event(
    PHCompositeNode*,
    PHCompositeNode*,
    PHCompositeNode*
    );

  //! print summary
  void print_summary( std::ostream& out = std::cout ) const;

  private:

  //! get pointers to needed working nodes
  void set_node_ptrs(PHCompositeNode* top_node);

 	//! get pointers to needed interface nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! merge mutr hits
  void do_embedding();

  //! module parameters
  const mMutEmbedPar* _mod_par;

  //! signal muid hit map
  TMutHitMap* _signal_hit_map;

  //! background muid hit map
  TMutHitMap* _backgr_hit_map;

  //! main (merged) mutr hit map
  TMutHitMap* _merged_hit_map;

  //! mutr working node
  PHCompositeNode* _mutoo_node;

  //! signal working node
  PHCompositeNode* _ioc_signal_node;

  //! background working node
  PHCompositeNode* _ioc_backgr_node;

  //! numbers of non overlapping mutr hits
  unsigned long _n_non_overlap_hits;

  //! number of overlapping (signal on background) mutr hits
  unsigned long _n_overlap_hits;

  //! module timer
  PHTimeServer::timer _timer;

};

#endif




