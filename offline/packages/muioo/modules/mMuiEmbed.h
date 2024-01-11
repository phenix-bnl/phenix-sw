// $Id: mMuiEmbed.h,v 1.3 2011/11/30 11:47:54 silvermy Exp $
#ifndef __mMuiEmbed_h__
#define __mMuiEmbed_h__

/*!
  \file mMuiEmbed.h
  \brief muid hit embedding module.
  Merge hits from signal map and background map, merge them into
  main map in MUIOO node, using clones.
  \author  S. Kelly
  \version $Revision: 1.3 $
  \date $Date: 2011/11/30 11:47:54 $
*/

#include<PHTimeServer.h>
#include<phool.h>

// forward declaration
class TMuiHitMapO;
class PHCompositeNode;
class mMuiEmbedPar;

/*! \ingroup modules */
//! muid hit embedding module.
/*!
  Merge hits from signal map and background map, merge them into
  main map in MUIOO node, using clones.
*/

class mMuiEmbed
{
 public:

  //! constructor
  mMuiEmbed();

  //! destructor
  virtual ~mMuiEmbed()
  {}

  //! event method
  /*! three nodes are passed: the signal node, the background node, and the main topnode */
  virtual PHBoolean event(
    PHCompositeNode*,
    PHCompositeNode*,
    PHCompositeNode*
    );

  //! print summary
  void print_summary( std::ostream& out = std::cout );

  private:

  //! get pointers to needed working nodes
  void set_node_ptrs(PHCompositeNode* top_node);

 	//! get pointers to needed interface nodes
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! merge mutr hits
  void do_embedding();

  //! module parameters
  const mMuiEmbedPar* _mod_par;

  //! signal muid hit map
  TMuiHitMapO* _signal_hit_map;

  //! background muid hit map
  TMuiHitMapO* _backgr_hit_map;

  //! main (merged) mutr hit map
  TMuiHitMapO* _merged_hit_map;

  //! mutr working node
  PHCompositeNode* _muioo_node;

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




