// $Id: mFvtxEmbedSili.h,v 1.1 2013/10/09 22:35:29 jinhuang Exp $
#ifndef __mFvtxEmbedSili_h__
#define __mFvtxEmbedSili_h__

/*!
 \file mFvtxEmbedSili.h
 \brief vtx and fvtx hit embedding module.
 \author  Jin Huang <jhuang@bnl.gov>
 \version $Revision: 1.1 $
 \date $Date: 2013/10/09 22:35:29 $
 */

#include <phool.h>
#include <PHTimeServer.h>
#include <iostream>
#include <string>
#include <mFvtxModuleBase.h>

class PHCompositeNode;
class TFvtxHitMap;
class TFvtxSvxClusterMap;
class mFvtxEmbedPar;

/*! \ingroup modules */
//! vtx and fvtx hit embedding module.
/*!
 Merge hits from signal map and background map, merge them into
 main map in FVTXOO node, using clones.

 This Similar to mFvtxEmbed, but with a few new features:
 1. support of VTX clusters (TFvtxSvxClusterMap)
 2. compatible to FvtxReco
 3. also handles DST read back for the data to be imbeded

 */
class mFvtxEmbedSili : public mFvtxModuleBase
{
public:

  //! constructor
  mFvtxEmbedSili();

  //! destructor
  virtual
  ~mFvtxEmbedSili()
  {
  }

  virtual void
  init(PHCompositeNode* top_node);

  virtual void
  init_run(PHCompositeNode* top_node);

  virtual PHBoolean
  event(PHCompositeNode* top_node);

  virtual void
  end(PHCompositeNode* top_node);

  //! print counts summary
  void
  print_summary(std::ostream& out = std::cout) const;

  std::string
  get_embed_src_topnode_name() const
  {
    return _embed_src_topnode_name;
  }

  void
  set_embed_src_topnode_name(std::string embedSrcTopnodeName)
  {
    _embed_src_topnode_name = embedSrcTopnodeName;
  }

private:

  //! get pointers to needed working nodes
  void
  set_node_ptrs(PHCompositeNode* top_node);

  //! get pointers to needed interface nodes
  void
  set_interface_ptrs(PHCompositeNode* top_node);

  //! merge fvtx hits
  void
  do_embedding_fvtx();

  //! merge vtx hits
  void
  do_embedding_vtx();

  //! module parameters
  const mFvtxEmbedPar* _mod_par;

  std::string _embed_src_topnode_name;

  //! background working node
  PHCompositeNode* _embed_src_topnode;

  //! source DST node
  PHCompositeNode* _embed_src_dst_node;

  //! fvtx working node
  PHCompositeNode* _fvtxoo_node;

  //! fvtx working node
  PHCompositeNode* _embed_src_fvtxoo_node;

  bool _readback_fvtx;

  //! signal fvtx hit map
  TFvtxHitMap* _embed_src_fvtx_hit_map;

  //! main (merged) fvtx hit map
  TFvtxHitMap* _fvtx_hit_map;

  bool _readback_vtx;

  //! signal fvtx hit map
  TFvtxSvxClusterMap* _embed_src_vtx_hit_map;

  //! main (merged) fvtx hit map
  TFvtxSvxClusterMap* _vtx_hit_map;

  //! number of hits for embedding
  unsigned long _n_tot_embed_src_fvtx_hits;

  //! number of original hits before embedding
  unsigned long _n_tot_original_fvtx_hits;

  //! numbers of non overlapping hits
  unsigned long _n_tot_non_overlap_fvtx_hits;

  //! number of overlapping (signal on background) hits
  unsigned long _n_tot_overlap_fvtx_hits;

  //! number of hits for embedding
  unsigned long _n_tot_embed_src_vtx_hits;

  //! number of original hits before embedding
  unsigned long _n_tot_original_vtx_hits;

  //! numbers of non overlapping hits
  unsigned long _n_tot_non_overlap_vtx_hits;

  //! number of overlapping (signal on background) hits
  unsigned long _n_tot_overlap_vtx_hits;

  //! module timer
  PHTimeServer::timer _timer;

};

#endif

