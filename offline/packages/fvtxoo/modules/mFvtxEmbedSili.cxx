// $Id: mFvtxEmbedSili.cxx,v 1.2 2014/01/26 16:47:37 bbannier Exp $

/*!
 \file mFvtxEmbedSili.cxx
 \brief vtx and fvtx hit embedding module.
 \author  Jin Huang <jhuang@bnl.gov>
 \version $Revision: 1.2 $
 \date $Date: 2014/01/26 16:47:37 $
 */

#include <recoConsts.h>
#include <Fun4AllServer.h>
#include <TMutNode.h>

// FVTXOO headers
#include <mFvtxEmbedSili.h>
#include <mFvtxEmbedPar.h>
#include <TFvtxHitMap.h>
#include <TFvtxSvxClusterMap.h>
#include <FVTXOO.h>

#include <string>

using namespace std;

//______________________________________________________
mFvtxEmbedSili::mFvtxEmbedSili() :
    _mod_par(0), //

    _embed_src_topnode_name("FvtxEmbed"), //

    //! background working node
    _embed_src_topnode(NULL),

    //! source DST node
    _embed_src_dst_node(NULL),

    //! fvtx working node
    _fvtxoo_node(NULL),

    //! fvtx working node
    _embed_src_fvtxoo_node(NULL),

    _readback_fvtx(false),

    //! signal fvtx hit map
    _embed_src_fvtx_hit_map(NULL),

    //! main (merged) fvtx hit map
    _fvtx_hit_map(NULL),

    _readback_vtx(false),

    //! signal fvtx hit map
    _embed_src_vtx_hit_map(NULL),

    //! main (merged) fvtx hit map
    _vtx_hit_map(NULL),

    //! number of hits for embedding
    _n_tot_embed_src_fvtx_hits(0),

    //! number of original hits before embedding
    _n_tot_original_fvtx_hits(0),

    //! numbers of non overlapping hits
    _n_tot_non_overlap_fvtx_hits(0),

    //! number of overlapping (signal on background) hits
    _n_tot_overlap_fvtx_hits(0),

    //! number of hits for embedding
    _n_tot_embed_src_vtx_hits(0),

    //! number of original hits before embedding
    _n_tot_original_vtx_hits(0),

    //! numbers of non overlapping hits
    _n_tot_non_overlap_vtx_hits(0),

    //! number of overlapping (signal on background) hits
    _n_tot_overlap_vtx_hits(0),

    _timer(PHTimeServer::get()->insert_new("mFvtxEmbedSili"))
{
  FVTXOO::TRACE("initializing module mFvtxEmbedSili");
}

void
mFvtxEmbedSili::init(PHCompositeNode* top_node)
{
}

void
mFvtxEmbedSili::init_run(PHCompositeNode* top_node)
{
  FVTXOO::TRACE("mFvtxEmbedSili::init_run");

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();

  if (rc->FlagExist("FVTX_EMBED_SRC_TOPNODE"))
    {
      cout
          << "mFvtxEmbedSili::InitRun - reading node name from recoConst FVTX_EMBED_SRC_TOPNODE"
          << endl;
      set_embed_src_topnode_name(rc->get_CharFlag("FVTX_EMBED_SRC_TOPNODE"));
    }

  set_node_ptrs(top_node);
}

//______________________________________________________
void
mFvtxEmbedSili::set_node_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mFvtxEmbedPar>::find_node(top_node, "mFvtxEmbedPar");

  // FVTXOO nodes as destination of embedding
    {
      PHNodeIterator nodeItr(top_node);
      if (!(_fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "FVTXOO"))))
        throw runtime_error(DESCRIPTION("Cannot locate FVTXOO node"));

      try
        {
          _fvtx_hit_map = TMutNode<TFvtxHitMap>::find_node(_fvtxoo_node,
              "TFvtxHitMap");
        }
      catch (...)
        {
          cout
              << "mFvtxEmbedSili::set_node_ptrs - WARNING - cannot find FVTXOO/TFvtxHitMap"
              << endl;
          _fvtx_hit_map = NULL;
        }

      try
        {
          _vtx_hit_map = TMutNode<TFvtxSvxClusterMap>::find_node(_fvtxoo_node,
              "TFvtxSvxClusterMap");
        }
      catch (...)
        {
          cout
              << "mFvtxEmbedSili::set_node_ptrs - WARNING - cannot find FVTXOO/TFvtxSvxClusterMap"
              << endl;
          _vtx_hit_map = NULL;
        }
    }

  // Source nodes of embedding
    {
      cout << "mFvtxEmbedSili::set_node_ptrs - will embed from source top node "
          << _embed_src_topnode_name << endl;

      Fun4AllServer* se = Fun4AllServer::instance();
      _embed_src_topnode = se->topNode(_embed_src_topnode_name);
      PHNodeIterator nodeItr(_embed_src_topnode);

      _embed_src_fvtxoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "FVTXOO"));
      if (!_embed_src_fvtxoo_node)
        {
          _embed_src_fvtxoo_node = new PHCompositeNode("FVTXOO");
          _embed_src_topnode->addNode(_embed_src_fvtxoo_node);
        }

      _embed_src_dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst(
          "PHCompositeNode", "DST"));
      if (!_embed_src_dst_node)
        {

          cout
              << "mFvtxEmbedSili::set_node_ptrs - Error - can not find embed source node with name "
              << _embed_src_topnode_name
              << "/DST. Please load it using a DstInputManager(<name>,\"TOP\",<top node name>) "
              << "and pass the top node name through recoConst::set_CharFlag(FVTX_EMBED_SRC_TOPNODE)"
              << endl;

          throw runtime_error(
          DESCRIPTION("Cannot locate source DST node for embedding"));
        }

      _embed_src_fvtx_hit_map = NULL;
      try
        {
          _embed_src_fvtx_hit_map = TMutNode<TFvtxHitMap>::find_node(
              _embed_src_fvtxoo_node, "TFvtxHitMap");
        }
      catch (...)
        {
          cout << "mFvtxEmbedSili::set_node_ptrs - INFO - "
              << "cannot find TFvtxHitMap from the embedding source, try to make it and do read back"
              << endl;

          PHNodeIterator iter(_embed_src_dst_node);
          if (iter.findFirst("PHIODataNode", "TFvtxHit"))
            {
              _readback_fvtx = true;

              _embed_src_fvtx_hit_map =
                  TMutNode<TFvtxHitMap>::new_dst_input_node(
                      _embed_src_fvtxoo_node, "TFvtxHitMap",
                      _embed_src_dst_node, "TFvtxHit");
            }
          else
            {

              cout << "mFvtxEmbedSili::set_node_ptrs - WARNING - "
                  << "cannot find DST/TFvtxHit from the embedding source, ignore FVTX hit embedding. Existing embedding source nodes are:"
                  << endl;

              _embed_src_dst_node->print();

              _readback_fvtx = false;
              _embed_src_fvtx_hit_map = NULL;

            }

        } //  catch (...)

      _embed_src_vtx_hit_map = NULL;
      try
        {
          _embed_src_vtx_hit_map = TMutNode<TFvtxSvxClusterMap>::find_node(
              _embed_src_fvtxoo_node, "TFvtxSvxClusterMap");
        }
      catch (...)
        {
          cout << "mFvtxEmbedSili::set_node_ptrs - INFO - "
              << "cannot find TFvtxSvxClusterMap from the embedding source, try to make it and do read back"
              << endl;

          PHNodeIterator iter(_embed_src_dst_node);
          if (iter.findFirst("PHIODataNode", "TFvtxSvxCluster"))
            {
              _readback_vtx = true;

              _embed_src_vtx_hit_map =
                  TMutNode<TFvtxSvxClusterMap>::new_dst_input_node(
                      _embed_src_fvtxoo_node, "TFvtxSvxClusterMap",
                      _embed_src_dst_node, "TFvtxSvxCluster");
            }
          else
            {

              cout << "mFvtxEmbedSili::set_node_ptrs - WARNING - "
                  << "cannot find DST/TFvtxSvxCluster from the embedding source, ignore VTX hit embedding.  Existing embedding source nodes are:"
                  << endl;

              _embed_src_dst_node->print();

              _readback_vtx = false;
              _embed_src_vtx_hit_map = NULL;

            }

        } //  catch (...)
    }

}

void
mFvtxEmbedSili::end(PHCompositeNode* top_node)
{
  print_summary();
}

//______________________________________________________
PHBoolean
mFvtxEmbedSili::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();

  try
    {

      // Reset IOC pointers
      set_interface_ptrs(top_node);

      // embedding - FVTX
      if (_embed_src_fvtx_hit_map)
        {
          if (_readback_fvtx)
            {
              _embed_src_fvtx_hit_map->read_array(_embed_src_dst_node);
            }

          do_embedding_fvtx();
        }

      // embedding - VTX
      if (_embed_src_vtx_hit_map)
        {
          if (_readback_vtx)
            {
              _embed_src_vtx_hit_map->read_array(_embed_src_dst_node);
            }

          do_embedding_vtx();
        }

      //! increment counters
//      _n_tot_signal_hits += _signal_hit_map->size();
//      _n_tot_backgr_hits += _backgr_hit_map->size();
//      _n_tot_non_overlap_hits += _n_non_overlap_hits;
//      _n_tot_overlap_hits += _n_overlap_hits;

    }
  catch (exception& e)
    {
      FVTXOO::TRACE(e.what());
      return False;
    }

  // Timer
  _timer.get()->stop();
  if (_mod_par->get_verbosity() == FVTXOO::SOME)
    _timer.get()->print();

  return True;
}

//______________________________________________________
void
mFvtxEmbedSili::print_summary(ostream& out) const
{
  FVTXOO::PRINT(out, "mFvtxEmbedSili::print_summary");

  out << "FVTX summary: " << endl;
  out << "total original hits: " << "\t" << _n_tot_original_fvtx_hits << endl;
  out << "total hits for embedding: " << "\t" << _n_tot_embed_src_fvtx_hits
      << endl;
  out << "total non-overlap merged hits: " << "\t"
      << _n_tot_non_overlap_fvtx_hits << endl;
  out << "total overlap merged hits: " << "\t" << _n_tot_overlap_fvtx_hits
      << endl;

  out << "VTX summary: " << endl;
  out << "total original hits: " << "\t" << _n_tot_original_vtx_hits << endl;
  out << "total hits for embedding: " << "\t" << _n_tot_embed_src_vtx_hits
      << endl;
  out << "total non-overlap merged hits: " << "\t"
      << _n_tot_non_overlap_vtx_hits << endl;
  out << "total overlap merged hits: " << "\t" << _n_tot_overlap_vtx_hits
      << endl;

  FVTXOO::PRINT(out, "**");
}

//______________________________________________________
void
mFvtxEmbedSili::set_interface_ptrs(PHCompositeNode* top_node)
{

  try
    {
      _fvtx_hit_map = TMutNode<TFvtxHitMap>::find_node(_fvtxoo_node,
          "TFvtxHitMap");
    }
  catch (...)
    {
      _fvtx_hit_map = NULL;
    }

  try
    {
      _vtx_hit_map = TMutNode<TFvtxSvxClusterMap>::find_node(_fvtxoo_node,
          "TFvtxSvxClusterMap");
    }
  catch (...)
    {
      _vtx_hit_map = NULL;
    }

  try
    {
      _embed_src_fvtx_hit_map = TMutNode<TFvtxHitMap>::find_node(
          _embed_src_fvtxoo_node, "TFvtxHitMap");
    }
  catch (...)
    {
      _embed_src_fvtx_hit_map = NULL;
    }

  try
    {
      _embed_src_vtx_hit_map = TMutNode<TFvtxSvxClusterMap>::find_node(
          _embed_src_fvtxoo_node, "TFvtxSvxClusterMap");
    }
  catch (...)
    {
      _embed_src_vtx_hit_map = NULL;
    }

}

//______________________________________________________
void
mFvtxEmbedSili::do_embedding_fvtx()
{

  _n_tot_original_fvtx_hits += _fvtx_hit_map->size();
  _n_tot_non_overlap_fvtx_hits += _fvtx_hit_map->size();
  _n_tot_embed_src_fvtx_hits += _embed_src_fvtx_hit_map->size();

  // Insert background if hit already exists do the merge
  TFvtxHitMap::iterator backgr_iter = _embed_src_fvtx_hit_map->range();
  while (TFvtxHitMap::pointer backgr_ptr = backgr_iter.next())
    {

      TFvtxHitMap::iterator merged_iter = _fvtx_hit_map->get(
          backgr_ptr->get()->get_arm(), backgr_ptr->get()->get_cage(),
          backgr_ptr->get()->get_station(), backgr_ptr->get()->get_sector(),
          backgr_ptr->get()->get_column(), backgr_ptr->get()->get_strip());

      if (merged_iter.count() == 0)
        {
          _fvtx_hit_map->insert_clone(backgr_ptr);
          _n_tot_non_overlap_fvtx_hits++;
        }
      else
        {
          _n_tot_non_overlap_fvtx_hits--;
          _n_tot_overlap_fvtx_hits++;

          // Hit exist -- do the merge
          TFvtxHitMap::pointer merged_ptr = merged_iter.current();
          merged_ptr->get()->set_q(
              merged_ptr->get()->get_q() + backgr_ptr->get()->get_q());

          // for the error on the charge, we set the squared sum
          merged_ptr->get()->set_error_q(
              sqrt(
                  FVTXOO::SQUARE(merged_ptr->get()->get_error_q())
                      + FVTXOO::SQUARE(backgr_ptr->get()->get_error_q())));

        }
    }
}

//______________________________________________________
void
mFvtxEmbedSili::do_embedding_vtx()
{

  _n_tot_original_vtx_hits += _vtx_hit_map->size();
  _n_tot_non_overlap_vtx_hits += _vtx_hit_map->size();
  _n_tot_embed_src_vtx_hits += _embed_src_vtx_hit_map->size();

  // Insert background if hit already exists do the merge
  TFvtxSvxClusterMap::iterator backgr_iter = _embed_src_vtx_hit_map->range();
  while (TFvtxSvxClusterMap::pointer backgr_ptr = backgr_iter.next())
    {
      _vtx_hit_map->insert_clone(backgr_ptr);
      _n_tot_non_overlap_vtx_hits++;
    }
}

