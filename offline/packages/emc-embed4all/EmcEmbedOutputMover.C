#include "EmcEmbedOutputMover.h"


#include <fkinWrapper.h>
#include <dEmcGeaClusterTrackWrapper.h>
#include <dEmcGeaTrackWrapper.h>
#include <dEmcGeaTrackClusterWrapper.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <emcNodeHelper.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <PHGlobal.h>
#include <primaryWrapper.h>
#include <SyncObject.h>
#include <VtxOut.h>

#include <cassert>

namespace
{
  template <class T>
  void replaceObject(PHCompositeNode* topnode,
                     T* object,
                     const char* nodename = "")
  {
    assert(topnode!=0);

    PHNodeIterator iter(topnode);

    PHIODataNode<T>* node = 
      static_cast<PHIODataNode<T>*>(iter.findFirst("PHIODataNode",
						   nodename));
    
    assert(node!=0);

    T* oldobject = node->getData();
    delete oldobject;
    node->setData(object);

  }

  //__________________________________________________________________________
  void copy(const dEmcGeaTrackWrapper& src,
            dEmcGeaTrackWrapper& dest,
            int centClass)
  {
    // dEmcGeaTrack table data-members:
    const Int_t kmaxhits = 15000;
    static Int_t tr_id[kmaxhits] ;
    static Int_t tr_trkno[kmaxhits] ;
    static Int_t tr_input[kmaxhits] ;
    static Int_t tr_anclvl[kmaxhits] ;
    static Int_t tr_pid[kmaxhits] ;
    static Float_t tr_ekin[kmaxhits] ;
    static Float_t tr_ptot[kmaxhits] ;
    static Float_t tr_xyz[3][kmaxhits] ;
    static Float_t tr_pxyz[3][kmaxhits] ;
    static Float_t tr_impxyz[3][kmaxhits] ;
    static Int_t tr_itparent[kmaxhits] ;
    static Int_t tr_idparent[kmaxhits] ;
    static Int_t tr_parent_ptr[kmaxhits] ;
    static Int_t tr_twrhit[kmaxhits] ;
    static Float_t tr_edep[kmaxhits] ;
    static size_t ktrcount = 0;

    ktrcount = src.RowCount();

    for (size_t j = 0; j < ktrcount; ++j)
      {
        tr_id[j] = (Int_t) src.get_id(j);
        tr_trkno[j] = (Int_t) src.get_trkno(j);
        tr_input[j] = (Int_t) src.get_input(j);
        tr_anclvl[j] = (Int_t) src.get_anclvl(j);
        tr_pid[j] = (Int_t) src.get_pid(j);
        tr_ekin[j] = (Float_t) src.get_ekin(j);
        for (Int_t k = 0; k <= 2; k++)
          {
            tr_xyz[k][j] = (Float_t) src.get_xyz(k, j);
            tr_pxyz[k][j] = (Float_t) src.get_pxyz(k, j);
            tr_impxyz[k][j] = (Float_t) src.get_impxyz(k, j);
          }
        tr_ptot[j] = (Float_t) src.get_ptot(j);
        tr_itparent[j] = (Int_t) src.get_itparent(j);
        tr_idparent[j] = (Int_t) src.get_idparent(j);
        tr_parent_ptr[j] = (Int_t) src.get_parent_ptr(j);
        tr_twrhit[j] = (Int_t) src.get_twrhit(j);
        tr_edep[j] = (Float_t) src.get_edep(j);
      }

    for (size_t j = 0; j < ktrcount; ++j)
      {
        dest.set_id(j, (Int_t) tr_id[j]);
        dest.set_trkno(j, (Int_t) tr_trkno[j]);
        // WATCH-OUT we make use of input dEmcGeaTrack field to store
        // the real-event centrality class now !!!
        //dest.set_input(j, (Int_t) tr_input[j]);
        dest.set_input(j, (Int_t) centClass);
        dest.set_anclvl(j, (Int_t) tr_anclvl[j]);
        dest.set_pid(j, (Int_t) tr_pid[j]);
        dest.set_ekin(j, (Float_t) tr_ekin[j]);
        for (Int_t k = 0; k <= 2; k++)
          {
            dest.set_xyz(k, j, (Float_t) tr_xyz[k][j]);
            dest.set_pxyz(k, j, (Float_t) tr_pxyz[k][j]);
            dest.set_impxyz(k, j, (Float_t) tr_impxyz[k][j]);
          }
        dest.set_ptot(j, (Float_t) tr_ptot[j]);
        dest.set_itparent(j, (Int_t) tr_itparent[j]);
        dest.set_idparent(j, (Int_t) tr_idparent[j]);
        dest.set_parent_ptr(j, (Int_t) tr_parent_ptr[j]);
        dest.set_twrhit(j, (Int_t) tr_twrhit[j]);
        dest.set_edep(j, (Float_t) tr_edep[j]);
      }

    dest.SetRowCount(ktrcount);
  }

  //_____________________________________________________________________
  void copy(const dEmcGeaClusterTrackWrapper& src,
            dEmcGeaClusterTrackWrapper& dest)
  {
    for ( size_t i = 0; i < src.RowCount(); ++i )
      {
        dest.set_id(i, src.get_id(i));
        dest.set_clusid(i, src.get_clusid(i));
        dest.set_evno(i, src.get_evno(i));
        dest.set_keycent(i, src.get_keycent(i));
        dest.set_input(i, src.get_input(i));
        dest.set_type(i, src.get_type(i));
        dest.set_arm(i, src.get_arm(i));
        dest.set_sector(i, src.get_sector(i));
        for ( size_t j = 0; j < 3; ++j )
          {
            dest.set_trkno(j, i, src.get_trkno(j, i));
            dest.set_tracktwrhit(j, i, src.get_tracktwrhit(j, i));
            dest.set_edep_nom(j, i, src.get_edep_nom(j, i));
            dest.set_pid(j, i, src.get_pid(j, i));
            dest.set_ptot(j, i, src.get_ptot(j, i));
            for ( size_t k = 0; k < 3; ++k )
              {
                dest.set_vertex(j, k, i, src.get_vertex(j, k, i));
                dest.set_xyz(j, k, i, src.get_xyz(j, k, i));
              }
            dest.set_ancestry(j, i, src.get_ancestry(j, i));
            dest.set_edep(j, i, src.get_edep(j, i));
            dest.set_efrac(j, i, src.get_efrac(j, i));
            dest.set_measxyz(j, i, src.get_measxyz(j, i));
          }
        dest.set_mease(i, src.get_mease(i));
        dest.set_ecore(i, src.get_ecore(i));
        dest.set_tof(i, src.get_tof(i));
        dest.set_etof(i, src.get_etof(i));
        dest.set_tofmin(i, src.get_tofmin(i));
        dest.set_etofmin(i, src.get_etofmin(i));
        dest.set_tofmax(i, src.get_tofmax(i));
        dest.set_etofmax(i, src.get_etofmax(i));
        dest.set_twrhit(i, src.get_twrhit(i));
        for ( size_t j = 0; j < 2; ++j )
          {
            dest.set_disp(j, i, src.get_disp(j, i));
            dest.set_padisp(j, i, src.get_padisp(j, i));
          }
        for ( size_t j = 0; j < 8; ++j )
          {
            dest.set_partesum(j, i, src.get_partesum(j, i));
          }
        dest.set_charged(i, src.get_charged(i));
        for ( size_t j = 0; j < 3; ++j )
          {
            dest.set_pc3proj(j, i, src.get_pc3proj(j, i));
          }
        dest.set_chi2_sh(i, src.get_chi2_sh(i));
        dest.set_prob_photon_sh(i, src.get_prob_photon_sh(i));
        for ( size_t j = 0; j < 2; ++j )
          {
            dest.set_e_sh(j, i, src.get_e_sh(j, i));
          }
        for ( size_t j = 0; j < 2; ++j )
          {
            dest.set_chglist(j, i, src.get_chglist(j, i));
          }
      }

    dest.SetRowCount(src.RowCount());
  }

  //__________________________________________________________________________
  void copy(const dEmcGeaTrackClusterWrapper& src,
            dEmcGeaTrackClusterWrapper& dest)
  {
    for ( size_t i = 0; i < src.RowCount(); ++i )
      {
        dest.set_id(i, src.get_id(i));
        dest.set_trkno(i, src.get_trkno(i));
        dest.set_track_ptr(i, src.get_track_ptr(i));
        dest.set_input(i, src.get_input(i));
        for ( size_t j = 0; j < 3; ++j )
          {
            dest.set_clusid(j, i, src.get_clusid(j, i));
          }
        dest.set_pid(i, src.get_pid(i));
        dest.set_ptot(i, src.get_ptot(i));
        dest.set_nom_edep(i, src.get_nom_edep(i));
        for ( size_t j = 0; j < 3; ++j )
          {
            dest.set_edep(j, i, src.get_edep(j, i));
            dest.set_efrac(j, i, src.get_efrac(j, i));
          }
      }

    dest.SetRowCount(src.RowCount());
  }

  //__________________________________________________________________________
  void copy(const fkinWrapper& src, fkinWrapper& dest)
  {
    for ( size_t i = 0; i < src.RowCount(); ++i ) 
      {
	dest.set_true_track(i,src.get_true_track(i));
	dest.set_subevent(i,src.get_subevent(i));
	dest.set_ntrack(i,src.get_ntrack(i));
	dest.set_ptot(i,src.get_ptot(i));
	dest.set_pthet(i,src.get_pthet(i));
	dest.set_pphi(i,src.get_pphi(i));
	dest.set_r_vertex(i,src.get_r_vertex(i));
	dest.set_z_vertex(i,src.get_z_vertex(i));
	dest.set_th_vertx(i,src.get_th_vertx(i));
	dest.set_ph_vertx(i,src.get_ph_vertx(i));
	dest.set_itparent(i,src.get_itparent(i));
	dest.set_idparent(i,src.get_idparent(i));
	dest.set_idpart(i,src.get_idpart(i));
	dest.set_nfile(i,src.get_nfile(i));	
      }

    dest.SetRowCount(src.RowCount());
  }

  //_____________________________________________________________________________
  void copy(const primaryWrapper& src, primaryWrapper& dest)
  {
     for ( size_t i = 0; i < src.RowCount(); ++i ) 
      {
	dest.set_key(i,src.get_key(i));
	dest.set_event_track(i,src.get_event_track(i));
	dest.set_subevent_track(i,src.get_subevent_track(i));
	dest.set_true_track(i,src.get_true_track(i));
	dest.set_subevent(i,src.get_subevent(i));
	dest.set_idpart(i,src.get_idpart(i));
	dest.set_nfile(i,src.get_nfile(i));
	dest.set_px_momentum(i,src.get_px_momentum(i));
	dest.set_py_momentum(i,src.get_py_momentum(i));
	dest.set_pz_momentum(i,src.get_pz_momentum(i));
      }
     dest.SetRowCount(src.RowCount());
  }
}

//_____________________________________________________________________________
EmcEmbedOutputMover::EmcEmbedOutputMover(const char* realnode /* ="REAL" */,
					 const char* simunode /* ="SIMU" */,
					 const char* mergednode /* ="TOP" */)
  : SubsysReco("EmcEmbedOutputMover"),
    fRealNode(realnode),
    fSimuNode(simunode),
    fMergedNode(mergednode)
{}

//_____________________________________________________________________________
bool
EmcEmbedOutputMover::moveMergedClusters()
{
  // Move clusters from (fMergedTopNode)/DST/EMC/emcClusterContainer to
  // (fMergedTopNode)/DST/emcClusterContainer. Move only clusters that
  // were merged (i.e. simfrac > 0 && simfrac <= 1 )

  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode != 0);

  emcNodeHelper nh;

  PHCompositeNode* emcNode = nh.findCompositeNode(mergedTopNode, "EMC");
  assert(emcNode != 0);

  emcClusterContainer* src =
    nh.getObject<emcClusterContainer>("emcClusterContainer", emcNode);
  assert(src != 0);

  PHCompositeNode* dstNode = nh.findCompositeNode(mergedTopNode, "DST");
  assert(dstNode != 0);

  emcClusterContainer* dest =
    nh.getObject<emcClusterContainer>("emcClusterContainer", dstNode);
  assert(dest != 0);

  dest->Reset();

  for ( size_t i = 0; i < src->size(); ++i )
    {
      emcClusterContent* t = src->getCluster(i);
      assert(t != 0);
      if ( t->isMerged() || t->isSimulated() )
        {
          emcClusterContent* td = dest->addCluster(dest->size(), *t);
          assert(td != 0);
        }
    }

  return true;
}

//_____________________________________________________________________________
bool
EmcEmbedOutputMover::movePHGlobal()
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode != 0);

  PHCompositeNode* realTopNode = se->topNode(fRealNode.c_str());
  assert(realTopNode != 0);

  PHGlobal* src = findNode::getClass<PHGlobal>(realTopNode, "PHGlobal");
  assert(src != 0);

  PHGlobal* global = src->clone();
  assert(global!=0);

  replaceObject<PHGlobal>(mergedTopNode,global,"PHGlobal");
  return true;
}

//_____________________________________________________________________________
bool
EmcEmbedOutputMover::moveSimulatedClusters()
{
  // Move pure simulated clusters to DST/EVA output node.

  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode != 0);
  PHCompositeNode* simuTopNode = se->topNode(fSimuNode.c_str());
  assert(simuTopNode != 0);

  emcClusterContainer* src =
    findNode::getClass<emcClusterContainer>(simuTopNode, "emcClusterContainer");
  emcClusterContainer* dest =
    findNode::getClass<emcClusterContainer>(mergedTopNode,
                                      "emcSimClusterContainer");

  if (!src || !dest)
    {
      return false;
    }

  dest->Reset();

  for ( size_t i = 0; i < src->size(); ++i )
    {
      emcClusterContent* t = src->getCluster(i);
      assert(t != 0);
      emcClusterContent* td = dest->addCluster(dest->size(), *t);
      assert(td != 0);
    }

  assert(src->size() == dest->size());

  return true;
}

//_____________________________________________________________________________
bool
EmcEmbedOutputMover::moveSTAFtables()
{
  // Move the STAF Evaluation tables from the SIMU node
  // to the Merged (output) node.

  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode != 0);
  PHCompositeNode* simuTopNode = se->topNode(fSimuNode.c_str());
  assert(simuTopNode != 0);

  dEmcGeaTrackWrapper* geaTrack_src =
    findNode::getClass<dEmcGeaTrackWrapper>(simuTopNode, "dEmcGeaTrack");
  dEmcGeaTrackWrapper* geaTrack_dest =
    findNode::getClass<dEmcGeaTrackWrapper>(mergedTopNode, "dEmcGeaTrack");

  dEmcGeaClusterTrackWrapper* geaClusterTrack_src =
    findNode::getClass<dEmcGeaClusterTrackWrapper>(simuTopNode, "dEmcGeaClusterTrack");
  dEmcGeaClusterTrackWrapper* geaClusterTrack_dest =
    findNode::getClass<dEmcGeaClusterTrackWrapper>(mergedTopNode, "dEmcGeaClusterTrack");

  dEmcGeaTrackClusterWrapper* geaTrackCluster_src =
    findNode::getClass<dEmcGeaTrackClusterWrapper>(simuTopNode, "dEmcGeaTrackCluster");
  dEmcGeaTrackClusterWrapper* geaTrackCluster_dest =
    findNode::getClass<dEmcGeaTrackClusterWrapper>(mergedTopNode, "dEmcGeaTrackCluster");

  fkinWrapper* fkin_src = 
    findNode::getClass<fkinWrapper>(simuTopNode, "fkin");
  assert(fkin_src!=0);
  fkinWrapper* fkin_dest = 
    findNode::getClass<fkinWrapper>(mergedTopNode, "fkin");
  assert(fkin_dest!=0);
 
  primaryWrapper* primary_src = 
    findNode::getClass<primaryWrapper>(simuTopNode, "primary");
  assert(primary_src!=0);
  primaryWrapper* primary_dest = 
    findNode::getClass<primaryWrapper>(mergedTopNode, "primary");
  assert(primary_dest!=0);

  if (!geaTrack_src || !geaTrack_dest ||
      !geaClusterTrack_src || !geaClusterTrack_dest ||
      !geaTrackCluster_src || !geaTrackCluster_dest ||
      !fkin_src || !fkin_dest ||
      !primary_src || !primary_dest)

    {
      return false;
    }

  int centClass = 0; // FIXME: get this one from real node.

  copy(*geaTrack_src, *geaTrack_dest, centClass);
  copy(*geaClusterTrack_src, *geaClusterTrack_dest);
  copy(*geaTrackCluster_src, *geaTrackCluster_dest);
  copy(*fkin_src,*fkin_dest);
  copy(*primary_src,*primary_dest);

  return true;
}

//_____________________________________________________________________________
bool
EmcEmbedOutputMover::moveSync()
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode != 0);

  PHCompositeNode* realTopNode = se->topNode(fRealNode.c_str());
  assert(realTopNode != 0);

  SyncObject* src = findNode::getClass<SyncObject>(realTopNode, "Sync");
  assert(src != 0);
  SyncObject* dest = findNode::getClass<SyncObject>(mergedTopNode, "Sync");
  assert(dest != 0);

  *dest = *src;
  return true;
}

//_____________________________________________________________________________
bool
EmcEmbedOutputMover::moveVtxOut()
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode != 0);

  PHCompositeNode* simuTopNode = se->topNode(fSimuNode.c_str());
  assert(simuTopNode != 0);

  VtxOut* src = findNode::getClass<VtxOut>(simuTopNode, "VtxOut");
  assert(src != 0);

  VtxOut* vtxout = src->clone();
  assert(vtxout!=0);

  replaceObject<VtxOut>(mergedTopNode,vtxout,"VtxOut");
  return true;
}

//_____________________________________________________________________________
int
EmcEmbedOutputMover::process_event(PHCompositeNode*)
{
  // First, move our meat : the merged clusters !
  bool ok = moveMergedClusters();

  if (!ok)
    {
      // No merged clusters, or copy failed.
      // In any case, do not output this event.
      return DISCARDEVENT;
    }

  // The Sync stuff, so our output DST
  // will be readable together with other DSTs.

  ok = moveSync();

  if (!ok)
    {
      std::cerr << PHWHERE << " Copy of sync failed." << std::endl;
      return ABORTEVENT;
    }

  // We have merged clusters to output, so look at what else
  // we need to move to the output nodes.

  ok = moveSTAFtables();

  if (!ok)
    {
      std::cerr << PHWHERE << " Copy of STAF tables failed." << std::endl;
      return ABORTEVENT;
    }

  ok = moveSimulatedClusters();

  if (!ok)
    {
      std::cerr << PHWHERE << " Copy of simulated clusters failed."
      << std::endl;
      return ABORTEVENT;
    }

  ok = movePHGlobal();

  if (!ok)
    {
      std::cerr << PHWHERE << " Copy of PHGlobal failed."
      << std::endl;
      return ABORTEVENT;
    }

  ok = moveVtxOut();

  if (!ok)
    {
      std::cerr << PHWHERE << " Copy of VtxOut failed."
      << std::endl;
      return ABORTEVENT;
    }

  return EVENT_OK;
}
