
#define EIGEN_INITIALIZE_MATRICES_BY_ZERO 1

// PHENIX
#include <SvxGhit.h>
#include <SvxGhitRawhit.h>
#include <SvxRawhitCluster.h>
#include <SvxClusterv4.h>
#include <SvxSegment.h>
#include <SvxCentralTrack.h>
#include <SvxTrack.h>
#include <SvxTrackFit_v1.h>

#include <SvxGhitList.h>
#include <SvxGhitRawhitList.h>
#include <SvxRawhitClusterList.h>
#include <SvxClusterListv4.h>
#include <SvxSegmentList.h>
#include <SvxCentralTrackList.h>
#include <SvxTrackList_v1.h>
#include <VtxOut.h>
#include <Fun4AllReturnCodes.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <RunHeader.h>

// ROOT
#include <TTree.h>
#include <TFile.h>

#include <Eigen/Core>

#include "SvxKalmanFitter.h"
#include "SvxKalmanNode.h"
#include "PCAKalmanNode.h"
#include "KFDebugTree.h"
#include "RKTrackRep.h"

#include <algorithm>
#include <string>
using namespace std;
using Eigen::VectorXd;

double sqr(const double &);
float sqr(const float &);

//#define DEBUG

SvxKalmanFitter* SvxKalmanFitter::instance_ = 0;

//-------------------------------------------------------------------
SvxKalmanFitter* SvxKalmanFitter::get()
{
  if(!instance_)
    instance_ = new SvxKalmanFitter(false, "tree.root");
  return instance_;
}

//-------------------------------------------------------------------
SvxKalmanFitter::SvxKalmanFitter(bool use_real_field, const char* debugfilename) :
  SubsysReco("SvxKalmanFitter"),
  geom_(0),
  vtxout_(0),
  cnt_(0),
  mc_eval_list_(0),
  ghit_list_(0),
  ghit_rawhit_list_(0),
  rawhit_cluster_list_(0),
  cluster_list_(0),
  segment_list_(0),
  central_track_list_(0),
  track_list_(0),
  current_segment_(0),
  current_central_track_(0),
  _mag_field_center(0),
  _cm_to_gev(0),
  read_par_from_file_(true),
  include_vtx_pos_(false),
  use_vertex_(false),
  reverse_direction_(false),
  use_real_fieldmap_(use_real_field),
  field_filename_("Sim3D++.root"),
  reading_segments_(false),
  reading_central_tracks_(false),
  _timer(PHTimeServer::get()->insert_new("SvxKalmanFitter")),
  tree_(0)
{
#ifdef DEBUG
  tree_ = new KFDebugTree(5, debugfilename);
#endif

  verbosity = 0;
  return;
}

//-------------------------------------------------------------------
SvxKalmanFitter::~SvxKalmanFitter()
{
  if(tree_) delete tree_;

  RKTrackRep::delete_field();

  for(unsigned int i=0; i<svxnodes.size(); ++i)
    delete svxnodes[i];
  svxnodes.clear();
}

//-------------------------------------------------------------------
void SvxKalmanFitter::setup_magnetic_field()
{
  if(use_real_fieldmap_) {
    int field_verbosity = 0;
    RKTrackRep::load_field(field_filename_.c_str(), 1., field_verbosity);

    // store field value at origin for helix calculations
    TVector3 h1 = RKTrackRep::getFieldValTesla( TVector3(0,0,0) );
    _mag_field_center = h1(2); // convert to Tesla
    _cm_to_gev = fabs(_mag_field_center)/333.6;
    
  } else {
    _mag_field_center = -0.9;
    _cm_to_gev = fabs(_mag_field_center)/333.6;
    RKTrackRep::set_uniform_field_tesla( TVector3(0, 0, _mag_field_center) );
  }
  return;
}


//-------------------------------------------------------------------
// InitRun just fills the geometry object and grabs pointers to the
// things on the node tree that we'll need.
//
int SvxKalmanFitter::InitRun(PHCompositeNode *topNode)
{
  setup_magnetic_field();

  // check magnet current 
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!runheader) {
    cout << PHWHERE<< "ERROR: Can't find RunHeader on node tree. " << endl;
    return ABORTRUN;
  }

  float field_scale = (runheader->get_currentCentral()>0) ? 1.0 : -1.0;
  _mag_field_center *= field_scale;

  cout << PHWHERE << "Setting magnetic field scale factor to: " << field_scale << endl;
  RKTrackRep::set_field_scale_factor(field_scale);
  return 0;
}

//-------------------------------------------------------------------
int SvxKalmanFitter::GetNodes(PHCompositeNode *topNode)
{
  // Grab the DST nodes we need
  geom_ = findNode::getClass<svxDetectorGeo>(topNode,"svxDetectorGeo");
  if(!geom_) {
    cerr << PHWHERE << " ERROR: Can't find svxDetectorGeo on node tree." << endl;
    return ABORTRUN;
  }
  
  ghit_list_ = findNode::getClass<SvxGhitList>(topNode,"SvxGhitList");

  cluster_list_ = findNode::getClass<SvxClusterList>(topNode,"SvxClusterList");
  if(!cluster_list_) {
    cerr << PHWHERE << " ERROR: Can't find SvxClusterList." << endl;
    return ABORTEVENT;
  }

  if(reading_central_tracks_) {
    central_track_list_ = findNode::getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackList");
    if(!central_track_list_) {
      cerr << PHWHERE << " ERROR: Can't find SvxCentralTrackList." << endl;
      return ABORTEVENT;
    }
    
    cnt_ = findNode::getClass<PHCentralTrack>(topNode,"PHCentralTrack");
    if(!cnt_) {
      cerr << PHWHERE << " ERROR: Can't find PHCentralTrack." << endl;
      return ABORTEVENT;
    }

  } else {
    segment_list_ = findNode::getClass<SvxSegmentList>(topNode,"SvxSegmentList");
    if(!segment_list_) {
      cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;
      return ABORTEVENT;
    }
  }
  
  rawhit_cluster_list_ = findNode::getClass<SvxRawhitClusterList>(topNode,"SvxRawhitClusterList");
  if(!rawhit_cluster_list_) {
    cerr << PHWHERE << " ERROR: Can't find SvxRawhitClusterList." << endl;
    return ABORTEVENT;
  }
  
  ghit_rawhit_list_ = findNode::getClass<SvxGhitRawhitList>(topNode,"SvxGhitRawhitList");
 
  vtxout_ = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if(!vtxout_) {
    cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;
    return ABORTEVENT;
  } else if( !vtxout_->isValid() ) {
    cerr << PHWHERE << " VtxOut: no vertex for this event!" << endl;
    return ABORTEVENT;
  }

  PHNodeIterator node_iter(topNode);
  
  PHCompositeNode *svx_node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", "SVX"));
  if(!svx_node) {
    cerr << PHWHERE << "SVX node missing, doing nothing." << endl;
    return ABORTEVENT;
  }
      
#ifdef DEBUG
  if(reading_central_tracks_){
    mc_eval_list_ = findNode::getClass<McEvalSingleList>(topNode,"McSingle");
    if(!mc_eval_list_) {
      cerr << PHWHERE << " ERROR: Can't find McEvalSingleList." << endl;
      return ABORTEVENT;
    }
  
    tree_->fill_dc_map(mc_eval_list_, cnt_);
  }
#endif

  return 0;
}

//-------------------------------------------------------------------
//
int SvxKalmanFitter::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  static unsigned long ievent=0;
  if(ievent%1000==0 || verbosity>1) cout << "SvxKalmanFitter::process_event() --- event " << ievent << endl;
  ++ievent;

  int retcode = GetNodes(topNode);
  if(retcode!=0) return retcode;

  // print some event info
  if(verbosity>0) {
    if(segment_list_) cout << segment_list_->get_nSegments() << " segments in this event." << endl;
    if(cluster_list_) cout << cluster_list_->get_nClusters() << " Clusters in this event" << endl;
    if(ghit_list_) cout << ghit_list_->get_nGhits() << " Ghits in this event" << endl;
    cout << rawhit_cluster_list_->get_nRawhitClusters() << " RawhitClusters in this event" << endl;
    if(ghit_rawhit_list_) cout << ghit_rawhit_list_->get_nGhitRawhits() << " GhitRawhits in this event" << endl;
    
    for(int iclus=0; iclus<cluster_list_->get_nClusters(); iclus++) {
      SvxCluster* cluster = cluster_list_->get_Cluster(iclus);
      cout << "cluster " << iclus << ": (" << cluster->get_xyz_global(0) << "," << cluster->get_xyz_global(1) << "," << cluster->get_xyz_global(2) << ")    r=" << sqrt(sqr(cluster->get_xyz_global(0)) + sqr(cluster->get_xyz_global(1))) << "  phi=" << atan2(cluster->get_xyz_global(1), cluster->get_xyz_global(0)) << endl;
    }

    if(ghit_list_) {
      for(int igh=0; igh<ghit_list_->get_nGhits(); igh++ ){
      
        SvxGhit* ghit = ghit_list_->get_Ghit(igh);
      
        cout << "MC hit " << igh << "  part:" << ghit->get_idPart()
             << " trk:" << ghit->get_mctrack()
             << " x=(";
        for(int idir=0; idir<3; idir++)
          cout << ghit->get_xyzglobal(idir) << ((idir==2)?")":",");
        
        float mcptot = 0;
        cout << " p=(";
        for(int idir=0; idir<3; idir++) {
          cout << ghit->get_pmomxyz(idir) << ((idir==2)?")":",");
          mcptot += (ghit->get_pmomxyz(idir)*ghit->get_pmomxyz(idir));
        }
        cout << "  ptot=" << sqrt(mcptot) << endl;
      }
    }
  }  // if(verbosity>0)
  
  
  //-------------------------------------------------------------------
  // loop over tracks
  // convert input track into our storage object SvxTrack
  // get measured cluster positions
  // perform kalman fit
  //
  int ntracks = reading_central_tracks_ ?
    central_track_list_->get_nCentralTracks() :
    segment_list_->get_nSegments();    

  SvxTrack* track_ptr = 0;
  for(int iseg=0; iseg<ntracks; iseg++) {
    if(reading_central_tracks_) {
      current_central_track_ = central_track_list_->getCentralTrack(iseg);
      track_ptr = create_track_from_centraltrack(current_central_track_);
    } else {
      current_segment_ = segment_list_->get_segment(iseg);
      track_ptr = create_track_from_segment(current_segment_);
    }

    bool success = fit_track(track_ptr, use_vertex_, reverse_direction_);
    if(!success)
      cerr << PHWHERE << "Kalman fit failed for track " << iseg << endl;
  
    if(verbosity>1) track_ptr->print();
    delete track_ptr;
  }

  _timer.get()->stop();
  if(verbosity>1) cout << "SvxKalmanFitter::process_event() - END" << endl;
  return 0;
}

//-------------------------------------------------------------------
bool SvxKalmanFitter::fit_track(SvxSegment* segment)
{
  // first convert SvxSegment to our internal class (SvxTrack)
  SvxTrack* track_ptr = create_track_from_segment(segment);

  // now do the fit
  bool success = fit_track(track_ptr, use_vertex_, reverse_direction_);
  if(!success)
    cerr << PHWHERE << "Kalman fit failed for SvxSegment " << segment << endl;
  
  return success;
}

//-------------------------------------------------------------------
bool SvxKalmanFitter::fit_track(SvxCentralTrack* central_track)
{
  // first convert SvxCentralTrack to our internal class (SvxTrack)
  SvxTrack* track_ptr = create_track_from_centraltrack(central_track);

  // now do the fit
  bool success = fit_track(track_ptr, use_vertex_, reverse_direction_);
  if(!success)
    cerr << PHWHERE << "Kalman fit failed for SvxCentralTrack " << central_track << endl;
  
  return success;
}

//-------------------------------------------------------------------
// For this track:
//  get measured cluster positions
//  perform kalman fit
bool SvxKalmanFitter::fit_track(SvxTrack* track_ptr, bool use_vertex, bool reverse_direction)
{
  vector<KalmanNode*> nodes;

  for(unsigned int i=0; i<svxnodes.size(); i++) {
    svxnodes[i]->reset();
  }
  dcnode.reset();

  unsigned int inode = 0;
    
#ifdef DEBUG
  tree_->reset();
  tree_->nclusters = 0;
  if(segment_list_) tree_->nsegments = segment_list_->get_nSegments();
#endif

  SvxTrackFit_v1 ff;
  if(reading_central_tracks_) {
    ff = fill_dc_info(track_ptr);
  } else {
    ff = fast_fit_track(track_ptr);
  }

  //const SvxTrackFit* fastfit = track_ptr->get_fit(SvxTrackFit::FastHelix);
  const SvxTrackFit* fastfit = &ff;
  if(!fastfit) {
    cout << PHWHERE << "Fast fit failed!  ABORTING!!!" << endl;
    return false;
  }

  if(verbosity>2) {
    cout << "Track before Kalman fit:" << endl;
    track_ptr->print();
  }

#ifdef DEBUG
  float clus_pos[4][3] = {{0}};
#endif

  vector<float> momenta;
  vector<SvxCluster*> clusters = track_ptr->get_clusters_ordered();
  for(unsigned int iclus=0; iclus<clusters.size(); iclus++) {
#ifdef DEBUG
    tree_->nclusters += 1;
#endif
    
    // Add a new node, and set the particle charge
    //         unsigned int inode = svxnodes.size();
    if( inode>=svxnodes.size() )
      svxnodes.push_back(new SvxKalmanNode);

    svxnodes[inode]->set_verbosity( verbosity );
    dcnode.set_verbosity( verbosity );
        
    SvxCluster* cluster = clusters[iclus];
    SvxSensor* sensor = geom_->GetSensorPtr(cluster->get_layer(),
                                            cluster->get_ladder(),
                                            cluster->get_sensor());
    svxnodes.at(inode)->set_sensor(sensor);

#ifdef DEBUG
    int clusid = clusters[iclus]->get_hitID();
    SvxGhit* ghit = 0;
    if(ghit_list_ && ghit_rawhit_list_) {
      for(int irc=0; irc<rawhit_cluster_list_->get_nRawhitClusters(); irc++) {
        int hitid=-1;
        if( clusid == rawhit_cluster_list_->get_RawhitCluster(irc)->get_clusterID() ){
          hitid = rawhit_cluster_list_->get_RawhitCluster(irc)->get_rawhitID();
              
          for(int igr=0; igr<ghit_rawhit_list_->get_nGhitRawhits(); igr++ ){
                
            SvxGhitRawhit* sgr = ghit_rawhit_list_->get_GhitRawhit(igr);
            int ghitid=-1;
            if( hitid == sgr->get_rawhitID() ){
              ghitid = sgr->get_ghitID();
              for(int igh=0; igh<ghit_list_->get_nGhits(); igh++ ){
                if(ghitid == ghit_list_->get_Ghit(igh)->get_hitID())
                  ghit = ghit_list_->get_Ghit(igh);
              }

              //ghit = ghit_list_->get_Ghit(ghitid);
              break;
            }
          }
        }
      }
    }
#endif
    
#ifdef DEBUG
    float deltap = 0;
#endif

    // Save out the MC true momentum at the first and last layers
#ifdef DEBUG
    if(ghit) { // only if an associated SvxGhit was found for this cluster
      int layer = cluster->get_layer();
      for(int idir=0; idir<3; idir++){
        tree_->mcmom[layer][idir] = ghit->get_pmomxyz(idir);
        tree_->mcpid[layer] = ghit->get_idPart();
        
        tree_->clusterpos[tree_->nclusters-1][idir] = cluster->get_xyz_global(idir);
        tree_->mcpos[tree_->nclusters-1][idir] = ghit->get_xyzglobal(idir);
      }

      double momentum = sqrt(sqr(ghit->get_pmomxyz(0))+sqr(ghit->get_pmomxyz(1))+sqr(ghit->get_pmomxyz(2)));
      if(momenta.size()>0)
        deltap = momentum - momenta.back();
      momenta.push_back(momentum);
      
      // print some stuff
      if(verbosity > 1) {
        cout << "MC hit x=(";
        for(int idir=0; idir<3; idir++){
          cout << ghit->get_xyzglobal(idir) << ((idir==2)?")":",");
        }
        
        float mcptot = 0;
        cout << " p=(";
        for(int idir=0; idir<3; idir++) {
          cout << ghit->get_pmomxyz(idir) << ((idir==2)?")":",");
          mcptot += (ghit->get_pmomxyz(idir)*ghit->get_pmomxyz(idir));
        }
        cout << "  ptot=" << sqrt(mcptot) << endl;
      }
    } else {
      cout << PHWHERE << "No associated SvxGhit found!" << endl;
    }
#endif
    
    svxnodes.at(inode)->load_sensor_geometry(sensor);
    // svxnodes.at(inode)->use_energy_loss(true);
    // svxnodes.at(inode)->set_energy_loss(deltap);

    // print some rotation-debugging stuff
    if( verbosity > 1 ){
      double localx[3] = {1,0,0};
      double localy[3] = {0,1,0};
      double localz[3] = {0,0,1};
      double globalx[3] = {0};
      double globaly[3] = {0};
      double globalz[3] = {0};
      sensor->vector_local2global(localx, globalx);
      sensor->vector_local2global(localy, globaly);
      sensor->vector_local2global(localz, globalz);

      cout << "x-axis: (" << globalx[0] << ", " << globalx[1] << ", " << globalx[2] << ")" << endl;
      cout << "y-axis: (" << globaly[0] << ", " << globaly[1] << ", " << globaly[2] << ")" << endl;
      cout << "z-axis: (" << globalz[0] << ", " << globalz[1] << ", " << globalz[2] << ")" << endl;
    }
    
#ifdef DEBUG
    for(int idir=0; idir<3; idir++)
      clus_pos[cluster->get_layer()][idir] = cluster->get_xyz_global(idir);
      
    if(ghit_list_ && ghit_rawhit_list_ && ghit) {
      float mcpt = 0;
      float mcptot = 0;
      for(int idir=0; idir<3; idir++) {
        if(idir<2) mcpt += (ghit->get_pmomxyz(idir)*ghit->get_pmomxyz(idir));
        mcptot += (ghit->get_pmomxyz(idir)*ghit->get_pmomxyz(idir));
      }
      
      Eigen::VectorXd mcpos(3,1);
      for(int idir=0; idir<3; idir++)
        mcpos(idir,0) = ghit->get_xyzglobal(idir);
      Eigen::VectorXd mcpos_local( svxnodes.at(inode)->global_to_local_xyz(mcpos) );
      
      Eigen::VectorXd mcmom(3,1);
      for(int idir=0; idir<3; idir++)
        mcmom(idir) = ghit->get_pmomxyz(idir);
      Eigen::VectorXd mcmom_local( svxnodes.at(inode)->global_to_local_mom(mcmom) );
      
      int glayer = ghit->get_layer();
      tree_->statemc[glayer][0] = 1./sqrt(mcptot);
      tree_->statemc[glayer][1] = mcmom_local(1,0)/mcmom_local(0,0);
      tree_->statemc[glayer][2] = mcmom_local(2,0)/mcmom_local(0,0);
      tree_->statemc[glayer][3] = mcpos_local(1,0);
      tree_->statemc[glayer][4] = mcpos_local(2,0);

      // cout << "USING GHIT FOR POSITIONS! REMEMBER TO DISABLE!!!" << endl;
      //meas_pos(0,0) = mcpos_local[0];
      //meas_pos(1,0) = mcpos_local[2];
    }
#endif
    
    svxnodes.at(inode)->load_measurement(cluster);

    Eigen::VectorXd meas_pos = svxnodes.at(inode)->get_meas();
    Eigen::VectorXd meas_pos_sigma = svxnodes.at(inode)->get_meas_sigma();
                        
    // print some stuff        
    if(verbosity>1) {
      cout << "meas pos:" << meas_pos << endl;
      cout << "local: (" << cluster->get_xyz_local(0) << "," << cluster->get_xyz_local(1)
           << "," << cluster->get_xyz_local(2) << ")" << endl;
      cout << "global: (" << cluster->get_xyz_global(0) << "," << cluster->get_xyz_global(1)
           << "," << cluster->get_xyz_global(2) << ")  r="
           << sqrt(sqr(cluster->get_xyz_global(0)) + sqr(cluster->get_xyz_global(1))) << endl;
      cout << "sigma_x: " << meas_pos_sigma(0) << "  sigma_z: " << meas_pos_sigma(1) << endl;
    }

    inode++;
  }  // end loop over clusters (iclus)

  if(reading_central_tracks_) {
    PHSnglCentralTrack *sngl_cnt_trk = cnt_->get_track( current_central_track_->getDchIndex() );
    dcnode.load_measurement( sngl_cnt_trk );
  }

  // Set up a vector of node pointers in whatever order we're using
  if(!reverse_direction) {
    for(unsigned int jnode=0; jnode<inode; ++jnode) {
      nodes.push_back(svxnodes[jnode]);
    }
    if(reading_central_tracks_) nodes.push_back(&dcnode);
  } else {
    if(reading_central_tracks_) nodes.push_back(&dcnode);
    for(unsigned int jnode=inode-1; /*jnode>=0&&*/jnode<inode; --jnode) {
      nodes.push_back(svxnodes.at(jnode));
    }
  }
  
  // Some things that all nodes need to know...
  for(unsigned int jnode=0; jnode<nodes.size(); ++jnode) {
    nodes[jnode]->set_verbosity( verbosity );
    nodes[jnode]->set_is_positive( fastfit->get_charge() > 0 );
    if(reverse_direction)
      nodes[jnode]->set_reverse_direction(true);
  }

  VectorXd pos(3);
  VectorXd mom(3);

  // Get the vertex position
  pos(0) = vtxout_->get_Vertex().getX();
  pos(1) = vtxout_->get_Vertex().getY();
  pos(2) = vtxout_->get_Vertex().getZ();

  // Set the initial momentum to the fast fit result
  SvxTrackProj::Location vertexloc(SvxTrackProj::PrimaryVertex);
  SvxTrackProj::Location innerloc(fastfit->get_innermost_projection()->get_location());
  SvxTrackProj::Location outerloc(fastfit->get_outermost_projection()->get_location());
  SvxTrackProj::Location startloc(innerloc);
  if(use_vertex) startloc = vertexloc;
  if(reverse_direction) startloc = outerloc;

  if(fastfit->has_projection(startloc)) {
    mom(0) = fastfit->get_projection(startloc)->get_px();
    mom(1) = fastfit->get_projection(startloc)->get_py();
    mom(2) = fastfit->get_projection(startloc)->get_pz();
    if(verbosity>1)
      cout << "Startloc: " << startloc << "  (px,py,pz): " << mom
           << "  p: " << mom.norm() << endl;
    
    if(mom(0) != mom(0))
      cout << "mom(0) != mom(0)" << endl;
    if(mom(1) != mom(1))
      cout << "mom(1) != mom(1)" << endl;
    if(mom(2) != mom(2))
      cout << "mom(2) != mom(2)" << endl;

    double ptot = mom.norm();
    if(ptot != ptot) {
      cout << PHWHERE << " fast fit momentum is nan!!!" << endl;
      return false;
    }

  } else {
    cout << PHWHERE << " The fast fit has no projection at the starting layer!" << endl;
    return false;
  }

  if(verbosity>1) {
    cout << "VtxOut pos (" << pos(0) << ","  << pos(1) << ","  << pos(2) << ")" << endl;
    cout << "SvxSegment mom (" << mom(0) << ","  << mom(1) << ","  << mom(2) << ")" << endl;
  }

  // initialize momentum to the fast fit value
  unsigned int lastnode = nodes.size()-1;
  unsigned int n_svx_nodes = svxnodes.size();
  
  // Set up the first node with a seed momentum vector
  bool failed = false;
  Eigen::VectorXd input_mom(3,1);
  input_mom << mom(0), mom(1), mom(2);
  nodes[0]->initialize_first_node(input_mom);
  
  // print what we got out
  if( verbosity > 1 ){
    cout << "x_filt:" << nodes[0]->get_x_filt() << endl;
    cout << "C_filt:" << nodes[0]->get_C_filt() << endl;
  }
 
  for(unsigned int jnode=1; jnode<=lastnode; jnode++) {
    if( !nodes[jnode]->predict(nodes[jnode-1]) ){
      failed=true;
      cerr << "PREDICTION FAILED ON NODE " << jnode << "!!!" << endl;
      break;
    }
    nodes[jnode]->filter();
    
    // print what we got out
    if( verbosity > 1 ){
      cout << "x_pred:" << nodes[jnode]->get_x_pred() << endl;
      cout << "C_pred:" << nodes[jnode]->get_C_pred() << endl;
      cout << "x_filt:" << nodes[jnode]->get_x_filt() << endl;
      cout << "C_filt:" << nodes[jnode]->get_C_filt() << endl;
    }
  }

  if(failed) return false;

  if(verbosity > 1) cout << "Smoothing node " << lastnode << endl;
  nodes[lastnode]->smooth();
  for(int jnode=lastnode-1; jnode>=0; jnode--) {
    nodes[jnode]->smooth( nodes[jnode+1] );
  }

  TVector3 vertex(pos(0), pos(1), pos(2));
  PCAKalmanNode pkn;
  pkn.set_verbosity( verbosity );
  pkn.set_is_positive( fastfit->get_charge() > 0 );
  pkn.load_measurement(vertex);
  pkn.predict(svxnodes[0]);
  Eigen::VectorXd PCA = pkn.get_best_pos_global();
  if(verbosity>0)
    cout << "DCA: " << (PCA-pos).norm() << endl;

#ifdef DEBUG
  for(int i=0; i<3; i++) {
    tree_->vtx[i] = pos(i);
    tree_->pca[i] = PCA(i);
  }

  {
    int idc = tree_->nlayers - 1;
    float mcptot = tree_->simdcmom;
      
    Eigen::VectorXd mcpos(3,1);
    for(int idir=0; idir<3; idir++)
      mcpos(idir) = tree_->mcpos[idc][idir];
    Eigen::VectorXd mcpos_local( dcnode.global_to_local_xyz(mcpos) );
      
    Eigen::VectorXd mcmom(3,1);
    for(int idir=0; idir<3; idir++)
      mcmom(idir) = tree_->mcmom[idc][idir];
    Eigen::VectorXd mcmom_local( dcnode.global_to_local_mom(mcmom) );
      
    tree_->statemc[idc][0] = 1./sqrt(mcptot);
    tree_->statemc[idc][1] = mcmom_local(1)/mcmom_local(0);
    tree_->statemc[idc][2] = mcmom_local(2)/mcmom_local(0);
    tree_->statemc[idc][3] = mcpos_local(1);
    tree_->statemc[idc][4] = mcpos_local(2);
  }
#endif
  
  if(verbosity>1)
    track_ptr->print();

  const SvxTrackFit* oldfit = 0;
  if( !track_ptr->has_fit(SvxTrackFit::OldSvxTracker) ){
    cerr << PHWHERE << "The SvxTrack is missing the OldSvxTracker fit!" << endl;
    return 10;
  } else {
    oldfit = track_ptr->get_fit(SvxTrackFit::OldSvxTracker);
#ifdef DEBUG
    tree_->initmom[0][0] = oldfit->get_projection(innerloc)->get_px();
    tree_->initmom[0][1] = oldfit->get_projection(innerloc)->get_py();
    tree_->initmom[0][2] = oldfit->get_projection(innerloc)->get_pz();
    tree_->initcharge = oldfit->get_charge();
#endif
  }
  
  if( verbosity > 0 ){
    if(oldfit && oldfit->has_projection(innerloc))
      cout << "old mom: " << oldfit->get_projection(innerloc)->get_ptot();

    if(fastfit && fastfit->has_projection(innerloc))
      cout << "  fast mom: " << fastfit->get_projection(innerloc)->get_ptot()
           << "  new mom: " << 1./nodes[lastnode]->get_x_smooth()(0)
           << "  first layer (smoothed): " << 1./svxnodes[0]->get_x_smooth()(0) << endl;
  }
  
  //--------------------------------------------------
  // Let's save some output to a TTree for examination
  //
  // best chi-square is where ever the filtering stopped
  if(verbosity>1)
    cout << "Filling the TTree..." << endl;
  
  float chi_square = nodes[lastnode]->get_chi_square();
  
#ifdef DEBUG
  for(unsigned int jnode=0; jnode<nodes.size(); ++jnode) {
    tree_->kfmom[jnode][0] = nodes[jnode]->get_smoothed_mom_global()(0);
    tree_->kfmom[jnode][1] = nodes[jnode]->get_smoothed_mom_global()(1);
    tree_->kfmom[jnode][2] = nodes[jnode]->get_smoothed_mom_global()(2);
    
    for(unsigned int i=0; i<5; ++i) {
      tree_->state[jnode][0][i] = nodes[jnode]->get_x_pred()(i);
      tree_->state[jnode][1][i] = nodes[jnode]->get_x_filt()(i);
      tree_->state[jnode][2][i] = nodes[jnode]->get_x_smooth()(i);
      for(int j=0; j<5; j++) {
        tree_->covar[jnode][0][i][j] = nodes[jnode]->get_C_pred()(i,j);
        tree_->covar[jnode][1][i][j] = nodes[jnode]->get_C_filt()(i,j);
          tree_->covar[jnode][2][i][j] = nodes[jnode]->get_C_smooth()(i,j);
      }
    }

    tree_->chisq[jnode] = nodes[jnode]->get_chi_square_increment();
  }
  
  // Fill the KFDebugTree object for later examination
  tree_->kfcharge = fastfit->get_charge();
  tree_->Fill();
#endif

  //---------------------------
  // Now fill the SvxTrack object
  //
  // SvxTrackFit_v1 newfit(SvxTrackFit::Kalman);
  // newfit.set_charge( fastfit->get_charge() );
  // newfit.set_chi_square(chi_square);

  // int layercount=0,prevlayer=0;
  // for(unsigned int i=0; i<n_svx_nodes; i++) {
  //   SvxTrackProj_v1 proj;

  //   //cout << PHWHERE << " sensor: " << nodes.at(i).get_sensor() << endl;
  //   int ilayer = svxnodes[i].get_sensor()->get_layer();
  //   if(ilayer != prevlayer)
  //     layercount=0;

  //   if(ilayer==0)      proj.set_location(innerloc);
  //   else if(ilayer==1) proj.set_location(SvxTrackProj::Layer1);
  //   else if(ilayer==2) {
  //     if(layercount==0)      proj.set_location(SvxTrackProj::Layer2A);
  //     else if(layercount==1) proj.set_location(SvxTrackProj::Layer2B);
  //     else if(layercount==2) proj.set_location(SvxTrackProj::Layer2C);
  //     layercount++;
  //   } else if(ilayer==3) {
  //     if(layercount==0)      proj.set_location(SvxTrackProj::Layer3A);
  //     else if(layercount==1) proj.set_location(SvxTrackProj::Layer3B);
  //     else if(layercount==2) proj.set_location(SvxTrackProj::Layer3C);
  //     layercount++;
  //   }
  //   prevlayer = ilayer;

  //   G4Point3D smoothpos = svxnodes[i].get_smoothed_pos_global();
  //   G4Vector3D smoothmom = svxnodes[i].get_smoothed_mom_global();
  //   PHGslMatrix smoothcov = svxnodes[i].get_C_smooth();
  //   proj.set_x( smoothpos(0) );
  //   proj.set_y( smoothpos(1) );
  //   proj.set_z( smoothpos(2) );
  //   proj.set_px( smoothmom(0) );
  //   proj.set_py( smoothmom(1) );
  //   proj.set_pz( smoothmom(2) );
  //   for(unsigned int m=0; m<5; m++)
  //     for(unsigned int n=0; n<5; n++)
  //       proj.set_covar( m, n, smoothcov(m,n) );
  //   proj.set_chi_square( svxnodes[i].get_chi_square_increment() );
  //   newfit.set_projection(proj);
  // }

  // track_ptr->add_fit(newfit);

  //----------------------------------
  // Refill SvxSegment
  //         
  if(reading_central_tracks_) {
    if(verbosity>1)
      cout << "Replacing parameters in segment " << current_central_track_ << endl;

    //current_central_track_->setIsPositive( fastfit->get_charge() > 0 );
    current_central_track_->setChiSquare(chi_square);
    current_central_track_->setIsPrimary(use_vertex);
    
    current_central_track_->set3MomentumAtPrimaryVertex( pkn.get_best_mom_global()(0),
                                                         pkn.get_best_mom_global()(1),
                                                         pkn.get_best_mom_global()(2) );
    
    // MGW - apparently there is no equivalent function in SvxCentralTrack???    
    //   current_central_track_->setProjectedPosition( ilayer,
    //                                        svxnodes[0].get_smoothed_pos_global()(0),
    //                                        svxnodes[0].get_smoothed_pos_global()(1),
    //                                        svxnodes[0].get_smoothed_pos_global()(2),
    //                                        ihit );

  } else {
    if(verbosity>1)
      cout << "Replacing parameters in segment " << current_segment_ << endl;

    current_segment_->setIsPositive( fastfit->get_charge() > 0 );
    current_segment_->setQuality(chi_square);
    current_segment_->setPrimary(use_vertex);

    current_segment_->set3Momentum( svxnodes[0]->get_smoothed_mom_global()(0),
                                    svxnodes[0]->get_smoothed_mom_global()(1),
                                    svxnodes[0]->get_smoothed_mom_global()(2) );
    
    // current_segment_->set3MomentumAtPrimaryVertex( pkn.get_smoothed_mom_global()(0),
    //                                       pkn.get_smoothed_mom_global()(1),
    //                                       pkn.get_smoothed_mom_global()(2) );
  
    int layercount=0,prevlayer=0;
    for(unsigned int i=0; i<n_svx_nodes; i++) {

      int ihit = 0;
      int ilayer = svxnodes.at(i)->get_sensor()->get_layer();
      if(ilayer != prevlayer)
        layercount=0;

      if(ilayer==0 || ilayer==1) {
        ihit=0;
      } else if(ilayer==2) {
        if(layercount==0)      ihit=0;
        else if(layercount==1) ihit=1;
        else if(layercount==2) ihit=2;
        layercount++;
      } else if(ilayer==3) {
        if(layercount==0)      ihit=0;
        else if(layercount==1) ihit=1;
        else if(layercount==2) ihit=2;
        layercount++;
      }
      prevlayer = ilayer;

      current_segment_->setProjectedPosition( ilayer,
                                     svxnodes[0]->get_smoothed_pos_global()(0),
                                     svxnodes[0]->get_smoothed_pos_global()(1),
                                     svxnodes[0]->get_smoothed_pos_global()(2),
                                     ihit );
    }
  }

  return true;
}

//-------------------------------------------------------------------
//
int SvxKalmanFitter::End(PHCompositeNode *)
{
#ifdef DEBUG
  tree_->Finish();
#endif
  return 0;
}

//-------------------------------------------------------------------
//
SvxTrack* SvxKalmanFitter::create_track_from_segment(SvxSegment* segment)
{
  SvxTrack* track_ptr = new SvxTrack_v1;
    
  SvxTrackFit_v1 segfit(SvxTrackFit::OldSvxTracker);

  segfit.set_charge( segment->IsPositive() ? 1 : -1 );
  segfit.set_chi_square( segment->getQuality() );

  SvxTrackProj_v1 segproj(segment->getPrimary() ? 
                          SvxTrackProj::PrimaryVertex : SvxTrackProj::DCA);
  segproj.set_px( segment->get3MomentumAtPrimaryVertex(0) );
  segproj.set_py( segment->get3MomentumAtPrimaryVertex(1) );
  segproj.set_pz( segment->get3MomentumAtPrimaryVertex(2) );
  segfit.set_projection(segproj);

  int tmpcount = 0;
  for(int ilayer=0; ilayer<4; ilayer++) {
    tmpcount += segment->getNhits(ilayer);
  }
    
  if(verbosity>1)
    cout << "SEGMENT " << segment << " HAS " << tmpcount << " CLUSTERS" << endl;
    
  for(int ilayer=0; ilayer<4; ilayer++) {
      
    // Loop over the clusters in each layer (since the panels
    // overlap, it is possible, though unlikely, to have multiple
    // hits in a layer).
    for(int ihit=0; ihit<segment->getNhits(ilayer); ihit++) {
    
      int clusid = segment->getClusterID(ilayer, ihit);
      SvxCluster* cluster = cluster_list_->get_Cluster(cluster_list_->indexOfCluster(clusid));
      track_ptr->add_cluster(cluster);

      SvxTrackProj_v1 segproj;
      if(ilayer==0)      segproj.set_location(SvxTrackProj::Layer0);
      else if(ilayer==1) segproj.set_location(SvxTrackProj::Layer1);
      else if(ilayer==2) {
        if(ihit==0)      segproj.set_location(SvxTrackProj::Layer2A);
        else if(ihit==1) segproj.set_location(SvxTrackProj::Layer2B);
        else if(ihit==2) segproj.set_location(SvxTrackProj::Layer2C);
      } else if(ilayer==3) {
        if(ihit==0)      segproj.set_location(SvxTrackProj::Layer3A);
        else if(ihit==1) segproj.set_location(SvxTrackProj::Layer3B);
        else if(ihit==2) segproj.set_location(SvxTrackProj::Layer3C);
      }
    
      segproj.set_x( segment->getProjectedPosition(ilayer, 0, ihit) );
      segproj.set_y( segment->getProjectedPosition(ilayer, 1, ihit) );
      segproj.set_z( segment->getProjectedPosition(ilayer, 2, ihit) );

      if(ilayer==0) {
        segproj.set_px( segment->get3Momentum(0) );
        segproj.set_py( segment->get3Momentum(1) );
        segproj.set_pz( segment->get3Momentum(2) );
      }
    
      segfit.set_projection(segproj);

    } // loop over ihit (per layer)
  } // loop over ilayer
    
  track_ptr->add_fit(&segfit);
  //delete segfit;

  return track_ptr;
}

//-------------------------------------------------------------------
//
SvxTrack* SvxKalmanFitter::create_track_from_centraltrack(SvxCentralTrack* central_track)
{
  SvxTrack* track_ptr = new SvxTrack_v1;
  
  // Need the CNT for the track charge
  PHSnglCentralTrack *sngl_cnt_trk = cnt_->get_track( central_track->getDchIndex() );
  
  SvxTrackFit_v1 segfit(SvxTrackFit::OldSvxTracker);  
  segfit.set_charge( sngl_cnt_trk->get_charge() );
  segfit.set_chi_square( central_track->getChiSquare() );
  
  SvxTrackProj_v1 segproj(central_track->isPrimary() ? 
                          SvxTrackProj::PrimaryVertex : SvxTrackProj::DCA);
  segproj.set_px( central_track->get3MomentumAtPrimaryVertex(0) );
  segproj.set_py( central_track->get3MomentumAtPrimaryVertex(1) );
  segproj.set_pz( central_track->get3MomentumAtPrimaryVertex(2) );
  segfit.set_projection(segproj);
  
  int tmpcount = central_track->getNhits();
  
  if(verbosity>1)
    cout << "CENTRAL_TRACK " << central_track << " HAS " << tmpcount << " CLUSTERS" << endl;
  
  // Loop over the clusters in each layer (since the panels
  // overlap, it is possible, though unlikely, to have multiple
  // hits in a layer).
  for(int ihit=0; ihit<central_track->getNhits(); ihit++) {
    
    SvxClusterInfo* clusinfo = central_track->getClusterInfo(ihit);
    int ilayer = clusinfo->getLayer();
    int clusid = clusinfo->getClusterId();
      
    SvxCluster* cluster = cluster_list_->get_Cluster(clusid);
    //cluster->set_hitID(clusinfo->getClusterId());
    //       cluster->set_layer(clusinfo->getLayer());
    //       cluster->set_ladder(clusinfo->getLadder());
    //       cluster->set_sensor(clusinfo->getSensor());
      
    //       cluster->set_edgeflag(clusinfo->getEdgeFlag());
    //       cluster->set_adc(0, clusinfo->getAdc(0));
    //       cluster->set_adc(1, clusinfo->getAdc(1));
    //       cluster->set_xyz_global(0, clusinfo->getPosition(0));
    //       cluster->set_xyz_global(1, clusinfo->getPosition(1));
    //       cluster->set_xyz_global(2, clusinfo->getPosition(2));
    //       cluster->set_ambiguous(clusinfo->getAmbiguous());
      
    track_ptr->add_cluster(cluster);

    SvxTrackProj_v1 segproj;
    if(ilayer==0)      segproj.set_location(SvxTrackProj::Layer0);
    else if(ilayer==1) segproj.set_location(SvxTrackProj::Layer1);
    else if(ilayer==2) {
      if(ihit==0)      segproj.set_location(SvxTrackProj::Layer2A);
      else if(ihit==1) segproj.set_location(SvxTrackProj::Layer2B);
      else if(ihit==2) segproj.set_location(SvxTrackProj::Layer2C);
    } else if(ilayer==3) {
      if(ihit==0)      segproj.set_location(SvxTrackProj::Layer3A);
      else if(ihit==1) segproj.set_location(SvxTrackProj::Layer3B);
      else if(ihit==2) segproj.set_location(SvxTrackProj::Layer3C);
    }
    
    //         segproj.set_x( central_track->getProjectedPosition(ilayer, 0, ihit) );
    //         segproj.set_y( central_track->getProjectedPosition(ilayer, 1, ihit) );
    //         segproj.set_z( central_track->getProjectedPosition(ilayer, 2, ihit) );

    //         if(ilayer==0) {
    //           segproj.set_px( central_track->get3Momentum(0) );
    //           segproj.set_py( central_track->get3Momentum(1) );
    //           segproj.set_pz( central_track->get3Momentum(2) );
    //         }
    
    segfit.set_projection(segproj);

  } // loop over ihit (per layer)
  
  track_ptr->add_fit(&segfit);
  return track_ptr;
}

//-------------------------------------------------------------------
//
void SvxKalmanFitter::set_step_size_limit(const double& limit)
{
  return;
}

//-------------------------------------------------------------------
//
SvxTrackFit_v1 SvxKalmanFitter::fast_fit_track(SvxTrack* track)
{
  vector<SvxCluster*> clusters = track->get_clusters_ordered();

  vector<SvxCluster*> use_clusters;
  vector<SvxCluster*> unuse_clusters;
  int prev_layer = -1;
  vector<SvxCluster*>::reverse_iterator riter=clusters.rbegin();
  for(; riter!=clusters.rend(); ++riter) {
    int layer = (*riter)->get_layer();
    if(layer!=prev_layer) {
      use_clusters.push_back(*riter);
      prev_layer = layer;
    } else {
      unuse_clusters.push_back(*riter);
    }
  }

  // if we don't have enough clusters in separate layers, just use them all
  if(use_clusters.size()<3) {
    if(unuse_clusters.size()>0) {
      use_clusters = clusters;
    } else {
      return SvxTrackFit_v1();
    }
  }

  if(verbosity>1)
    cout << "Fitting use_clusters.size(): " << use_clusters.size() << endl;

  // use_clusters has up to one cluster per layer. If it has more than
  // 3 clusters, we should drop the second pixel layer to keep largest
  // spacing
  SvxCluster* clusterA = (use_clusters.size()>3) ? use_clusters[3] : use_clusters[2];
  SvxCluster* clusterB = use_clusters[1];
  SvxCluster* clusterC = use_clusters[0];

  float Ax = clusterA->get_xyz_global(0);
  float Ay = clusterA->get_xyz_global(1);
  float Az = clusterA->get_xyz_global(2);
  
  float Bx = clusterB->get_xyz_global(0);
  float By = clusterB->get_xyz_global(1);
  //float Bz = clusterB->get_xyz_global(2);
  
  float Cx = clusterC->get_xyz_global(0);
  float Cy = clusterC->get_xyz_global(1);
  float Cz = clusterC->get_xyz_global(2);
  
  // find the center of rotation
  float Asum = (Ay*Ay+Ax*Ax);
  float Bsum = (By*By+Bx*Bx);
  float Csum = (Cy*Cy+Cx*Cx);
  float D = 2*(Ax*(By-Cy)+Bx*(Cy-Ay)+Cx*(Ay-By));

  float circ_x = (Asum*(By-Cy)+Bsum*(Cy-Ay)+Csum*(Ay-By))/D;
  float circ_y = (Asum*(Cx-Bx)+Bsum*(Ax-Cx)+Csum*(Bx-Ax))/D;
  
  // find the radius of rotation
  float circ_r_sqr = sqr(Ax-circ_x)+sqr(Ay-circ_y);
  float circ_r = sqrt(circ_r_sqr);
  
  // convert to momentum assuming a constant PHENIX field
  float pt = circ_r * _cm_to_gev;
  
  // compute the pz
  float tan_lambda = (Cz-Az)/(sqrt(Csum)-sqrt(Asum));
  float pz = pt*tan_lambda;

  if(verbosity>0)
    cout << "FastHelix pt: " << pt << "  pz: " << pz << endl;
  
  // find the helicity from the cross product of the vector from the
  // center of rotation to the inner point and the rotation center to
  // the outer point (C x A).
  float cross = (Ax-circ_x)*(Cy-circ_y) - (Ay-circ_y)*(Cx-circ_x);

  //  1 = postive charge in ++ field
  // -1 = negative charge in ++ field
  float helicity = (cross > 0) ? 1 : -1;

  // begin constructing the fast fit for this track
  SvxTrackFit_v1 fast_fit(SvxTrackFit::FastHelix);

  for(unsigned int iclus=0; iclus<clusters.size(); ++iclus) {
    int ilayer = clusters[iclus]->get_layer();
    float layer_r = sqrt( sqr(clusters[iclus]->get_xyz_global(0)) + 
                          sqr(clusters[iclus]->get_xyz_global(1)) );

    // This is for a cylindrical detector centered around (0,0)
    float d_sqr = sqr(circ_x)+sqr(circ_y);
    float dist = sqrt(d_sqr);
    float px = helicity*pt*circ_y/dist;
    float py = helicity*pt*(-circ_x)/dist;

    // Intersections of two circles A (detector) and B (helix track):
    // x = (1/2)(xB+xA) + (1/2)(xB-xA)(rA^2-rB^2)/d^2 \pm 2(yB-yA)K/d^2 
    // y = (1/2)(yB+yA) + (1/2)(yB-yA)(rA^2-rB^2)/d^2 \pm -2(xB-xA)K/d^2
    float layer_r_sqr = sqr(layer_r);
    float K = 0.25*sqrt( (sqr(layer_r+circ_r)-d_sqr) * (d_sqr-sqr(layer_r-circ_r)) );
    float x = 0.5*circ_x + 0.5*circ_x*(layer_r_sqr - circ_r_sqr)/d_sqr + helicity*2*circ_y*K/d_sqr;
    float y = 0.5*circ_y + 0.5*circ_y*(layer_r_sqr - circ_r_sqr)/d_sqr - helicity*2*circ_x*K/d_sqr;
    float z = Az;
    if(ilayer>0)
      z += (layer_r-sqrt(Asum))*tan_lambda;

    SvxTrackProj::Location layer;
    if(ilayer==0)      layer = SvxTrackProj::Layer0;
    else if(ilayer==1) layer = SvxTrackProj::Layer1;
    else if(ilayer==2) layer = SvxTrackProj::Layer2A;
    else               layer = SvxTrackProj::Layer3A;

    // since clusters don't know about sublayers (argh) I have to kludge this myself
    if(ilayer==2) {
      if(fast_fit.has_projection(SvxTrackProj::Layer2A)) {
        if(fast_fit.has_projection(SvxTrackProj::Layer2B)) {
          layer = SvxTrackProj::Layer2C;
        }
      } else {
        layer = SvxTrackProj::Layer2B;
      }
    }

    if(ilayer==3) {
      if(fast_fit.has_projection(SvxTrackProj::Layer3A)) {
        if(fast_fit.has_projection(SvxTrackProj::Layer3B))
          layer = SvxTrackProj::Layer3C;
      } else {
        layer = SvxTrackProj::Layer3B;
      }
    }

    SvxTrackProj_v1 proj(layer, x, y, z,
                         px, py, pz, -1.);
    fast_fit.set_projection(proj);
  }

  fast_fit.set_charge( (_mag_field_center>0) ? -helicity : helicity);
  if(verbosity>1) {
    cout << "FastFit: charge " << fast_fit.get_charge() << endl;
  }
  
  //track->add_fit(fast_fit);
  return fast_fit;
}

//-------------------------------------------------------------------
//
SvxTrackFit_v1 SvxKalmanFitter::fill_dc_info(SvxTrack* track)
{
  double vertex[3] = {0};
  vertex[0] = vtxout_->get_Vertex().getX();
  vertex[1] = vtxout_->get_Vertex().getY();
  vertex[2] = vtxout_->get_Vertex().getZ();

  vector<SvxCluster*> clusters = track->get_clusters_ordered();

  vector<SvxCluster*> use_clusters;
  int prev_layer = -1;
  vector<SvxCluster*>::reverse_iterator riter=clusters.rbegin();
  for(; riter!=clusters.rend(); ++riter) {
    int layer = (*riter)->get_layer();
    if(layer!=prev_layer) {
      use_clusters.push_back(*riter);
      prev_layer = layer;
    }
  }

  // Take the DC momentum at the vertex to get helix parameters
  //
  SvxCentralTrack* svxct = current_central_track_;
  PHSnglCentralTrack *sngl_cnt_trk = cnt_->get_track( svxct->getDchIndex() );
  
  double mom0 = sngl_cnt_trk->get_mom();
  double phi0 = sngl_cnt_trk->get_phi0();
  double theta0 = sngl_cnt_trk->get_the0();

  if(verbosity>1)
    cout << "DC mom, phi0, theta0: " << mom0 << "  " << phi0 << "  " << theta0 << endl;
  
  double pt = mom0*sin(theta0);
  double pz = mom0*cos(theta0);
  double tan_lambda = pz/pt;

  double R = mom0 / _cm_to_gev;
  double circ_x = R*sin(phi0);
  double circ_y = -R*cos(phi0);
  
  // find the radius of rotation
  double circ_r_sqr = sqr(R);
  double circ_r = R;
  
  if(verbosity>0)
    cout << "DC fit pt: " << pt << "  pz: " << pz << endl;


  if(verbosity>1)
    cout << "R: " << R
         << "circ_x: " << circ_x
         << "circ_y: " << circ_y
         << "circ_r_sqr: " << circ_r_sqr
         << "circ_r: " << circ_r
         << endl;
  
  // helicity  1 = postive charge in ++ field
  // helicity -1 = negative charge in ++ field
  double charge = sngl_cnt_trk->get_charge(); //(cross > 0) ? 1 : -1;
  double helicity = (_mag_field_center>0) ? -charge : charge;

  // begin constructing the fast fit for this track
  SvxTrackFit_v1 fast_fit(SvxTrackFit::FastHelix);

  for(unsigned int iclus=0; iclus<clusters.size(); ++iclus) {
    int ilayer = clusters[iclus]->get_layer();
    double layer_r = sqrt( sqr(clusters[iclus]->get_xyz_global(0)) + 
                           sqr(clusters[iclus]->get_xyz_global(1)) );

    // This is for a cylindrical detector centered around (0,0)
    double d_sqr = sqr(circ_x)+sqr(circ_y);
    double dist = sqrt(d_sqr);
    double px = helicity*pt*circ_y/dist;
    double py = helicity*pt*(-circ_x)/dist;

    // Intersections of two circles A (detector) and B (helix track):
    // x = (1/2)(xB+xA) + (1/2)(xB-xA)(rA^2-rB^2)/d^2 \pm 2(yB-yA)K/d^2 
    // y = (1/2)(yB+yA) + (1/2)(yB-yA)(rA^2-rB^2)/d^2 \pm -2(xB-xA)K/d^2
    double layer_r_sqr = sqr(layer_r);
    double K = 0.25*sqrt( (sqr(layer_r+circ_r)-d_sqr) * (d_sqr-sqr(layer_r-circ_r)) );
    double x = 0.5*circ_x + 0.5*circ_x*(layer_r_sqr - circ_r_sqr)/d_sqr + helicity*2*circ_y*K/d_sqr;
    double y = 0.5*circ_y + 0.5*circ_y*(layer_r_sqr - circ_r_sqr)/d_sqr - helicity*2*circ_x*K/d_sqr;
    double z = vertex[2] + layer_r*tan_lambda;

    SvxTrackProj::Location layer;
    if(ilayer==0)      layer = SvxTrackProj::Layer0;
    else if(ilayer==1) layer = SvxTrackProj::Layer1;
    else if(ilayer==2) layer = SvxTrackProj::Layer2A;
    else               layer = SvxTrackProj::Layer3A;

    // since clusters don't know about sublayers (argh) I have to kludge this myself
    if(ilayer==2) {
      if(fast_fit.has_projection(SvxTrackProj::Layer2A)) {
        if(fast_fit.has_projection(SvxTrackProj::Layer2B)) {
          layer = SvxTrackProj::Layer2C;
        }
      } else {
        layer = SvxTrackProj::Layer2B;
      }
    }

    if(ilayer==3) {
      if(fast_fit.has_projection(SvxTrackProj::Layer3A)) {
        if(fast_fit.has_projection(SvxTrackProj::Layer3B))
          layer = SvxTrackProj::Layer3C;
      } else {
        layer = SvxTrackProj::Layer3B;
      }
    }

    SvxTrackProj_v1 proj(layer, x, y, z,
                         px, py, pz, -1.);
    fast_fit.set_projection(proj);
  }
 

  double dcphi = sngl_cnt_trk->get_phi();
  double dcalpha = sngl_cnt_trk->get_alpha();
  double dcbeta = sngl_cnt_trk->get_beta();
  if(verbosity>1)
    cout << "DC phi, alpha, beta: " << dcphi << "  " << dcalpha << "  " << dcbeta << endl;

  double dcx = 220.*cos(dcphi);
  double dcy = 220.*sin(dcphi);
  double dcz = sngl_cnt_trk->get_zed();
  if(verbosity>1)
    cout << "DC x, y, z: " << dcx << "  " << dcy << "  " << dcz << endl;
  
  double dcmomphi = dcphi-dcalpha;
  double dcpt = mom0*sin(dcbeta);
  double dcpx = dcpt*cos(dcmomphi);
  double dcpy = dcpt*sin(dcmomphi);
  double dcpz = mom0*cos(dcbeta);
  if(verbosity>1)
    cout << "DC px, py, pz: " << dcpx << "  " << dcpy << "  " << dcpz << endl;

  SvxTrackProj_v1 proj(SvxTrackProj::DCH,
                       dcx, dcy, dcz,
                       dcpx, dcpy, dcpz, -1.);
  fast_fit.set_projection(proj);


  fast_fit.set_charge( charge );
  //track->add_fit(fast_fit);

  if(verbosity>1) fast_fit.print();

#ifdef DEBUG
  tree_->fill_dc_info(sngl_cnt_trk);
#endif
  return fast_fit;
}
