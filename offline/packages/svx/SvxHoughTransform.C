#include <SvxHoughTransform.h>

// PHENIX includes
#include <Fun4AllReturnCodes.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <VtxOut.h>

// SVX includes
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <SvxTrackList_v1.h>
#include <SvxTrack.h>
#include <SvxSegmentListv6.h>
#include <SvxGhit.h>
#include <SvxGhitRawhit.h>
#include <SvxRawhitCluster.h>
#include <SvxSegmentv6.h>
#include <SvxGhitList.h>
#include <SvxGhitRawhitList.h>
#include <SvxRawhitClusterList.h>
#include <SvxClusterList.h>

// Helix Hough includes
#include <SimpleHit3D.h>
#include <SimpleTrack3D.h>
#include <HelixResolution.h>
#include <HelixRange.h>
#include <HelixHough.h>
#include <VertexFinder.h>
#include <sPHENIXTracker.h>

// standard includes
#include <iostream>
#include <map>
#include <math.h>

// root includes
#include <TMath.h>

using namespace std;


SvxHoughTransform::SvxHoughTransform(const string &name ) :
  SubsysReco(name),
  _timer(PHTimeServer::get()->insert_new("SvxHoughTransform")),
  _timer_initial_hough(PHTimeServer::get()->insert_new("SvxHoughTransform::inital track finding")),
  _timer_full_hough(PHTimeServer::get()->insert_new("SvxHoughTransform::full track finding"))
{
  //defaults
  _verbosity = 0;
  _required_hits = 4;
  _kappacut = 0.1; 
  _chi2_cut = 10.0;
  _phi_cut=M_PI/3.;
}

int SvxHoughTransform::Init(PHCompositeNode *topNode)
{
  // create SvxTrack node...
  PHNodeIterator iter(topNode);
  
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cerr << PHWHERE << "DST Node missing, doing nothing." << endl;
      return EVENT_OK;
    }
      
  PHIODataNode<PHObject>* SvxTrackListNode = NULL;
  SvxTrackListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxTrackList");
  if(!SvxTrackListNode)
    {
      PHCompositeNode* svxNode = new PHCompositeNode("SVX");
      dstNode->addNode(svxNode);

      SvxTrackList* tracklist = new SvxTrackList_v1();
      SvxTrackListNode = new PHIODataNode<PHObject>(tracklist, "SvxTrackList", "PHObject");
      svxNode->addNode(SvxTrackListNode);
    }


  PHIODataNode<PHObject>* SvxSegmentListNode = NULL;
  SvxSegmentListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxSegmentList");
  if(!SvxSegmentListNode)
    {
      PHCompositeNode* svxNode = new PHCompositeNode("SVX");
      dstNode->addNode(svxNode);
      cout<<"svxtracks node added"<<endl;
      SvxSegmentList* svxsegments = new SvxSegmentListv6();
      SvxSegmentListNode = new PHIODataNode<PHObject>(svxsegments, "SvxSegmentList", "PHObject");
      svxNode->addNode(SvxSegmentListNode);
      cout<<"svx node added"<<endl;
    }

  //=================================================//
  //  Initializing HelixHough objects                //
  //=================================================//

  // detector characteristics
  _nlayers = 4;
  _radii.assign(_nlayers, 0.0);
  _radii[0] = 2.5;
  _radii[1] = 5.0;
  _radii[2] = 10.0;
  _radii[3] = 14.0;
  _smear_xy_layer.assign(_nlayers,0);
  _smear_z_layer.assign(_nlayers,0);
  float sqrt_12 = sqrt(12.);
  _smear_xy_layer[0] = (50.0e-4/sqrt_12);
  _smear_z_layer[0] = (425.0e-4/sqrt_12);
  _smear_xy_layer[1] = (50.0e-4/sqrt_12);
  _smear_z_layer[1] = (425.0e-4/sqrt_12);
  _smear_xy_layer[2] = (80.0e-4/sqrt_12);
  _smear_z_layer[2] = (1000.0e-4/sqrt_12);
  _smear_xy_layer[3] = (80.0e-4/sqrt_12);
  _smear_z_layer[3] = (1000.0e-4/sqrt_12);

  //------------------------------------------------------------
  // HelixHough object for initial track finding without 
  // well-known primary vertex position (needs +/- 0.5 cm input)

  // center of rotation resolution, dca resolution, kappa resolution, theta resolution, z0 resolution
  HelixResolution min_res_init(0.01, 10.5, 0.01, 0.01, 0.04);

  // center of rotation resolution, dca resolution, kappa resolution, theta resolution, z0 resolution
  HelixResolution max_res_init(0.001, 0.5, 0.01, 0.01, 0.04);
  
  float kappa_max = 0.003*0.9/0.1;

  // top_range( min,max for center of rotation angle, min,max for dca, min,max for kappa, min,max for theta, min,max for z0 )  
  HelixRange top_range_init( 0.0, 2.*M_PI,   -0.3, 0.3,   0.0, kappa_max,   -0.9, 0.9,   -10.0, 10.0);
  
  vector<unsigned int> onezoom(5,0);
  vector<vector<unsigned int> > zoomprofile_init;
  zoomprofile_init.assign(9,onezoom);
  for(unsigned int i=0;i<=0;++i)
  {
    zoomprofile_init[i][0] = 8;
    zoomprofile_init[i][1] = 1;
    zoomprofile_init[i][2] = 1;
    zoomprofile_init[i][3] = 5;
    zoomprofile_init[i][4] = 5;
  }
  for(unsigned int i=1;i<=2;++i)
  {
    zoomprofile_init[i][0] = 5;
    zoomprofile_init[i][1] = 1;
    zoomprofile_init[i][2] = 2;
    zoomprofile_init[i][3] = 4;
    zoomprofile_init[i][4] = 4;
  }
  for(unsigned int i=3;i<=3;++i)
  {
    zoomprofile_init[i][0] = 5;
    zoomprofile_init[i][1] = 2;
    zoomprofile_init[i][2] = 2;
    zoomprofile_init[i][3] = 4;
    zoomprofile_init[i][4] = 1;
  }
  for(unsigned int i=4;i<=8;++i)
  {
    zoomprofile_init[i][0] = 3;
    zoomprofile_init[i][1] = 3;
    zoomprofile_init[i][2] = 3;
    zoomprofile_init[i][3] = 3;
    zoomprofile_init[i][4] = 3;
  }
  
  vector<float> material;
  material.assign(4, 0.015);
  
  _tracker_init = new sPHENIXTracker(zoomprofile_init, 3, top_range_init, material, _radii, _magField);
  _tracker_init->setNLayers(4);
  _max_hits_init = 50;
  _maxtracks = 160;
  _min_hits_init = 4;
  _tracker_init->setClusterStartBin(2);
  _tracker_init->setRejectGhosts(true);
  _tracker_init->setChi2Cut(6.0);
  _tracker_init->setChi2RemovalCut(4.0);
  _tracker_init->setPrintTimings(true);
  _tracker_init->setVerbosity(_verbosity);
  _tracker_init->setCutOnDca(false);
  _tracker_init->requireLayers(3);
  _tracker_init->setSmoothBack(false);
  _tracker_init->setBinScale(1.0);
  _tracker_init->setZBinScale(1.0);
  _tracker_init->setRemoveHits(false);
  _tracker_init->setSeparateByHelicity(true);
  _tracker_init->setMaxHitsPairs(0);
  
  

  //------------------------------------------------------------
  // HelixHough object for primary tracking after pinning down 
  // the primary vertex position

  // center of rotation resolution, dca resolution, kappa resolution, theta resolution, z0 resolution
  HelixResolution min_res(0.01,  0.5, 0.002, 0.01, 2.0);

  // center of rotation resolution, dca resolution, kappa resolution, theta resolution, z0 resolution
  HelixResolution max_res(0.01,  0.5, 0.002, 0.01, 2.0);
  
  // top_range( min,max for center of rotation angle, min,max for dca, min,max for kappa, min,max for theta, min,max for z0 )  
  HelixRange top_range( 0.0, 2.*M_PI,   -0.5, 0.5,   0.0, kappa_max,   -0.9, 0.9,   -0.5, 0.5 );
  
  
  vector<vector<unsigned int> > zoomprofile;
  zoomprofile.assign(9,onezoom);
  for(unsigned int i=0;i<=0;++i)
  {
    zoomprofile[i][0] = 10;
    zoomprofile[i][1] = 1;
    zoomprofile[i][2] = 1;
    zoomprofile[i][3] = 7;
    zoomprofile[i][4] = 1;
  }
  for(unsigned int i=1;i<=2;++i)
  {
    zoomprofile[i][0] = 5;
    zoomprofile[i][1] = 2;
    zoomprofile[i][2] = 2;
    zoomprofile[i][3] = 4;
    zoomprofile[i][4] = 2;
  }
  for(unsigned int i=3;i<=3;++i)
  {
    zoomprofile[i][0] = 5;
    zoomprofile[i][1] = 2;
    zoomprofile[i][2] = 2;
    zoomprofile[i][3] = 4;
    zoomprofile[i][4] = 2;
  }
  for(unsigned int i=4;i<=8;++i)
  {
    zoomprofile[i][0] = 3;
    zoomprofile[i][1] = 3;
    zoomprofile[i][2] = 3;
    zoomprofile[i][3] = 3;
    zoomprofile[i][4] = 3;
  }

  _tracker = new sPHENIXTracker(zoomprofile, 1, top_range, material, _radii, _magField);
  _tracker->setNLayers(4);
  _max_hits = 50;
  _min_hits = 4;
  _tracker->setClusterStartBin(2);
  _tracker->setRejectGhosts(true);
  _tracker->setChi2Cut(6.0);
  _tracker->setChi2RemovalCut(4.0);
  _tracker->setPrintTimings(true);
  _tracker->setVerbosity(_verbosity);
  _tracker->setCutOnDca(false);
  _tracker->requireLayers(3);
  _tracker->setSmoothBack(true);
  _tracker->setBinScale(1.0);
  _tracker->setZBinScale(1.0);
  _tracker->setRemoveHits(false);
  _tracker->setSeparateByHelicity(true);
  _tracker->setMaxHitsPairs(0);

  return EVENT_OK;
}

/// This method passes over the current track objects and preforms
/// a fast calculation to fill the initial momentum fields.
///
int SvxHoughTransform::process_event(PHCompositeNode *topNode)
{
  _tracker_init->clear();
  _tracker->clear();
  
  _timer.get()->restart();
  if(_verbosity > 0) cout << "SvxHoughTransform::process_event -- entered" << endl;

  //---------------------------------
  // Get Objects off of the Node Tree
  //---------------------------------

  if(_verbosity > 0) cout << "SvxHoughTransform::process_event -- grabbing node tree objects..." << endl;

  // Pull the vertex information off the node tree...
  VtxOut *vtxout = 0;
  PHTypedNodeIterator<VtxOut> vtxoutiter(topNode);
  PHIODataNode<VtxOut> *VtxOutNode = vtxoutiter.find("VtxOut");
  if(!VtxOutNode) 
    {
      cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;
      return EVENT_OK;
    }
  vtxout = (VtxOut*)VtxOutNode->getData();
  if(!vtxout) 
    {
      cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;
      return EVENT_OK;
    }

  // Pull the clusters off of the node tree...
  SvxClusterList *clusterList = findNode::getClass<SvxClusterList>(topNode,"SvxClusterList");
  if(!clusterList) 
    {
      cerr << PHWHERE << " ERROR: Can't find SvxClusterList." << endl;
      return EVENT_OK;
    }

  SvxSegmentList *d_segment = 0;           
  PHTypedNodeIterator<SvxSegmentList> segmentiter(topNode);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  if (!SvxSegmentListNode) 
    { 
      cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;  
      return EVENT_OK; 
    }
  else 
    { 
      d_segment = (SvxSegmentList*)SvxSegmentListNode->getData(); 
    }
 
  //-------------------------
  // Fill Simple3DHit vectors
  //-------------------------

  if(_verbosity > 0) cout << "SvxHoughTransform::process_event -- filling internal SimpleHit3D objects..." << endl;

  for(int icluster = 0; icluster < clusterList->get_nClusters(); icluster++)
    {
      SvxCluster* cluster = clusterList->get_Cluster(icluster);

      float phi = atan2(cluster->get_xyz_global(1),cluster->get_xyz_global(0));

      // from sqrt(12)=3.4641 of the pixel size
      
      float xy_error = _smear_xy_layer[cluster->get_layer()]*3.4641*0.5*cluster->get_xz_size(0);
      float z_error  = _smear_z_layer[cluster->get_layer()]*3.4641*0.5*cluster->get_xz_size(1);

			//SimpleHit3D(float xx, float dxx, float yy, float dyy, float zz, float dzz, unsigned int ind, int lyr=-1)  
      _clusters.push_back(SimpleHit3D(cluster->get_xyz_global(0),
				      fabs(xy_error*sin(phi)),
				      cluster->get_xyz_global(1),
				      fabs(xy_error*cos(phi)),
				      cluster->get_xyz_global(2),
				      z_error,
				      icluster,
				      cluster->get_layer()));
      
    }

  //------------------------------------
  // Preform the initial zvertex finding
  //------------------------------------

  // start with a vertex position guess
  _vertex.clear();
  _vertex.push_back(0.0); // x guess
  _vertex.push_back(0.0); // y guess
  _vertex.push_back(0.0); // z guess

  // determine a small number of tracks based on the number of hits
  _maxtracks = 12*(_clusters.size())/1000;
  if(_maxtracks < 100)
    {
      _maxtracks=100;
    }

  // find the small number of tracks
  _tracks.clear();
  _timer_initial_hough.get()->restart();
  _tracker_init->findHelices( _clusters, _min_hits_init, _max_hits_init, _tracks, _maxtracks); // findHelices( clusters, track length, threshold for entering track construction, track output )
  
  
  if(_verbosity > 0)
  {
    cout << "SvxHoughTransform::process_event -- initial track finding pass found: " << _tracks.size() << " tracks" << endl;
  }
  
  for(unsigned int i=0;i<_tracks.size();++i)
  {
    cout<<"init track "<<i<<" : "<<endl;
    for(unsigned int h=0;h<_tracks[i].hits.size();++h)
    {
      cout<<"hit "<<h<<" : "<<_tracks[i].hits[h].x<<" "<<_tracks[i].hits[h].y<<" "<<_tracks[i].hits[h].z<<endl;
    }
    cout<<_tracks[i].phi<<" "<<_tracks[i].kappa<<" "<<_tracks[i].d<<" "<<_tracks[i].dzdl<<" "<<_tracks[i].z0<<endl<<endl;
  }
  
  if(_tracks.size() == 0)
  {
    
  }
  else if(_tracks.size() < 5)
  {
    double zavg=0.;
    for(unsigned int i=0;i<_tracks.size();++i)
    {
      zavg += _tracks[i].z0;;
    }
    zavg/=((double)(_tracks.size()));
    _vertex[2] = zavg;
  }
  else
  {
    double zavg=0.;
    for(unsigned int i=0;i<_clusters.size();++i)
    {
      zavg += _clusters[i].z;
    }
    zavg/=((double)(_clusters.size()));
    _vertex[2] = zavg;
    
    // calculate the vertex position
    _vertexFinder.findVertex(_tracks, _vertex, 10.0);
    _vertexFinder.findVertex(_tracks, _vertex, 3.0);
    _vertexFinder.findVertex(_tracks, _vertex, 1.0);
    _vertexFinder.findVertex(_tracks, _vertex, 0.3);
    _vertexFinder.findVertex(_tracks, _vertex, 0.05);
    
    _timer_initial_hough.get()->stop();
    
    //   _vertex[0] = 0.;
    //   _vertex[1] = 0.;
  }
  
  if(_verbosity > 0)
  {
    cout << "SvxHoughTransform::process_event -- initial vertex found: " << _vertex[0] << " " << _vertex[1] << " " << _vertex[2] << endl;
  }

  // drop the tracks and start over
  _tracks.clear();

  //--------------------------------
  // Re-center event on vertex guess
  //--------------------------------

  // temporarily shift all the hits to vertex centered coordinates
  for(unsigned int hh=0;hh<_clusters.size();hh++)
    {
      _clusters[hh].x = _clusters[hh].x - _vertex[0];
      _clusters[hh].y = _clusters[hh].y - _vertex[1];
      _clusters[hh].z = _clusters[hh].z - _vertex[2];
    }
  
  //----------------------------------
  // Preform the primary track finding
  //----------------------------------
  
  _timer_full_hough.get()->restart();
  _tracker->findHelices(_clusters, _min_hits, _max_hits, _tracks); // findHelices( clusters, track length, threshold for entering track construction, track output )
  _timer_full_hough.get()->stop();

  if(_verbosity > 0)
    {
      cout << "SvxHoughTransform::process_event -- full track finding pass found: " << _tracks.size() << " tracks" << endl;
    }
    
    for(unsigned int i=0;i<_tracks.size();++i)
    {
      cout<<"track "<<i<<" : "<<endl;
      for(unsigned int h=0;h<_tracks[i].hits.size();++h)
      {
        cout<<"hit "<<h<<" : "<<_tracks[i].hits[h].x<<" "<<_tracks[i].hits[h].y<<" "<<_tracks[i].hits[h].z<<endl;
      }
      cout<<_tracks[i].phi<<" "<<_tracks[i].kappa<<" "<<_tracks[i].d<<" "<<_tracks[i].dzdl<<" "<<_tracks[i].z0<<endl<<endl;
    }
    
  //----------------------------
  // Re-center event on detector
  //----------------------------

  if(_verbosity > 0) cout << "SvxHoughTransform::process_event -- recentering event on detector..." << endl;
  vector<double> chi_squareds;
  for(unsigned int tt=0;tt<_tracks.size();tt++)
    {
      // move the hits in the track back to their original position                
      for(unsigned int hh=0;hh<_tracks[tt].hits.size();hh++)
        {
          _tracks[tt].hits[hh].x = _tracks[tt].hits[hh].x + _vertex[0];
          _tracks[tt].hits[hh].y = _tracks[tt].hits[hh].y + _vertex[1];
          _tracks[tt].hits[hh].z = _tracks[tt].hits[hh].z + _vertex[2];
        }

      vector<double> chi2_hit;
      chi_squareds.push_back(_tracker->getKalmanStates()[tt].chi2);
      cout<<"chi2 = "<<_tracker->getKalmanStates()[tt].chi2<<endl;
    }

  if(_verbosity > 0)
    {
      cout << "SvxHoughTransform::process_event -- final track count: " << _tracks.size() << endl;
    }
  
  //---------------------------
  // Final vertex determination
  //---------------------------
  
  // final best guess of the primary vertex position here...  
  //_vertexFinder.findVertex(_tracks, _vertex, 0.3);   //then continue with a smaller sigma
  //_vertexFinder.findVertex(_tracks, _vertex, 0.03);  //then continue with a smaller sigma
  //_vertexFinder.findVertex(_tracks, _vertex, 0.001); //then continue with a smaller sigma
    
  if(_verbosity > 0)
    {
      cout << "SvxHoughTransform::process_event -- final vertex: " << _vertex[0] << " " << _vertex[1] << " " << _vertex[2] << endl;
    }

  //--------------------------------
  // Translate back into Svx objects
  //--------------------------------

  // at this point we should already have an initial pt and pz guess...
  // need to translate this into the SvxTrack object...
  // similarly to how SvxStandAloneReco does it

  vector<SimpleHit3D> track_hits;
  int clusterID;
  int clusterLayer;
  float cluster_x;
  float cluster_y;
  float cluster_z;
  float dEdx1 = 0.0;
  float dEdx2 = 0.0;
  
  for(unsigned int itrack=0; itrack<_tracks.size(); itrack++)
    {
      
      SvxSegment *segment = new SvxSegmentv6();
      segment->Reset();
      segment->setSegmentID(itrack);
      track_hits.clear();
      track_hits = _tracks.at(itrack).hits;
      int Nhits_layer[4] = {0,0,0,0};
      
      dEdx1 = 0.0;
      dEdx2 = 0.0;

      for(unsigned int ihit = 0; ihit<track_hits.size();ihit++)
        {
          SvxCluster* cluster = clusterList->get_Cluster(track_hits.at(ihit).index);
          clusterID = cluster->get_hitID();
          clusterLayer = cluster->get_layer();
          cluster_x = cluster->get_xyz_global(0);
          cluster_y = cluster->get_xyz_global(1);
          cluster_z = cluster->get_xyz_global(2);

          if(clusterLayer>=0 && clusterLayer<4) 
            {              
              if(Nhits_layer[clusterLayer]<2)
		{
		  segment->setClusterID(clusterLayer,Nhits_layer[clusterLayer],clusterID);
		  segment->setProjectedPosition(clusterLayer,cluster_x,cluster_y,cluster_z,Nhits_layer[clusterLayer]);
		}
              if(clusterLayer == 2) dEdx1 += (cluster->get_adc(0)+cluster->get_adc(1));
              if(clusterLayer == 3) dEdx2 += (cluster->get_adc(0)+cluster->get_adc(1));
		
              Nhits_layer[clusterLayer]++;
            }
        }
      segment->set_dEdX1(dEdx1);
      segment->set_dEdX2(dEdx2);

      for(int layer = 0; layer <4; layer++)
        {
          segment->setNhits(layer, Nhits_layer[layer]);
        }
        
        
      if(!(_tracks.at(itrack).kappa == _tracks.at(itrack).kappa)){continue;}
      if(!(_tracks.at(itrack).d == _tracks.at(itrack).d)){continue;}
      if(!(_tracks.at(itrack).phi == _tracks.at(itrack).phi)){continue;}
      if(!(_tracks.at(itrack).dzdl == _tracks.at(itrack).dzdl)){continue;}
      if(!(_tracks.at(itrack).z0 == _tracks.at(itrack).z0)){continue;}
      float kappa = _tracks.at(itrack).kappa;
      float d = _tracks.at(itrack).d;
      float phi = _tracks.at(itrack).phi;

      segment->setDCA2D(d);

      float dzdl = _tracks.at(itrack).dzdl;
      float pT =_cmToGeV/kappa;
      
      float x_center = cos(phi)*(d+1.0/kappa); // x coordinate of circle center
      float y_center = sin(phi)*(d+1.0/kappa); // y    "      "     "      "
      
      // find helicity from cross product sign
      short int helicity;
      if((track_hits[0].x-x_center)*(track_hits[track_hits.size()-1].y-y_center) -
         (track_hits[0].y-y_center)*(track_hits[track_hits.size()-1].x-x_center) > 0)
        {
          helicity = 1;
	  segment->setIsPositive(true);
        }
      else
        { 
          helicity = -1;
	  segment->setIsPositive(false);
        }
      
      float pZ = 0;
      if(dzdl != 1)
        {
          pZ = pT * dzdl / sqrt(1.0 - dzdl*dzdl);
        }
      segment->setChiSq(chi_squareds[itrack]);
      unsigned int NDF = 0;
      if((_tracks.at(itrack).hits).size()>2) 
	{
	  NDF = 2*(_tracks.at(itrack).hits).size() - 5;
	}
      segment->setQuality(TMath::Log(TMath::Prob(chi_squareds[itrack],NDF)));
      segment->set3MomentumAtPrimaryVertex(pT*cos(phi-helicity*Pi/2),
                                           pT*sin(phi-helicity*Pi/2),
					   pZ);
	  ((SvxSegmentListv6*)d_segment)->AddSegment((unsigned int)(d_segment->get_nSegments()), *segment);
	  delete segment;
    }
    
  _clusters.clear();
  _tracks.clear();
  
  _timer.get()->stop();

  return EVENT_OK;
}

int SvxHoughTransform::End(PHCompositeNode *topNode)
{
  delete _tracker_init;
  delete _tracker;

  return EVENT_OK;
}
