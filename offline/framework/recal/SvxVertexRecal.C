#include "SvxVertexRecal.h"


#include "PHCentralTrack.h"
#include "PHCompositeNode.h"
#include "SvxSegmentList.h"
#include "SvxSegment.h"
#include "getClass.h"
#include "Fun4AllReturnCodes.h"
#include "VtxOut.h"
#include "PHPoint.h"
#include "TMath.h"
#include "TFile.h"
#include "TH2D.h"
#include "compactCNT/SvxHitMap.h"
#include "SvxClusterList.h"
#include "SvxCentralTrackList.h"
#include "SvxCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "SvxCentralTrackRecal.h"
#include "SvxCentralTrackRecalList.h"
#include <cmath>
#include <iostream>
#include <sstream>


using namespace std;
using namespace Eigen;


SvxVertexRecal::SvxVertexRecal()
{
  //************************************************************
  // very approximate radii or the VTX layers
  // these do not have to be exact, but are merely for
  // backwards-compatibility in the Kalman filter
  vector<float> detector_radii;
  detector_radii.push_back(2.5);
  detector_radii.push_back(5.);
  detector_radii.push_back(10.);
  detector_radii.push_back(14.);
  //************************************************************
  
  
  //************************************************************
  // these rough materials-per-layer are used by the Kalman
  // the units are in radiation lengths
  // as above, they are merely approximate
  vector<float> detector_material;
  detector_material.push_back(0.015);
  detector_material.push_back(0.015);
  detector_material.push_back(0.04);
  detector_material.push_back(0.04);
  //************************************************************
  
  // TODO the 0.92 is a hard-coded inner magnetic field
  // get this number from the field map for a given run instead
  kalman = new CylinderKalman(detector_radii, detector_material, 0.92);
}


SvxVertexRecal::~SvxVertexRecal()
{
  delete kalman;
}


int SvxVertexRecal::getNodes(PHCompositeNode *topNode)
{
  // Grab the DST nodes we need
  svxcntrecallist = findNode::getClass<SvxCentralTrackRecalList>(topNode,"SvxCentralTrackRecalList");
  if(!svxcntrecallist)
  {
    cerr << PHWHERE << " ERROR: Can't find SvxCentralTrackRecalList on node tree." << endl;
    return ABORTRUN;
  }
  
  vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if(!vtxout)
  {
    cerr << PHWHERE << " ERROR: Can't find VtxOut on node tree." << endl;
    return ABORTRUN;
  }
  
  svxcentral = findNode::getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackList");
  if(!svxcentral)
  {
    cerr << PHWHERE << " ERROR: Can't find SvxCentralTrackList on node tree." << endl;
    return ABORTRUN;
  }
  
  central = findNode::getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(!central)
  {
    cerr << PHWHERE << " ERROR: Can't find PHCentralTrack on node tree." << endl;
    return ABORTRUN;
  }
  
  clusters = findNode::getClass<SvxClusterList>(topNode,"SvxClusterList");
  if(!clusters)
  {
    cerr << PHWHERE << " ERROR: Can't find SvxClusterList on node tree." << endl;
    return ABORTRUN;
  }
  
  return EVENT_OK;
}


static float two_over_sqrt_12 = 2./sqrt(12.);

int SvxVertexRecal::process_event(PHCompositeNode *topNode)
{
  if(getNodes(topNode) != EVENT_OK){return ABORTRUN;};
  
  tracks.clear();
  
  SimpleHit3D hit;
  SimpleTrack3D track;
  // associate tracks to their VTX hits and convert them
  // to a format suitable for the Kalman filter
  for(int i=0,nsvxcnt=svxcntrecallist->GetNentries();i<nsvxcnt;i+=1)
  {
    hits.clear();
    
    SvxCentralTrackRecal *svxcntrecal = svxcntrecallist->GetSvxCNTRecal(i);
    
    SvxCentralTrack* svxcnt = svxcentral->getCentralTrack(i);
    PHSnglCentralTrack* phcent = central->get_track( svxcnt->getDchIndex() );
    if( !( (phcent->get_quality() == 63) || (phcent->get_quality() == 31) ) ){continue;}
    
    for(int l=0;l<4;++l)
    {
      if( svxcntrecal->get_nhit(l) == 0 ){continue;}
      hit.x = svxcntrecal->get_ClusterPosition(l,0,0);
      hit.y = svxcntrecal->get_ClusterPosition(l,1,0);
      hit.z = svxcntrecal->get_ClusterPosition(l,2,0);
      
      if(l < 2)
      {
        hit.dx = 0.005*two_over_sqrt_12 * hit.x/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dy = 0.005*two_over_sqrt_12 * hit.y/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dz = 0.0425*two_over_sqrt_12;
      }
      else
      {
        hit.dx = 0.008*two_over_sqrt_12 * hit.x/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dy = 0.008*two_over_sqrt_12 * hit.y/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dz = 0.1*two_over_sqrt_12;
      }
      
      hit.layer = l;
      hit.index = l;
      
      hits.push_back(hit);
      
      if(hits.size() == 3){break;}
    }
    if(hits.size() < 3){continue;}
    
    
    if( fabs( svxcntrecal->get_mom() * sin( svxcntrecal->get_the0() ) ) < 0.2 ){continue;}
    
    // initialize track coordinates from VTX hits
    track.hits = hits;
    sPHENIXTracker::fitTrack_3(track);
    
    // except for ce the momentum (kappa) to that from CNT track
    track.kappa = fabs( (0.92/333.6)/( svxcntrecal->get_mom() * sin( svxcntrecal->get_the0() ) ) );
    
    HelixKalmanState state;
    state.C = Matrix<float,5,5>::Zero(5,5);
    state.C(0,0) = 1.1;
    state.C(1,1) = 1.1;
    state.C(2,2) = 1.0e-5;
    state.C(3,3) = 1.1;
    state.C(4,4) = 1.1;
    
    state.phi = track.phi;
    if(state.phi < 0.){state.phi += 2.*TMath::Pi();}
    state.d = track.d;
    state.kappa = track.kappa;
    state.nu = sqrt(state.kappa);
    state.z0 = track.z0;
    state.dzdl = track.dzdl;
    
    kalman->addHit(track.hits[2], state);
    kalman->addHit(track.hits[1], state);
    kalman->addHit(track.hits[0], state);
    
    track.phi = state.phi;
    if(track.phi < 0.){track.phi += 2.*TMath::Pi();}
    if(track.phi > 2.*TMath::Pi()){track.phi -= 2.*TMath::Pi();}
    track.d = state.d;
    track.kappa = state.kappa;
    track.z0 = state.z0;
    track.dzdl = state.dzdl;
    tracks.push_back(track);
  }
  
  vertex.assign(3,0.);
  // if there are fewer than 10 tracks, don't refit the collision vertex
  // using svxcentraltracks
  if(tracks.size() < 10)
  {
    vertex[0] = vtxout->get_Vertex().getX();
    vertex[1] = vtxout->get_Vertex().getY();
    vertex[2] = vtxout->get_Vertex().getZ();
  }
  else
  {
    vertexFinder.findVertex(tracks, vertex, 20.0);
    vertexFinder.findVertex(tracks, vertex, 6.0);
    vertexFinder.findVertex(tracks, vertex, 1.0);
    vertexFinder.findVertex(tracks, vertex, 0.3);
    vertexFinder.findVertex(tracks, vertex, 0.05);
    vertexFinder.findVertex(tracks, vertex, 0.02);
    vertexFinder.findVertex(tracks, vertex, 0.01);
  }
  
  // regardless of whether the vertex was refit, we calculate the dca2d from
  // the Kalman filter
  for(int i=0,nsvxcnt=svxcntrecallist->GetNentries();i<nsvxcnt;i+=1)
  {
    hits.clear();
    
    SvxCentralTrackRecal *svxcntrecal = svxcntrecallist->GetSvxCNTRecal(i);
    
    
    
    for(int l=0;l<4;++l)
    {
      if( svxcntrecal->get_nhit(l) == 0 ){continue;}
      hit.x = svxcntrecal->get_ClusterPosition(l,0,0);
      hit.y = svxcntrecal->get_ClusterPosition(l,1,0);
      hit.z = svxcntrecal->get_ClusterPosition(l,2,0);
      
      if(l < 2)
      {
        hit.dx = 0.005*two_over_sqrt_12 * hit.x/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dy = 0.005*two_over_sqrt_12 * hit.y/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dz = 0.0425*two_over_sqrt_12;
      }
      else
      {
        hit.dx = 0.008*two_over_sqrt_12 * hit.x/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dy = 0.008*two_over_sqrt_12 * hit.y/sqrt(hit.x*hit.x + hit.y*hit.y);
        hit.dz = 0.1*two_over_sqrt_12;
      }
      
      hit.x -= vertex[0];
      hit.y -= vertex[1];
      hit.z -= vertex[2];
      
      hit.layer = l;
      hit.index = l;
      
      hits.push_back(hit);
      
      if(hits.size() == 3){break;}
    }
    if(hits.size() < 3){continue;}
    
    track.hits = hits;
    sPHENIXTracker::fitTrack_3(track);
    
    track.kappa = fabs( (0.92/333.6)/( svxcntrecal->get_mom() * sin( svxcntrecal->get_the0() ) ) );
    
    HelixKalmanState state;
    state.C = Matrix<float,5,5>::Zero(5,5);
    state.C(0,0) = 1.1;
    state.C(1,1) = 1.1;
    state.C(2,2) = 1.0e-5;
    state.C(3,3) = 1.1;
    state.C(4,4) = 1.1;
    
    state.phi = track.phi;
    if(state.phi < 0.){state.phi += 2.*TMath::Pi();}
    state.d = track.d;
    state.kappa = track.kappa;
    state.nu = sqrt(state.kappa);
    state.z0 = track.z0;
    state.dzdl = track.dzdl;
    
    kalman->addHit(track.hits[2], state);
    kalman->addHit(track.hits[1], state);
    kalman->addHit(track.hits[0], state);
    
    if(state.d == 0.0){continue;}
    
    SvxCentralTrack* svxcnt = svxcentral->getCentralTrack(i);
    svxcnt->setDCA2D(state.d);
    svxcnt->setDCAZ(state.z0);
  }
  
  if(tracks.size() >= 10)
  {
    float vtx[3] = {vertex[0], vertex[1], vertex[2]};
    float vtxerr[3] = {0.002, 0.0025, 0.005};
    vtxout->AddVtx("SVX_PRECISE_RECAL", vtx, vtxerr, 1);
  }
  
  
  return EVENT_OK;
}


