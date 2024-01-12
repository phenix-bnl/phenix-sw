#include "SvxMomentumRecal.h"

#include "PHCentralTrack.h"
#include "PHCompositeNode.h"
#include "SvxCentralTrackList.h"
#include "SvxCentralTrack.h"
#include "getClass.h"
#include "Fun4AllReturnCodes.h"
#include "SvxCentralTrackRecalList.h"
#include <cmath>
#include <iostream>
#include <sstream>


using namespace std;

//For simulation we can just use svxcentraltracks
//For Run11 Pro98 we need both svxcentraltrack and svxcentraltrackrecal tracks
// which you can get by calling the following in the fun4all macro:
// recoConsts *rc = recoConsts::instance();
// rc->set_IntFlag("SVXFIT",1); // refitting

//Right now the momentum in the svxcentraltracks is updated
//but the momentum in svxcentraltrackrecal tracks is not
//This is so that the original and recalibrated momenta can be compared.

SvxMomentumRecal::SvxMomentumRecal() : is_simulation(false)
{
  
}


SvxMomentumRecal::~SvxMomentumRecal()
{
  
}


int SvxMomentumRecal::getNodes(PHCompositeNode *topNode)
{
  // Grab the DST nodes we need
  if(is_simulation)
  {
    svxcentraltrack = findNode::getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackList");
    if(!svxcentraltrack)
    {
      cerr << PHWHERE << " ERROR: Can't find SvxCentralTrackList on node tree." << endl;
      return ABORTRUN;
    }
  }
  else
  {
    //For Run11 Pro98 we need both svxcentraltrack and svxcentraltrackrecal tracks
    svxcentraltrack = findNode::getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackList");
    if(!svxcentraltrack)
    {
      cerr << PHWHERE << " ERROR: Can't find SvxCentralTrackList on node tree." << endl;
      return ABORTRUN;
    }

    svxcentraltrackrecal = findNode::getClass<SvxCentralTrackRecalList>(topNode,"SvxCentralTrackRecalList");
    if(!svxcentraltrackrecal)
    {
      cerr << PHWHERE << " ERROR: Can't find SvxCentralTrackRecalList on node tree." << endl;
      return ABORTRUN;
    }
  }
  
  centraltrack = findNode::getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(!centraltrack)
  {
    cerr << PHWHERE << " ERROR: Can't find PHCentralTrack on node tree." << endl;
    return ABORTRUN;
  }
  
  return EVENT_OK;
}


int SvxMomentumRecal::process_event(PHCompositeNode *topNode)
{
  if(getNodes(topNode) != EVENT_OK){return ABORTRUN;};
  
  if(is_simulation)
  {
    for(int i=0;i<svxcentraltrack->get_nCentralTracks();++i)
    {
      SvxCentralTrack* track = svxcentraltrack->getCentralTrack(i);
      float px = track->get3MomentumAtPrimaryVertex(0);
      float py = track->get3MomentumAtPrimaryVertex(1);
      float pz = track->get3MomentumAtPrimaryVertex(2);
      
      float px_cnt = centraltrack->get_px( track->getDchIndex() );
      float py_cnt = centraltrack->get_py( track->getDchIndex() );
      float pt_cnt = sqrt(px_cnt*px_cnt + py_cnt*py_cnt);
      
      float phi0_vtx = atan2(py, px);
      float phi0_cnt = atan2(py_cnt, px_cnt);
      // using 0.002 as uncertainty for phi0 measurement
      // no need to check for 0,2pi wrap in PHENIX acceptance
      float phi_sigs = (phi0_vtx - phi0_cnt)/0.002;
      
      // pt resolution for dc track
      float dpt = 0.00772556*pt_cnt + 0.00368955*pt_cnt*pt_cnt;
      float cal_pt = pt_cnt + ( 0.277222 + 0.123333*pt_cnt )*phi_sigs*dpt;
      
      float momscale = cal_pt/pt_cnt;
      
      track->set3MomentumAtPrimaryVertex(px*momscale, py*momscale, pz*momscale);
    }
  }
  else
  {
    for(unsigned int i=0;i<(unsigned int)(svxcentraltrackrecal->GetNentries());++i)
    {
      SvxCentralTrackRecal* track = svxcentraltrackrecal->GetSvxCNTRecal(i);
      SvxCentralTrack* svxcnt_track = svxcentraltrack->getCentralTrack(i);
      float pt_cnt = track->get_mom() * sin( track->get_the0() );
      float the0_vtx = track->get_the0();
      float phi0_vtx = track->get_phi0();
      float px_cnt = centraltrack->get_px( track->get_CNTID() );
      float py_cnt = centraltrack->get_py( track->get_CNTID() );
      float phi0_cnt = atan2(py_cnt, px_cnt);
      if(phi0_vtx > M_PI) phi0_vtx = phi0_vtx-2*M_PI;
      if(phi0_cnt > M_PI) phi0_cnt = phi0_cnt-2*M_PI;
      
      // using 0.002 as uncertainty for phi0 measurement
      // no need to check for 0,2pi wrap in PHENIX acceptance
      float phi_sigs = (phi0_vtx - phi0_cnt)/0.002;
      
      // pt resolution for dc track
      float dpt = 0.00772556*pt_cnt + 0.00368955*pt_cnt*pt_cnt;
      float cal_pt = pt_cnt + ( 0.277222 + 0.123333*pt_cnt )*phi_sigs*dpt;
      
      float momscale = cal_pt/pt_cnt;
      
      float mom      = track->get_mom();
      float mom_recl = abs(mom*momscale);

      //track->set_mom( mom_recl ); //This is temporarily commented out to keep the original momentum around
      // in the SvxCentralTrackRecal object
      float px = (mom_recl)*(sin(the0_vtx))*(cos(phi0_vtx));
      float py = (mom_recl)*(sin(the0_vtx))*(sin(phi0_vtx));     
      float pz = (mom_recl)*(cos(the0_vtx));

      svxcnt_track->set3MomentumAtPrimaryVertex(px, py, pz);

    }
    
  }
  
  
  
  return EVENT_OK;
}



