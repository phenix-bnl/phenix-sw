#include "SvxCentralTrackReFit.h"

#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <VtxOut.h>
#include <getClass.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <RunToTime.hh>
#include <PHTimeStamp.h>
#include <SvxSensor.h>
#include <SvxClusterv4.h>
#include <SvxTracker.h>
#include <SvxCentralTrackRecalList.h>
#include <SvxCentralTrackRecal.h>
#include <SvxCentralTrackRecalReco.h>

#include <cstdlib>
#include <iostream>
#include <memory>
#include <cmath>

using namespace std;

static const float B = 0.90;
static const float b = 0.003*B;

// PLEASE: use definitions from RunNumberRanges.h for run numbers. We can always add to them but we 
// want to avoid hardcoded runnumbers as much as possible


//_______________________________________________________________________________

SvxCentralTrackReFit::SvxCentralTrackReFit(const string &name)
  : Recalibrator(name)
{
  m_timeStamp = PHTimeStamp();
  m_timeStamp.setToSystemTime();

  m_fieldScale = 1.0;
  m_ievt = 0;

  //baseclasses.insert("SvxCentralTrackRecal");
  //baseclasses.insert("VariableArray");
  //baseclasses.insert("VtxOut");
  baseclasses.insert("VariableArrayInt");
}

int SvxCentralTrackReFit::isValidRun(const int runno) const
{
  if ( (int)BEGIN_OF_RUN11<=runno && runno< (int)BEGIN_OF_RUN12) {
    return 1;  // only run11 200GeV
  }
  
  return 0;
}

//_______________________________________________________________________________

int SvxCentralTrackReFit::Init(PHCompositeNode *topNode)
{
  //m_geometry = new svxDetectorGeo();

  return EVENT_OK;
}

//_______________________________________________________________________________

int SvxCentralTrackReFit::InitRun(PHCompositeNode *topNode)
{
  RunHeader *runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");
  int runnumber = runhdr->get_RunNumber();
  if ( !runhdr ) {
    cerr << PHWHERE << "RunHeader is not in Node Tree" << endl;
    return EVENT_OK;
  }
  if ( runhdr->get_currentCentral()>0 ) {
    m_fieldScale = 1.0;
  } else {
    m_fieldScale = -1.0;
  }

  RunToTime *rt = RunToTime::instance();
  PHTimeStamp* tStamp = rt->getBeginTime(runnumber);
  m_timeStamp = *(tStamp);
  delete tStamp;
  if ( verbosity>0 ) {
    cout << "SvxCentralTrackReFit::InitRun: Reading geometry from database." << endl;
  }

  PHNodeIterator iter(topNode);
  PHDataNode<svxDetectorGeo>* SvxGeometryNode = NULL;
  SvxGeometryNode = (PHIODataNode<svxDetectorGeo>*)iter.findFirst("PHDataNode", "svxDetectorGeo");
  if ( SvxGeometryNode ) {
    m_geometry = (svxDetectorGeo*)SvxGeometryNode->getData();
  } else {
    return EVENT_OK;
  }
  m_geometry->Fetch_coordinateOffset(runnumber);
  if ( !m_geometry->Fetch_svxPISApar(&m_timeStamp) ) {
    cerr << PHWHERE << "ERROR reading VTX geometry from database." << endl; 
  }
  //  float rsublayer[svxDetectorGeo::SVXNSUBLAYER];
  for ( int i=0; i<svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    m_rsublayer[i] = m_geometry->get_Rsublayer(i);
  }
  m_tracker.set_DetectorGeo(m_geometry);

  return EVENT_OK;
}

//_______________________________________________________________________________

int SvxCentralTrackReFit::process_event(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  if ( rc->get_IntFlag("SVXFIT",SVXNOFIT)==SVXNOFIT ) return EVENT_OK;

  ///
  /// get vertex point
  ///
  SvxCentralTrackRecalList *svxcntrecallist
    = findNode::getClass<SvxCentralTrackRecalList>(topNode, "SvxCentralTrackRecalList");
  if ( !svxcntrecallist ) {
    cerr << "SvxCentralTrackRecalList node not found." << endl;
    return EVENT_OK;
  }

  VtxOut *vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if ( !vtxout ) {
    cerr << PHWHERE << " VtxOut node not found." << endl;
    return EVENT_OK;
  }
  PHPoint vtx = vtxout->get_Vertex();
  float vx = vtx.getX();
  float vy = vtx.getY();
  float vz = vtx.getZ();
  
  m_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if ( !m_cnt ) {
    cerr << PHWHERE << " PHCentralTrack node not found." << endl;
    return EVENT_OK;
  }
  
  int nsvxcnt = svxcntrecallist->GetNentries();
  for( int isvxcnt=0; isvxcnt<nsvxcnt; isvxcnt++ ) {
    SvxCentralTrackRecal *svxcntrecal = svxcntrecallist->GetSvxCNTRecal(isvxcnt);
    /// re-fitting
    if ( svxcntrecal->get_nhit()<2 ) continue;
    ReFit_SvxCNT(svxcntrecal, vx, vy, vz);
  } /// isvxcnt

  if ( rc->get_IntFlag("SVXBG",SvxCentralTrackRecalReco::SVXDEFAULTBG)
       !=SvxCentralTrackRecalReco::SVXDEFAULTBG ) {
    SvxCentralTrackRecalList *svxcntrecallist_bg
      = findNode::getClass<SvxCentralTrackRecalList>(topNode, "SvxBGCentralTrackRecalList");
    if ( !svxcntrecallist_bg ) {
      cerr << "SvxBGCentralTrackRecalList node not found." << endl;
      return EVENT_OK;
    }
    int nsvxcntbg = svxcntrecallist_bg->GetNentries();
    for( int isvxcnt=0; isvxcnt<nsvxcntbg; isvxcnt++ ) {
      SvxCentralTrackRecal *svxcntrecal = svxcntrecallist_bg->GetSvxCNTRecal(isvxcnt);
      /// re-fitting
      if ( svxcntrecal->get_nhit()<2 ) continue;
      ReFit_SvxCNT(svxcntrecal, vx, vy, vz);
    } /// isvxcnt
  }

  return EVENT_OK;
}

//_______________________________________________________________________________

void SvxCentralTrackReFit::ReFit_SvxCNT(SvxCentralTrackRecal *svxcntrecal,
					float vx, float vy, float vz)
{
  vector<SvxCluster*> vhit;
  //SVXLAYERNUMBER == 4
  //there might be more than one hit in vhit correspongind to one layer
  for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
    int nhit = svxcntrecal->get_nhit(ilayer);
    for ( int ihit=0; ihit<nhit; ihit++ ) {
      float x = svxcntrecal->get_ClusterPosition(ilayer,0,ihit);
      float y = svxcntrecal->get_ClusterPosition(ilayer,1,ihit);
      float z = svxcntrecal->get_ClusterPosition(ilayer,2,ihit);
      int ladder = svxcntrecal->get_ClusterLadder(ilayer, ihit);
      int sensor = svxcntrecal->get_ClusterSensor(ilayer, ihit);
      SvxCluster *t_cls = new SvxClusterv4();
      t_cls->set_layer(ilayer);
      t_cls->set_ladder(ladder);
      t_cls->set_sensor(sensor);
      t_cls->set_xyz_global(0, x);
      t_cls->set_xyz_global(1, y);
      t_cls->set_xyz_global(2, z);
      SvxSensor *svxsen = m_geometry->GetSensorPtr(ilayer, ladder, sensor);
      double pos_local[3];
      double pos_global[3] = {x, y, z};
      svxsen->position_global2local(pos_global, pos_local);
      t_cls->set_xyz_local(0, pos_local[0]);
      t_cls->set_xyz_local(1, pos_local[1]);
      t_cls->set_xyz_local(2, pos_local[2]);
      vhit.push_back(t_cls);
    } // ihit
  } // ilayer

  ///
  /// start refitting
  ///
  int charge = svxcntrecal->get_charge();
  double mom = svxcntrecal->get_mom();
  double phi0 = svxcntrecal->get_phi0();
  double the0 = svxcntrecal->get_the0();
  double pt = mom;
  if ( the0>-999 ) pt = mom*sin(the0);

  double cax = svxcntrecal->get_ClosestApproach(0);
  double cay = svxcntrecal->get_ClosestApproach(1);
  //float caz = svxcntewcal->get_ClosestApproach(2);

  bool helicity = (charge*m_fieldScale>0) ? true : false;
  double chisq;
  int ndf;
  double chisq_phi, chisq_z, chisq_angle;
  m_tracker.TrackFit_SvxCNT(vhit, helicity, pt, phi0, the0,
			    cax, cay, chisq, ndf,
                            chisq_phi, chisq_z, chisq_angle);
  /// calculate DCA again
  float px = m_tracker.get3Mom(0);
  float py = m_tracker.get3Mom(1);
  float xexp;
  float yexp;
  float zexp;


  //this gets the expeced position of the first cluster [0] ?? 
  m_tracker.getExpectedPosition(vhit[0]->get_layer(), xexp, yexp, zexp, 0);
  
  float pos_primary[3];
  float new_pt2 = sqrt(px*px+py*py);
  float dca2d;
  calcDCAbyCircleProjection(new_pt2, atan2(py,px), the0, charge,
			    xexp, yexp, zexp,
			    vx, vy,
			    &(pos_primary[0]), &(pos_primary[1]), &(pos_primary[2]),
			    &dca2d);

  //--- Save the Residuals from the Multi Circle Fit
  //loop over the array of clusters vhit
  //there might be more than one hit in vhit corresponding to each layer
  // so for each layer save the residual from the cluster which has the smallest residual
  float smallest_multi_dphi[4] = {-9999,-9999,-9999,-9999};
  float smallest_multi_dz[4]   = {-9999,-9999,-9999,-9999};
  for ( int i_hit = 0; i_hit < (int) vhit.size(); i_hit++ ) 
    {
      //Get the Layer Associated with this cluster
      //(SvxCluster*) -> get_layer() returns 0,1,2,3 from INNER to OUTER (0==B0)
      int i_act_layer = vhit[i_hit]->get_layer();

      //just save 4 hits for now
      if((i_act_layer < 0) || (i_act_layer > 3) ) continue;

      //get Multi Circle Fit Residuals
      //the vector of SvxClusters "vhit" is passed to SvxTracker (where its called vcluster)
      float multi_dphi = m_tracker.getDPHI(i_hit);
      float multi_dz   = m_tracker.getDZ(i_hit);
    
      float multi_sq          = sqrt(multi_dphi*multi_dphi + multi_dz*multi_dz);
      float smallest_multi_sq = sqrt(smallest_multi_dphi[i_act_layer]*smallest_multi_dphi[i_act_layer] + smallest_multi_dz[i_act_layer]*smallest_multi_dz[i_act_layer]);

      //see if this cluster has the smallest residual for this track
      bool b_smallest = ((multi_sq <= smallest_multi_sq) || (smallest_multi_dphi[i_act_layer] == -9999) || (smallest_multi_dphi[i_act_layer] == -9999) );

      if(!b_smallest) continue;

      //If this is the smallest residual for this layer save it
      // (if another one is smaller this one will be overwritten)
      smallest_multi_dphi[i_act_layer] = multi_dphi;
      smallest_multi_dz[i_act_layer]   = multi_dz;

      svxcntrecal -> set_multi_dphi(i_act_layer,multi_dphi);
      svxcntrecal -> set_multi_dz(i_act_layer,multi_dz);

      //also save the expected cluster position
      m_tracker.getExpectedPosition(i_act_layer, xexp, yexp, zexp, 0);

      svxcntrecal -> set_expected_position(i_act_layer,0,xexp);
      svxcntrecal -> set_expected_position(i_act_layer,1,yexp);
      svxcntrecal -> set_expected_position(i_act_layer,2,zexp);
    }//  for ( int i_hit=0; i_hit<(int)vhit.size(); i_hit++ ) {
  //--- END Save the Residuals from the Multi Circle Fit

  ///
  /// update
  ///
  svxcntrecal->set_ClosestApproach(0, pos_primary[0]);
  svxcntrecal->set_ClosestApproach(1, pos_primary[1]);
  svxcntrecal->set_ClosestApproach(2, pos_primary[2]);
  svxcntrecal->set_Chisquare(chisq);
  svxcntrecal->set_DCA2D(dca2d);
  svxcntrecal->set_DCAZ((pos_primary[2]-vz));
  for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
    /// calculate projection position
    float proj[3]={0.,0.,0.};
    float rlayer = m_rsublayer[ilayer];
    if ( ilayer>=2 ) rlayer = m_rsublayer[3*ilayer-3]; /// for stripixel layers
    CircleProjection(pt, phi0, the0, charge,
		     pos_primary[0], pos_primary[1], pos_primary[2], rlayer,
		     proj[0], proj[1], proj[2]);
    svxcntrecal->set_ProjectedPosition(ilayer, proj[0], proj[1], proj[2]);

  } // ilayer

  /// Update phi0 and pt using updated closest approach position
  int cntid = svxcntrecal->get_CNTID();
  float alpha_cnt = m_cnt->get_alpha(cntid);
  float phi_cnt = m_cnt->get_phi(cntid);
  float mom_cnt = m_cnt->get_mom(cntid);
  float phi0_cnt = m_cnt->get_phi0(cntid);
  float the0_cnt = m_cnt->get_the0(cntid);
  float pt_cnt = mom_cnt;
  if ( the0_cnt>-999 ) pt_cnt = mom_cnt*sin(the0_cnt);
  if ( isinf(mom_cnt) ) pt_cnt = 100.;
  float new_alpha = alpha_cnt
    + pos_primary[0]*sin(phi_cnt)/220. - pos_primary[1]*cos(phi_cnt)/220.;
  float new_pt = pt_cnt*alpha_cnt/new_alpha;
  float new_phi0 = phi0_cnt + 2.0195*(new_alpha-alpha_cnt);
  svxcntrecal->set_phi0(new_phi0);
  svxcntrecal->set_mom(new_pt/sin(the0_cnt));
  
  for ( int i=0; i<(int)vhit.size(); i++ ) {
    delete vhit[i];
  }
}

//_______________________________________________________________________________

void SvxCentralTrackReFit::calcDCAbyCircleProjection
(
 float pt, float phi, float the,  // pt in xy-plane, phi, theta at inner most layer
 int charge,                      // charge of the track
 float hx, float hy, float hz,    // hit position at inner most layer
 float vx, float vy,              // primary vertex
 float* dx, float* dy, float* dz, // dca position
 float* d2dca)                    // return
{
  // vector to rotation center at hit1 
  float R  = pt/b;
  float pz = pt/tan(the);

  float b_sign = (m_fieldScale>0) ? -1.0 : 1.0;
  float dir = ( (b_sign)*charge>0. ) ? -1.0 : 1.0;
  float cx = hx - dir*R*sin(phi);
  float cy = hy + dir*R*cos(phi);

  // L is a distance btw the rotation center and primary vtx  
  // psi is a angle of the vector starting from the rotation center to primary vertex
  float L = sqrt((vx-cx)*(vx-cx) + (vy-cy)*(vy-cy));
  float psi = atan2((vy-cy), (vx-cx));

  // DCA point
  *dx = cx + R*cos(psi);
  *dy = cy + R*sin(psi);

  float dphi = atan2((hy-cy),(hx-cx)) - psi;
  if ( dphi>M_PI*1.5 ) dphi -= M_PI*2;
  else if ( dphi<-M_PI*1.5 ) dphi += M_PI*2;
  float dzdphi = pz*R/pt;
  *dz = hz - fabs(dphi)*dzdphi;

  // DCA value
  *d2dca = b_sign*(charge*(R - L));
}

//_______________________________________________________________________________

void SvxCentralTrackReFit::CircleProjection
(
 float pt, float phi0, float the0, // pt & phi, theta at the closest approach
 int charge,
 float cax, float cay, float caz,  // closest approach
 float r,                          // radius of projected cylinder
 float &xproj, float &yproj, float &zproj) // projected position
{
  float pz = pt/tan(the0);
  
  // vector to rotation center at hit1 
  float R  = pt/b;

  float b_sign = (m_fieldScale>0) ? -1.0 : 1.0;
  float dir = ( (b_sign)*charge>0. ) ? -1.0 : 1.0;
  float cx = cax - dir*R*sin(phi0);
  float cy = cay + dir*R*cos(phi0);

  // L is a distance btw the rotation center and primary vtx  
  // psi is a angle of the vector starting from the rotation center to primary vertex
  float L = sqrt(cx*cx+cy*cy);
  float psi = atan2(cy, cx);
  
  float cosA = (r*r+L*L-R*R)/2./r/L;
  if((1.-cosA*cosA) < 0)
    {
      if ( verbosity>0 ) cout<< "ERROR: Value out of bounds." << endl;      
      return;
    }

  float sinA = sqrt(1.-cosA*cosA);
  if ( dir>0 ) {
    xproj = r*(cos(psi)*cosA-sin(psi)*sinA);
    yproj = r*(sin(psi)*cosA+cos(psi)*sinA);
  } else {
    xproj = r*(cos(psi)*cosA+sin(psi)*sinA);
    yproj = r*(sin(psi)*cosA-cos(psi)*sinA);
  }

  float dphi = atan2(yproj-cy,xproj-cx) - atan2(cay-cy,cax-cx);
  if ( dphi>M_PI*1.5 ) dphi -= M_PI*2;
  else if ( dphi<-M_PI*1.5 ) dphi += M_PI*2;
  float dzdphi = pz*R/pt;
  zproj = caz + fabs(dphi)*dzdphi;
}
