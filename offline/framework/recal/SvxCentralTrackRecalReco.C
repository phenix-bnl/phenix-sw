#include "SvxCentralTrackRecalReco.h"

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
#include <compactCNT/SvxHitMap.h>
#include <compactCNT/SvxCentralTrackMap.h>
//#include <SvxHitMap.h>
//#include <SvxCentralTrackMap.h>
#include <SvxCentralTrackRecalList.h>
#include <SvxCentralTrackRecal.h>

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

SvxCentralTrackRecalReco::SvxCentralTrackRecalReco(const string &name)
  : Recalibrator(name)
{
  m_timeStamp = PHTimeStamp();
  m_timeStamp.setToSystemTime();

  m_layer[0] = 0;
  m_layer[1] = 1;
  m_layer[2] = 2;
  m_layer[3] = 2;
  m_layer[4] = 2;
  m_layer[5] = 3;
  m_layer[6] = 3;
  m_layer[7] = 3;

  m_fieldScale = 1.0;

  m_max_sensor[0] = SVXSENSORSLAYER0;
  m_max_sensor[1] = SVXSENSORSLAYER1;
  m_max_sensor[2] = SVXSENSORSLAYER2;
  m_max_sensor[3] = SVXSENSORSLAYER3;

  m_startladder[0][0] = 0;
  m_startladder[1][0] = 0;
  m_startladder[2][0] = 0;
  m_startladder[3][0] = 0;
  m_startladder[0][1] = SVXLADDERSLAYER0;
  m_startladder[1][1] = SVXLADDERSLAYER1;
  m_startladder[2][1] = SVXLADDERSLAYER2;
  m_startladder[3][1] = SVXLADDERSLAYER3;
  m_endladder[0][0] = SVXLADDERSLAYER0;
  m_endladder[1][0] = SVXLADDERSLAYER1;
  m_endladder[2][0] = SVXLADDERSLAYER2;
  m_endladder[3][0] = SVXLADDERSLAYER3;
  m_endladder[0][1] = SVXLADDERSLAYER0*2;
  m_endladder[1][1] = SVXLADDERSLAYER1*2;
  m_endladder[2][1] = SVXLADDERSLAYER2*2;
  m_endladder[3][1] = SVXLADDERSLAYER3*2;
  
  m_sensorZwidth[0] = 5.56;
  m_sensorZwidth[1] = 5.56;
  m_sensorZwidth[2] = 6.;
  m_sensorZwidth[3] = 6.;

  m_pattern = true;
  m_bg = true;

  //baseclasses.insert("VariableArrayInt");
  //baseclasses.insert("VtxOut");
  baseclasses.insert("VariableArrayInt"); // require central-arm
}

int SvxCentralTrackRecalReco::isValidRun(const int runno) const
{
  if ( (int)BEGIN_OF_RUN11<=runno )  {
    return 1;  // only run11 200GeV 
  }
  
  return 0;
}

//_______________________________________________________________________________

int SvxCentralTrackRecalReco::Init(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  if ( rc->get_IntFlag("SVXHITPATTERN",SVXDEFAULTPATTERN)==SVXDEFAULTPATTERN ) {
    m_pattern = false;
  }
  if ( rc->get_IntFlag("SVXBG",SVXDEFAULTBG)==SVXDEFAULTBG ) {
    m_bg = false;
  }

  return EVENT_OK;
}

//_______________________________________________________________________________

int SvxCentralTrackRecalReco::InitRun(PHCompositeNode *topNode)
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
    cout << "SvxCentralTrackRecalReco::InitRun: Reading geometry from database." << endl;
  }
 
  PHNodeIterator iter(topNode);
  PHDataNode<svxDetectorGeo>* SvxGeometryNode = NULL;
  SvxGeometryNode = (PHIODataNode<svxDetectorGeo>*)iter.findFirst("PHDataNode", "svxDetectorGeo");
  if ( SvxGeometryNode ) {
    m_geometry = (svxDetectorGeo*)SvxGeometryNode->getData();
  } else {
    // Look for the PAR node
    PHCompositeNode *parNode;
    parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
    if (!parNode) { 
      cerr << PHWHERE << "PAR node missing, doing nothing." << endl; 
      return EVENT_OK; 
    }
    m_geometry = new svxDetectorGeo();
    SvxGeometryNode = new PHDataNode<svxDetectorGeo>(m_geometry, "svxDetectorGeo");
    parNode->addNode(SvxGeometryNode);
  }
  
  m_geometry->Fetch_coordinateOffset(runnumber);
  if ( !m_geometry->Fetch_svxPISApar(&m_timeStamp) ) {
    cerr << PHWHERE << "ERROR reading VTX geometry from database." << endl; 
  }
  //  float rsublayer[svxDetectorGeo::SVXNSUBLAYER];
  for ( int i=0; i<svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    m_rsublayer[i] = m_geometry->get_Rsublayer(i);
    cout<<"Sublayer "<<i<<" : "<<m_rsublayer[i]<<endl;
  }
  m_rlow[0] = 0.;
  m_rlow[1] = (m_rsublayer[1]+m_rsublayer[0])*0.5;
  m_rlow[2] = (m_rsublayer[3]+m_rsublayer[1])*0.5;
  m_rlow[3] = (m_rsublayer[6]+m_rsublayer[3])*0.5;
  m_rhigh[0] = (m_rsublayer[1]+m_rsublayer[0])*0.5;
  m_rhigh[1] = (m_rsublayer[3]+m_rsublayer[1])*0.5;
  m_rhigh[2] = (m_rsublayer[6]+m_rsublayer[3])*0.5;
  m_rhigh[3] = m_rsublayer[6]*1.5-m_rsublayer[3]*0.5;

  PHCompositeNode *dstNode;
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  m_svxcntrecallist = findNode::getClass<SvxCentralTrackRecalList>(topNode, "SvxCentralTrackRecalList");
  if ( !m_svxcntrecallist ) {
    m_svxcntrecallist = new SvxCentralTrackRecalList();
    PHIODataNode<PHObject> *IONode = new PHIODataNode<PHObject>(m_svxcntrecallist, "SvxCentralTrackRecalList", "PHObject");
    dstNode->addNode(IONode);
    if ( verbosity>0 ) {
      cout << "SvxCentralTrackRecalReco::InitRun SvxCentralTrackRecalList node added to DST node." << endl;
    }
  }

  if ( m_bg ) {
    cout << "analyze BG" << endl;
    m_svxcntrecallist_bg = findNode::getClass<SvxCentralTrackRecalList>(topNode, "SvxBGCentralTrackRecalList");
    if ( !m_svxcntrecallist_bg ) {
      m_svxcntrecallist_bg = new SvxCentralTrackRecalList();
      PHIODataNode<PHObject> *IONode = new PHIODataNode<PHObject>(m_svxcntrecallist_bg, "SvxBGCentralTrackRecalList", "PHObject");
      dstNode->addNode(IONode);
      if ( verbosity>0 ) {
	cout << "SvxCentralTrackRecalReco::InitRun SvxBGCentralTrackRecalList node added to DST node." << endl;
      }
    }
  }

  // get verbosity from MasterRecalibratorManager
  {
    Fun4AllServer *se = Fun4AllServer::instance();
    SubsysReco* sub = se->getSubsysReco("MASTERRECALIBRATORMANAGER");
    if(sub!=NULL){
      verbosity = sub->Verbosity();
    }
    
  }

  return EVENT_OK;
}

//_______________________________________________________________________________

int SvxCentralTrackRecalReco::process_event(PHCompositeNode *topNode)
{
  ///
  /// get VTX hit clusters
  ///
  for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
    m_vcluster[ilayer].clear();
  }
  SvxHitMap* clusterlist = findNode::getClass<SvxHitMap>(topNode, "SvxHit_comp");
  if ( !clusterlist ) {
    if(verbosity>0) cerr << PHWHERE << " SvxHit_comp node not found." << endl;
    return EVENT_OK;
  }
  int nsvxcls = (clusterlist!=NULL) ? clusterlist->GetNentries() : 0;
  /// store clusters in vectors to arrange clusters by its layer.
  //vector<const SvxHitMapEntry*> vcluster[SVXLAYERNUMBER];
  for ( int icls=0; icls<nsvxcls; icls++ ) {
    const SvxHitMapEntry *cluster = clusterlist->GetHit(icls);
    float x = float(cluster->get_x())/1000.;
    float y = float(cluster->get_y())/1000.;
    float r = sqrt(x*x+y*y);
    for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
      if ( r>m_rlow[ilayer] && r<m_rhigh[ilayer] ) {
	m_vcluster[ilayer].push_back(cluster);
	break;
      } // if(r>...)
    } // ilayer
  } // icls
  
  ///
  /// get vertex point
  ///
  VtxOut *vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if ( !vtxout ) {
    if(verbosity>0) cerr << PHWHERE << " VtxOut node not found." << endl;
    return EVENT_OK;
  }
  PHPoint vtx = vtxout->get_Vertex();
  float vx = vtx.getX();
  float vy = vtx.getY();
  float vz = vtx.getZ();

  m_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if ( !m_cnt ) {
    if(verbosity>0) cerr << PHWHERE << " PHCentralTrack node not found." << endl;
  }

  SvxCentralTrackMap *svxcntlist
    = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrack_comp");
  if ( !svxcntlist ) {
    if(verbosity>0) cerr << PHWHERE << " SvxCentralTrack_comp node not found." << endl;
  }
  int nsvxcnt = (svxcntlist!=NULL) ? svxcntlist->GetNentries() : 0;
  for( int isvxcnt=0; isvxcnt<nsvxcnt; isvxcnt++ ) {
    const SvxCentralTrackMapEntry *svxcnt = svxcntlist->GetHit(isvxcnt);
    SvxCentralTrackRecal *svxcntrecal = m_svxcntrecallist->AddEntry();
    /// re-fitting
    make_SvxCNTRecal(svxcnt, svxcntrecal, vx, vy, vz);
  } /// isvxcnt

  if ( m_bg ) {
    SvxCentralTrackMap *svxcntlist_bg
      = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrackBG_comp");
    if ( !svxcntlist_bg ) {
      if(verbosity>0) cerr << PHWHERE << " SvxCentralTrackBG_comp node not found." << endl;
    }
    int nsvxcnt = (svxcntlist_bg!=NULL) ? svxcntlist_bg->GetNentries() : 0;
    for( int isvxcnt=0; isvxcnt<nsvxcnt; isvxcnt++ ) {
      const SvxCentralTrackMapEntry *svxcnt = svxcntlist_bg->GetHit(isvxcnt);
      SvxCentralTrackRecal *svxcntrecal = m_svxcntrecallist_bg->AddEntry();
      /// re-fitting
      make_SvxCNTRecal(svxcnt, svxcntrecal, vx, vy, vz);
    } /// isvxcnt
  }

  for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
    m_vcluster[ilayer].clear();
  }
  
  return EVENT_OK;
}

//_______________________________________________________________________________

void SvxCentralTrackRecalReco::get_LadderSensor(float gx, float gy, float gz, int layer,
						int &ladder, int &sensor)
{
  int w_e = ( gx>0 ) ? 0 : 1;

  /// find ladder
  float min_localy = -1.;
  for ( int ildr=m_startladder[layer][w_e]; ildr<m_endladder[layer][w_e]; ildr++ ) {
    SvxSensor *svxsen = m_geometry->GetSensorPtr(layer, ildr, 0);
    double pos_local[3];
    double pos_global[3] = {gx, gy, gz};
    svxsen->position_global2local(pos_global, pos_local);
    if ( min_localy<0 || min_localy>fabs(pos_local[1]) ) {
      min_localy = fabs(pos_local[1]);
      ladder = ildr;
    }
  } // ildr
  if ( min_localy>1. ) {
    if(verbosity>0)
      cerr << PHWHERE << " fail to find ladder : minimum is too large | " 
	   << min_localy <<" : "<< gx <<", "<< gy <<", "<< gz << endl;
  }

  /// find sensor
  float min_abslocalz = -1.;
  for ( int isn=0; isn<m_max_sensor[layer]; isn++ ) {
    SvxSensor *svxsen = m_geometry->GetSensorPtr(layer, ladder, isn);
    double pos_local[3];
    double pos_global[3] = {gx, gy, gz};
    svxsen->position_global2local(pos_global, pos_local);
    if ( min_abslocalz<0 || min_abslocalz>fabs(pos_local[2]) ) {
      min_abslocalz = fabs(pos_local[2]);
      sensor = isn;
    }
  }
}

//_______________________________________________________________________________

void SvxCentralTrackRecalReco::make_SvxCNTRecal(const SvxCentralTrackMapEntry *svxcntent,
						SvxCentralTrackRecal *svxcntout,
						float vx, float vy, float vz)
{
  int hitpattern = svxcntent->get_HitPattern();
  if ( m_pattern ) {
    if ( hitpattern>=128 ) hitpattern = 256+(128-hitpattern);
  }
  int nhit[SVXLAYERNUMBER] = {0, 0, 0, 0};
  vector<int> sublayerlist;
  for ( int i=0; i<svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    if ( (hitpattern&(0x1<<i))>0 ) {
      nhit[m_layer[i]] ++;
      sublayerlist.push_back(i);
    } // if(hitpattern&...)
  } // i
  int cntid = svxcntent->get_DchIndex();
  svxcntout->set_CNTID(cntid);
  svxcntout->set_HitPattern(hitpattern);
  svxcntout->set_Unique(svxcntent->get_Unique());
  for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
    svxcntout->set_nhit(ilayer, nhit[ilayer]);
  }
  svxcntout->set_Chisquare((float)svxcntent->get_Chisquare()/100.);
  svxcntout->set_ChisquareOld((float)svxcntent->get_Chisquare()/100.);
  svxcntout->set_Chisquare2((float)svxcntent->get_Chisquare2()/100.);
  svxcntout->set_ChisquarePhi((float)svxcntent->get_ChisquarePhi()/100.);
  svxcntout->set_ChisquareZ((float)svxcntent->get_ChisquareZ()/100.);
  svxcntout->set_DCA2D((float)svxcntent->get_DCA2D()/10000.);
  svxcntout->set_DCAZ((float)svxcntent->get_DCAZ()/10000.);
  svxcntout->set_DCA2DOld((float)svxcntent->get_DCA2D()/10000.);
  svxcntout->set_DCAZOld((float)svxcntent->get_DCAZ()/10000.);
  
  int ncls = 0;
  for ( int ilayer=SVXLAYERNUMBER-1; ilayer>=0; ilayer-- ) {
    for ( int ihit=nhit[ilayer]-1; ihit>=0; ihit-- ) {
      int clsidx = svxcntent->get_ClusterID(ncls);
      float x = 0.;
      float y = 0.;
      float z = 0.;
      int ladder;
      int sensor;
      bool found = false;
      for ( int icls=0; icls<(int)m_vcluster[ilayer].size(); icls++ ) {
	if ( clsidx==(int)m_vcluster[ilayer][icls]->get_id() ) {
	  x = float(m_vcluster[ilayer][icls]->get_x())/1000.;
	  y = float(m_vcluster[ilayer][icls]->get_y())/1000.;
	  z = float(m_vcluster[ilayer][icls]->get_z())/1000.;
	  svxcntout->set_ClusterPosition(ilayer, x, y, z, ihit);
	  svxcntout->set_ClusterID(ilayer, clsidx, ihit);
	  found = true;
	  break;
	} // if(clsidx==...)
      } // icls
      if ( !found ) {
	if ( verbosity>0 ) cerr << PHWHERE << "fails to find cluster." << endl;
      }
      get_LadderSensor(x, y, z, ilayer, ladder, sensor);
      svxcntout->set_ClusterLadder(ilayer, ladder, ihit);
      svxcntout->set_ClusterSensor(ilayer, sensor, ihit);
      ncls ++;
    } // ihit
  } // ilayer
  
  float cax = float(svxcntent->get_ClosestApproach(0))/10000.;
  float cay = float(svxcntent->get_ClosestApproach(1))/10000.;
  float caz = float(svxcntent->get_ClosestApproach(2))/1000.;
  svxcntout->set_ClosestApproach(0, cax);
  svxcntout->set_ClosestApproach(1, cay);
  svxcntout->set_ClosestApproach(2, caz);
  int charge = m_cnt->get_charge(cntid);
  float alpha = m_cnt->get_alpha(cntid);
  float phi = m_cnt->get_phi(cntid);
  float mom = m_cnt->get_mom(cntid);
  float phi0 = m_cnt->get_phi0(cntid);
  float the0 = m_cnt->get_the0(cntid);
  float pt = mom;
  if ( the0>-999 ) pt = mom*sin(the0);
  if ( isinf(mom) ) pt = 100.;
  /// Update alpha, phi0, pt
  float new_alpha = alpha + cax*sin(phi)/220. - cay*cos(phi)/220.;
  float new_pt = pt*alpha/new_alpha;
  float new_phi0 = phi0 + 2.0195*(new_alpha-alpha);
  svxcntout->set_phi0(new_phi0);
  svxcntout->set_the0(the0);
  svxcntout->set_mom(new_pt/sin(the0));
  svxcntout->set_charge(charge);

  int nhits = 0;
  for ( int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++ ) {
    for ( int ihit=0; ihit<nhit[ilayer]; ihit++ ) {
      float dphi = (float)svxcntent->get_ClusterDPhi(nhits)/10000.;
      float dz = (float)svxcntent->get_ClusterDZ(nhits)/10000.;
      svxcntout->set_ClusterDPhi(ilayer, dphi, ihit);
      svxcntout->set_ClusterDZ(ilayer, dz, ihit);
      nhits ++;
    } // ihit
    /// calculate projection position
    float proj[3]={0.,0.,0.};
    float rlayer = m_rsublayer[ilayer];
    if ( ilayer>=2 ) rlayer = m_rsublayer[3*ilayer-3]; /// for stripixel layers
     CircleProjection(new_pt, new_phi0, the0, charge, cax, cay, caz, rlayer,
		      proj[0], proj[1], proj[2]);
     svxcntout->set_ProjectedPosition(ilayer, proj[0], proj[1], proj[2]);
  } // ilayer
}

//_______________________________________________________________________________

void SvxCentralTrackRecalReco::calcDCAbyCircleProjection
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

  float dphi = phi - dir*0.5*M_PI - psi;
  if ( dphi>M_PI*1.5 ) dphi -= M_PI*2;
  else if ( dphi<-M_PI*1.5 ) dphi += M_PI*2;
  float dzdphi = dir*pz*R/pt;
  *dz = hz + dphi*dzdphi;

  // DCA value
  *d2dca = b_sign*(charge*(R - L));
}

//_______________________________________________________________________________

void SvxCentralTrackRecalReco::CircleProjection
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
