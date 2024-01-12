#include "SvxCentralTrackFitRecal.h"

#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <RunToTime.hh>
#include <PHTimeStamp.h>

#include <recoConsts.h>
#include <getClass.h>
#include <RunNumberRanges.h>
#include <PHPoint.h>

#include <RunHeader.h>
#include <VtxOut.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <SvxCentralTrack.h>
#include <SvxCentralTrackList.h>

#include <SvxClusterv5.h>
#include <SvxClusterList.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>


#include <SvxSensor.h>
#include <SvxTracker.h>

#include <TMath.h>

//#include <cstdlib>
//#include <iostream>
//#include <memory>
//#include <cmath>

using namespace std;
using namespace findNode;

static const float B = 0.90;
static const float b = 0.003*B;

// PLEASE: use definitions from RunNumberRanges.h for run numbers. We can always add to them but we 
// want to avoid hardcoded runnumbers as much as possible


//_______________________________________________________________________________

SvxCentralTrackFitRecal::SvxCentralTrackFitRecal(const string &name)
  : Recalibrator(name),
  m_tracker_out(NULL),
  m_geometry(NULL),
  //m_isFitNoCNT(false),
  m_isFitNoCNT(true),
  m_fieldScale(1.0),
  m_shiftphcentral(true),
  m_readphcntrotdb(true),
  m_phcntrotdbname("svxphcntrotations"),
  m_ievt(0),
  m_enableModule(true)
{
  //baseclasses.insert("SvxCentralTrackList");
  baseclasses.insert("VariableArrayInt");

  for(int iarm=0; iarm<2; iarm++){
    m_rotPHcntPhi[iarm]   = 0.;
    m_rotPHcntTheta[iarm] = 0.;
  }
}

int SvxCentralTrackFitRecal::isValidRun(const int runno) const
{
  // always enable
  if(runno>=BEGIN_OF_RUN11) return 1;

  return 0;
}

//_______________________________________________________________________________

int SvxCentralTrackFitRecal::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//_______________________________________________________________________________

int SvxCentralTrackFitRecal::InitRun(PHCompositeNode *topNode)
{
  //check if SvxCentralTrackList exist. if not, disable this module
  SvxCentralTrackList *svxcntlist
                 = getClass<SvxCentralTrackList>(topNode, "SvxCentralTrackList");
  if ( svxcntlist == NULL ) {
    cout << "No SvxCentralTrackList node. SvxCentralTrackFitRecal disabled" << endl;
    m_enableModule = false;
    return EVENT_OK;
  }

  ////////////////////////////
  RunHeader *runhdr = getClass<RunHeader>(topNode, "RunHeader");
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

  recoConsts *rc =  recoConsts::instance();
  //
  if ( rc->FlagExist("SVXCENTRALTRACKFITRECAL_SETFITNOCNT") )
  {
     int flag_fitnocnt = rc->get_IntFlag("SVXCENTRALTRACKFITRECAL_SETFITNOCNT");
     setFitNoCNT( (flag_fitnocnt != 0) );

     cout<<"flag SVXCENTRALTRACKFITRECAL_SETFITNOCNT is set : "<< flag_fitnocnt<<endl;
  }


  m_geometry = getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if ( m_geometry==NULL ) {
    cerr << PHWHERE << "no svxDetectorGeo in the node true" <<endl;
    return EVENT_OK;
  }
  for ( int i=0; i<svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    m_rsublayer[i] = m_geometry->get_Rsublayer(i);
  }
  m_tracker.set_DetectorGeo(m_geometry);
  m_tracker.setFitNoCNT(m_isFitNoCNT);

  //Set SvxTracker in Zero Field mode
  if ( fabs(runhdr->get_currentCentral())<0.1 ) {
    m_tracker.set_ZeroFieldFlag(true);
      cout << "SvxCentralTrackFitRecal:: SvxTracker is set as ZERO field " <<endl;
  }


  if ( rc->FlagExist("SIMULATIONFLAG") )
  {
    // Change the default behavior of PHCentralTrack rotations in SIMULATIONS
    m_shiftphcentral = false;
    if ( verbosity)
    {
      cout << "SvxCentralTrackFitRecal:: --"
           << " Found SIMULATIONFLAG in recoConsts."
           << " Turning off PHCentralTrack rotations by default."
           << endl;
    }

  }
  // set fit parameters
  if ( rc->FlagExist("SIMULATIONFLAG") && rc->get_IntFlag("SIMULATIONFLAG") >= 1 )
  {
    m_tracker.SetFitParameters("SIM");
    cout << "SvxCentralTrackFitRecal:: SvxTracker is set as SIM " <<endl;
  }
  else
  {
    m_tracker.SetFitParameters("DATA");
    cout << "SvxCentralTrackFitRecal:: SvxTracker is set as DATA " <<endl;
  }


  

  //read PHCentralTrack rotation parameters from DB (if desired)
  if (m_shiftphcentral && m_readphcntrotdb)
    fetchDBPHCentralTrackDphiDtheta(runnumber);

  cout<<"SvxCentralTrackFitRecal : mode = "<<(m_isFitNoCNT?"not use CNT":"use CNT")<<endl;
  cout<<"SvxCentralTrackFitRecal : verbosity = "<<Verbosity()<<" "<<verbosity<<endl;

  return EVENT_OK;
}

//_______________________________________________________________________________

int SvxCentralTrackFitRecal::process_event(PHCompositeNode *topNode)
{
  if(!m_enableModule) { 
    return EVENT_OK;
  }

  if(verbosity>1) cout<<"SvxCentralTrackFitRecal::process_event"<<endl;

  ///
  /// get data object from NODE
  ///
  static int nErrCnt = 0;
  PHCentralTrack* cntlist = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if ( cntlist == NULL ) {
    if(nErrCnt<5) { cerr << PHWHERE << " PHCentralTrack node not found." << endl; nErrCnt++; }
    return EVENT_OK;
  }
  SvxCentralTrackList *svxcntlist
                 = getClass<SvxCentralTrackList>(topNode, "SvxCentralTrackList");
  if ( svxcntlist == NULL ) {
    if(nErrCnt<5) { cerr << "SvxCentralTrackList node not found." << endl; nErrCnt++; }
    return EVENT_OK;
  }
  SvxClusterList *svxclslist
                 = getClass<SvxClusterList>(topNode, "SvxClusterList");
  if ( svxclslist == NULL ) {
    if(nErrCnt<5) { cerr << "SvxClusterList node not found." << endl; nErrCnt++; }
    return EVENT_OK;
  }
  VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
  if ( vtxout == NULL ) {
    if(nErrCnt<10) { cerr << PHWHERE << " VtxOut node not found." << endl; nErrCnt++; }
    return EVENT_OK;
  }
  if(nErrCnt==4){
    cerr<<"Stop printing error from SvxCentralTrackFitRecal"<<endl;
    nErrCnt=5;
  }
  

  // primary vertex
  PHPoint vtx = vtxout->get_Vertex();
  float vx = vtx.getX();
  float vy = vtx.getY();
  float vz = vtx.getZ();
  
  
  // track loop
  int nCnt = cntlist->get_npart();
  int nSvxCnt = svxcntlist->get_nCentralTracks();
  //--cout<<"FitRecal : "<<nCnt<<" "<<nSvxCnt<<endl;
  for( int isvx=0; isvx<nSvxCnt; isvx++ ) {
    SvxCentralTrack *svxcnt = svxcntlist->getCentralTrack(isvx);
    //--cout<<"  nhit : "<<svxcnt->getNhits()<<endl;
    if ( svxcnt->getNhits()<2 ) {
      continue;
    }
    

    int cntidx = svxcnt->getDchIndex();
    if(cntidx<0||nCnt<=cntidx){
      cout<<"cntidx is out of range : "<<cntidx<<endl;
      continue;
    }

    PHSnglCentralTrack *cnt = cntlist->get_track(cntidx);
 
    /// fitting
    fitSvxCNT(cnt, svxcnt, vx, vy, vz, svxclslist);
  } /// isvxcnt


/*
  // background?
  if ( rc->get_IntFlag("SVXBG",SvxCentralTrackRecalReco::SVXDEFAULTBG)
       !=SvxCentralTrackRecalReco::SVXDEFAULTBG ) 
  {
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
*/

  return EVENT_OK;
}

//_______________________________________________________________________________

void SvxCentralTrackFitRecal::fitSvxCNT(
                        PHSnglCentralTrack* cnt, 
                        SvxCentralTrack *svxcnt,
                        float vx, float vy, float vz,
                        SvxClusterList *svxclslist
                    )
{
  if(verbosity>0) cout<<"SvxCentralTrackFitRecal::fitSvxCNT"<<endl;

  if(cnt   ==NULL){ cout<<"No CNT object"<<endl;    return; }
  if(svxcnt==NULL){ cout<<"No SvxCNT object"<<endl; return; }
  if(svxclslist==NULL){ cout<<"No SvxClusterList object"<<endl; return; }

  vector<SvxCluster*> vhit;

  // reverse order since ClusterInfo is from most outer layer
  for ( int ihit=svxcnt->getNhits()-1; ihit>=0; ihit-- ) {
    SvxClusterInfo *cinfo = svxcnt->getClusterInfo(ihit);
    if(cinfo==NULL) { cout<<"No SvxClusterInfo, trackidx="<<ihit<<endl; continue; }

/* this is for test. using standard cluster
 * 
    SvxCluster * cls = NULL;
    int cid = cinfo->getClusterId();
    for ( int icls=0; svxclslist->get_nClusters(); icls++ ) {
      SvxCluster * cls_tmp = svxclslist->get_Cluster(icls);
      if( cls_tmp->get_hitID() == cid ) {
        cls = cls_tmp;
        break;
      }
    }
    if(cls==NULL) { cout<<"No cluster found"<<endl; continue; }
*/

    int layer  = cinfo->getLayer();
    int ladder = cinfo->getLadder();
    int sensor = cinfo->getSensor();
    double pos_global[3];
    for(int ixyz=0; ixyz<3; ixyz++){
      pos_global[ixyz] = cinfo->getPosition(ixyz);
    }
    //--cout<<ihit<<" "<<layer<<endl;

    // convert hit position into local geometry
    SvxSensor *svxsen = m_geometry->GetSensorPtr(layer, ladder, sensor);
    double pos_local[3];
    svxsen->position_global2local(pos_global, pos_local);
    if( fabs(pos_local[1])>0.001) {
      cout<<"local_y is too large "<<endl;
    }

    pos_local[1] = 0.; // local_y should be always 0

    SvxCluster *cls = new SvxClusterv5();
    cls->set_layer(layer);
    cls->set_ladder(ladder);
    cls->set_sensor(sensor);
    for(int ixyz=0; ixyz<3; ixyz++){
      cls->set_xyz_global(ixyz, pos_global[ixyz]);
      cls->set_xyz_local( ixyz, pos_local[ixyz]);
    }

    vhit.push_back(cls);
  } // ihit


  ///
  /// start fitting
  ///
 
  // CNT track info
  int charge = cnt->get_charge();
  double mom  = cnt->get_mom();
  float phi0f = cnt->get_phi0();
  float the0f = cnt->get_the0();
  if (m_shiftphcentral)
  {
    shiftPHCentralTrack(&phi0f, &the0f);
  }
  double phi0 = phi0f;
  double the0 = the0f;
  if (phi0 > TMath::Pi()) // phi0 range is +-pi
  {
    phi0 -= (2.0 * TMath::Pi());
  }
  double pt = mom;
  if ( the0>-999 ) pt = mom*sin(the0);

/*
  // DCA position from reconstruction
  float cax = svxcnt->getClosestApproach(0);
  float cay = svxcnt->getClosestApproach(1);
  //float caz = svxcnt->getClosestApproach(2);
*/

  double hx = vhit[0]->get_xyz_global(0);
  double hy = vhit[0]->get_xyz_global(1);
  double hz = vhit[0]->get_xyz_global(1);

  // phi1 which is phi angle at innermost layer
  double r = sqrt((hx - vx) * (hx - vx) + (hy - vy) * (hy - vy));
  double phi1 = phi0 + charge * (m_fieldScale * b * r / pt); // dphi = b*r/pT

  double dcapos[3];
  double d2dca;
  calcDCAbyCircleProjection
	  (pt, phi1, the0, charge, hx, hy, hz,// pt , phi, charge at inner most layer and its hit position
	   vx, vy,                   // primary vertex
	   &(dcapos[0]), &(dcapos[1]), &(dcapos[2]),  // DCA position
	   &d2dca);                       // return
  double cax = dcapos[0];
  double cay = dcapos[1];
  

  
  bool helicity = (charge * m_fieldScale > 0 ) ? true : false;

  /// update alpha, phi0, pt
  double dphi0 = phi0 - cnt->get_phi0();
  if ( dphi0 > TMath::Pi() *   1.5 ) dphi0 -= TMath::Pi() * 2.;
  if ( dphi0 < TMath::Pi() * (-1.5)) dphi0 += TMath::Pi() * 2.;

  double alpha = cnt->get_alpha();
  double phi   = cnt->get_phi() + dphi0;
  /// the second terms of alpha and phi are for 5-deg rotated fake tracks.
  double new_alpha = alpha + cax * sin(phi) / 220. - cay * cos(phi) / 220.;
  double new_pt    = pt * fabs(alpha / new_alpha);
  double new_phi0  = phi0 + 2.0195 * (new_alpha - alpha);

/*
  cout<<"FitRecal : "<<new_pt<<" "<< new_phi0<<" "<< the0<<" : "<<cax<<" "<< cay<<" "<<helicity<<endl;
  cout<<"cluster: "<<endl;
  for ( unsigned int i=0; i<vhit.size(); i++ ) {
    cout<<"   "<<i<<" "<<vhit[i]->get_layer()<<" ";
    cout<<               vhit[i]->get_ladder()<<" ";
    cout<<"("<<          vhit[i]->get_xyz_global(0)<<" ";
    cout<<               vhit[i]->get_xyz_global(1)<<" ";
    cout<<               vhit[i]->get_xyz_global(2)<<") ";
    cout<<"("<<          vhit[i]->get_xyz_local(0)<<" ";
    cout<<               vhit[i]->get_xyz_local(1)<<" ";
    cout<<               vhit[i]->get_xyz_local(2)<<") ";
    cout<<endl;
  }
*/
  // 
  // do multi-circle fitting
  // 
  int ndf;
  double chisq;
  double chi_phi, chi_z, chi_angle;
  m_tracker.TrackFit_SvxCNT(vhit, helicity, new_pt, new_phi0, the0,
                            cax, cay, chisq, ndf,
                            chi_phi, chi_z, chi_angle);

  if(verbosity>2 && m_tracker_out!=NULL){
    int ndf_out;
    double chisq_out;
    double chi_phi_out, chi_z_out, chi_angle_out;
    m_tracker_out->TrackFit_SvxCNT(vhit, helicity, new_pt, new_phi0, the0,
                              cax, cay, chisq_out, ndf_out,
                              chi_phi_out, chi_z_out, chi_angle_out);

    cout<<"fitting : "<<chisq<<" "<<chisq_out<<endl;
  
  }

  /// calculate DCA again
  double px = m_tracker.get3Mom(0);
  double py = m_tracker.get3Mom(1);

  //this gets the expeced position of the first cluster [0] ?? 
  float xexp, yexp, zexp;
  m_tracker.getExpectedPosition(vhit[0]->get_layer(), xexp, yexp, zexp, 0);
  
  double pos_primary[3];
  double new_pt2 = sqrt(px*px+py*py);
  double dca2d;
  calcDCAbyCircleProjection(new_pt2, atan2(py, px), the0, charge,
			    xexp, yexp, zexp,
			    vx, vy,
			    &(pos_primary[0]), &(pos_primary[1]), &(pos_primary[2]),
			    &dca2d);
  float dcaz = pos_primary[2] - vz;


  ///
  /// update
  ///
  svxcnt->setChiSquareNoCNT(chisq);
  svxcnt->setNDFNoCNT(ndf);
  svxcnt->setDCA2DNoCNT(dca2d);
  svxcnt->setDCAZNoCNT(dcaz);

  for ( unsigned int i=0; i<vhit.size(); i++ ) {
    delete vhit[i];
  }

  //----------------------------------
  if(verbosity>2){
    double chi2_diffr  = fabs(svxcnt->getChiSquare() - svxcnt->getChiSquareNoCNT())/svxcnt->getChiSquare();

    cout<<"chi2_cmp : "<< svxcnt->getChiSquare() << " " << svxcnt->getChiSquareNoCNT()<<", ";
    cout<<": "     << svxcnt->getNDOF()      << " " << svxcnt->getNDOFNoCNT()<<", ";
    cout<<": "     << svxcnt->getDCA2D()     << " " << svxcnt->getDCA2DNoCNT()<<" ";
    cout<<": "<< (chi2_diffr < 0.01?"OK":"Large");
    cout<<": "<< (chi2_diffr > 20 ? "TOO LARGE":"");
    cout<<endl;
  }

}

//_______________________________________________________________________________

void SvxCentralTrackFitRecal::calcDCAbyCircleProjection
(
 double pt, double phi, double the,  // pt in xy-plane, phi, theta at inner most layer
 int charge,                      // charge of the track
 double hx, double hy, double hz,    // hit position at inner most layer
 double vx, double vy,              // primary vertex
 double* dx, double* dy, double* dz, // dca position
 double* d2dca)                    // return
{
  // vector to rotation center at hit1 
  double R  = pt/b;
  double pz = pt/tan(the);

  double b_sign = (m_fieldScale>0) ? -1.0 : 1.0;
  double dir = ( (b_sign)*charge>0. ) ? -1.0 : 1.0;
  double cx = hx - dir*R*sin(phi);
  double cy = hy + dir*R*cos(phi);

  // L is a distance btw the rotation center and primary vtx  
  // psi is a angle of the vector starting from the rotation center to primary vertex
  double L = sqrt((vx-cx)*(vx-cx) + (vy-cy)*(vy-cy));
  double psi = atan2((vy-cy), (vx-cx));

  // DCA point
  *dx = cx + R*cos(psi);
  *dy = cy + R*sin(psi);

  double dphi = atan2((hy-cy),(hx-cx)) - psi;
  if ( dphi>M_PI*1.5 ) dphi -= M_PI*2;
  else if ( dphi<-M_PI*1.5 ) dphi += M_PI*2;
  double dzdphi = pz*R/pt;
  *dz = hz - fabs(dphi)*dzdphi;

  // DCA value
  *d2dca = b_sign*(charge*(R - L));
}

//_______________________________________________________________________________

void SvxCentralTrackFitRecal::CircleProjection
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


void SvxCentralTrackFitRecal::shiftPHCentralTrack(float *phi0, float *the0)
{
  //copy from SvxCentralTrackReco
  //TODO
  //  1) use some sort of DB structure for offsets
 
  //get arm East=0, West=1
  // D. McGlinchey 12/23/2014 - Could use PHCentralTrack dcarm instead.
  int arm = *phi0 < (float)1.5 ? 1 : 0;

  //rotate phi0
  *phi0 = *phi0 + m_rotPHcntPhi[arm];

  //rotate the0
  *the0 = *the0 + m_rotPHcntTheta[arm];

  return;

}

void SvxCentralTrackFitRecal::setPHCentralTrackDphiDtheta(int arm, float dphi, float dtheta)
{
  if (arm >= 0 && arm < 2)
    {
        m_rotPHcntPhi[arm] = dphi;
        m_rotPHcntTheta[arm] = dtheta;

        cout << PHWHERE << " - set PHCentralTrack rotations for arm " << arm
                        << " to dphi=" << dphi << " dtheta=" << dtheta << endl;
    }
    else
    {
        cout << PHWHERE << " - arm must be 0 (East) or 1 (West)! arm=" << arm << endl;
    }
}
    
 
bool SvxCentralTrackFitRecal::fetchDBPHCentralTrackDphiDtheta(const int runnumber)
{
  //is fetch based off run number or time stamp?
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Trun = *ts;
  delete ts;
  
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if (!application->startRead())
  {
    cerr << PHWHERE << " DB not readable."
         << " Setting PHCentralTrack rotations to default values (0)." << endl;
    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);
    
    application->abort();
    return false;
  }
  
  PdbBankID bankID;
  bankID.setInternalValue(0);
  PdbParameter *parameter;
  int index = 0;
  
  PdbCalBank *phcntrotBank = bankManager->fetchBank(
                "PdbParameterBank",
                bankID,
                m_phcntrotdbname,
                Trun);
  if (!phcntrotBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
         << runnumber << " from " << m_phcntrotdbname << endl;
    cout << "    Setting PHCentralTrack rotations to default values (0)." << endl;

    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);
 
    return false;
  }
  
  int length = 2 * 2 + 1; // 2 arms * 2 rotations  + 1 header value
  
  int truelength = phcntrotBank->getLength();
  if (length != truelength)
  {
    cout << PHWHERE;
    cout << " FATAL...wrong length DB read for PHCnt Rotations t: " << truelength << endl;
    cout << "                                    expected length: " << length << endl;
    cout << "    Setting PHCentralTrack rotations to default values (0)." << endl;
    
    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);
    
    return false;
  }
  
  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
  {
    cout << PHWHERE
         << " FATAL...unknown scheme DB read for PHCnt rotations:"
         << scheme << endl;
    cout << "    Setting PHCentralTrack rotations to default values (0)." << endl;
    
    setPHCentralTrackDphiDtheta(0, 0, 0);
    setPHCentralTrackDphiDtheta(1, 0, 0);
    
    return false;
  }
  
  // OK, ALL Checks passed...get the parameters...
  
  if (verbosity)
  {
    cout << PHWHERE
         << "READING PHCentralTrack rotations from DB ..."
         << endl;
  }
  
  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dphiE = parameter->getParameter();
  
  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dphiW = parameter->getParameter();
  
  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dtheE = parameter->getParameter();

  parameter = (PdbParameter *) & phcntrotBank->getEntry(index++);
  float dtheW = parameter->getParameter();
  
  if (verbosity)
  {
    cout << "    dphi   : " << dphiE << " " << dphiW << endl;
    cout << "    dtheta : " << dtheE << " " << dtheW << endl;
  }
  
  delete phcntrotBank;
  
  //set the values
  setPHCentralTrackDphiDtheta(0, dphiE, dtheE);
  setPHCentralTrackDphiDtheta(1, dphiW, dtheW);
  
  return true;

}
  
