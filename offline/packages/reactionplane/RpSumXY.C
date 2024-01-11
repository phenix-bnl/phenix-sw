
#include <iostream>

#include "getClass.h"
#include "RunNumberRanges.h"
#include "RunToTime.hh"

#include "BbcCalib.hh"
#include "BbcGeo.hh"
#include "BbcRaw.h"
#include "SmdOut.h"
#include "MvdGeometry.hh"
#include "MvdRPhiZOut.h"
#include "FclIndexer.h"
#include "FclOut.h"
#include "TRxnpScint.h"
#include "TRxnpScintMap.h"
#include "RXNP.h"
#include "mRxnpCalXang.h"
#include "TRxnpRawXangMap.h"
#include "MpcMap.h"
#include "mpcTowerContainer.h"
#include "mpcTowerContent.h"
#include "PHCentralTrack.h"

#include "RpSumXY.h"

static BbcCalib* bbccalib = 0;
static BbcGeo* bbcgeo = 0;
static MvcGeometry* mvdgeo = 0;
// ofstream mpcfile;

using namespace std;
using namespace RP;

RpSumXY::RpSumXY()
{
  verbosity = 0;

  // BBC
  bbccalib = new BbcCalib();
  bbcgeo = new BbcGeo();

  // MVD
  MvdParameter par;
  par.Init();
  MvdGeoParameter geopar;
  geopar.Init();

  mvdgeo = new MvcGeometry(par, geopar);
  mvdgeo->Init();

  ResetEvent();
}

RpSumXY::~RpSumXY()
{
  delete bbccalib;
  delete bbcgeo;
  delete mvdgeo;
}

int RpSumXY::EndRun()
{
//cout << "closing mpcfile.dat " << endl;
//mpcfile.close();
  cout << "RpSumXY::EndRun, analyzed run/events :" << _runNumber << " " << _anaEvent << endl;
  return 0;
}

int RpSumXY::InitRun(const int runNumber)
{

  RunToTime* runTime = RunToTime::instance();
  
  PHTimeStamp* ts( runTime->getBeginTime(runNumber) );
  PHTimeStamp tstart = *ts;
  delete ts;

  int bbccalib_version = 3006;
  if( runNumber > BEGIN_OF_RUN5 ){
    bbccalib_version = 4002; // RUN5 BFIELD ON
  }

  if(verbosity>0){
    cout << PHWHERE << " RpSumXY::InitRun, runNumber= " << runNumber
         << " Version of BbcCalib = " << bbccalib_version << endl;
  }

  bbccalib->restore(tstart, bbccalib_version);
//cout << "opening mpcfile.dat" << endl;
//mpcfile.open("mpcfile.dat");
  _runNumber=runNumber;
  _anaEvent=0;
  cout << "RpSumXY::initRun, analyzed run/events :" << _runNumber << " " << _anaEvent << endl;

  return 0;
}

int RpSumXY::ResetEvent()
{
  memset(Qx,0,sizeof(Qx));
  memset(Qy,0,sizeof(Qy));
  memset(Qw,0,sizeof(Qw));

  memset(Qx2,0,sizeof(Qx2));
  memset(Qy2,0,sizeof(Qw2));
  memset(Qw2,0,sizeof(Qw2));

//################################################//

  return 0;
}

void RpSumXY::fillFlowVectorAll(PHCompositeNode* topNode)
{
  // Reset Flow Vector
  ResetEvent();

  // BBC
  fillFlowVectorBbc(topNode);

  // SMD
  fillFlowVectorSmd(topNode);

  // FCL
  fillFlowVectorFcl(topNode);

  // CNT
  fillFlowVectorCnt(topNode);

  if( _runNumber > BEGIN_OF_RUN7 ){
    //RXN
    fillFlowVectorRxn(topNode);

    //MPC
    fillFlowVectorMpc(topNode);
  }
  // Normalize
  Normalization();
  _anaEvent++;
}

void RpSumXY::fillFlowVectorBbc(PHCompositeNode* topNode)
{
  BbcRaw* bbcraw = findNode::getClass<BbcRaw>(topNode, "BbcRaw");
  if(!bbcraw) return;

  for (int ipmt = 0; ipmt < BBC_N_PMT; ipmt++) {
    short adc = bbcraw->get_Adc(ipmt);
    short tdc = bbcraw->get_Tdc0(ipmt);
    float time0 = bbccalib->getHitTime0(ipmt, tdc, adc);
    float charge = bbccalib->getCharge(ipmt, adc);
    if (time0 > 0.0 && charge > 0.0) {
      //------------------------------------------------
      //         South(<64)  North   South+North
      // sign      +1          -1
      // idet       0           1         2
      // v1         +           -
      // v2         +           +
      //------------------------------------------------

      int ibbc = (ipmt < 64) ? 0 : 1;
      float sign = (ipmt < 64) ? 1.0 : -1.0;
      float phi = atan2(bbcgeo->getY(ipmt), bbcgeo->getX(ipmt));

      fillFlowVector(ibbc, phi, charge, 1.0);  // SOUTH and NORTH
      fillFlowVector(2, phi, charge, sign); // SOUTH  +  NORTH
    }
  }

}


void RpSumXY::fillFlowVectorSmd(PHCompositeNode* topNode)
{
  SmdOut* smdout = findNode::getClass<SmdOut>(topNode, "SmdOut");

  if ( !smdout ) {
    return ;
  }

  for (int ns = 0;ns < 2;ns++) {
    if (fabs(smdout->get_Xpos(ns)) < 9999 && fabs(smdout->get_Ypos(ns)) < 9999) {
      // SMD south or north
      Qx[ns + 3][0] = smdout->get_Xpos(ns);
      Qy[ns + 3][0] = smdout->get_Ypos(ns);
      Qw[ns + 3][0] = 1.0;
    }
    else {
      Qx[ns + 3][0] = -9999.;
      Qy[ns + 3][0] = -9999.;
      Qw[ns + 3][0] = 0.0;
    }
  }

  // SMD south + north
  if (Qx[3][0] > -9999 && Qy[3][0] > -9999 && Qx[4][0] > -9999 && Qy[4][0] > -9999) {
    Qx[5][0] = Qx[3][0] - Qx[4][0];
    Qy[5][0] = Qy[3][0] - Qy[4][0];
    Qw[5][0] = 1.0;
  }
  else {
    Qx[5][0] = -9999.;
    Qy[5][0] = -9999.;
    Qw[5][0] = 0.0;
  }

  // no elliptic event plane for SMD
  for (int idet = 3;idet < 6;idet++) {
    Qx[idet][1] = -9999.;
    Qy[idet][1] = -9999.;
    Qw[idet][1] = 0.0;
  }

}

void RpSumXY::fillFlowVectorMvd(PHCompositeNode* topNode)
{
  MvdRPhiZOut* mvdrphiz = findNode::getClass<MvdRPhiZOut>(topNode, "MvdRPhiZOut");

  if( !mvdrphiz ) {
    return;
  }

  for(unsigned int ihit=0;ihit<mvdrphiz->get_MvdNRPhiZ();ihit++){
    int zid   = mvdrphiz->get_z(ihit);
//    int rid   = mvdrphiz->get_r(ihit);
    int phiid = mvdrphiz->get_phi(ihit);

//    float adc = (float)mvdrphiz->get_adc(ihit);
//    float r   = mvdgeo->Radius( rid );
    float phi = mvdgeo->Phi(phiid/12, phiid%12);
    float z   = mvdgeo->Z( zid );
   
    int imvd   = (z<0.0) ? 0 : 1;
    float sign = (z<0.0) ? 1.0 : -1.0;
    float weight = 1.0;
    fillFlowVector(imvd+6, phi, weight, 1.0); // SOUTH or NORTH
    fillFlowVector(8, phi, weight, sign);   // SOUTH + NORTH
  }

}

void RpSumXY::fillFlowVectorFcl(PHCompositeNode* topNode)
{
  FclOut* fclout[2];
  fclout[0] = findNode::getClass<FclOut>(topNode, "fclOutSouth");
  fclout[1] = findNode::getClass<FclOut>(topNode, "fclOutNorth");

  if(!fclout[0] || !fclout[1]) {
    return;
  }

//  const float col_center = 5.5; 
//  const float row_center = 5.5;
  const float col_center = 4.5; 
  const float row_center = 5.0;
  const float col_max = 4.5;
  const float row_max = 5.0;

  int ihar = 0;
  for(int iside=0;iside<2;iside++){
    float sign = (iside==0) ? 1.0 : -1.0;
    for(int ich=0;ich<144;ich++){
      int row = FclIndexer::Instance()->getRow(iside, ich);
      int col = FclIndexer::Instance()->getColumn(iside, ich);
      float adc = fclout[iside]->getLowGain(row, col);
      if(row!=-1 && col!=-1 && adc>0){
        float xfcl = -(col - col_center)/col_max;
        float yfcl = (row - row_center)/row_max;

	fillFlowVector(iside+9, ihar, xfcl, yfcl, adc, 1.0);
	fillFlowVector(11, ihar, xfcl, yfcl, adc, sign);
      }
    }
  }

}

void RpSumXY::fillFlowVectorCnt(PHCompositeNode* topNode)
{
  PHCentralTrack* central = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  if( !central ) {
    return;
  }

  for(unsigned int itrk=0;itrk<central->get_npart();itrk++){
    short quality  = central->get_quality(itrk);
    float pc3dphi = central->get_pc3dphi(itrk);
    float pc3sdphi = pc3dphi/0.001; // around 1 sigma for dphi
    float pc3dz   = central->get_pc3dz(itrk);
    float pc3sdz = pc3dz/0.7; // around 1 sigma for dz 
    float mom      = central->get_mom(itrk);
    float the0     = central->get_the0(itrk);
    float zed      = central->get_zed(itrk);
    float phi0     = central->get_phi0(itrk);

    // Good track selection
    if((quality==31||quality==63)
      && sqrt(pc3sdphi*pc3sdphi+pc3sdz*pc3sdz)<3.0
      && the0>-100
      && fabs(zed)<100
      && phi0>-100
      && mom!=0.0 && mom<20.0){

      float phi1 = atan2(sin(phi0),cos(phi0));
      float pt   = mom * sin(the0);
      float eta  = -log(tan(the0/2.0));
      float ieta = (eta<0.0) ? 1.0 : -1.0;
  
      // sub event definition from zed
      int ized = (int)( (zed+80.0)/160.0 * 20 );
      int icnt = ized % 4;
      if(icnt<0) icnt = 0;
      if(icnt>3) icnt = 3;

      fillFlowVector(icnt+12, phi1, pt, ieta);
      fillFlowVector(16, phi1, pt, ieta);
    }
  }

}


void RpSumXY::fillFlowVectorRxn(PHCompositeNode* topNode)
{
  TRxnpScintMap* _scint_map;

  try{
  _scint_map = TMutNode<TRxnpScintMap>::find_node(topNode, "TRxnpScintMap");
  }

  catch ( exception& e)
    {
      //      cout << e.what() << endl;
      return;
    }

//  TRxnpScint* scint = findNode::getClass<TRxnpScint>(topNode, "TRxnpScint");

// if( !scint ){
//   return;
// }
 int count[2][2];
 for(unsigned short iarm = 0; iarm < RXNP::NARM; iarm++)
   {

      for(unsigned short iring = 0; iring < RXNP::NRING; iring++)
        {
          count[iarm][iring]=0;
          TRxnpScintMap::const_iterator scint_iter = _scint_map->get(iarm,iring);
          while(TRxnpScintMap::const_pointer scint_ptr = scint_iter.next()) {
          count[iarm][iring]++;
          float sign = NAN;
          int irxn1 =0;
          int irxn2 =0;
          int irxn3 =0;
          int irxn4 =8;

          if(iarm==0 && iring==0){
             irxn1 = 0;
             irxn2 = 2;
             irxn3 = 6;
             sign = 1.0;
          }
          else if(iarm==0 && iring==1){
             irxn1 = 1;
             irxn2 = 2;
             irxn3 = 7;
             sign = 1.0;
          }
          else if(iarm==1 && iring==0){
             irxn1 = 3;
             irxn2 = 5;
             irxn3 = 6;
             sign = -1.0;
          }
          else if(iarm==1 && iring==1){
             irxn1 = 4;
             irxn2 = 5;
             irxn3 = 7;
             sign = -1.0;
          }
	  else
	    {
	      cout << PHWHERE << "Invalid arm/ring: arm " << iarm 
                   << ", ring " << iring << endl;
	      exit(1);
	    }

          // calculate the coordinates first
          //
          float rxnp_phi = scint_ptr->get()->get_phi();
          float rxnp_low = scint_ptr->get()->get_low_e();
          float rxnp_tim = scint_ptr->get()->get_tof();
          if (rxnp_tim<500 || rxnp_tim>1800 || rxnp_low<0) rxnp_low=0;

          fillFlowVector(irxn1+17, rxnp_phi, rxnp_low, 1.0);
          fillFlowVector(irxn2+17, rxnp_phi, rxnp_low, 1.0);
          fillFlowVector(irxn3+17, rxnp_phi, rxnp_low, sign);
          fillFlowVector(irxn4+17, rxnp_phi, rxnp_low, sign);

          }
        }
   }
   if (count[0][0]==12 && count[0][1]==12 && count[1][0]==12 && count[1][1]==12) {
   } else {
     cout << "rxn hit " << count[0][0] << " " << count[0][1] << " "
                        << count[1][0] << " " << count[1][1] << endl;
   }
}

void RpSumXY::fillFlowVectorMpc(PHCompositeNode* topNode)
{

  // Don't forget to set the MPC_DATABASE
  static MpcMap *mpcmap = 0;
  if ( mpcmap==0 ) mpcmap = MpcMap::instance();

  mpcTowerContainer *mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctower)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return;
    }
  int ntowers = mpctower->size();
// if (_anaEvent%10==0) mpcfile << ntowers << endl;
   for (int itower=0; itower<ntowers; itower++){

      mpcTowerContent* tower = mpctower->getTower(itower);

      int fee_ch = tower->get_ch();
      float mpc_e = tower->get_energy();        // get energy in 1 tower
      float mpc_tof = tower->get_tof();         // get tof of hit in tower

      float mpc_x = mpcmap->getX(fee_ch);       // get x position of tower
      float mpc_y = mpcmap->getY(fee_ch);       // get y position of tower
      float mpc_z = mpcmap->getZ(fee_ch);       // z>0 (north), z<0 (south)
      float mpc_phi = atan2(mpc_y,mpc_x);
//    if (_anaEvent%10==0) {
//      mpcfile << itower << " " << xpos << " " << ypos << " "
//              << mpc_x << " " << mpc_y << " " << mpc_z << " "
//              << fee_ch << " " << mpc_e << " " << mpc_tof << endl;
//    }
      if ( mpc_e<0 || mpc_tof<500 || mpc_tof>2200) continue;
      if ( fabs(mpc_z) <1. ) continue;          // skip non-existent channels
      float sign = -1.0;
      int iarm = 1;
      if(mpc_z < 0){
        sign = 1.0;
        iarm = 0;
      }


      fillFlowVector(26+iarm, mpc_phi,mpc_e, 1.0);
      fillFlowVector(28,      mpc_phi,mpc_e,sign);
   }
}

void RpSumXY::fillFlowVector(const int idet, const float phi,
                             const float weight, const float sign)
{
  // fill Flow Vector for each event plane
  // idet : the detector id
  // phi  : azimuthal angle of particle or PMT.
  // weight : appropriate weight (pT, multiplicity etc)

  if(idet<NDET){
    for (int ihar = 0;ihar < NHAR;ihar++) {
      float w = weight;
      if (ihar % 2 == 0) w *= sign;
 
      Qx[idet][ihar] += w * cos( (ihar + 1.0) * phi );
      Qy[idet][ihar] += w * sin( (ihar + 1.0) * phi );
      Qw[idet][ihar] += weight;
    }
  }else{
    for (int ihar = 0;ihar < NHAR2;ihar++) {
      float w = weight;
      if (ihar % 2 == 0) w *= sign;

      Qx2[idet-NDET][ihar] += w * cos( (ihar + 1.0) * phi );
      Qy2[idet-NDET][ihar] += w * sin( (ihar + 1.0) * phi );
      Qw2[idet-NDET][ihar] += weight;
    }
  }
}

void RpSumXY::fillFlowVector(const int idet, const int ihar,
                             const float x, const float y,
                             const float weight, const float sign)
{

  float w = weight;
  if (ihar % 2 == 0) w *= sign;

  if(idet<NDET){
    Qx[idet][ihar] += w * x;
    Qy[idet][ihar] += w * y;
    Qw[idet][ihar] += weight;
  }else{
    Qx2[idet-NDET][ihar] += w * x;
    Qy2[idet-NDET][ihar] += w * y;
    Qw2[idet-NDET][ihar] += weight;
  }
}

void RpSumXY::Normalization()
{

  for(int idet=0;idet<NDET;idet++){
    for(int ihar=0;ihar<NHAR;ihar++){

      if( Qw[idet][ihar] != 0.0 ){
        Qx[idet][ihar] /= Qw[idet][ihar];
        Qy[idet][ihar] /= Qw[idet][ihar];
      }
      else{
        Qx[idet][ihar] = -9999.;
        Qy[idet][ihar] = -9999.;
        Qw[idet][ihar] = 0.0;
      }

    }
  }
  for(int idet=NDET;idet<NDET2;idet++){
    for(int ihar=0;ihar<NHAR2;ihar++){

      if( Qw2[idet-NDET][ihar] != 0.0 ){
        Qx2[idet-NDET][ihar] /= Qw2[idet-NDET][ihar];
        Qy2[idet-NDET][ihar] /= Qw2[idet-NDET][ihar];
      }
      else{
        Qx2[idet-NDET][ihar] = -9999.;
        Qy2[idet-NDET][ihar] = -9999.;
        Qw2[idet-NDET][ihar] = 0.0;
      }

    }
  }
  
}

void RpSumXY::set_Qx(const int idet, const int ihar, const float val)
{
  if(idet<NDET){
   if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     Qx[idet][ihar] = -9999.;
   }

   Qx[idet][ihar] = val;
  }else{
   if(idet<NDET || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
       exit(1);

     Qx2[idet-NDET][ihar] = -9999.;
   }

   Qx2[idet-NDET][ihar] = val;
  }
}

void RpSumXY::set_Qy(const int idet, const int ihar, const float val)
{
  if(idet<NDET){
   if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     Qy[idet][ihar] = -9999.;
   }

   Qy[idet][ihar] = val;

  }else{
   if(idet<NDET || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
       exit(1);

     Qy2[idet-NDET][ihar] = -9999.;
   }

   Qy2[idet-NDET][ihar] = val;
  }
}

void RpSumXY::set_Qw(const int idet, const int ihar, const float val)
{
  if(idet<NDET){
   if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     Qw[idet][ihar] = -9999.;
   }

   Qw[idet][ihar] = val;
  }else{
   if(idet<NDET || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
       exit(1);
     Qw2[idet-NDET][ihar] = -9999.;
   }

   Qw2[idet-NDET][ihar] = val;
  }
}

float RpSumXY::get_Qx(const int idet, const int ihar) const
{
  if(idet<NDET){
   if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     return -9999;
   }

   return Qx[idet][ihar];
  }else{
   if(idet<NDET || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     return -9999;
   }

   return Qx2[idet-NDET][ihar];
  }
}

float RpSumXY::get_Qy(const int idet, const int ihar) const
{
  if(idet<NDET){
   if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     return -9999;
   }

   return Qy[idet][ihar];
  }else{
   if(idet<NDET || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     return -9999;
   }

   return Qy2[idet-NDET][ihar];
 }
}

float RpSumXY::get_Qw(const int idet, const int ihar) const
{
  if(idet<NDET){
   if(idet<0 || idet>NDET-1 || ihar<0 || ihar>NHAR-1){
     if(verbosity>0){
       cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
     }

     return -9999;
   }

   return Qw[idet][ihar];
  }else{
   if(idet<NDET || idet>NDET2-1 || ihar<0 || ihar>NHAR2-1){
    if(verbosity>0){
      cout << PHWHERE << " Invalid (idet,ihar)=(" << idet << "," << ihar << ")" << endl;
    }

    return -9999;
   }

   return Qw2[idet-NDET][ihar];
  }
}
