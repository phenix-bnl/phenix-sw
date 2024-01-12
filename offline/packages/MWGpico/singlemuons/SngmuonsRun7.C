// $Id: SngmuonsRun7.C,v 1.3 2014/12/09 06:28:10 rseidl Exp $

/*!
   \file SngmuonsRun7.C
   \brief single muon ntuple booking and feeling
   \version $Revision: 1.3 $
   \date $Date: 2014/12/09 06:28:10 $
*/

#include <iostream>
#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <ReactionPlaneObject.h> 
#include <RpSumXYObject.h>

#include <MWGVertex.h>
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <SpinDataEventOut.h>
#include <stdexcept>
#include <string>
#include <TChain.h>
#include <TH1.h>

#include <TMutTrkMap.h>
#include <TMutMCTrkMap.h>
#include <TMutMCHitMap.h>
#include <TMuiRoadMapO.h>
#include <TMCPrimaryMap.h>

#include <root_ptrk.h>
#include <TNtuple.h>
#include <TriggerHelper.h>
#include <utiCentrality.h>
#include <vector>

#include <MUTOO.h>
#include "../MWGpico.h"
#include <Tools.h>
#include <MWGConsts.h>
#include <MWGVersion.h>

#include "TrigLvl1v1.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"


using namespace std;
using namespace PhUtilities;

//__________________________________________________
void MWGpico::BookSngmuonsNtupleRun7(TNtuple*& sngmuons, TString m_name, TString m_title)
{

  // define variable list for single muons ntuple
  /*
    IMPORTANT NOTE:
    here we write only 10 variables/line to make counting easier when filling the ntuple
    please follow this policy in later modifications to avoid stupid bugs
  */
  const char* sngvarlist=0;
  
  sngvarlist=
    "Run_Number:Evt_Number:Evt_Nmu:BbcZVertex:bbcCentrality:BBCQN:BBCQS:BBCNN:BBCNS:charge:"
    "px:py:pz:pxSt1:pySt1:pzSt1:pxSt3:pySt3:pzSt3:pT:"
    "p:pSt1:ELoss:chi2:idhits:idquad:trhits:DS3:DS3ctp:idchi2:"
    "gap0x:gap0y:gap0z:dxdz:dydz:trstat:ghost:lastGap:eta:phi:"
    "DG0:DDG0:DS0:mutr_nhits:muid_nhits:xSt1:ySt1:zSt1:xSt2:ySt2:"
    "zSt2:xSt3:ySt3:zSt3:ref_vtx_rdca:ref_vtx_r:ref_vtx_z:refit_zvtx:dAngle:dAngle_xyz:"
    "dTheta:dPhi:road_slope:RPbbcrp10:RPbbcrp11:RPbbcrp12:RPmpcrp10:RPmpcrp11:RPmpcrp12:RPrxnrp10:"
    "RPrxnrp11:RPrxnrp12:v2_S:v2_N:v2_both";
 
  sngmuons = new TNtuple( m_name, m_title, sngvarlist );
  sngmuons->SetAutoSave(160000000);
  
  return;
}
//__________________________________________________________________
void MWGpico::BookSngmuonsEvtNtupleRun7(TNtuple*& sngvtx, TString v_name, TString v_title )
{
  // define variable list for vertex ntuple
  const char* vtxvarlist=0;

  vtxvarlist= "Run_Number:BbcZVertex:bbcCentrality:BBCQN:BBCQS:BBCNN:BBCNS:RPbbcrp10:RPbbcrp11:RPbbcrp12:"
	      "RPmpcrp10:RPmpcrp11:RPmpcrp12:RPrxnrp10:RPrxnrp11:RPrxnrp12";
  sngvtx = new TNtuple( v_name, v_title, vtxvarlist );
  sngvtx->SetAutoSave(160000000);

  return;
}
//__________________________________________________________________
int MWGpico::FillSngmuonsNtpRun7(PHMuoTracksOut* &muo, TNtuple* sngmuons,  TNtuple* sngvtx)
{
  //=== event information
  Float_t bbcCentrality=-9999;

  // R.P variables
  double RPbbcrp10 = -9999;
  double RPbbcrp11 = -9999;
  double RPbbcrp12 = -9999;
  double RPmpcrp10 = -9999;
  double RPmpcrp11 = -9999;
  double RPmpcrp12 = -9999;
  double RPrxnrp10 = -9999;
  double RPrxnrp11 = -9999;
  double RPrxnrp12 = -9999;

  // z_vertex
  float BbcZVertex( -9999 );
  if( _choice == "simu" || _choice == "simu_file" )
  {
    bool error( false );
    BbcZVertex = Tools::zVertexMC( header, error );
    if( error ) BbcZVertex = muo->get_zpos(0,0);
  } else if(evt) BbcZVertex = evt->getBbcZVertex();

  // event/run number
  int RunNumber = ( run_header ) ?  run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;
  
  // try load MC if failed from PHGlobal
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber  = Tools::eventNumberMC( header, error );
  }
  
  if (evt) {
    if (_type=="AuAu") {
      bbcCentrality = (Float_t) evt->getCentrality();
    }
    if (rp) {  // Load R.P variables
      RPbbcrp10 = rp->getBBCrp10();
      RPbbcrp11 = rp->getBBCrp11();
      RPbbcrp12 = rp->getBBCrp12();
      RPmpcrp10 = rp->getMPCrp10();
      RPmpcrp11 = rp->getMPCrp11();
      RPmpcrp12 = rp->getMPCrp12();
      RPrxnrp10 = rp->getRXNrp12();
      RPrxnrp11 = rp->getRXNrp15();
      RPrxnrp12 = rp->getRXNrp18();
    }
  } // evt
  
  float  BBCQN = evt->getBbcChargeN();
  float  BBCQS = evt->getBbcChargeS();
  float  BBCNN = evt->getBbcMultN();
  float  BBCNS = evt->getBbcMultS();

  Float_t Evt_Data[16];
  Evt_Data[0] = RunNumber;
  Evt_Data[1] = BbcZVertex;
  Evt_Data[2] = bbcCentrality;
  Evt_Data[3] = BBCQN;
  Evt_Data[4] = BBCQS;
  Evt_Data[5] = BBCNN;
  Evt_Data[6] = BBCNS;
  Evt_Data[7] = RPbbcrp10;                      // R.P v2
  Evt_Data[8] = RPbbcrp11;
  Evt_Data[9] = RPbbcrp12;

  Evt_Data[10] = RPmpcrp10;                      // R.P v2
  Evt_Data[11] = RPmpcrp11;
  Evt_Data[12] = RPmpcrp12;
  Evt_Data[13] = RPrxnrp10;
  Evt_Data[14] = RPrxnrp11;
  Evt_Data[15] = RPrxnrp12;

  sngvtx->Fill(Evt_Data);
  //_____________ end of sngvtx _____________

  // muon variables
  Float_t dS30=999, dS3ctp0=999, muIDchis0=999, DG0=999, DDG0=999, DS0=999;
  Float_t pseudo_rapidity = 999;
  Int_t muIDquad0=-1, lastGap=0;

  if (muo) { // check PHMuoTracks or PHMuoTracksOO exist

    int npart = muo->get_npart();
    totMU += npart;
    
    for (int ipart=0; ipart<npart; ipart++) 
    {
      
      // get best road
      int iroad = Cuts().get_best_road_oo( ipart, muo );
      
      //=== First track variables
      muIDquad0 = (muo->get_muIDOO_gap0( 0, iroad, ipart )>0) + 2*(muo->get_muIDOO_gap0( 1, iroad, ipart )<0);
      muIDchis0 = muo->get_muIDOOchi(iroad,ipart);
      //      muTRhits0 = muo->get_muTRhits(ipart);

      dS30 = Tools::DS3(muo,ipart, iroad);
      DG0 = Tools::DG0(muo,ipart, iroad);
      DDG0 = Tools::DDG0(muo,ipart, iroad);
      dS3ctp0 = Tools::DS3ctp(muo,ipart, iroad );
      DS0 = Tools::DS0(muo,ipart, iroad );

      for(int igap=4; igap>0; igap--)
      {
        if (muo->is_muIDOOhit( iroad, ipart, igap, 0) || muo->is_muIDOOhit( iroad, ipart, igap, 1 )) 
        {
          lastGap = igap;
          break;
        }
      }

      // fit single track together with BBC vertex
      double px_vertex( muo->get_px(0,ipart) );
      double py_vertex( muo->get_py(0,ipart) );
      double pz_vertex( muo->get_pz(0,ipart) );
      
      bool do_refit( true );
      
      MWGVertex vertex;
      if (_framework == MUTOO && do_refit )	try {
	  
        vertex.set_verbosity( 0 );
        
        // add track
        vertex.add_track( ipart, muo );
        
        // add vertex information
        double z_vertex( 0 );
        double z_vertex_error( 0 );
        
        if( _choice == "simu" || _choice == "simu_file" ) {

          // retrieve vertex from pisa header file or first track
          if( header ) {
            bool error( true );
            z_vertex = Tools::zVertexMC( header, error );
            if( error ) z_vertex = muo->get_zpos(0,0);
          } else z_vertex = muo->get_zpos(0,0);
          // vertex error (from MC) is hard coded to 2cm
          z_vertex_error = 2;
          
        } else if( evt ) {
          
          // retrieve vertex from BBC
          z_vertex = evt->getBbcZVertex();
          /*
          up to now, found no way to retrieve bbc vertex error
          from PHGlobal. Assign a 2cm error
          */
          z_vertex_error = 2;
        }
        
        vertex.add_vertex( z_vertex, z_vertex_error );
        
        // refit muon tracks with the correct event vtx (BBC_vtx for real one and PISA_vtx for sim ) fit
        vertex.fit();
        
        // retrieve track momentum
        px_vertex = vertex.get_px( 0 );
        py_vertex = vertex.get_py( 0 );
        pz_vertex = vertex.get_pz( 0 );
        
      }
      catch( std::exception &e ) {
        cout << e.what() << endl;
      }
      
      float gap0x = muo->get_muIDOO_gap0(0, iroad, ipart);
      float gap0y = muo->get_muIDOO_gap0(1, iroad, ipart);
      float gap0z = muo->get_muIDOO_gap0(2, iroad,ipart);
      float dirX  = muo->get_muIDOO_gap0(3, iroad,ipart);
      float dirY  = muo->get_muIDOO_gap0(4, iroad,ipart);
//      float refX  = -dirX*gap0z + gap0x;
//      float refY  = -dirY*gap0z + gap0y;
      float muTRhits = muo->get_muTRhits(ipart);
      float muIDhits = 999;
      if( iroad >= 0 ) muIDhits = muo->get_muIDOOhits(iroad, ipart);
      
      float mutr_nhits = Tools::sumbit(muTRhits);
      float muid_nhits = Tools::sumbit(muIDhits);
      
      float px =  px_vertex;
      float py =  py_vertex;
      float pz =  pz_vertex;

      float xSTI  = muo->get_xpos(1,ipart);
      float ySTI  = muo->get_ypos(1,ipart);
      float zSTI  = muo->get_zpos(1,ipart);
      float pxSTI = muo->get_px(1,ipart);
      float pySTI = muo->get_py(1,ipart);
      float pzSTI = muo->get_pz(1,ipart);

      float xSTII  = muo->get_xpos(2,ipart);
      float ySTII  = muo->get_ypos(2,ipart);
      float zSTII  = muo->get_zpos(2,ipart);

      float xSTIII  = muo->get_xpos(3,ipart);
      float ySTIII  = muo->get_ypos(3,ipart);
      float zSTIII  = muo->get_zpos(3,ipart);
      float pxSTIII = muo->get_px(3,ipart);
      float pySTIII = muo->get_py(3,ipart);
      float pzSTIII = muo->get_pz(3,ipart);

      float pT = sqrt(px*px+py*py);
      float p = sqrt(px*px+py*py+pz*pz);
      float pSTI = sqrt(pxSTI*pxSTI + pySTI*pySTI + pzSTI*pzSTI);
      float ELoss = p - pSTI;

      //-------------------------------
      //calculate deflection angle, delta theta, delta phi and road_slope.. put in to the X1,X2...

      //cout << "_choice" << _choice << endl;
      //cout << "BbcZVertex " << BbcZVertex << endl;
      //cout << "refit_z " << vertex.get_vtx_z() << endl;

      if( _choice == "simu" || _choice == "simu_file" ) {
        BbcZVertex = vertex.get_vtx_z();
      }
      float costheta, costheta_xyz, xyz_St1;
      float dANGLE =-999;
      float dANGLE_xyz=-999;
      float dPHI=-999;
      float dTHETA=-999;
      float ROAD_SLOPE=-999;

      ROAD_SLOPE = sqrt(dirX*dirX+dirY*dirY); // bug fixed on 6/6/2007 MXL

      xyz_St1 = sqrt(xSTI*xSTI+ySTI*ySTI+(zSTI-BbcZVertex)*(zSTI-BbcZVertex) );
      costheta = (px*pxSTI + py*pySTI + pz*pzSTI) / ( p*pSTI ) ;

      costheta_xyz = (pxSTI*xSTI + pySTI*ySTI + pzSTI*(zSTI-BbcZVertex) ) / (pSTI*xyz_St1) ;

      if ( abs(costheta) < 1.0 ) {
        dANGLE = acos (costheta);
      }else{
        dANGLE = 0.0;
      }
      dANGLE = dANGLE*0.5*(p+pSTI);

      if ( abs(costheta_xyz) < 1.0 ) {
        dANGLE_xyz = acos (costheta_xyz);
      }else{
        dANGLE_xyz = 0.0;
      }
      dANGLE_xyz = dANGLE_xyz*0.5*(p+pSTI);

      //cout << "dangle = " << dANGLE << endl;
      // cout << "dangle_xyz = " << dANGLE_xyz << endl;

      dPHI = (atan2(ySTI,xSTI)-atan2(ySTIII,xSTIII));  //ST1 - ST3  // (x,y) ->(y,x) bug fix  12/03/2009  MXL 
      dTHETA = (atan((sqrt(xSTI*xSTI+ySTI*ySTI))/(sqrt((zSTI-BbcZVertex)*(zSTI-BbcZVertex))))-atan((sqrt(xSTIII*xSTIII+ySTIII*ySTIII))/(sqrt((zSTIII-BbcZVertex)*(zSTIII-BbcZVertex))))) ; //ST1 - ST3

      //-----------------------------------------------------------------------------

      //
      // calculate reference vtx from track
      // the shortest distance between the trk at station 1 and beam line
      // two space points are (xSTI, ySTI, zSTI) and (0, 0, BbcZVertex)
      // two momentum are (pxSTI, pySTI, pzSTI) and (0,0,1)

      Float_t ref_vtx_rdca;//, ref_vtx_xdca, ref_vtx_ydca, ref_vtx_zdca;

      // total momentum is p and pSTI
      /* x1[2],y1[2],z1[2]; position at station 1 and vtx point */
      
      Float_t a=0,b=0,c=0,d=0; /* equation du plan*/
      Float_t wx=0,wy=0,wz=0;
      Float_t X1=0,X2=0,Y1=0,Y2=0,Z1=0,Z2=0;
      Float_t P1[2],xp[2],yp[2];
      Float_t pp[2][3];
      Float_t x1[2]={0},y1[2]={0},z1[2]={0};

      x1[0] = 0.0;
      y1[0] = 0.0;
      z1[0] = BbcZVertex;
      x1[1] = muo->get_xpos(1,ipart);
      y1[1] = muo->get_ypos(1,ipart);
      z1[1] = muo->get_zpos(1,ipart);

      pp[0][0] = 0.0;
      pp[0][1] = 0.0;
      pp[0][2] = 1.0;
      pp[1][0] = muo->get_px(1,ipart);
      pp[1][1] = muo->get_py(1,ipart);
      pp[1][2] = muo->get_pz(1,ipart);
      /* calcul de z dca */
      for(Int_t k =0;k<2;k++)
        {
          P1[k]=pow(pp[k][0],2)+pow(pp[k][1],2)+pow(pp[k][2],2);
          P1[k]=sqrt(P1[k]);
          xp[k]=pp[k][0]/pp[k][2];
          yp[k]=pp[k][1]/pp[k][2];
        }

      /* calcul de w (produit vectoriel) */
      wx = yp[0]-yp[1];
      wy = xp[1]-xp[0];
      wz = xp[0]*yp[1]-xp[1]*yp[0];
      /*calcul des coeff pour eqn plan */

      a = wy-wz*yp[0];
      b = xp[0]*wz-wx;
      c = wx*yp[0]-xp[0]*wy;
      d = - x1[0]*a -y1[0]*b - z1[0]*c;

      /* sur la droite 2*/
      Z2 = a*(x1[1]-xp[1]*z1[1]) + b*(y1[1]-yp[1]*z1[1]) + d ;
      Z2 = -  Z2/(a*xp[1]+b*yp[1]+c);

      X2 = x1[1] + xp[1]*(Z2 - z1[1]);
      Y2 = y1[1] + yp[1]*(Z2 - z1[1]);


      Z1 = Z2 - xp[0]*(x1[0] - xp[0]*z1[0]- X2) - yp[0]*(y1[0] - yp[0]*z1[0] - Y2);
      Z1 = Z1/(1 +xp[0]*xp[0] + yp[0]*yp[0] );

      X1 = x1[0] + xp[0]*(Z1 - z1[0]);
      Y1 = y1[0] + yp[0]*(Z1 - z1[0]);

      //      ref_vtx_zdca =(Z1+Z2)/2; // z DCA vertex !
      // ref_vtx_ydca =(Y1+Y2)/2;
      //ref_vtx_xdca =(X1+X2)/2;
      ref_vtx_rdca = sqrt( pow((X2-X1),2)+pow((Y2-Y1),2)+pow((Z2-Z1),2) );

      //distance between bbcZ and Z1;
      Float_t ref_vtx_z = Z1;

      //calculate reference vtx from track

      float z_ref = muo->get_zpos(0,ipart);
      float x_ref = muo->get_xpos(0,ipart)+( BbcZVertex-z_ref)*(px/pz);
      float y_ref = muo->get_ypos(0,ipart) + ( BbcZVertex-z_ref)* (py/pz);

      float ref_vtx_r = sqrt(x_ref*x_ref + y_ref*y_ref + z_ref*z_ref);


      // Calculate pseudo-rapidity
      if (p == pz){pseudo_rapidity = -999.999;}
      else {pseudo_rapidity = 0.5*log((p+pz)/(p-pz));}

      // single muon v2 by using RXN
      float phi0 = atan2(py,px);
      float dphi = phi0 - RPrxnrp12;
      dphi = 0.5*atan2(sin(2*dphi),cos(2*dphi));
      float sng_v2 = cos(2*dphi);

      float dphi_N = phi0 - RPrxnrp10;
      dphi_N = 0.5*atan2(sin(2*dphi_N),cos(2*dphi_N));
      float sng_v2_N = cos(2*dphi_N);

      float dphi_S = phi0 - RPrxnrp11;
      dphi_S = 0.5*atan2(sin(2*dphi_S),cos(2*dphi_S));
      float sng_v2_S = cos(2*dphi_S);

      Float_t varNT[75];

      //=== Fill ntuple
      varNT[0]  = RunNumber;                       // run number
      varNT[1]  = EventNumber;                     // event number
      varNT[2]  = npart;                           // number of muons
      varNT[3]  = BbcZVertex;                           // Global
      varNT[4]  = bbcCentrality;                   // zdcCentrality
      varNT[5]  = BBCQN;                      // zdcEnerN;
      varNT[6]  = BBCQS;                      // zdcEnerN;
      varNT[7]  = BBCNN;                      // zdcEnerN;
      varNT[8]  = BBCNS;                      // zdcEnerN;
      varNT[9]  = muo->get_charge(ipart);         // muon charge

      varNT[10] = px;     			  // muon px at Vertex
      varNT[11] = py;			          // muon py at Vertex
      varNT[12] = pz;			          // muon pz at Vertex
      varNT[13] = pxSTI;		          // muon px at St1
      varNT[14] = pySTI;		          // muon py at St1
      varNT[15] = pzSTI;		          // muon pz at St1
      varNT[16] = pxSTIII;		          // muon px at St3
      varNT[17] = pySTIII;  		          // muon py at St3
      varNT[18] = pzSTIII;;            		  // muon pz at St3
      varNT[19] = pT;				  // muon pT

      varNT[20] = p;
      varNT[21] = pSTI;
      varNT[22] = ELoss;
      varNT[23] = muo->get_chisquare(ipart);      // chi2 of 1st muon
      varNT[24] = muIDhits; 		           // muid hit pattern of 1st muon
      varNT[25] = muIDquad0;                       // muid quadrant at gap0 of 1st muon
      varNT[26] = muTRhits;                       // mutr hit pattern of 1st muon
      varNT[27] = dS30;                            // DS3 of 1st muon (à la Olivier)
      varNT[28] = dS3ctp0;                         // DS3 of 1st muon (w const theta & phi)
      varNT[29] = muIDchis0;                       // Muid Chi2

      varNT[30] = gap0x;          // Muid Gap0 x
      varNT[31] = gap0y;          // Muid Gap0 y
      varNT[32] = gap0z;          // Muid Gap0 z
      varNT[33] = dirX;           // Muid Road dxdz
      varNT[34] = dirY;           // Muid Road  dydz
      varNT[35] = muo->get_TMutTrk_status(ipart);       // Mutr track status
      varNT[36] = muo->get_ghostflag(ipart);
      varNT[37] = lastGap;
      varNT[38] = pseudo_rapidity;		   // pseudo rapidity
      varNT[39] = atan(py/px);       // Reaction plane phi

      varNT[40] = DG0;				   // DG0
      varNT[41] = DDG0;				   // DDG0 - opening angle
      varNT[42] = DS0;				   // DS0 - muID road doca @z=0
      varNT[43] = mutr_nhits;
      varNT[44] = muid_nhits;     
      varNT[45] = xSTI;
      varNT[46] = ySTI;
      varNT[47] = zSTI;
      varNT[48] = xSTII;
      varNT[49] = ySTII;

      varNT[50] = zSTII;
      varNT[51] = xSTIII;
      varNT[52] = ySTIII;
      varNT[53] = zSTIII;
      varNT[54] = ref_vtx_rdca;           	   // muon px at Vertex
      varNT[55] = ref_vtx_r;            	   // muon py at Vertex
      varNT[56] = ref_vtx_z;                       // muon pz at Vertex
      varNT[57] = vertex.get_vtx_z();
      varNT[58] = dANGLE;                           // multiple-scattering angle form P_0 & P1
      varNT[59] = dANGLE_xyz;                       // multiple-scattering angle form P_0 & X1

      varNT[60] = dPHI;                             // bending angle = St1 -St3 
      varNT[61] = dTHETA;                           // bending angle = St1 -St3 
      varNT[62] = ROAD_SLOPE;                       // muID road slope
      varNT[63] = RPbbcrp10;                      // Reaction Plane
      varNT[64] = RPbbcrp11;                      // Reaction Plane
      varNT[65] = RPbbcrp12;                      // Reaction Plane
      varNT[66] = RPmpcrp10;                      // Reaction Plane
      varNT[67] = RPmpcrp11;                      // Reaction Plane
      varNT[68] = RPmpcrp12;                      // Reaction Plane
      varNT[69] = RPrxnrp10;                      // Reaction Plane

      varNT[70] = RPrxnrp11;                      // Reaction Plane
      varNT[71] = RPrxnrp12;                      // Reaction Plane
      varNT[72] = sng_v2_S;                      // Reaction Plane
      varNT[73] = sng_v2_N;                      // Reaction Plane
      varNT[74] = sng_v2;                      // Reaction Plane

      sngmuons->Fill(varNT);
      accMU++;
      
    }
  }
  accEVT++;

  return 0;
}

