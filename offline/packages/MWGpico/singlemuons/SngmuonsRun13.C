// $Id: SngmuonsRun13.C,v 1.3 2014/12/15 05:47:16 rseidl Exp $

/*!
   \file SngmuonsRun3.C
   \brief single muon ntuple booking and feeling
   \version $Revision: 1.3 $
   \date $Date: 2014/12/15 05:47:16 $
*/

#include <iostream>
#include <boost/array.hpp>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <ReactionPlaneObject.h> // event information
#include <RpSumXYObject.h> // event information
#include <RpConst.h>

#include <Bbc.hh>
#include <BbcRaw.h>
#include <BbcCalib.hh>
#include <BbcGeo.hh>
#include <RunToTime.hh>

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

#include <TRxnpRawScintMap.h>
#include <TRxnpScintMap.h>
#include <TRxnpRawXangMap.h>

#include "TrigLvl1v3.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"


using namespace std;
using namespace PhUtilities;

//__________________________________________________
void MWGpico::BookSngmuonsNtupleRun13(TNtuple*& sngmuons, TString m_name, TString m_title)
{

  // define variable list for single muons ntuple
  /*
    IMPORTANT NOTE:
    here we write only 10 variables/line to make counting easier when filling the ntuple
    please follow this policy in later modifications to avoid stupid bugs
  */
  const char* sngvarlist=0;
  
  if ( _choice == "simu" ){
	  sngvarlist=
		"Run_Number:Evt_Number:Evt_Nmu:Evt_Z:Evt_bbcZ:" //5
		"Evt_bbcCentrality:Evt_zdcEnerN:charge:px:py:" //10
		"pz:pxSt1:pySt1:pzSt1:pxSt3:" //15
		"pySt3:pzSt3:pT:p:pSt1:" //20
		"ELoss:chi2:idhits:idquad:trhits:" //25
		"idchi2:gap0x:gap0y:gap0z:dxdz:" //30
		"dydz:trstat:lastGap:eta:phi:" //35
		"DG0:DDG0:refX:refY:mutr_nhits:" //40
		"muid_nhits:Evt_realtrig_BBCLL1:Evt_realtrig_BBCLL1_novtx:" //43
		"Evt_realtrig_SN_1D:Evt_realtrig_SN_SG3_1H:xSt1:"//46
		"ySt1:zSt1:xSt2:ySt2:zSt2:" //51
		"xSt3:ySt3:zSt3:ref_vtx_rdca:ref_vtx_r:" //56
		"ref_vtx_z:refit_zvtx:Clock_trig:Evt_realtrig_MB:dAngle:" //61
		"dAngle_xyz:dTheta:dPhi:road_slope:vtx_chi2:vtx_chi2pdf:" //67
		"mc_n_part:mc_px:mc_py:mc_pz:mc_ptot:mc_z:" //73
		"mc_pid:mc_hits:mc_p_pid:mc_p_px:mc_p_py:" //78
		"mc_p_pz:mc_p_ptot:mc_p_z:mc_d_pid:mc_d_px:" //83
		"mc_d_py:mc_d_pz:mc_d_ptot:mc_d_z:mc_d_n:" //88
		"mc_x:mc_y:mc_g_pid:mc_g_px:mc_g_py:" //93
		"mc_g_pz:mc_g_ptot:mc_g_z:mc_trk"; //97
  }else{
	  sngvarlist=
		"Run_Number:Evt_Number:Evt_Nmu:Evt_Z:Evt_bbcZ:" //5
		"Evt_bbcCentrality:Evt_zdcEnerN:charge:px:py:" //10
		"pz:pxSt1:pySt1:pzSt1:pxSt3:" //15
		"pySt3:pzSt3:pT:p:pSt1:" //20
		"ELoss:chi2:idhits:idquad:trhits:" //25
		"idchi2:gap0x:gap0y:gap0z:dxdz:" //30
		"dydz:trstat:lastGap:eta:phi:" //35
		"DG0:DDG0:refX:refY:mutr_nhits:" //40
		"muid_nhits:Evt_realtrig_BBCLL1:Evt_realtrig_BBCLL1_novtx:" //43
		"Evt_realtrig_SN_1D:Evt_realtrig_SN_SG3_1H:xSt1:"//46
		"ySt1:zSt1:xSt2:ySt2:zSt2:" //51
		"xSt3:ySt3:zSt3:ref_vtx_rdca:ref_vtx_r:" //56
		"ref_vtx_z:refit_zvtx:Clock_trig:Evt_realtrig_MB:dAngle:" //61
		"dAngle_xyz:dTheta:dPhi:road_slope:vtx_chi2:vtx_chi2pdf"; //67
  }
  
  sngmuons = new TNtuple( m_name, m_title, sngvarlist );
  sngmuons->SetAutoSave(160000000);
  
  return;
}

//__________________________________________________________________
void MWGpico::BookSngmuonsEvtNtupleRun13(TNtuple*& sngvtx, TString v_name, TString v_title )
{
  // define variable list for vertex ntuple
  const char* vtxvarlist=0;


  vtxvarlist= "Run_Number:Evt_Z:bbcZ:zdcCentrality:bbcCentrality:" //5
	"BBCQN:BBCQS:BBCNN:BBCNS:ZDCN:" //10
	"ZDCS:Clock_trig:Evt_realtrig_MB:Evt_realtrig_BBCLL1:Evt_realtrig_BBCLL1_novtx:" //15
	"Evt_realtrig_SN_1D:Evt_realtrig_SN_SG3_1H"; //17

  sngvtx = new TNtuple( v_name, v_title, vtxvarlist );
  sngvtx->SetAutoSave(160000000);

  return;
}

 
//__________________________________________________
int MWGpico::FillSngmuonsNtpRun13(PHMuoTracksOut* &muo, TNtuple* sngmuons,  TNtuple* sngvtx)
{
  //=== event selection
  //    if (!PassCuts(evt)) return 1; // see ../PassCuts.h

  //=== event information
  Float_t bbcCentrality=-9999;
  Float_t zdcCentrality=-9999;
  Float_t ZdcEnergyN=-9999;
  Float_t ZdcEnergyS=-9999;

  // z_vertex
  float BbcZVertex( -9999 );
  float Evt_Z( -9999 );
  if (evt) {
    BbcZVertex = evt->getBbcZVertex();
    bbcCentrality = (Float_t) evt->getCentrality();
    //cout << "dAu8888888 bbc Centrality= " << bbcCentrality << endl;
    ZdcEnergyN=evt->getZdcEnergyN();
    ZdcEnergyS=evt->getZdcEnergyS();

  }
  
  // event/run number
  int RunNumber = ( run_header ) ?  run_header->get_RunNumber():0;
  int EventNumber = (event_header ) ? event_header->get_EvtSequence():0;
  
  // try load MC if failed from PHGlobal
  {
    bool error( false );
    if( !RunNumber ) RunNumber = Tools::runNumberMC( header, error );
    if( !EventNumber ) EventNumber  = Tools::eventNumberMC( header, error );
  }
  
  
  Float_t realtrigBBCLL1     			=  -999; // real trigger BBCLL1
  Float_t realtrigBBCLL1_novtx			=  -999; // real trigger BBCLL1 no vtx
  Float_t realtrigSN_1D 			    =  -999; // real trigger S|N 1D
  Float_t realtrigSN_SG3_1H			    =  -999; // real trigger N SG3&(D|H)

  //Float_t livetrigBBCLL1     			=  -999; // real trigger BBCLL1
  //Float_t livetrigBBCLL1_narrowvtx		=  -999; // real trigger BBCLL1 narrow vtx
  //Float_t livetrigBBCLL1_narrowvtxA     =  -999; // real trigger BBCLL1 narrow vtx copyA
  //Float_t livetrigBBCLL1_narrowvtxB     =  -999; // real trigger BBCLL1 narrow vtx copyB
  //Float_t livetrigBBCLL1_novtx		    =  -999; // real trigger BBCLL1 novtx
  
  Float_t realtrig_MB      =  -999;
  Float_t Clock_trig       =  0;
  //  Float_t Clock_live       =  0;
  
  // real triggers: 200GeV and 62 GeV runs
  
  if(_trig_lvl1)
  { 
    
	  realtrig_MB = 0;
	  // Trigger Node exists in data file
	  // one should really use the trigger name instead.
	  // not clear why the above did not work

	  // --- use trigger bit for trigger selection to cover run6 200GeV and 62 GeV runs for MUIDLL1_trigger with and w/o BBCLL1
	  // for low energy runs 
	  // 08/18/2006  MXL
	  
	  //scaled triggers
	  if ( _trig_lvl1->get_lvl1_trigscaled()&0x00000001) realtrigBBCLL1=1;
	  if ( _trig_lvl1->get_lvl1_trigscaled()&0x00000002) realtrigBBCLL1_novtx=1;
	  if ( _trig_lvl1->get_lvl1_trigscaled()&0x00020000) realtrigSN_1D=1;
	  if ( _trig_lvl1->get_lvl1_trigscaled()&0x00000200) realtrigSN_SG3_1H=1;
	  
	  //live triggers
	  //if (_trig_lvl1->get_lvl1_triglive()&0x00000002) livetrigBBCLL1=1;
	  //if (_trig_lvl1->get_lvl1_triglive()&0x00000004) livetrigBBCLL1_narrowvtx=1;
	  //if (_trig_lvl1->get_lvl1_triglive()&0x00000008) livetrigBBCLL1_narrowvtxA=1;
	  //if (_trig_lvl1->get_lvl1_triglive()&0x00000010) livetrigBBCLL1_narrowvtxB=1;
	  //if (_trig_lvl1->get_lvl1_triglive()&0x00000020) livetrigBBCLL1_novtx=1;
  }

  // minimum bias
  if (TrigHelp->IsEventMinBias()) realtrig_MB=1;
  
  //------------//////////////////////////////
  
  //Clock triggers
  if (TrigHelp->didLevel1TriggerGetScaled("Clock")) Clock_trig=1;
  //  if (TrigHelp->didLevel1TriggerFire("Clock")) Clock_live=2;
  
  /////  end of trigger selection ------
  //spin information
  /*
  Float_t SpinX_ID =  -999; // beam crossing ID from spin DB
  Float_t Pol_Y    =  -999; // yellow beam polarization
  Float_t Pol_B    =  -999; // blue beam polarization
  Float_t GL1X_ID  =  -999; // beam crossing ID from GL1
  
  if (spin)
  {
    
    SpinX_ID= spin->GetSpinGL1CrossingID();
    Pol_Y   = spin->GetSpinDirectionYellowFromV124();
    Pol_B   = spin->GetSpinDirectionBlueFromV124();
    GL1X_ID = spin->GetGL1CrossingID();
    
  } // if (spin)
  */
  
  Float_t  BBCQN= -999;
  Float_t  BBCQS= -999;
  Float_t  BBCNN= -999;
  Float_t  BBCNS= -999;  
  
  if ( evt){
    BBCQN = evt->getBbcChargeN();
    BBCQS = evt->getBbcChargeS();
    BBCNN = evt->getBbcMultN();  
    BBCNS = evt->getBbcMultS();  
  }


  Float_t Evt_Data[17] = {-999};
  Evt_Data[0] = RunNumber;
  Evt_Data[1] = Evt_Z;
  Evt_Data[2] = BbcZVertex;
  Evt_Data[3] = zdcCentrality;
  Evt_Data[4] = bbcCentrality;

  Evt_Data[5] = BBCQN;
  Evt_Data[6] = BBCQS;
  Evt_Data[7] = BBCNN;
  Evt_Data[8] = BBCNS;
  Evt_Data[9] = ZdcEnergyN;
 
  Evt_Data[10] = ZdcEnergyS;
  Evt_Data[11] = Clock_trig;
  Evt_Data[12] = realtrig_MB;                    // live real   trigger for MB
  Evt_Data[13] = realtrigBBCLL1;
  Evt_Data[14] = realtrigBBCLL1_novtx;

  Evt_Data[15] = realtrigSN_1D;
  Evt_Data[16] = realtrigSN_SG3_1H;

  sngvtx->Fill(Evt_Data);


  //_____________ end of sngvtx _____________


  // muon variables
  Float_t muIDchis0=999, DG0=999, DDG0=999;
  Float_t pseudo_rapidity = 999;
  Int_t muIDquad0=-1, lastGap=0;

  if (muo) { 

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
 
      //      dS30 = Tools::DS3(muo,ipart, iroad);
      DG0 = Tools::DG0(muo,ipart, iroad);
      DDG0 = Tools::DDG0(muo,ipart, iroad);
      //      dS3ctp0 = Tools::DS3ctp(muo,ipart, iroad );
      //DS0 = Tools::DS0(muo,ipart, iroad );

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
          
        } 
        else if( evt ) 
        {
          
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
        
        //       // some dump
        //                cout << "MWGpico::FillSngmuonsNtp -"
        //         << " momentum=("
        //                        << vertex.get_px( 0 ) << ","
        //                        << vertex.get_py( 0 ) << ","
        //                        << vertex.get_pz( 0 ) << ")"
        //         << " position=("
        //                        << vertex.get_trk_x( 0 ) << ","
        //                        << vertex.get_trk_y( 0 ) << ","
        //                        << vertex.get_trk_z( 0 ) << ")"
        //         << " chisquare=" << vertex.get_chisquare()
        //                      << endl;
        
      } catch( std::exception &e ) {
        cout << e.what() << endl;
      }
      
      float gap0x = muo->get_muIDOO_gap0(0, iroad, ipart);
      float gap0y = muo->get_muIDOO_gap0(1, iroad, ipart);
      float gap0z = muo->get_muIDOO_gap0(2, iroad,ipart);
      float dirX  = muo->get_muIDOO_gap0(3, iroad,ipart);
      float dirY  = muo->get_muIDOO_gap0(4, iroad,ipart);
      float refX  = -dirX*gap0z + gap0x;
      float refY  = -dirY*gap0z + gap0y;
      float muTRhits = muo->get_muTRhits(ipart);

      float muIDhits = 999;
      if( iroad >= 0 ){ muIDhits = muo->get_muIDOOhits(iroad, ipart); }
      
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

      //calculate deflection angle, delta theta, delta phi and road_slope.. put in to the X1,X2...
      //cout << "_choice" << _choice << endl;
      //cout << "BbcZVertex " << BbcZVertex << endl;
      //cout << "refit_z " << vertex.get_vtx_z() << endl;

      if( _choice == "simu" || _choice == "simu_file" ) {
 	BbcZVertex = vertex.get_vtx_z();
      }

      float costheta = -999, costheta_xyz = -999, xyz_St1 = -999;
      float dANGLE =-999;
      float dANGLE_xyz=-999;
      float dPHI=-999;
      float dTHETA=-999;
      float ROAD_SLOPE=-999;

      ROAD_SLOPE = sqrt(dirX*dirX+dirY*dirY);
      
      xyz_St1 = sqrt(xSTI*xSTI+ySTI*ySTI+(zSTI-BbcZVertex)*(zSTI-BbcZVertex) );
      costheta = (px*pxSTI + py*pySTI + pz*pzSTI) / ( p*pSTI ) ;
      
      costheta_xyz = (pxSTI*xSTI + pySTI* ySTI + pzSTI*(zSTI-BbcZVertex) ) / (pSTI*xyz_St1) ;
      
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

      dPHI = (atan2(ySTI,xSTI)-atan2(ySTIII,xSTIII));  // (x,y) ->(y,x) bug fix  12/03/2009  MXL 
      dTHETA = (atan((sqrt((xSTI)*(xSTI)+(ySTI)*(ySTI)))/(sqrt((zSTI-BbcZVertex)*(zSTI-BbcZVertex))))-atan((sqrt((xSTIII)*(xSTIII)+(ySTIII)*(ySTIII)))/(sqrt((zSTIII-BbcZVertex)*(zSTIII-BbcZVertex))))) ;
      
      //-----------------------------------------------------------------------------
      
      // calculate reference vtx form track
      // the shortest distance between the trk at station 1 and beam line
      // two space points are (xSTI, ySTI, zSTI) and (0, 0, BbcZVertex)
      // two vetctors  are (pxSTI, pySTI, pzSTI) and (0,0,1)

      Float_t ref_vtx_rdca;//,ref_vtx_xdca,ref_vtx_ydca,ref_vtx_zdca;

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
      //ref_vtx_ydca =(Y1+Y2)/2;
      //ref_vtx_xdca =(X1+X2)/2;
      ref_vtx_rdca = sqrt( pow((X2-X1),2)+pow((Y2-Y1),2)+pow((Z2-Z1),2) );

      //distance between bbcZ and Z1;
      Float_t ref_vtx_z = Z1;
      //  cout << "X1, Y1,Z1 = " << X1 << " " << Y1 << " " <<Z1 << endl;
      //  cout << "X2, Y2,Z2 = " << X2 << " " << Y2 << " " <<Z2 <<  " " << BbcZVertex << endl;

      //calculate reference vtx form track

      float z_ref = muo->get_zpos(0,ipart);

      float x_ref = muo->get_xpos(0,ipart)+( BbcZVertex-z_ref)*(px/pz);
      float y_ref = muo->get_ypos(0,ipart) + ( BbcZVertex-z_ref)* (py/pz);

      float ref_vtx_r = sqrt(x_ref*x_ref + y_ref*y_ref + z_ref*z_ref);

      // Calculating pseudo-rapidity
      if (p == pz){pseudo_rapidity = -999.999;}
      else {pseudo_rapidity = 0.5*log((p+pz)/(p-pz));}

      // single muon v2
      //      float phi0 = atan2(py,px);
      //float dphi = phi0 - RPbbcrp12;
      //dphi = 0.5*atan2(sin(2*dphi),cos(2*dphi));

      //float dphi_N = phi0 - RPbbcrp10;
      //dphi_N = 0.5*atan2(sin(2*dphi_N),cos(2*dphi_N));

      //float dphi_S = phi0 - RPbbcrp11;
      //dphi_S = 0.5*atan2(sin(2*dphi_S),cos(2*dphi_S));

      /*      	  Float_t pseudotrigS_1D   = -1;
      	  Float_t pseudotrigS_1S   = -1;
	  Float_t pseudotrigN_1D   = -1;
	  Float_t pseudotrigN_1S   = -1;

      // if (_choice == "simu" || _choice == "simu_file") 
	  //  {
		  pseudotrigN_1S =  Tools::LL1_1S_Decision( MUTOO::North );
		  pseudotrigN_1D =  Tools::LL1_1D_Decision( MUTOO::North );

		  pseudotrigS_1S =  Tools::LL1_1S_Decision( MUTOO::South );
		  pseudotrigS_1D =  Tools::LL1_1D_Decision( MUTOO::South );
		  //	  }
		  */
      //for MC evaluation
      //We try to get the major MC particle's information
      //major MC particle == MC particle contributes the most to muTr hits

      //may need to rethink about this "major MC" approach for tracks coming from decay
      //inside muTr volume        03/05/2005  MXL

      Float_t MC_N_PART =0;   // total # of MC particles associated with this reco'd track
      Float_t MC_PX=-999;        // the major MC particle's px
      Float_t MC_PY=-999;
      Float_t MC_PZ=-999;
      Float_t MC_PTOT =-999;      // the major MC particle's lastGap
      Float_t MC_X=-999;         // the major MC particle's X_orign
      Float_t MC_Y=-999;         // the major MC particle's Y_orign
      Float_t MC_Z=-999;         // the major MC particle's Z_orign
      Float_t MC_PID =-999;
      Float_t MC_HITS =0;     // the major MC particle's total contribution to muTr hits

      Float_t MC_P_PID =-999;    // MC particle's parent information
      Float_t MC_P_PX =-999;     //
      Float_t MC_P_PY =-999;
      Float_t MC_P_PZ =-999;
      Float_t MC_P_PTOT =-999;
      Float_t MC_P_Z =-999;

      Float_t MC_D_PID =-999;    // MC particle's daughter information
      Float_t MC_D_PX =-999;     //
      Float_t MC_D_PY =-999;
      Float_t MC_D_PZ =-999;
      Float_t MC_D_PTOT =-999;
      Float_t MC_D_Z =-999;
      Float_t MC_D_N =-999;

      Float_t MC_G_PID =-999;    // MC particle's grandparent (ancestor, original, very primary) informationi
      Float_t MC_G_PX =-999;     //
      Float_t MC_G_PY =-999;
      Float_t MC_G_PZ =-999;
      Float_t MC_G_PTOT =-999;
      Float_t MC_G_Z =-999;

      Int_t  MC_TRK   = -999;
      Int_t  MC_P_TRK = -999;    // parent track ID: for debuggin now
      Int_t  MC_D_TRK = -999;    // daughter track ID: for debuggin now
      Int_t  MC_G_TRK = -999;    // grandparent track ID: for debuggin now
  
      //how to associate a PHMuonTrack with a MutTrk? St1-P?
      //
      if (!mut_trk_map) cout << "no mut_trk_map " << endl;
      if (!mut_mctrk_map) cout << "not mut_mctrk_map "  << endl;
      
      if (mut_trk_map && mut_mctrk_map) {

  	TMutTrkMap::const_iterator trk_iter = mut_trk_map->range();

	while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	{
	  //
	  //pick up the right reconstructed track
	  //
	  float	    p_xx  = trk_ptr->get()->get_trk_par_vtx()->get_px();
	  float	    p_yy  = trk_ptr->get()->get_trk_par_vtx()->get_py();
	  float	    p_zz  = trk_ptr->get()->get_trk_par_vtx()->get_pz();
	  //float	ptot_x  = trk_ptr->get()->get_trk_par_vtx()->get_ptot();

	  float pxv =  muo->get_px(0,ipart);            // muon px at Vertex
          float pyv =  muo->get_py(0,ipart);            // muon px at Vertex
          float pzv =  muo->get_pz(0,ipart);            // muon px at Vertex

          float diff_x = 999;
          diff_x = (pxv-p_xx)*(pxv-p_xx) + (pyv-p_yy)*(pyv-p_yy) + (pzv-p_zz)*(pzv-p_zz);
          //	    cout << " >>>>>>   diff_x == " << diff_x << endl;
          if (diff_x > 0.000001) continue; // not the same track

          //found the associated MutTrk
          MC_N_PART=-999;
          MC_PX=-999;        // the major MC particle's px
          MC_PY=-999;
          MC_PZ=-999;
          MC_PTOT =-999;      // the major MC particle's lastGap
          MC_X=-999;         // the major MC particle's X_orign
          MC_Y=-999;         // the major MC particle's Y_orign
          MC_Z=-999;         // the major MC particle's Z_orign
          MC_PID =-999;
          MC_HITS =-999;     // the major MC particle's total contribution to muTr hits

          MC_P_PID =-999;    // MC particle's parent information
          MC_P_PX =-999;     //
          MC_P_PY =-999;
          MC_P_PZ =-999;
          MC_P_PTOT =-999;
          MC_P_Z =-999;

          MC_D_PID =-999;    // MC particle's daughter information
          MC_D_PX =-999;     //
          MC_D_PY =-999;
          MC_D_PZ =-999;
          MC_D_PTOT =-999;
          MC_D_Z =-999;
          MC_D_N =-999;

          MC_G_PID =-999;    // MC particle's parent information
          MC_G_PX =-999;     //
          MC_G_PY =-999;
          MC_G_PZ =-999;
          MC_G_PTOT =-999;
          MC_G_Z =-999;

          MC_TRK   = -999;
	  //          MC_P_TRK = -999;    // parent track ID: for debuggin now
          //MC_D_TRK = -999;    // daughter track ID: for debuggin now
          //MC_G_TRK = -999;    // grandparent track ID: for debuggin now

	  //----------------
          //get associated MC tracks information
          //----------------
          TMutMCTrkMap::key_iterator mc_trk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();

          // Loop over associated MCTracks
          mc_trk_iter.reset(); // reset ???
          while (TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {

            MC_N_PART++;

            MC_PX  = mc_trk_ptr->get()->get_px_orig();
            MC_PY  = mc_trk_ptr->get()->get_py_orig();
            MC_PZ  = mc_trk_ptr->get()->get_pz_orig();
            //	      MC_GAP = mc_trk_ptr->get()->get_depth();
            MC_PTOT = sqrt(MC_PX*MC_PX + MC_PY*MC_PY + MC_PZ*MC_PZ);

            MC_X = mc_trk_ptr->get()->get_x_orig();
            MC_Y = mc_trk_ptr->get()->get_y_orig();
            MC_Z = mc_trk_ptr->get()->get_z_orig();
            MC_PID = mc_trk_ptr->get()->get_pid();
	    //            MC_P_PID = mc_trk_ptr->get()->get_parent_id();
            MC_TRK   = mc_trk_ptr->get()->get_track_id();

            //
            //get associated MCHits information
            //
            TMutMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
            //how many hits from this MC particle ?
            MC_HITS = mc_hit_iter.count();
            //cout << "thetr are \t " << MC_HITS << " \t MC hits from this MC particle" << endl;

            float mchitx = 0;
            while(mc_hit_iter.next()) mchitx++;
            //	      MC_HITS = mchitx;

            Int_t trk_IDx = -99;
            Int_t trk_IDxP = -99;

            //reset parent  MC information
            MC_P_PID =-999;    // MC particle's parent information
            MC_P_PX  =-999;     //
            MC_P_PY  =-999;
            MC_P_PZ  =-999;
            MC_P_PTOT=-999;
            MC_P_Z   =-999;
	    //            MC_P_TRK =-999;    // parent track ID: for debuggin now

            //
            //get 1st parent information - PID, P, charge etc.
            //
            MC_P_TRK = mc_trk_ptr->get()->get_parent_track_id();

            TMutMCTrkMap::const_iterator mc_trk_iter_1p = mut_mctrk_map->range();

            while (TMutMCTrkMap::const_pointer mc_trk_ptr_1p = mc_trk_iter_1p.next()) {
              //for debugging
	      //get the track id index
	      trk_IDx  = mc_trk_ptr_1p->get()->get_track_id();  // trk_id ==0 for the very primary particle

	      /*
	      std::cout << "in the MCbank,  MC track id trk_IDx == " << trk_IDx;
	      cout << "  pz  ="     <<  mc_trk_ptr_1p->get()->get_pz_orig() ;
	      cout << "  z_orig  =" <<  mc_trk_ptr_1p->get()->get_z_orig() ;
	      cout << "  pID ="     <<  mc_trk_ptr_1p->get()->get_pid() << endl;
	      */
	      //end of debugging

	      //
	      //currently  only MCtracks with hits deposited in muTr are kept in TMutMCTrkMap
	      //need to expand the map to include all MC particles   MXL 03/15/2005
	      //
	
	      if ( MC_P_TRK == trk_IDx ){ // found the parent MC track
	      // Float_t prnt_IDx = mc_trk_ptr_1p->get()->get_pid();

	        MC_P_PID= mc_trk_ptr_1p->get()->get_pid();
	        MC_P_PX = mc_trk_ptr_1p->get()->get_px_orig();
	        MC_P_PY = mc_trk_ptr_1p->get()->get_py_orig();
	        MC_P_PZ = mc_trk_ptr_1p->get()->get_pz_orig();
	        MC_P_PTOT = sqrt(MC_P_PX*MC_P_PX + MC_P_PY*MC_P_PY + MC_P_PZ*MC_P_PZ);
	        MC_P_Z = mc_trk_ptr_1p->get()->get_z_orig();
	      } // found parent track ID

	      //
	      //if no parent found, then the current MC is the primary particle
	      //set parent = current mc, but set mc_p_ptot = - value so we can tell it from others
	      //
	      if (MC_P_PID==-999) {
	        MC_P_PID= mc_trk_ptr->get()->get_pid();
	        MC_P_PX = mc_trk_ptr->get()->get_px_orig();
	        MC_P_PY = mc_trk_ptr->get()->get_py_orig();
	        MC_P_PZ = mc_trk_ptr->get()->get_pz_orig();
	        MC_P_PTOT = -sqrt(MC_P_PX*MC_P_PX + MC_P_PY*MC_P_PY + MC_P_PZ*MC_P_PZ);
	        MC_P_Z = mc_trk_ptr->get()->get_z_orig();
	      }

	      //
	      //daughter particle - exclude gamma(1), electrons(2+,3-), pi0(7);
	      //

	      //reset daughter  MC information
	      MC_D_PID =-999;    // MC particle's daughter information
	      MC_D_PX  =-999;     //
	      MC_D_PY  =-999;
	      MC_D_PZ  =-999;
	      MC_D_PTOT=-999;
	      MC_D_Z   =-999;
	      //	      MC_D_N   =-999;

	      //	      MC_D_TRK =-999;    // daughter track ID: for debuggin now
	      MC_D_TRK = mc_trk_ptr->get()->get_track_id(); // current MC particle's trk_ID = daughter's parent trk_id
	      trk_IDxP = mc_trk_ptr_1p->get()->get_parent_track_id();  // trk_id ==0 for the very primary particle

	      MC_D_N=0;
	      if ( MC_D_TRK == trk_IDxP ){ // found the daughter MC track

	        MC_D_N++; // count how many daughter particles we have

	        Int_t MC_D_PIDx= mc_trk_ptr_1p->get()->get_pid();

	        // always pick up the MC decay vtx
	        MC_D_Z = mc_trk_ptr_1p->get()->get_z_orig();

	        if (MC_D_PIDx!=1&&MC_D_PIDx!=2&&MC_D_PIDx!=3&&MC_D_PIDx!=7){
	          MC_D_PID= mc_trk_ptr_1p->get()->get_pid();
	          MC_D_PX = mc_trk_ptr_1p->get()->get_px_orig();
	          MC_D_PY = mc_trk_ptr_1p->get()->get_py_orig();
	          MC_D_PZ = mc_trk_ptr_1p->get()->get_pz_orig();
	          MC_D_PTOT = sqrt(MC_D_PX*MC_D_PX + MC_D_PY*MC_D_PY + MC_D_PZ*MC_D_PZ);
	        }
	      } // found daughter track ID

              // get mc_primary track info.
              if (_mc_primary_map) {

                //cout << "_mc_primary_map found, size " << _mc_primary_map->size() << endl;
                TMCPrimaryMap::const_iterator prim_iter = _mc_primary_map->range();

                MC_G_TRK = mc_trk_ptr->get()->get_grandparent_track_id();
                //cout << "grandparent_track_id " << MC_G_TRK << endl;

                while( TMCPrimaryMap::const_pointer prim_ptr = prim_iter.next() )
                {
                  Int_t trk_IDx = -99;
                  trk_IDx  = prim_ptr->get()->get_trk_id();

                  if ( MC_G_TRK == trk_IDx ){ // found the parent MC track

                    MC_G_PID  = prim_ptr->get()->get_pid();
                    MC_G_PX   = prim_ptr->get()->get_px_orig();
                    MC_G_PY   = prim_ptr->get()->get_py_orig();
                    MC_G_PZ   = prim_ptr->get()->get_pz_orig();
                    MC_G_PTOT = prim_ptr->get()->get_ptot_orig();
                    MC_G_Z    = prim_ptr->get()->get_z_orig();

                  /*  cout << "PRIMARY pID " << prim_ptr->get()->get_pid() << " tID " << prim_ptr->get()->get_trk_id() << endl
                         << "        vertex (" << prim_ptr->get()->get_x_orig() << ", " << prim_ptr->get()->get_y_orig()
			 << ", " << prim_ptr->get()->get_z_orig() << " )"<< endl
                         << "        mom    (" << prim_ptr->get()->get_px_orig() << ", " << prim_ptr->get()->get_py_orig()
			 << ", " << prim_ptr->get()->get_pz_orig() << " )" << endl;
                  */
                  }
                }
              }
              else {
                cout << "mc_primary_map not found " << endl;
              }
            } // loop MC track bank for the 1st parent and daughter
          } // loop MC trackMap bank for the reco associated track
        } //end of TMutTrk loop
      } // end of if (mut_trk_map && mut_mctrk_map) loop



      Float_t varNT[97] = {-999};

      //=== Fill ntuple
      varNT[0]  = RunNumber;                       // run number
      varNT[1]  = EventNumber;                     // event number
      varNT[2]  = npart;                           // number of muons
      varNT[3]  = Evt_Z;                           // Global
      varNT[4]  = BbcZVertex;                      // BBC Zvertex

      varNT[5]  = bbcCentrality;            
      varNT[6]  = ZdcEnergyN;                      // zdcEnerN;
      varNT[7] = muo->get_charge(ipart);         // muon charge
      varNT[8] = px;     			  // muon px at Vertex
      varNT[9] = py;			          // muon py at Vertex

      varNT[10] = pz;			          // muon pz at Vertex
      varNT[11] = pxSTI;		          // muon px at St1
      varNT[12] = pySTI;		          // muon py at St1
      varNT[13] = pzSTI;		          // muon pz at St1
      varNT[14] = pxSTIII;		          // muon px at St3

      varNT[15] = pySTIII;  		          // muon py at St3
      varNT[16] = pzSTIII;;            		  // muon pz at St3
      varNT[17] = pT;				  // muon pT
      varNT[18] = p;
      varNT[19] = pSTI;

      varNT[20] = ELoss;
      varNT[21] = muo->get_chisquare(ipart);      // chi2 of 1st muon
      varNT[22] = muIDhits; 		           // muid hit pattern of 1st muon
      varNT[23] = muIDquad0;                       // muid quadrant at gap0 of 1st muon
      varNT[24] = muTRhits;                       // mutr hit pattern of 1st muon

      varNT[25] = muIDchis0;                       // Muid Chi2
      varNT[26] = gap0x;            // Muid Gap0 x
      varNT[27] = gap0y;            // Muid Gap0 y
      varNT[28] = gap0z;            // Muid Gap0 z
      varNT[29] = dirX;             // Muid Road dxdz

      varNT[30] = dirY;             // Muid Road  dydz
      varNT[31] = muo->get_TMutTrk_status(ipart);       // Mutr track status
      varNT[32] = lastGap;
      varNT[33] = pseudo_rapidity;		   // pseudo rapidity
      varNT[34] = atan(py/px);       // Reaction plane phi

      varNT[35] = DG0;				   // DG0
      varNT[36] = DDG0;				   // DDG0 - opening angle
      varNT[37] = refX;
      varNT[38] = refY;
      varNT[39] = mutr_nhits;

      varNT[40] = muid_nhits;
      varNT[41] = realtrigBBCLL1;        
      varNT[42] = realtrigBBCLL1_novtx; 
      varNT[43] = realtrigSN_1D;

      varNT[44] = realtrigSN_SG3_1H;
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
      varNT[57] = vertex.get_vtx_z();              // event Z-vtx from refit
      varNT[58] = Clock_trig;
      varNT[59] = realtrig_MB;                     // live real   trigger for MB
      varNT[60] = dANGLE;

      varNT[61] = dANGLE_xyz;
      varNT[62] = dPHI;      
      varNT[63] = dTHETA;    
      varNT[64] = ROAD_SLOPE;
	  varNT[65] = vertex.get_chisquare();
	  varNT[66] = vertex.get_chisquare_pdf();

      varNT[67] = MC_N_PART; //
      varNT[68] = MC_PX; // from MC tracker_map
      varNT[69] = MC_PY; // from MC tracker_map
      varNT[70] = MC_PZ; // from MC tracker_map
      varNT[71] = MC_PTOT; // from MC tracker_map
      varNT[72] = MC_Z; // from MC tracker_map

      varNT[73] = MC_PID; // from MC tracker_map
      varNT[74] = MC_HITS; // hits of the major MC trk
      varNT[75] = MC_P_PID; // from MC tracker_map
      varNT[76] = MC_P_PX; // from MC tracker_map
      varNT[77] = MC_P_PY; // from MC tracker_map

      varNT[78] = MC_P_PZ; // from MC tracker_map
      varNT[79] = MC_P_PTOT; //
      varNT[80] = MC_P_Z; //
      varNT[81] = MC_D_PID; // from MC tracker_map for daughter particle
      varNT[82] = MC_D_PX; // from MC tracker_map

      varNT[83] = MC_D_PY; // from MC tracker_map
      varNT[84] = MC_D_PZ; // from MC tracker_map
      varNT[85] = MC_D_PTOT; //
      varNT[86] = MC_D_Z; // origin_z
      varNT[87] = MC_D_N; // total # of daughter particles

      varNT[88] = MC_X; // from MC tracker_map
      varNT[89] = MC_Y; // from MC tracker_map
      varNT[90] = MC_G_PID; // from MC tracker_map
      varNT[91] = MC_G_PX; // from MC tracker_map
      varNT[92] = MC_G_PY; // from MC tracker_map
	  
      varNT[93] = MC_G_PZ; // from MC tracker_map
      varNT[94] = MC_G_PTOT; //
      varNT[95] = MC_G_Z; //
      varNT[96] = MC_TRK;

	  //if ( (_choice == "simu") && (MC_G_PID < 0) ) continue;

      sngmuons->Fill(varNT);
      accMU++;

    } // for
  } // if
  accEVT++;

  return 0;
}
