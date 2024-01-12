#include <EventHeader.h>
#include <PHGlobal.h> 
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <TChain.h>
#include <TNtuple.h>
#include <TH1.h>
#include <vector>
#include <string>

#include "../MWGpico.h"
#include <Tools.h>
#include <MWGConsts.h>

using namespace std;

//! global enumeration for filling the MUInfo
enum {
  Mfreq,
  MRunNumber,
  MEventNumber,
  MBBCzvertex,
  MbbcCentrality, 
  Mcharge,
  Mchisquare,
  Miquad,
  Mpx,
  Mpy, 
  Mpz,
  MDG0,
  Mst1_bp_pos_x, 
  Mst1_bp_pos_y, 
  Mst1_bp_pos_z, 
  Mst1_bp_Px, 
  Mst1_bp_Py,  
  Mst1_bp_Pz,
  Midhits,
  MDS3,
  MDS3ctp,
  MDDG0
};

//_________________________________________________________________
void MWGpico::BookEvtMixNtupleRun3( TNtuple*& evtMix, TString name, TString title )
{
  // define variable list
  
  const char* varlist = 
    "Evt_bbcZ:Evt_bbcCentrality:mass:charge:rapidity:pT:p:dca:xvtxbp:yvtxbp:zvtxbp:"
    "Tr0_chi2:Tr1_chi2:Tr0_idquad:Tr1_idquad:Tr0_DS3:Tr1_DS3:Tr0_DS3ctp:Tr1_DS3ctp:"
    "Tr0_DG0:Tr1_DG0:Tr0_DDG0:Tr1_DDG0:Tr0_p:Tr1_p:Tr0_y:Tr1_y:Tr0_idhits:Tr1_idhits:"
    "Tr0_pT:Tr1_pT";
    
  // create the ntuple
  evtMix = new TNtuple( name, title, varlist);   

  return;

}

//_________________________________________________________________
int MWGpico::StoreEvtMixMuonsRun3(PHMuoTracksOut* &muo)
{
  
  // check PHGlobal
  if( !evt ) return 1;
  
  //============================= Event selection
  if (!Cuts().pass_event_cuts(_top_node)) return 1; // see ../PassCuts.C

  //============================= Event information
  float RunNumber = ( run_header ) ? run_header->get_RunNumber():0;
  float EventNumber = (event_header ) ? event_header->get_EvtSequence():0;
  float BBCzvertex = (evt) ? evt->getBbcZVertex():-9999; 
  float bbcCentrality = -9999;
  if ( _type == "dAu") bbcCentrality = evt->get_dAuBbcCentrality(); 

  //============================= Muons
  if (!muo) return 1; 
  int npart = muo->get_npart();
  if (npart < 2) return 2;
  if (muo->get_ndimu() < 1) return 2;
  
  vector<float> MuInfo(22); // vector to store muon variables 
  for (int itrk=0; itrk<npart; itrk++) {
    if ((_choice=="dAu03N2D" || _choice=="dAu03NDS" || _choice=="pp03N") && 
  !(muo->get_pz(0,itrk)>0)) continue;
    if ((_choice=="dAu03S2D" || _choice=="dAu03SDS" || _choice=="pp03S") && 
  !(muo->get_pz(0,itrk)<0)) continue;
    MuInfo[Mfreq]          = 0;
    MuInfo[MRunNumber]     = RunNumber;
    MuInfo[MEventNumber]   = EventNumber;
    MuInfo[MBBCzvertex]    = BBCzvertex;    
    MuInfo[MbbcCentrality] = bbcCentrality;
    MuInfo[Mcharge]        = muo->get_charge(itrk);
    MuInfo[Mchisquare]     = muo->get_chisquare(itrk);
    MuInfo[Miquad]         = (muo->get_muID_gap0(0,itrk)>0) + 2*(muo->get_muID_gap0(1,itrk)<0);
    MuInfo[Mpx]            = muo->get_px(0,itrk); 
    MuInfo[Mpy]            = muo->get_py(0,itrk);
    MuInfo[Mpz]            = muo->get_pz(0,itrk);
    MuInfo[MDG0]           = Tools::DG0(muo,itrk);                                            
    MuInfo[MDDG0]          = Tools::DDG0(muo,itrk);
    MuInfo[Mst1_bp_pos_x]  = muo->get_st1_bp_pos(0,itrk);                                      
    MuInfo[Mst1_bp_pos_y]  = muo->get_st1_bp_pos(1,itrk);                                      
    MuInfo[Mst1_bp_pos_z]  = muo->get_st1_bp_pos(2,itrk);                                      
    MuInfo[Mst1_bp_Px]     = muo->get_st1_bp_P(0,itrk);                                        
    MuInfo[Mst1_bp_Py]     = muo->get_st1_bp_P(1,itrk);                                        
    MuInfo[Mst1_bp_Pz]     = muo->get_st1_bp_P(2,itrk);                                        
    MuInfo[Midhits]        = muo->get_muIDhits(itrk);
    MuInfo[MDS3]           = Tools::DS3(muo,itrk);
    MuInfo[MDS3ctp]        = Tools::DS3ctp(muo,itrk);
    MuInfos.push_back(MuInfo);
  }
  return 0;
}

//_________________________________________________________________
int MWGpico::FillEvtMixRun3(TNtuple* evtMix)
{

  //=== criteria
  // if |vertexmu1-vertexmu2|<VertexWindow, build the fake dimuon.
  const Float_t VertexWindow = 2.0; 
  
  // if |centmu1 - centmu2|<CentWindow, build the fake dimuon.
  const Float_t CentWindow = 10;
  
  // maximum number of times a muon should be used to build fake dimuons.    
  const Int_t frequency = 5;  

  Int_t nMuons = MuInfos.size();
  cout<<endl<<nMuons<<" muons selected. Now, produce dimuons."<<endl;

  Int_t produced=0;
 
  Float_t varNtuple[33]; for (int i=0; i!=33; i++) varNtuple[i]=0;

  for (int i=0; i<nMuons-1; i++) for (int j=i+1; j<nMuons; j++) 
    //== check muon frequency 
    if (MuInfos[i][Mfreq]<frequency && MuInfos[j][Mfreq]<frequency) 
      //== check that muons don't belong to the same event
      if (!(MuInfos[i][MRunNumber]==MuInfos[j][MRunNumber] && 
      MuInfos[i][MEventNumber]==MuInfos[j][MEventNumber]))
  //== check event z vertex
    if (fabs(MuInfos[i][MBBCzvertex] - MuInfos[j][MBBCzvertex]) < VertexWindow) 
      //== check event centrality
      if (fabs(MuInfos[i][MbbcCentrality] - MuInfos[j][MbbcCentrality])<CentWindow)   
      {
        MuInfos[i][Mfreq]++; MuInfos[j][Mfreq]++; // frequency incrementation

        //==================== compute fake dimuon dca, xvtxbp, yvtxbp, zvtxbp
        Float_t mydca=0, xvtxbp=0, yvtxbp=0, zvtxbp=0;
        Float_t xbp0[3] = {MuInfos[i][Mst1_bp_pos_x], 
         MuInfos[i][Mst1_bp_pos_y], 
         MuInfos[i][Mst1_bp_pos_z]};
        Float_t xbp1[3] = {MuInfos[j][Mst1_bp_pos_x], 
         MuInfos[j][Mst1_bp_pos_y], 
         MuInfos[j][Mst1_bp_pos_z]};
        Float_t pbp0[3] = {MuInfos[i][Mst1_bp_Px],    
         MuInfos[i][Mst1_bp_Py],    
         MuInfos[i][Mst1_bp_Pz]};
        Float_t pbp1[3] = {MuInfos[j][Mst1_bp_Px],    
         MuInfos[j][Mst1_bp_Py],    
         MuInfos[j][Mst1_bp_Pz]};
        Tools::vtxBP(xbp0, pbp0, xbp1, pbp1, mydca, xvtxbp, yvtxbp, zvtxbp);
        
        //==================================================== variable filling
        varNtuple[0] = (MuInfos[i][MBBCzvertex] + 
            MuInfos[j][MBBCzvertex])/2;    // event vertex
        varNtuple[1] = (MuInfos[i][MbbcCentrality] + 
            MuInfos[j][MbbcCentrality])/2; // event centrality
        varNtuple[2] = Tools::invMass(Const::MUMASS,
              MuInfos[i][Mpx],
              MuInfos[i][Mpy],
              MuInfos[i][Mpz],
              Const::MUMASS,
              MuInfos[j][Mpx],
              MuInfos[j][Mpy],
              MuInfos[j][Mpz]); // dimuon mass
        varNtuple[3] = ((int)MuInfos[i][Mcharge] + 
            (int)MuInfos[j][Mcharge])/2;   // dimuon charge
         varNtuple[4] = Tools::rapidity(varNtuple[2],
                MuInfos[i][Mpx]+
               MuInfos[j][Mpx],
                MuInfos[i][Mpy]+
               MuInfos[j][Mpy],
                MuInfos[i][Mpz]+
               MuInfos[j][Mpz]); // dimuon rapidity
        varNtuple[5] = Tools::pT(MuInfos[i][Mpx]+
               MuInfos[j][Mpx],
               MuInfos[i][Mpy]+
               MuInfos[j][Mpy]); // dimuon pT
        varNtuple[6] = Tools::p(MuInfos[i][Mpx]+
              MuInfos[j][Mpx],
              MuInfos[i][Mpy]+
              MuInfos[j][Mpy],
              MuInfos[i][Mpz]+
              MuInfos[j][Mpz]); // dimuon p
        varNtuple[7] = mydca;                   // dca
        varNtuple[8] = xvtxbp;                  // xvtxbp
        varNtuple[9] = yvtxbp;                  // yvtxbp
        varNtuple[10] = zvtxbp;                 // zvtxbp
        varNtuple[11] = MuInfos[i][Mchisquare]; // Tr0_chi2
        varNtuple[12] = MuInfos[j][Mchisquare]; // Tr1_chi2
        varNtuple[13] = MuInfos[i][Miquad];     // Tr0_idquad
        varNtuple[14] = MuInfos[j][Miquad];     // Tr1_idquad
        varNtuple[15] = MuInfos[i][MDS3];       // Tr0_DS3
        varNtuple[16] = MuInfos[j][MDS3];       // Tr1_DS3
        varNtuple[17] = MuInfos[i][MDS3ctp];    // Tr0_DS3ctp 
        varNtuple[18] = MuInfos[j][MDS3ctp];    // Tr1_DS3ctp
        varNtuple[19] = MuInfos[i][MDG0];       // Tr0_DG0
        varNtuple[20] = MuInfos[j][MDG0];       // Tr1_DG0
        varNtuple[21] = MuInfos[i][MDDG0];      // Tr0_DDG0
        varNtuple[22] = MuInfos[j][MDDG0];      // Tr1_DDG0
        varNtuple[23] = Tools::p(MuInfos[i][Mpx],
               MuInfos[i][Mpy],
               MuInfos[i][Mpz]); // Tr0_p
        varNtuple[24] = Tools::p(MuInfos[j][Mpx],
               MuInfos[j][Mpy],
               MuInfos[j][Mpz]); // Tr1_p
        varNtuple[25] = Tools::rapidity(Const::MUMASS,
                MuInfos[i][Mpx],
                MuInfos[i][Mpy],
                MuInfos[i][Mpz]); // Tr0_y
        varNtuple[26] = Tools::rapidity(Const::MUMASS,
                MuInfos[j][Mpx],
                MuInfos[j][Mpy],
                MuInfos[j][Mpz]); // Tr1_y
        varNtuple[27] = MuInfos[i][Midhits]; // Tr0_idhits
        varNtuple[28] = MuInfos[j][Midhits]; // Tr1_idhits
        varNtuple[29] = Tools::pT(MuInfos[i][Mpx],MuInfos[i][Mpy]); // Tr0_pT
        varNtuple[30] = Tools::pT(MuInfos[j][Mpx],MuInfos[j][Mpy]); // Tr1_pT
        evtMix->Fill(varNtuple);
        produced++;
        if (produced%10000==0) {
    cout<<"Making background dimuons : "<<produced<<" dimuons \r"; 
    cout.flush();
        }
      }
  cout<<endl<<">>> Ending background computing : "<<produced
      <<" fake dimuons created <<<"<<endl;
  return 0;
}

