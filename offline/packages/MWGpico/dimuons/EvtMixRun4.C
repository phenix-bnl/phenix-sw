#include <TChain.h>
#include <EventHeader.h>
#include <PHGlobal.h> 
#include <PHMuoTracksOut.h>
#include <RunHeader.h>
#include <string>
#include <TNtuple.h>
#include <TH1.h>
#include <vector>
#include <utiCentrality.h>

#include "../MWGpico.h"
#include <Tools.h>
#include <MWGConsts.h>
   
using namespace std;

//! enumeration to fill MUInfo vector
enum 
{
  Mfreq,     
  MRunNumber,  
  MEventNumber,  
  MBBCzvertex,   
  MbbcCentrality,
  Mcharge,   
  Mvtxchi2,  
  Mxvtx,  
  Myvtx,   
  Mzvtx,
  Mpxvtx,    
  Mpyvtx,  
  Mpzvtx,  
  MDG0,   
  MDDG0,
  Miquad,
  Midhits,  
  Mtrhits,  
  Mtrackchi2 
};

//_________________________________________________________________
void MWGpico::BookEvtMixNtupleRun4( TNtuple*& evtMix, TString name, TString title )
{
  // define variable list
  
  const char *varlist = "Evt_bbcZ:Evt_bbcCentrality:mass:charge:rapidity:pT:p:Evt_vtxchi2:"
    "Tr0_chi2:Tr1_chi2:Tr0_idquad:Tr1_idquad:Tr0_DG0:Tr1_DG0:Tr0_DDG0:Tr1_DDG0:"
    "Tr0_px:Tr0_py:Tr0_pz:Tr1_px:Tr1_py:Tr1_pz:Tr0_idhits:Tr1_idhits:Tr0_trhits:Tr1_trhits";
  
  // create the ntuple
  evtMix = new TNtuple( name, title, varlist);

  return;

}

//_________________________________________________________________
int MWGpico::StoreEvtMixMuonsRun4(PHMuoTracksOut* &muo)
{
  
  // check PHGlobal
  if( !evt ) return 1;
  
  totEVT++; totDIMU+=muo->get_ndimu();

  //============================= Parameters
  const unsigned int keepMuons = 1000000; // maximum number of muons to be stored to produce fake dimuons.

  //============================= Event selection
  if (!Cuts().pass_event_cuts(_top_node)) return 1; // see ../PassCuts.C

  //============================= Event information
  float RunNumber = ( run_header ) ? run_header->get_RunNumber():0;
  float EventNumber = (event_header ) ? event_header->get_EvtSequence():0;
  float BBCzvertex =  evt->getBbcZVertex();
  float bbcCentralityByClock = (float) PhUtilities::getCentralityByClockRun4(_top_node); 

  //============================= Muons
  if (!muo) return 1;
  int ndimu = muo->get_ndimu();
  if (ndimu != 1) return 2;
      
  vector<float> MuInfo(19); // vector to store muon variables 
  
  for (int idimu=0; idimu<ndimu; idimu++) {
    
   int ipart_1 = muo->get_ditrkIndex(0,idimu);
    int ipart_2 = muo->get_ditrkIndex(1,idimu);
  
    // Remove dimuons with unassociated tracks (negative index)
    if(ipart_1 < 0 || ipart_2 < 0 ) {
      if( verbosity ) cout << "MWGpico::evtMix - wrong track index. Vertex skipped.\n";
     negDIMU++;
      continue;
    }
   
   if ( Cuts().pass_single_muon_cuts( evt, ipart_1, muo, _framework ) ) {
      MuInfo[Mfreq]          = 0;
      MuInfo[MRunNumber]     = RunNumber;
      MuInfo[MEventNumber]   = EventNumber;
      MuInfo[MBBCzvertex]    = BBCzvertex;    
      MuInfo[MbbcCentrality] = bbcCentralityByClock;
      MuInfo[Mcharge]        = muo->get_vtx_chrg_1(idimu);
      MuInfo[Mvtxchi2]       = muo->get_vtx_chisquare(idimu);
      MuInfo[Mxvtx]          = muo->get_vtx_xpos(idimu);
      MuInfo[Myvtx]          = muo->get_vtx_ypos(idimu);
      MuInfo[Mzvtx]          = muo->get_vtx_zpos(idimu);
      MuInfo[Mpxvtx]         = muo->get_vtx_px_1(idimu);
      MuInfo[Mpyvtx]         = muo->get_vtx_py_1(idimu);
      MuInfo[Mpzvtx]         = muo->get_vtx_pz_1(idimu);
      MuInfo[MDG0]           = Tools::DG0(muo,ipart_1);       //  Mut variables are
      MuInfo[MDDG0]          = Tools::DDG0(muo,ipart_1);      //  set to Mutoo best
      MuInfo[Midhits]        = muo->get_muIDhits(ipart_1);    //  road's in MWGpico
      MuInfo[Miquad]         = (muo->get_muID_gap0(0,ipart_1)>0) + 2*(muo->get_muID_gap0(1,ipart_1)>0) ;
      MuInfo[Mtrhits]        = muo->get_muTRhits(ipart_1);    //  tr hits
      MuInfo[Mtrackchi2]     = muo->get_chisquare(ipart_1);   //  chi2 of single track
      MuInfos.push_back(MuInfo);
   }
     
   if( Cuts().pass_single_muon_cuts( evt, ipart_2, muo, _framework ) ) {
      MuInfo[Mfreq]          = 0;
      MuInfo[MRunNumber]     = RunNumber;
      MuInfo[MEventNumber]   = EventNumber;
      MuInfo[MBBCzvertex]    = BBCzvertex;
      MuInfo[MbbcCentrality] = bbcCentralityByClock;
      MuInfo[Mcharge]        = muo->get_vtx_chrg_2(idimu);
      MuInfo[Mvtxchi2]       = muo->get_vtx_chisquare(idimu);
      MuInfo[Mxvtx]          = muo->get_vtx_xpos(idimu);
      MuInfo[Myvtx]          = muo->get_vtx_ypos(idimu);
      MuInfo[Mzvtx]          = muo->get_vtx_zpos(idimu);
      MuInfo[Mpxvtx]         = muo->get_vtx_px_2(idimu);
      MuInfo[Mpyvtx]         = muo->get_vtx_py_2(idimu);
      MuInfo[Mpzvtx]         = muo->get_vtx_pz_2(idimu);
      MuInfo[MDG0]           = Tools::DG0(muo,ipart_2);       //  Mut variables are
      MuInfo[MDDG0]          = Tools::DDG0(muo,ipart_2);      //  set to Mutoo best
      MuInfo[Midhits]        = muo->get_muIDhits(ipart_2);    //  road's in MWGpico
      MuInfo[Miquad]         = (muo->get_muID_gap0(0,ipart_2)>0) + 2*(muo->get_muID_gap0(1,ipart_2)>0) ;    
      MuInfo[Mtrhits]        = muo->get_muTRhits(ipart_2);    //  tr hits
      MuInfo[Mtrackchi2]     = muo->get_chisquare(ipart_2);   //  chi2 of single track
      MuInfos.push_back(MuInfo);
   }

    if (MuInfos.size() > keepMuons) _thestopflag = true;
    accDIMU++;                    
  }
  accEVT++;
  return 0;
}

//_________________________________________________________________
int MWGpico::FillEvtMixRun4(TNtuple* evtMix)
{

  const Float_t VertexWindow = 2.0; // if |vertexmu1-vertexmu2|<VertexWindow, build the fake dimuon.
  const Float_t VertexTransPosWindow = 10.0; // if vtx transverse positions ok, build the fake dimuon.
  const Float_t VertexZPosWindow = 2.0; // if vtx Z positions ok, build the fake dimuon.
  const Float_t VertexChiWindow = 5.0; // if vtx chi2 ok, build the fake dimuon.
  const Float_t CentWindow = 10;    // if |centmu1 - centmu2|<CentWindow, build the fake dimuon.
  const Int_t frequency = 100;  // maximum number of times a muon should be used to build fake dimuons.

  Int_t nMuons = MuInfos.size();
  cout<<endl<<nMuons<<" muons selected. Now, produce dimuons."<<endl;

  Int_t produced=0;
 
  Float_t varNtuple[26]; for (int i=0; i<26; i++) varNtuple[i]=0;

  for (int i=0; i<nMuons-1; i++) for (int j=i+1; j<nMuons; j++)
    //== checks muon frequency 
    if (MuInfos[i][Mfreq]<frequency && MuInfos[j][Mfreq]<frequency)
     //== muons don't belong to the same event
     if (!(MuInfos[i][MRunNumber]==MuInfos[j][MRunNumber] && MuInfos[i][MEventNumber]==MuInfos[j][MEventNumber]))
     //== check event z bbc vertex
     if (fabs(MuInfos[i][MBBCzvertex] - MuInfos[j][MBBCzvertex]) < VertexWindow)
      //== check track x at vertex
      if (fabs(MuInfos[i][Mxvtx] - MuInfos[j][Mxvtx]) < VertexTransPosWindow)
       //== check track y at vertex
       if (fabs(MuInfos[i][Myvtx] - MuInfos[j][Myvtx]) < VertexTransPosWindow)
        //== check track z at vertex
        if (fabs(MuInfos[i][Mzvtx] - MuInfos[j][Mzvtx]) < VertexZPosWindow)
         //== check track chi2 at vertex
         if (fabs(MuInfos[i][Mvtxchi2] - MuInfos[j][Mvtxchi2]) < VertexChiWindow)
          //== check event centrality
          if (fabs(MuInfos[i][MbbcCentrality] - MuInfos[j][MbbcCentrality])<CentWindow) {
        
            MuInfos[i][Mfreq]++; MuInfos[j][Mfreq]++;
            //== vertex
            varNtuple[0] = (MuInfos[i][MBBCzvertex]    + MuInfos[j][MBBCzvertex])/2;
            //== centrality
            varNtuple[1] = (MuInfos[i][MbbcCentrality] + MuInfos[j][MbbcCentrality])/2;
            //== mass
            varNtuple[2] = Tools::invMass(Const::MUMASS,MuInfos[i][Mpxvtx],MuInfos[i][Mpyvtx],MuInfos[i][Mpzvtx],
                       Const::MUMASS,MuInfos[j][Mpxvtx],MuInfos[j][Mpyvtx],MuInfos[j][Mpzvtx]);
            //== charge
            varNtuple[3] = ((int)MuInfos[i][Mcharge] + (int)MuInfos[j][Mcharge])/2;
            //== rapidity
             varNtuple[4] = Tools::rapidity(varNtuple[2],
                MuInfos[i][Mpxvtx]+MuInfos[j][Mpxvtx],
                MuInfos[i][Mpyvtx]+MuInfos[j][Mpyvtx],
                MuInfos[i][Mpzvtx]+MuInfos[j][Mpzvtx]);
            //== transverse momentum
            varNtuple[5] = Tools::pT(MuInfos[i][Mpxvtx]+MuInfos[j][Mpxvtx],MuInfos[i][Mpyvtx]+MuInfos[j][Mpyvtx]);
            //== momentum
            varNtuple[6] = Tools::p(MuInfos[i][Mpxvtx]+MuInfos[j][Mpxvtx],
                MuInfos[i][Mpyvtx]+MuInfos[j][Mpyvtx],
                MuInfos[i][Mpzvtx]+MuInfos[j][Mpzvtx]);
            //== vtxchi2
            varNtuple[7] = (MuInfos[i][Mvtxchi2] + MuInfos[j][Mvtxchi2])/2 ;
            //== Tr0_chi2
            varNtuple[8] = MuInfos[i][Mtrackchi2];
            //== Tr1_chi2
            varNtuple[9] = MuInfos[j][Mtrackchi2];
            //== Tr0_iquad
            varNtuple[10] = MuInfos[i][Miquad];
            //== Tr1_iquad
            varNtuple[11] = MuInfos[j][Miquad];
            //== Tr0_DG0
            varNtuple[12] = MuInfos[i][MDG0];
            //== Tr1_DG0
            varNtuple[13] = MuInfos[j][MDG0];
            //== Tr0_DDG0
            varNtuple[14] = MuInfos[i][MDDG0];
            //== Tr1_DDG0
            varNtuple[15] = MuInfos[j][MDDG0];
            // Tr0_px
            varNtuple[16] = MuInfos[i][Mpxvtx];
            // Tr0_py
            varNtuple[17] = MuInfos[i][Mpyvtx];
            // Tr0_pz
            varNtuple[18] = MuInfos[i][Mpzvtx];
            // Tr1_px
            varNtuple[19] = MuInfos[j][Mpxvtx];
            // Tr1_py
            varNtuple[20] = MuInfos[j][Mpyvtx];
            // Tr1_pz
            varNtuple[21] = MuInfos[j][Mpzvtx];
            // Tr0_idhits
            varNtuple[22] = MuInfos[i][Midhits];
            // Tr1_idhits
            varNtuple[23] = MuInfos[j][Midhits]; 
            // Tr0_trhits
            varNtuple[24] = MuInfos[i][Mtrhits];
            // Tr1_trhits
            varNtuple[25] = MuInfos[j][Mtrhits]; 
            evtMix->Fill(varNtuple);
            produced++;
            if (produced%10000==0) {
             cout<<"Making background dimuons : "<<produced<<" dimuons \r"; 
             cout.flush();
            }
          }
           cout<<endl<<">>> Ending background computing : "<<produced<<" fake dimuons created <<<"<<endl;
  return 0;
}

