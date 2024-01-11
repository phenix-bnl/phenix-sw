#include "SimpleHistogrammer.hh"

#include "DchMcRecoTrack.hh"
#include "utiMatch.h"
#include "BbcOut.h"
#include "CglTrack.h"  
#include "PHTrackOut.h"
#include "DchTrack.h"
#include "TecOut.hh"
#include "dTofReconstructedWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "PHCompositeNode.h"
#include "McEvalSingleList_v1.h"
#include "primaryWrapper.h"
#include "dPadClusterWrapper.h"
#include "PHTrackOut.h"
#include "BbcOut.h"
#include "CglTrack.h"
#include "utiHadPid.h"

#include <cstdlib>
#include <iostream>

using namespace std;

SimpleHistogrammer* SimpleHistogrammer::_instance =0;
SimpleHistogrammer::SimpleHistogrammer()
{
  dowest = doeast = 1;
  run =-1;
  mat=new utiMatch;
  Pid = new utiHadPid;
  momcut=0.5;
  changematch =0;
  flagInit = 0;
  filename = "hijing.root";
  ntupleList = new TList();
  histoList  = new TList();
}

SimpleHistogrammer::~SimpleHistogrammer()
{
  if(Datafile){
    Datafile->Close();
    delete Datafile;
  }
  ntupleList->Delete();
  histoList->Delete();
  delete ntupleList;
  delete histoList;
}

SimpleHistogrammer* SimpleHistogrammer::instance()
{
  if(!_instance)
    _instance = new SimpleHistogrammer();
  return _instance;
}

bool SimpleHistogrammer::setFileName(TString name="nt.root"){
  if (!flagInit) {
    if(name.Length()>0){
      filename = name;
    }
    return true;
  }
  return false;
}

int SimpleHistogrammer::initializeFile(char* name)
{
  if (!flagInit) {
    if(name){
      filename = name;
    }
    cout << "SimpleHistogrammer file name is : " << filename << endl;
    Datafile = new TFile(filename,"RECREATE",filename);
    flagInit = 1;

    single = new TNtuple("single","single","evt:run:bbcz:pt:phi:zed:the0:phi0:"
			 "alpha:alpha1:alpha2:beta:quality:tecmc:ecorr:ecore:prob:emcchi2:plemc:"
			 "brp:nap:brz:naz:pc2sdp:pc2sdz:pc3sdp:pc3sdz:emcsdp:emcsdz:"
			 "n0:npe0:n1:npe1:crkchi2:crkdisp:nx1hits:nx2hits:mx1dist:mx2dist:"
			 "rvtx:zvtx:gen:partID:parID:xpurity:papaid:paid:papt:paphi:pathe:panum:"
			 "sibpt1:sibphi1:sibthe1:siba1:sibid1:sibpt2:sibphi2:sibthe2:siba2:sibid2:"
			 "sibpt3:sibphi3:sibthe3:siba3:sibid3:"
			 "meta:mphi0:mthe0:mp:mpt:good:moma:momb:Pid:Pphi:Pthe:Ppt:"
			 "tofsdz:tofsdp:m2:ttof:eloss:tofz:tofphi:pltof:slat:isPi:isK:isP");
    primary = new TNtuple("primary","primary","id:pt:p:phi:the:bbcz");
  }
  return 0;
}
int SimpleHistogrammer::saveToFile(int option)
{
  initializeFile();
  flush();
  cout<<"flush "<<endl;
  return 0;
}

int  SimpleHistogrammer::addNtuple(TNtuple* val)
{
  initializeFile();
  ntupleList->Add(val);
  return 1;
}

int SimpleHistogrammer::addHistogram(TH1*val)
{
  initializeFile();
  histoList->Add(val);
  return 1;
}

void SimpleHistogrammer::flush()
{
  TFile *tmp=Datafile;
  if(gFile!=Datafile){
    tmp = gFile;
    Datafile->cd();
  }
  TObject * obj;

  TIter ntuple(ntupleList);
  while((obj = ntuple()))
    obj->Write(0,TObject::kOverwrite);

  TIter histo(histoList);
  while((obj = histo()))
    obj->Write(0,TObject::kOverwrite);
  single->Write(0,TObject::kOverwrite);
  primary->Write(0,TObject::kOverwrite);
  Datafile->Flush();
  if(tmp) tmp->cd();
}
void SimpleHistogrammer::fillCompleteEvaluation(PHCompositeNode *root) 
{
  static TNtuple* completeEvaluation=0;
  PHNodeIterator iter(root);
  
  PHDataNode<PHPointerList<DchMcRecoTrack> >* tmpDchMcRecoTrackNode =
    (PHDataNode<PHPointerList<DchMcRecoTrack> >*)iter.findFirst("PHDataNode","DchMcRecoTrack");
  if(!tmpDchMcRecoTrackNode){
    cout<<"No Mc Reco Track "<<endl;
    return;
  }
  
  PHPointerList<DchMcRecoTrack>* mcRecoTrackList = tmpDchMcRecoTrackNode->getData();
  
  if(!completeEvaluation){
    initializeFile();
    TFile *tmp=0;
    if(gFile!=Datafile){
      tmp = gFile;
      Datafile->cd();
    }
    
    completeEvaluation = new TNtuple("completeEvaluation","completeEvaluation","eventID:eventxvtx:eventyvtx:eventzvtx:perfID:perfQual:momentumG:theta:phi:theta0G:phi0G:alphaG:betaG:zedG:generation:particleID:parentID:primaryID:rvtx:zvtx:phivtx:recoID:recoQual:momentumR:theta0R:phi0R:xhits:uvhits:mulcontrib:xmulcontrib:uvmulcontrib:mainID:xmainID:uvmainID:ambig:xambig:uvambig:purity:xpurity:uvpurity:dalpha:dbeta:dphi:dzed:ddist:sumfound:solution:perfDvertex:recoDvertex:chi2:numHitsFit:dalphaMin:dphiMin:avDist:gid:pc1recoid:pc2recoid:pc3recoid:pc1mcid:pc2mcid:pc3mcid:xpc1r:ypc1r:zpc1r:xpc1m:ypc1m:zpc1m:xpc2r:ypc2r:zpc2r:xpc2m:ypc2m:zpc2m:xpc3r:ypc3r:zpc3r:xpc3m:ypc3m:zpc3m:xpc1pro:ypc1pro:zpc1pro:xpc2pro:ypc2pro:zpc2pro:xpc3pro:ypc3pro:zpc3pro:pc13vtxm:pc13vtxr:bbcvtx:bbct0:tofrecoid:tofmcid:xtofr:ytofr:ztofr:xtofm:ytofm:ztofm:xtofpro:ytofpro:ztofpro:tofpath:toftofr:toftofm:elosstofr:elosstofm:pidtofr:pidtofm:emcrecoid:xemcreco:yemcreco:zemcreco:emcswkey:emcmease:emcecore:emcecorr:emcecent:emctof:emctofcorr:emctofmin:emcprobphot:twrhit:emcchi2:emcpartesum0:emcpartesum1:emcpartesum2:emcpartesum3:emcanctrkno0:emcanctrkno1:emcanctrkno2:emcanctwrhit0:emcanctwrhit1:emcanctwrhit2:emcancpid0:emcancpid1:emcancpid2:emcancedep0:emcancedep1:emcancedep2:emcancptot0:emcancptot1:emcancptot2:xemcproj:yemcproj:zemcproj:pathlemc:emcmcid:xemcmc:yemcmc:zemcmc:emcmcefrac:emcmcecore:emcmcmease:emcmctof:crkacc:crknpmt0:crknpmt1:crknpmt3:crknpe0:crknpe1:crknpe3:crkchi2:crkdisp:crkpath:ntrkG:ntrkR:sigpc1:sigpc1p:sigpc1z:delpc1p:delpc1z:sigpc2:sigpc2p:sigpc2z:delpc2p:delpc2z:sigpc3:sigpc3p:sigpc3z:delpc3p:delpc3z:sigtof:sigtofp:sigtofz:deltofp:deltofz:sigemc:sigemcp:sigemcz:delemcp:delemcz:a1:a2:a1G:a2G:nx1:nx2:nx1G:nx2G:mdist1:mdist2:mdist1G:mdist2G:chi21:chi22:chi21G:chi22G");
    addNtuple(completeEvaluation);
    if(tmp)tmp->cd();
  }
  float array[300];
  DchMcRecoTrack* track;

  int totalMcRecoTracks = mcRecoTrackList->length();
  for (int i = 0; i< totalMcRecoTracks; i++ ) {
    track = (*mcRecoTrackList)[i];
    if(!track) continue;
    int m =0;
    array[m++] = track->get_eventID();
    array[m++] = track->get_eventxvtx();
    array[m++] = track->get_eventyvtx();
    array[m++] = track->get_eventzvtx();
    array[m++] = track->get_perfID();
    array[m++] = track->get_perfQual();
    array[m++] = track->get_momentumG();
    array[m++] = track->get_theta();
    array[m++] = track->get_phi();
    array[m++] = track->get_theta0G();
    array[m++] = track->get_phi0G();
    array[m++] = track->get_alphaG();
    array[m++] = track->get_betaG(); 
    array[m++] = track->get_zedG();
    array[m++] = track->get_generation();
    array[m++] = track->get_particleID();
    array[m++] = track->get_parentID();
    array[m++] = track->get_primaryID();
    array[m++] = track->get_rvtx(); 
    array[m++] = track->get_zvtx();
    array[m++] = track->get_phivtx();
    array[m++] = track->get_recoID();
    array[m++] = track->get_recoQual();
    array[m++] = track->get_momentumR();
    array[m++] = track->get_theta0R();
    array[m++] = track->get_phi0R(); 
    array[m++] = track->get_xhits();
    array[m++] = track->get_uvhits();
    array[m++] = track->get_mulcontrib();
    array[m++] = track->get_xmulcontrib();
    array[m++] = track->get_uvmulcontrib();
    array[m++] = track->get_mainID(); 
    array[m++] = track->get_xmainID(); 
    array[m++] = track->get_uvmainID();
    array[m++] = track->get_ambig(); 
    array[m++] = track->get_xambig();
    array[m++] = track->get_uvambig(); 
    array[m++] = track->get_purity(); 
    array[m++] = track->get_xpurity();
    array[m++] = track->get_uvpurity();
    array[m++] = track->get_dalpha();
    array[m++] = track->get_dbeta();
    array[m++] = track->get_dphi();
    array[m++] = track->get_dzed(); 
    array[m++] = track->get_ddist(); 
    array[m++] = track->get_sumfound(); 
    array[m++] = track->get_solution(); 
    array[m++] = track->get_perfDvertex(); 
    array[m++] = track->get_recoDvertex();
    array[m++] = track->get_chi2(); 
    array[m++] = track->get_numHitsFit(); 
    array[m++] = track->get_dalphaMin(); 
    array[m++] = track->get_dphiMin();
    array[m++] = track->get_avDist();
    array[m++] = track->get_idGeantTrack();
    array[m++] = track->get_pc1RecoId();
    array[m++] = track->get_pc2RecoId();
    array[m++] = track->get_pc3RecoId(); 
    array[m++] = track->get_pc1McId(); 
    array[m++] = track->get_pc2McId(); 
    array[m++] = track->get_pc3McId(); 
    array[m++] = track->get_xPc1Reco(); 
    array[m++] = track->get_yPc1Reco(); 
    array[m++] = track->get_zPc1Reco(); 
    array[m++] = track->get_xPc1Mc(); 
    array[m++] = track->get_yPc1Mc();
    array[m++] = track->get_zPc1Mc(); 
    array[m++] = track->get_xPc2Reco(); 
    array[m++] = track->get_yPc2Reco(); 
    array[m++] = track->get_zPc2Reco(); 
    array[m++] = track->get_xPc2Mc(); 
    array[m++] = track->get_yPc2Mc();
    array[m++] = track->get_zPc2Mc(); 
    array[m++] = track->get_xPc3Reco();
    array[m++] = track->get_yPc3Reco(); 
    array[m++] = track->get_zPc3Reco(); 
    array[m++] = track->get_xPc3Mc();
    array[m++] = track->get_yPc3Mc(); 
    array[m++] = track->get_zPc3Mc();
    array[m++] = track->get_xPc1Proj();
    array[m++] = track->get_yPc1Proj();
    array[m++] = track->get_zPc1Proj();
    array[m++] = track->get_xPc2Proj();
    array[m++] = track->get_yPc2Proj();
    array[m++] = track->get_zPc2Proj();
    array[m++] = track->get_xPc3Proj();
    array[m++] = track->get_yPc3Proj();
    array[m++] = track->get_zPc3Proj();
    array[m++] = track->get_pc13vtxm();
    array[m++] = track->get_pc13vtxr();
    array[m++] = track->get_bbcvtx();
    array[m++] = track->get_bbct0();
    array[m++] = track->get_tofRecoId();
    array[m++] = track->get_tofMcId();
    array[m++] = track->get_xTofReco();
    array[m++] = track->get_yTofReco();
    array[m++] = track->get_zTofReco();
    array[m++] = track->get_xTofMc();
    array[m++] = track->get_yTofMc();
    array[m++] = track->get_zTofMc();
    array[m++] = track->get_xTofProj();
    array[m++] = track->get_yTofProj();
    array[m++] = track->get_zTofProj();
    array[m++] = track->get_pathlTof();
    array[m++] = track->get_tofTofReco();
    array[m++] = track->get_tofTofMc();
    array[m++] = track->get_elossTofReco();
    array[m++] = track->get_elossTofMc();
    array[m++] = track->get_pidTofReco();
    array[m++] = track->get_pidTofMc();
    array[m++] = track->get_emcRecoId();
    array[m++] = track->get_xEmcReco();
    array[m++] = track->get_yEmcReco();
    array[m++] = track->get_zEmcReco();
    array[m++] = track->get_emcswkey();
    array[m++] = track->get_emcmease();
    array[m++] = track->get_emcecore();
    array[m++] = track->get_emcecorr();
    array[m++] = track->get_emcecent();
    array[m++] = track->get_emctof();
    array[m++] = track->get_emctofcorr();
    array[m++] = track->get_emctofmin();
    array[m++] = track->get_emcprobphot();
    array[m++] = track->get_twrhit();
    array[m++] = track->get_emcchi2();
    array[m++] = track->get_emcpartesum0();
    array[m++] = track->get_emcpartesum1();
    array[m++] = track->get_emcpartesum2();
    array[m++] = track->get_emcpartesum3();
    array[m++] = track->get_emcAnctrkno0();
    array[m++] = track->get_emcAnctrkno1();
    array[m++] = track->get_emcAnctrkno2();
    array[m++] = track->get_emcAnctwrhit0();
    array[m++] = track->get_emcAnctwrhit1();
    array[m++] = track->get_emcAnctwrhit2();
    array[m++] = track->get_emcAncpid0();
    array[m++] = track->get_emcAncpid1();
    array[m++] = track->get_emcAncpid2();
    array[m++] = track->get_emcAncedep0();
    array[m++] = track->get_emcAncedep1();
    array[m++] = track->get_emcAncedep2();
    array[m++] = track->get_emcAncptot0();
    array[m++] = track->get_emcAncptot1();
    array[m++] = track->get_emcAncptot2();
    array[m++] = track->get_xEmcProj();
    array[m++] = track->get_yEmcProj();
    array[m++] = track->get_zEmcProj();
    array[m++] = track->get_pathlEmc();
    array[m++] = track->get_emcMcId();
    array[m++] = track->get_xEmcMc();
    array[m++] = track->get_yEmcMc();
    array[m++] = track->get_zEmcMc();
    array[m++] = track->get_emcMcefrac();
    array[m++] = track->get_emcMcecore();
    array[m++] = track->get_emcMcmease();
    array[m++] = track->get_emcMctof();
    array[m++] = track->get_crkacc();
    array[m++] = track->get_crknpmt0();
    array[m++] = track->get_crknpmt1();
    array[m++] = track->get_crknpmt3();
    array[m++] = track->get_crknpe0();
    array[m++] = track->get_crknpe1();
    array[m++] = track->get_crknpe3();
    array[m++] = track->get_crkchi2();
    array[m++] = track->get_crkdisp();
    array[m++] = track->get_crkpath();
    array[m++] = track->ntrkG;
    array[m++] = track->ntrkR;
    array[m++] = track->sigpc1;
    array[m++] = track->sigpc1p;
    array[m++] = track->sigpc1z;
    array[m++] = track->delpc1p;
    array[m++] = track->delpc1z;
    array[m++] = track->sigpc2;
    array[m++] = track->sigpc2p;
    array[m++] = track->sigpc2z;
    array[m++] = track->delpc2p;
    array[m++] = track->delpc2z;
    array[m++] = track->sigpc3;
    array[m++] = track->sigpc3p;
    array[m++] = track->sigpc3z;
    array[m++] = track->delpc3p;
    array[m++] = track->delpc3z;
    array[m++] = track->sigtof;
    array[m++] = track->sigtofp;
    array[m++] = track->sigtofz;
    array[m++] = track->deltofp;
    array[m++] = track->deltofz;
    array[m++] = track->sigemc;
    array[m++] = track->sigemcp;
    array[m++] = track->sigemcz;
    array[m++] = track->delemcp;
    array[m++] = track->delemcz;
    array[m++] = track->alpha1;
    array[m++] = track->alpha2;
    array[m++] = track->alpha1G;
    array[m++] = track->alpha2G;
    array[m++] = track->nx1;
    array[m++] = track->nx2;
    array[m++] = track->nx1G;
    array[m++] = track->nx2G;
    array[m++] = track->mdist1;
    array[m++] = track->mdist2;
    array[m++] = track->mdist1G;
    array[m++] = track->mdist2G;
    array[m++] = track->chi21;
    array[m++] = track->chi22;
    array[m++] = track->chi21G;
    array[m++] = track->chi22G;
    completeEvaluation->Fill(array);
  }
}
void SimpleHistogrammer::setup(PHCompositeNode *topNode){
  cout<<"here1"<<endl;
 //-------------------------------------
  PHTypedNodeIterator<McEvalSingleList_v1> singleiter(topNode);
  PHIODataNode <McEvalSingleList_v1> *SingleMicroNode = singleiter.find("McSingle");
  if (SingleMicroNode) {
    mcsingle = SingleMicroNode->getData();
  } 
  //------------------------- 
 PHNodeIterator iter(topNode);
 PHIODataNode<TObject> *padNode2,*padNode3,*tofNode,*emcNode,*dchNode,*primNode,*projNode,*cglNode,*bbcNode;

 primNode     = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","primary");
 dchNode     = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","DchTrack");
 padNode2      = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc2Cluster");
 padNode3      = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc3Cluster");
 tofNode   = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dTofReconstructed");
 emcNode   = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dEmcClusterLocalExt");
 cglNode   = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","CglTrack");
 projNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","PHTrackOut");
 bbcNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","BbcOut");
 
 d_dctrk   = (DchTrack*)(dchNode->getData());
 d_pc2     = (dPadClusterWrapper*)(padNode2->getData());
 d_pc3     = (dPadClusterWrapper*)(padNode3->getData());
 d_tof     = (dTofReconstructedWrapper*)(tofNode->getData());
 d_emc     = (dEmcClusterLocalExtWrapper*)(emcNode->getData());
 d_cgl     = (CglTrack*)(cglNode->getData());
 d_proj    = (PHTrackOut*)(projNode->getData());
 d_primary = (primaryWrapper*)(primNode->getData());
 d_bbc     = (BbcOut*)(bbcNode->getData());
 bbcz      = d_bbc->get_VertexPoint();
 cout<<"here2"<<endl;
 static int aaa=0;
 if(!aaa){
   aaa =1;
   topNode->print();
 }
 Fill();
}
void SimpleHistogrammer::Fill(){
  int k;
  int MCNTRACK=0;
  int MCRECONTRACK=0;
  if (mcsingle) {
    k=0;
    MCNTRACK = mcsingle->get_McEvalSingleTrackN();
    for (int i =0; i < MCNTRACK; i++) {
      McTrk_EVENTID[i]              = mcsingle->get_eventid(i);           
      McTrk_NRECO[i]                = mcsingle->get_Nreco(i)  ;             
      int jtrk = mcsingle->get_Nreco(i);
      for(int j=0;j<jtrk;j++){
	if(j==0) McTrk_RECOID1[i]   = mcsingle->get_recoid(i,j);
	if(j==1) McTrk_RECOID2[i]   = mcsingle->get_recoid(i,j);
	if(j==2) McTrk_RECOID3[i]   = mcsingle->get_recoid(i,j);
      }
      McTrk_MCTRACKID[i]            = i;
      McTrk_GENERATION[i]           = mcsingle->get_generation(i);
      McTrk_PARTICLEID[i]           = mcsingle->get_particleid(i);        
      McTrk_PARENTID[i]             = mcsingle->get_parentid(i);          
      McTrk_PRIMARYID[i]            = mcsingle->get_primaryid(i);         
      McTrk_VERTEXX[i]              = mcsingle->get_vertexx(i);           
      McTrk_VERTEXY[i]              = mcsingle->get_vertexy(i);           
      McTrk_VERTEXZ[i]              = mcsingle->get_vertexz(i);           
      McTrk_PARENTVERTEXX[i]        = mcsingle->get_parentvertexx(i);     
      McTrk_PARENTVERTEXY[i]        = mcsingle->get_parentvertexy(i);     
      McTrk_PARENTVERTEXZ[i]        = mcsingle->get_parentvertexz(i);     
      McTrk_PRIMARYVERTEXX[i]       = mcsingle->get_primaryvertexx(i);    
      McTrk_PRIMARYVERTEXY[i]       = mcsingle->get_primaryvertexy(i);    
      McTrk_PRIMARYVERTEXZ[i]       = mcsingle->get_primaryvertexz(i);    
      McTrk_MOMENTUMX[i]            = mcsingle->get_momentumx(i);         
      McTrk_MOMENTUMY[i]            = mcsingle->get_momentumy(i);         
      McTrk_MOMENTUMZ[i]            = mcsingle->get_momentumz(i);         
      McTrk_PARENTMOMENTUMX[i]      = mcsingle->get_parentmomentumx(i);   
      McTrk_PARENTMOMENTUMY[i]      = mcsingle->get_parentmomentumy(i);   
      McTrk_PARENTMOMENTUMZ[i]      = mcsingle->get_parentmomentumz(i);   
      McTrk_PRIMARYMOMENTUMX[i]     = mcsingle->get_primarymomentumx(i);  
      McTrk_PRIMARYMOMENTUMY[i]     = mcsingle->get_primarymomentumy(i);  
      McTrk_PRIMARYMOMENTUMZ[i]     = mcsingle->get_primarymomentumz(i);  
      McTrk_QUALITY[i]              = mcsingle->get_quality(i);           
      McTrk_THETA0[i]               = mcsingle->get_theta0(i);            
      McTrk_PHI0[i]                 = mcsingle->get_phi0(i);              
      McTrk_PHI[i]                  = mcsingle->get_phi(i);               
      McTrk_ALPHA[i]                = mcsingle->get_alpha(i);             
      McTrk_ZED[i]                  = mcsingle->get_zed(i);               
      McTrk_BETA[i]                 = mcsingle->get_beta(i);

      for(int j=0;j<jtrk;j++){
	McRecoTrk_EVENTID[k]    =      mcsingle->get_eventid(i);
	McRecoTrk_MCINDEX[k]    =      i;
	McRecoTrk_RECOID[k]     =      mcsingle->get_recoid(i,j); 
	McRecoTrk_QUALITY[k]    =      mcsingle->get_quality(i,j);
	McRecoTrk_MOMENTUM[k]   =      mcsingle->get_momentum(i,j);
	McRecoTrk_THETA0[k]     =      mcsingle->get_theta0(i,j); 
	McRecoTrk_PHI0[k]       =      mcsingle->get_phi0(i,j);   
	McRecoTrk_PHI[k]        =      mcsingle->get_phi(i,j);
	McRecoTrk_ALPHA[k]      =      mcsingle->get_alpha(i,j);  
	McRecoTrk_ZED[k]        =      mcsingle->get_zed(i,j);
	McRecoTrk_BETA[k]       =      mcsingle->get_beta(i,j);   
	McRecoTrk_AVERAGETIME[k]=      mcsingle->get_averagetime(i,j);
	McRecoTrk_XHITS[k]      =      mcsingle->get_xhits(i,j);  
	McRecoTrk_UVHITS[k]     =      mcsingle->get_uvhits(i,j); 
	McRecoTrk_MULMAIN[k]    =      mcsingle->get_mulmain(i,j);
	McRecoTrk_MULXMAIN[k]   =      mcsingle->get_mulxmain(i,j);
	McRecoTrk_MULUVMAIN[k]  =      mcsingle->get_muluvmain(i,j);
	McRecoTrk_MAIN[k]       =      mcsingle->get_main(i,j);
	McRecoTrk_XMAIN[k]      =      mcsingle->get_xmain(i,j);  
	McRecoTrk_UVMAIN[k]     =      mcsingle->get_uvmain(i,j); 
	McRecoTrk_AMBIGUITY[k]  =      mcsingle->get_ambiguity(i,j);
	McRecoTrk_PURITY[k]     =      mcsingle->get_purity(i,j); 
	McRecoTrk_XPURITY[k]    =      mcsingle->get_xpurity(i,j);
	McRecoTrk_UVPURITY[k]   =      mcsingle->get_uvpurity(i,j);
	McRecoTrk_PC1CLUSID[k]  =      mcsingle->get_pc1clusid(i,j);
	McRecoTrk_PC2CLUSID[k]  =      mcsingle->get_pc2clusid(i,j);
	McRecoTrk_PC3CLUSID[k]  =      mcsingle->get_pc3clusid(i,j);
	McRecoTrk_PC1CLUSIDTRUE[k] =   mcsingle->get_pc1clusidtrue(i,j);
	McRecoTrk_PC2CLUSIDTRUE[k] =   mcsingle->get_pc2clusidtrue(i,j);
	McRecoTrk_PC3CLUSIDTRUE[k] =   mcsingle->get_pc3clusidtrue(i,j);
	McRecoTrk_PC1CLUSIDG[k] =      mcsingle->get_pc1clusidg(i,j);
	McRecoTrk_PC2CLUSIDG[k] =      mcsingle->get_pc2clusidg(i,j);
	McRecoTrk_PC3CLUSIDG[k] =      mcsingle->get_pc3clusidg(i,j);
	McRecoTrk_PC1POINTXG[k] =      mcsingle->get_pc1pointxg(i,j);
	McRecoTrk_PC2POINTXG[k] =      mcsingle->get_pc2pointxg(i,j);
	McRecoTrk_PC3POINTXG[k] =      mcsingle->get_pc3pointxg(i,j);
	McRecoTrk_PC1POINTYG[k] =      mcsingle->get_pc1pointyg(i,j);
	McRecoTrk_PC2POINTYG[k] =      mcsingle->get_pc2pointyg(i,j);
	McRecoTrk_PC3POINTYG[k] =      mcsingle->get_pc3pointyg(i,j);
	McRecoTrk_PC1POINTZG[k] =      mcsingle->get_pc1pointzg(i,j);
	McRecoTrk_PC2POINTZG[k] =      mcsingle->get_pc2pointzg(i,j);
	McRecoTrk_PC3POINTZG[k] =      mcsingle->get_pc3pointzg(i,j);
	McRecoTrk_TOFID[k]      =      mcsingle->get_tofid(i,j);  
	McRecoTrk_TOFIDTRUE[k]  =      mcsingle->get_tofidtrue(i,j);
	McRecoTrk_TOFIDG[k]     =      mcsingle->get_tofidg(i,j); 
	McRecoTrk_TOFPOINTXG[k] =      mcsingle->get_tofpointxg(i,j);
	McRecoTrk_TOFPOINTYG[k] =      mcsingle->get_tofpointyg(i,j);
	McRecoTrk_TOFPOINTZG[k] =      mcsingle->get_tofpointzg(i,j);
	McRecoTrk_TOFG[k]       =      mcsingle->get_tofg(i,j);   
	McRecoTrk_TOFELOSSG[k]  =      mcsingle->get_tofelossg(i,j);
	McRecoTrk_EMCCLUSID[k]  =      mcsingle->get_emcclusid(i,j);
	McRecoTrk_EMCCLUSIDTRUE[k]=    mcsingle->get_emcclusidtrue(i,j);
	McRecoTrk_EMCCLUSIDG[k] =      mcsingle->get_emcclusidg(i,j);
	McRecoTrk_EMCANCTRK0[k] =      mcsingle->get_emcanctrk0(i,j);
	McRecoTrk_EMCANCTRK1[k] =      mcsingle->get_emcanctrk1(i,j);
	McRecoTrk_EMCANCTRK2[k] =      mcsingle->get_emcanctrk2(i,j);
	McRecoTrk_EMCANCTWRHIT0[k] =      mcsingle->get_emcanctwrhit0(i,j);
	McRecoTrk_EMCANCTWRHIT1[k] =      mcsingle->get_emcanctwrhit1(i,j);
	McRecoTrk_EMCANCTWRHIT2[k] =      mcsingle->get_emcanctwrhit2(i,j);
	McRecoTrk_EMCANCPID0[k] =      mcsingle->get_emcancpid0(i,j);
	McRecoTrk_EMCANCPID1[k] =      mcsingle->get_emcancpid1(i,j);
	McRecoTrk_EMCANCPID2[k] =      mcsingle->get_emcancpid2(i,j);
	McRecoTrk_EMCANCEDEP0[k] =      mcsingle->get_emcancedep0(i,j);
	McRecoTrk_EMCANCEDEP1[k] =      mcsingle->get_emcancedep1(i,j);
	McRecoTrk_EMCANCEDEP2[k] =      mcsingle->get_emcancedep2(i,j);
	McRecoTrk_EMCANCPTOT0[k] =      mcsingle->get_emcancptot0(i,j);
	McRecoTrk_EMCANCPTOT1[k] =      mcsingle->get_emcancptot1(i,j);
	McRecoTrk_EMCANCPTOT2[k] =      mcsingle->get_emcancptot2(i,j);
	McRecoTrk_EMCPOINTXG[k]  =      mcsingle->get_emcpointxg(i,j);
	McRecoTrk_EMCPOINTYG[k]  =      mcsingle->get_emcpointyg(i,j);
	McRecoTrk_EMCPOINTZG[k]  =      mcsingle->get_emcpointzg(i,j);
	McRecoTrk_EMCEFRACG[k]   =      mcsingle->get_emcefracg(i,j);
	McRecoTrk_EMCECOREG[k]   =      mcsingle->get_emcecoreg(i,j);
	McRecoTrk_EMCMEASEG[k]   =      mcsingle->get_emcmeaseg(i,j);
	McRecoTrk_EMCTOFG[k]     =      mcsingle->get_emctofg(i,j);
	McRecoTrk_CRKACC[k]      =      mcsingle->get_crkacc(i,j); 
	McRecoTrk_CRKNPMT0[k]    =      mcsingle->get_crknpmt0(i,j);
	McRecoTrk_CRKNPMT1[k]    =      mcsingle->get_crknpmt1(i,j);
	McRecoTrk_CRKNPMT3[k]    =      mcsingle->get_crknpmt3(i,j);
	McRecoTrk_CRKNPE0[k]     =      mcsingle->get_crknpe0(i,j);
	McRecoTrk_CRKNPE1[k]     =      mcsingle->get_crknpe1(i,j);
	McRecoTrk_CRKNPE3[k]     =      mcsingle->get_crknpe3(i,j);
	McRecoTrk_CRKCHI2[k]     =      mcsingle->get_crkchi2(i,j);
	McRecoTrk_CRKDISP[k]     =      mcsingle->get_crkdisp(i,j);
	McRecoTrk_CRKPATH[k]     =      mcsingle->get_crkpath(i,j);              
	k++;
      }
    }
    MCRECONTRACK = k;
  }
  int ntrk = d_cgl->get_CglNTrack();
  for(int icgl=0;icgl<ntrk;icgl++){
    int dchid = d_cgl->get_dctracksid(icgl);
    int ipc2  = d_cgl->get_pc2clusid (icgl);	
    int ipc3  = d_cgl->get_pc3clusid (icgl);
    int iemc  = d_cgl->get_emcclusid (icgl);
    int itof  = d_cgl->get_tofrecid  (icgl);
    if(dchid != icgl){
      cout<<"bad !"<<endl;
      exit(1);
    }
    float  alpha = d_dctrk->get_alpha (icgl);
    float momch = 100.0;
    float mom = d_dctrk->get_momentum(icgl);
    int charge=0;
    if(alpha>0) charge =-1;
    else if(alpha<0) charge =1;
    if(charge!=0) momch    = mom/charge;
 
    Central_dcarm[icgl]    = d_dctrk->get_arm    (icgl); 
    Central_charge[icgl]   = charge;
    Central_quality[icgl]  = d_dctrk->get_quality (icgl); 
    Central_zed[icgl]      = d_dctrk->get_zed     (icgl); 
    Central_phi[icgl]      = d_dctrk->get_phi     (icgl); 
    Central_alpha[icgl]    = d_dctrk->get_alpha   (icgl); 
    Central_alpha1[icgl]   = d_dctrk->get_alpha1   (icgl); 
    Central_alpha2[icgl]   = d_dctrk->get_alpha2   (icgl); 
    Central_beta[icgl]     = d_dctrk->get_beta    (icgl); 
    Central_phi0[icgl]     = d_dctrk->get_phi0    (icgl); 
    Central_the0[icgl]     = d_dctrk->get_theta0  (icgl); 
    Central_momentum[icgl] = d_dctrk->get_momentum(icgl);
    Central_nx1hits[icgl]  = d_dctrk->get_nx1hits (icgl);
    Central_nx2hits[icgl]  = d_dctrk->get_nx2hits (icgl);
    Central_dist1[icgl]    = d_dctrk->get_dist1   (icgl);
    Central_dist2[icgl]    = d_dctrk->get_dist2   (icgl);
    if(itof>=0){
      float z,phi;
      phi = atan2( d_tof->get_xtof(1,itof), d_tof->get_xtof(0,itof));
      if(phi<-1.57) phi += 6.2831852;
      z = d_tof->get_xtof(2,itof);

      Central_toftof[icgl] = d_tof->get_tof(itof);
      Central_eloss[icgl]  = d_tof->get_eloss(itof);
      Central_tofz[icgl]   = z;
      Central_tofphi[icgl] = phi;
      Central_slat[icgl]   = d_tof->get_slat(itof);
      Central_isPi[icgl]   = Pid->IsPion(charge,Central_toftof[icgl], Central_pltof[icgl],mom);
      Central_isK[icgl]    = Pid->IsKaon(charge,Central_toftof[icgl], Central_pltof[icgl],mom);
      Central_isP[icgl]    = Pid->IsProton(charge,Central_toftof[icgl], Central_pltof[icgl],mom);
    }else{
      Central_toftof[icgl] = -1;
      Central_eloss[icgl]  = -1;      
      Central_tofz[icgl]   = -999;
      Central_tofphi[icgl] = -999;
      Central_slat[icgl]   = -1;
      Central_isPi[icgl]   = -1;
      Central_isK[icgl]    = -1;
      Central_isP[icgl]    = -1;
    }

    Central_ppc2x[icgl]    = d_proj->get_projectionPc2(icgl, 0); 
    Central_ppc2y[icgl]    = d_proj->get_projectionPc2(icgl, 1); 
    Central_ppc2z[icgl]    = d_proj->get_projectionPc2(icgl, 2); 
    Central_ppc3x[icgl]    = d_proj->get_projectionPc3(icgl, 0); 
    Central_ppc3y[icgl]    = d_proj->get_projectionPc3(icgl, 1); 
    Central_ppc3z[icgl]    = d_proj->get_projectionPc3(icgl, 2); 
    Central_pemcx[icgl]    = d_proj->get_projectionEmc(icgl, 0); 
    Central_pemcy[icgl]    = d_proj->get_projectionEmc(icgl, 1); 
    Central_pemcz[icgl]    = d_proj->get_projectionEmc(icgl, 2); 
    Central_ptofx[icgl]    = d_proj->get_projectionTof(icgl, 0); 
    Central_ptofy[icgl]    = d_proj->get_projectionTof(icgl, 1); 
    Central_ptofz[icgl]    = d_proj->get_projectionTof(icgl, 2); 
    Central_pltof[icgl]    = d_proj->get_tofPathLength(icgl); 
    Central_plemc[icgl]    = d_proj->get_emcPathLength(icgl); 
    if (iemc>=0) {
      Central_sect[icgl]  = d_emc->get_sector (iemc);
      Central_temc[icgl]  = d_emc->get_tofcorr(iemc);
      Central_ecorr[icgl] = d_emc->get_ecorr  (iemc) ;
      Central_ecore[icgl] = d_emc->get_ecore  (iemc);
      Central_prob[icgl]  =  d_emc->get_prob_photon(iemc);
      Central_emcchi2[icgl]= d_emc->get_chi2   (iemc);
    }else{
      Central_sect[icgl]  = -9999;
      Central_temc[icgl]  = -9999;
      Central_ecorr[icgl] = -9999;
      Central_ecore[icgl] = -9999;
      Central_prob[icgl]  = -9999;
      Central_emcchi2[icgl]=-9999;
    }
    Central_n0[icgl]    = -9999;
    Central_npe0[icgl]  = -9999;
    Central_n1[icgl]    = -9999;
    Central_npe1[icgl]  = -9999;
    Central_tcrk[icgl]  = -9999;
    Central_disp[icgl]  = -9999;
    // change matching!!!!
    if(changematch==1){
      if(Central_zed[icgl]<0){
	Central_ppc2z[icgl] += -1.55*0.6;
	Central_ppc3z[icgl] += -0.99*0.82;
      }else{
	Central_ppc2z[icgl] += 1.23*0.6;
	Central_ppc3z[icgl] += 1.06*0.82;
      }
    }
    PHAngle rawdphi(0);
    float   rawdz, beta;
    beta    = d_dctrk->get_beta(icgl);
    // PC2 sdphi, sdz
    if (ipc2>=0 && Central_ppc2x[icgl] > -999.) {
      PHAngle phiM( atan2( d_pc2->get_xyz(1,ipc2), d_pc2->get_xyz(0,ipc2) )) ;
      PHAngle phiP( atan2(Central_ppc2y[icgl] , Central_ppc2x[icgl]));
      rawdphi =  phiM-phiP;
      float zM = d_pc2->get_xyz(2,ipc2);
      float zP = Central_ppc2z[icgl];
      rawdz      = zM-zP;

      Central_pc2sdphi[icgl] =  mat->d_PC2_phi_match(momch,rawdphi.getPhi());
      Central_pc2sdz[icgl]   = mat->d_PC2_z_match(momch,rawdz);
    }else {
      Central_pc2sdphi[icgl] = -9999;
      Central_pc2sdz[icgl] = -9999;
    }
	  
    // PC3 sdphi, sdz
    if (ipc3>=0 && Central_ppc3x[icgl] > -999.) {
      PHAngle phiM( atan2( d_pc3->get_xyz(1,ipc3), d_pc3->get_xyz(0,ipc3)) );
      PHAngle phiP( atan2( Central_ppc3y[icgl] , Central_ppc3x[icgl]));
      rawdphi = phiM-phiP;
      float zM = d_pc3->get_xyz(2,ipc3);
      float zP = Central_ppc3z[icgl];
      rawdz      = zM-zP;
      if (Central_ppc3x[icgl]>0) { //West arm
	Central_pc3sdphi[icgl] = mat->d_PC3w_phi_match(momch,rawdphi.getPhi());
	Central_pc3sdz  [icgl] = mat->d_PC3w_z_match(momch,rawdz);
      }
      else {
	Central_pc3sdphi[icgl] = mat->d_PC3e_phi_match(momch,rawdphi.getPhi());
	Central_pc3sdz  [icgl] = mat->d_PC3e_z_match(momch,rawdz);
      }
      //Central_pc3sect  [icgl] = d_pc3->get_sector(ipc3);
    }
    else {
      Central_pc3sdphi[icgl] = -9999;
      Central_pc3sdz  [icgl] = -9999;
      //Central_pc3sect  [icgl] = -9999;
    }
	  
    // EMC sdphi, sdz
    if (iemc>=0 && Central_pemcx[icgl] > -999.) {
      PHAngle phiM( atan2( d_emc->get_xyz(1,iemc), d_emc->get_xyz(0,iemc)) );
      PHAngle phiP( atan2( Central_pemcy[icgl], Central_pemcx[icgl]) );
      rawdphi = phiM-phiP;
      float zM = d_emc->get_xyz(2,iemc);
      float zP = Central_pemcz[icgl];
      rawdz      = zM-zP;
      
      int NUCL =3; // True according to utiMatch
      //int ELEC =1;
      if (Central_dcarm[icgl]>0) { //West arm
	Central_emcsdphi[icgl]   = mat->d_PBSCw_phi_match(momch,rawdphi.getPhi(),NUCL);
	Central_emcsdz  [icgl]   = mat->d_PBSCw_z_match(momch,beta,rawdz,NUCL);
      }
      else {
	if ( Central_dcarm[icgl] ==0 && Central_sect[icgl]>1) {
	  Central_emcsdphi[icgl] = mat->d_PBSCe_phi_match(momch,rawdphi.getPhi(),NUCL);
	  Central_emcsdz  [icgl] = mat->d_PBSCe_z_match(momch,beta,rawdz,NUCL);
	}
	else {
	  Central_emcsdphi[icgl] = mat->d_PBGL_phi_match(momch,rawdphi.getPhi(),NUCL);
	  Central_emcsdz  [icgl] = mat->d_PBGL_z_match(momch,beta,rawdz,NUCL);
	}	      
      }
    }
    else {
      Central_emcsdphi[icgl] = -9999;
      Central_emcsdz  [icgl] = -9999;
    }
    
    // TOF sdphi, sdz
    if (itof>=0 && Central_ptofx[icgl] > -999.) {
      PHAngle phiM( atan2( d_tof->get_xtof(1,itof), d_tof->get_xtof(0,itof)) );
      PHAngle phiP( atan2( Central_ptofy[icgl], Central_ptofx[icgl]) );
      rawdphi = phiM-phiP;
      float zM = d_tof->get_xtof(2,itof);
      float zP = Central_ptofz[icgl];
      rawdz      = zM-zP;
      Central_tofsdphi[icgl] = mat->d_TOF_phi_match(momch,rawdphi.getPhi());
      Central_tofsdz  [icgl] = mat->d_TOF_z_match(momch,rawdz);
      float pl = Central_pltof[icgl];
      float tof = Central_toftof[icgl];
      float aa;
      Central_m2[icgl] = -1;
      if(pl>0&&tof>0){
	aa = tof*29.97924/pl;
	Central_m2[icgl] = mom*mom*(aa*aa - 1);	
      }
    }
    else {
      Central_m2[icgl] = -1;
      Central_tofsdphi[icgl] = -9999;
      Central_tofsdz  [icgl] = -9999;
    }
  }
  
  //

  int panum,count,good,paid,papaid;
  float mom,pt;
  float array[200];
  float mpx,mpy,mpz,mpt;
  float papx,papy,papz,paphi,papt,pathe;
  float sibpx[10],sibpy[10],sibthe0[10],sibphi0[10],sibalp[10],sibpt[10];
  int sibid[10];

  //cout << "-------------------------" << MCNTRACK << endl;
  int found = 0;
  int m;
  if(d_primary){
    for(unsigned i=0;i<d_primary->RowCount();i++){
      mpx =d_primary->get_px_momentum(i);
      mpy =d_primary->get_py_momentum(i);
      mpz =d_primary->get_pz_momentum(i);
      mpt =sqrt(mpx*mpx+mpy*mpy);
      PHAngle phiM(atan2(mpy,mpx));
      
      m=0;
      array[m++] = d_primary->get_idpart(i);
      array[m++] = mpt;//pt
      array[m++] = sqrt(mpt*mpt+mpz*mpz);//p
      array[m++] = phiM.getPhi();
      array[m++] = atan2(mpt,mpz);
      array[m++] = bbcz;
      if(mpt>momcut){
	primary->Fill(array);
      }
    }
  }
  for (int j = 0; j < ntrk; j++) { // central track
    found = 0;
    for (k =0; k < MCRECONTRACK; k++) {
      if(McRecoTrk_RECOID[k] == j){
	if(fabs(Central_momentum[j] - McRecoTrk_MOMENTUM[k])>0.0001)
	  cout<<"Wrong!!!!!!!!!!!!!!!!!!"<<"\n\n\n\n\n"<<endl;
	//can't use McRecoTrk_RECOID[k] == j because CNT have cuts!!// match reco id 
	found = 1;
	int i = McRecoTrk_MCINDEX[k];
	count =0;
	panum = 1;
	papaid =0;
	paid =0;
	papt  = 0;
	paphi = -10;
	pathe = -10;
	for(int l=0;l<10;l++){
	  sibpt[l]  = 0;
	  sibphi0[l] = -10;
	  sibthe0[l] = -10;
	  sibalp[l]  = -10;
	  sibid[l]   = 0;
	}

	mpx    = McTrk_MOMENTUMX[i];
	mpy    = McTrk_MOMENTUMY[i];
	mpz    = McTrk_MOMENTUMZ[i];
	mpt    = sqrt(mpx*mpx+mpy*mpy);
	
	papx  = McTrk_PARENTMOMENTUMX[i];
	papy  = McTrk_PARENTMOMENTUMY[i];
	papz  = McTrk_PARENTMOMENTUMZ[i];
	papt  = sqrt(papx*papx+papy*papy);
	paphi = atan2(papy,papx);
	pathe = atan2(papz,papt)+1.57;
	if(papx>0){
	  for(int n=0;n<MCNTRACK;n++){
	    if(n!= i&&fabs(McTrk_PARENTMOMENTUMX[n]-papx)<0.0001&&fabs(McTrk_PARENTMOMENTUMY[n]-papy)<0.0001&&count<10){
	      sibpx[count] = McTrk_MOMENTUMX[n];
	      sibpy[count] = McTrk_MOMENTUMY[n];
	      sibpt[count] = sqrt(sibpx[count]*sibpx[count]+sibpy[count]*sibpy[count]);
	      sibid[count] = McTrk_PARTICLEID[n];
	      sibphi0[count] = McTrk_PHI0[n];
	      sibthe0[count] = McTrk_THETA0[n];
	      sibalp[count]  = McTrk_ALPHA[n];
	      count++;
	    }
	    panum = count+1;
	  }
	}
	mom = Central_momentum[j];
	pt = Central_momentum[j]*sin(Central_the0[j]);
	if((Central_phi[j]<1.57&&dowest)||(Central_phi[j]>1.57&&doeast)){  
	  if(fabs(mom)>momcut){
	    Float_t brphi =  0.6604*(1.137*Central_pc2sdphi[j] + Central_pc3sdphi[j]);
	    Float_t naphi =  0.6604*(Central_pc2sdphi[j] - 1.137*Central_pc3sdphi[j]);
	    Float_t brz   =  0.524*(1.03*Central_pc2sdz[j]   + Central_pc3sdz[j]);
	    Float_t naz   =  1.482*(Central_pc2sdz[j]   - 1.03*Central_pc3sdz[j]);
	    if(fabs(Central_pc2sdphi[j])>1000){
	      brphi = naphi  = -9999;
	    }
	    if(fabs(Central_pc2sdz[j])>1000){
	      brz = naz  = -9999;
	    }
	    m= 0;
	    array[m++] = McRecoTrk_EVENTID[k];
	    array[m++] = run;
	    array[m++] = bbcz;
	    array[m++] = pt;
	    array[m++] = Central_phi[j];
	    array[m++] = Central_zed[j];
	    array[m++] = Central_the0[j];
	    array[m++] = Central_phi0[j];
	    
	    array[m++] = Central_alpha[j];
	    array[m++] = Central_alpha1[j];
	    array[m++] = Central_alpha2[j];
	    array[m++] = Central_beta[j];
	    array[m++] = Central_quality[j];
	    array[m++] = Central_temc[j];
	    array[m++] = Central_ecorr[j];
	    array[m++] = Central_ecore[j];
	    array[m++] = Central_prob[j];
	    array[m++] = Central_emcchi2[j];
	    array[m++] = Central_plemc[j];
	    
	    array[m++] = brphi;
	    array[m++] = naphi;
	    array[m++] = brz;
	    array[m++] = naz;
	    array[m++] = Central_pc2sdphi[j];
	    array[m++] = Central_pc2sdz[j];
	    array[m++] = Central_pc3sdphi[j];
	    array[m++] = Central_pc3sdz[j];
	    array[m++] = Central_emcsdphi[j];
	    array[m++] = Central_emcsdz[j];
	    
	    array[m++] = McRecoTrk_CRKNPMT0[k];
	    array[m++] = McRecoTrk_CRKNPE0[k];
	    array[m++] = McRecoTrk_CRKNPMT1[k];
	    array[m++] = McRecoTrk_CRKNPE1[k];
	    array[m++] = McRecoTrk_CRKCHI2[k];
	    array[m++] = McRecoTrk_CRKDISP[k];
	    array[m++] = Central_nx1hits[j];
	    array[m++] = Central_nx2hits[j];
	    array[m++] = Central_dist1[j];
	    array[m++] = Central_dist2[j];
	    
	    array[m++] = sqrt(McTrk_VERTEXX[i]* McTrk_VERTEXX[i]+ McTrk_VERTEXY[i]*McTrk_VERTEXY[i]);
	    array[m++] = McTrk_VERTEXZ[i];
	    array[m++] = McTrk_GENERATION[i];
	    array[m++] = McTrk_PARTICLEID[i];
	    array[m++] = McTrk_PARENTID[i];
	    array[m++] = McRecoTrk_XPURITY[k];
	    array[m++] = papaid;
	    array[m++] = paid;
	    array[m++] = papt;
	    array[m++] = paphi;
	    array[m++] = pathe;
	    array[m++] = panum;
	    
	    array[m++] = sibpt[0];
	    array[m++] = sibphi0[0];
	    array[m++] = sibthe0[0];
	    array[m++] = sibalp[0];
	    array[m++] = sibid[0];
	    array[m++] = sibpt[1];
	    array[m++] = sibphi0[1];
	    array[m++] = sibthe0[1];
	    array[m++] = sibalp[1];
	    array[m++] = sibid[1];
	    array[m++] = sibpt[2];
	    array[m++] = sibphi0[2];
	    array[m++] = sibthe0[2];
	    array[m++] = sibalp[2];
	    array[m++] = sibid[2];
	    Float_t eta = -log(tan(McTrk_THETA0[i]/2.));
	    array[m++] = eta;
	    array[m++] = McTrk_PHI0[i];
	    array[m++] = McTrk_THETA0[i];
	    array[m++] = sqrt(mpt*mpt+mpz*mpz);
	    array[m++] = mpt;
	    //
	    good =1;
	    array[m++] = good;
	    array[m++] = Central_momentum[j];
	    array[m++] = McRecoTrk_MOMENTUM[k];
	    //primary momentum....,id....
	    float Px = McTrk_PRIMARYMOMENTUMX[i];
	    float Py = McTrk_PRIMARYMOMENTUMY[i];
	    float Pz = McTrk_PRIMARYMOMENTUMZ[i];
	    float Pt = sqrt(Px*Px+Py*Py);
	    PHAngle phiM(atan2(Py,Px));
	    array[m++] = McTrk_PRIMARYID[i];
	    array[m++] = phiM.getPhi();
	    array[m++] = atan2(Pt,Pz);
	    array[m++] = Pt;
	    array[m++] = Central_tofsdz[j];
	    array[m++] = Central_tofsdphi[j];
	    array[m++] = Central_m2[j];
	    array[m++] = Central_toftof[j];
	    array[m++] = Central_eloss[j];
	    array[m++] = Central_tofz[j];
	    array[m++] = Central_tofphi[j];
	    array[m++] = Central_pltof[j];
	    array[m++] = Central_slat[j];
	    array[m++] = Central_isPi[j];
	    array[m++] = Central_isK[j];
	    array[m++] = Central_isP[j];

	    single->Fill(array); 
	  }
	}
	goto out;
      }
    }// reco
  out:
    if(!found){
      cout << "central has not been matched to reco " << endl;
      cout << "mom : " << Central_momentum[j] << endl;
    } 
  } // central
}
