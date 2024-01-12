#include "TObject.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "recoConsts.h"

#include "RunHeader.h"

#include "getClass.h"
#include "EvaSimreco.h"

//
// Nodes for stand-alone Pad Chamber evaluation
//
#include "dPadClusterWrapper.h"
#include "dPadGhitClusWrapper.h"
#include "pcghitWrapper.h"

//
// Includes for Cgl evaluation
//
#include <PISAEventHeader.h>

#include "VtxOutv7.h"
#include "T0Outv3.h"

#include <BbcOutv1.h>

#include <dDchHitWrapper.h>
#include <dDchTracksWrapper.h>
#include <DchTrackv1.h>

#include "getClass.h"

#include <CglTrack.h>
#include <CglSnglTrack.h>

#include <dPHDchTrackWrapper.h>
#include <PHTrackOutv3.h>
#include <PHTrackOutv8.h>

#include <PHCentralTrack.h>

#include <TecOutV2.hh>

#include <dTofPidWrapper.h>
#include <dTofReconstructedWrapper.h>
#include <tofghitWrapper.h>

#include <utiMatch.h>
#include <geantMass.h>

#include <dEmcGeaClusterTrackWrapper.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>

//
// Additions for TOF-West  (see cglHitList.cc, line 505)
//
#include <TofwHit.h>
#include <tfwghitWrapper.h>


#include <utiHadPid.h>

#include <cmath>
#include <cstdlib>

static utiHadPid  *hadronPID  = 0;

long GeaTrkStack(const int& mcTrack, int& idPart, int& idParent,
                 float& pTot, float& rVertex, float& zVertex,
                 float& pTheta, float& pPhi, int& nFile,
                 int& itParent, int& itOrigin, int& idOrigin);

using namespace std;
using namespace findNode;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

EvaSimreco::EvaSimreco(const char *name, const int evaMode) :
  NTPL_PARAMPAD(11), NTPL_PARAMCGL(146), NTPL_PARAMPAIR(95), 
  NPAIR_DATA(34), NTPL_PARAMEMC(65), NTPL_REACPLANE(40),  thisMode(evaMode)
{
  ThisName = name;

  evaluateFile = new TFile("evalSim.root", "recreate", "Simulation Evaluation NTUPLEs");

  evaluateReacPlaneNtuple = new TNtuple("reacPlane", "Reaction Plane Evaluation in Fun4All",
					"IPAIR:PTOT1:PTHETA1:PPHI1:DCQUAL1:NFILE1:" // 5
					"PTOTVERT1:PTHEVERT1:PPHIVERT1:"// 8
					"PTOT2:PTHETA2:PPHI2:DCQUAL2:NFILE2:" // 13
					"PTOTVERT2:PTHEVERT2:PPHIVERT2:"// 16
					"RCPXRPL1:RCPYRPL1:RCPZRPL1:"// 19
					"MCPXRPL1:MCPYRPL1:MCPZRPL1:"// 22
					"RCPXRPL2:RCPYRPL2:RCPZRPL2:"// 25
					"MCPXRPL2:MCPYRPL2:MCPZRPL2:"// 28
					"RCALPHA1:RCALPHA2:MCALPHA1:MCALPHA2:"// 32
					"PAIRMASS:VERTMASS:MCIDSUM:MCPRNSUM:"// 36
					"RCREPLANG:MCREPLANG:EVENT"); // 39

  evaluatePadNtuple = new TNtuple("evalPadNtuple", "PAD Cluster Evaluation, Fun4All",
				  "IPC:KPC:RECOPHI:RECOTHET:MCTRACK:MCPHI:MCTHETA:"//
				  "VTXOUTT0:VTXOUTZ0:Z0VERTEX:EVENT");

  evaluateEmcNtuple = new TNtuple("evalEmcNtuple", "EMC Cluster Evaluation, Fun4All",
				  "EMCCLUSID:EMCMCMEA:EMCMCCOR:"// 2
				  "EMCMCTRK1:EMCMCTRK2:EMCMCTRK3:"// 5
				  "EMCMCEN1:EMCMCEN2:EMCMCEN3:"// 8
                                  "EMCTHECL:EMCPHICL:EMCMCTH1:EMCMCPH1:EMCMCTH2:EMCMCPH2:EMCMCTH3:EMCMCPH3:"// 16
                                  "DCMOMEN:MCMOMEN:PC1TRACK:PC3TRACK:"// 20
                                  "EMCARM:EMCSECT:EMCPID:EMCPTOT:EMCTOF:EMCTWRHT:EMCPATH:"// 27
                                  "GEAPTOT:GEAPTHET:GEAPPHI:GEARVERT:GEAZVERT:"// 32
                                  "RECOBETA:TRUEBETA:RECOMOM:TOFRECON:GEAPAREN:"// 37
                                  "DCMOMPX:DCMOMPY:DCMOMPZ:EMCMOMPX:EMCMOMPY:EMCMOMPZ:"// 43
                                  "EMCPARSEC:EMCPARARM:EMCPARID:TOFPARID:PARTPTOT:PARTPX:PARTPY:PARTPZ:PARTTRAK:"// 52
                                  "RECPARPX:RECPARPY:RECPARPZ:DCQUAL:R_VERTEX:EMCTCORR:"// 58
				  "VTXOUTT0:VTXOUTZ0:NEMCCLUS:BBCZVERT:Z0VERTEX:EVENT"); // 64

  evaluateCglNtuple = new TNtuple("evalCglNtuple", "CGL Track Evaluation, Fun4All",
				  "CGLTRACK:DCMOMEN:DCALPHA:DCQUAL:MCDCPC1TR:"//  4
				  "MCIDPART:MCMOMEN:MCPTHETA:MCPPHI:MCPARENT:"// 9
				  "DCPLANES:DCDOMSUM:DCPLANUV:"// 12  (not yet implemented)
				  "PC1THETA:PC1PHI:PC2THETA:PC2PHI:PC3THETA:PC3PHI:"// 18
				  "PC1TRACK:PC2TRACK:PC3TRACK:PCDOMSUM:PCMISSED:NFILE:"// 24
                                  "PC1MOMEN:PC2MOMEN:PC3MOMEN:PC1PAREN:PC2PAREN:PC3PAREN:"// 30
				  "PC1MULTI:PC2MULTI:PC3MULTI:"// 33 
                                  "PC1TRPAR:PC2TRPAR:PC3TRPAR:PC3PATHL:PC3TOF:"// 38
                                  "MCTOF:TOFRECON:PATHRECON:PATHPRED:"// 42
                                  "PC1RVERT:PC1ZVERT:PC2RVERT:PC2ZVERT:PC3RVERT:PC3ZVERT:"// 48
                                  "PC3BETA:PC3MASS:MCTFMASS:RECOBETA:RCTFMASS:"// 53
                                  "PC1IDPAR:PC2IDPAR:PC3IDPAR:MCPTMASS:MCPLMASS:"// 58
                                  "DCSIDE:DCARM:DCPC1MAT:DCPC1TRK:DCPC1MOM:DCPC1TH:DCPC1PHI:DCPC1PAR:"// 66
                                  "MCTOFMOM:PC1BETA:PC1LABMO:PC3LABMO:"// 70
				  "MCPC1TH:MCPC1PHI:MCPC2TH:MCPC2PHI:MCPC3TH:MCPC3PHI:"// 76
				  "PC1PROTH:PC1PROPH:PC2PROTH:PC2PROPH:PC3PROTH:PC3PROPH:"// 82
				  "MCTOFTH:MCTOFPH:TOFTH:TOFPH:TOFPROTH:TOFPROPH:PHDCTERR:"// 89
				  "DSIGZPC2:DSIGPPC2:DSIGZPC3:DSIGPPC3:DSIGZTOF:DSIGPTOF:"// 95
				  "MCTOFDIS:TOFDIS:DCHZED:DCHPHI:TOFISKA:TOFDELE:"// 101
                                  "DSIGZEMC:DSIGPEMC:EMCECORR:EMCTCORR:EMCPROTH:EMCPROPH:EMCPATHL:EMCBETA:EMCMASS:"// 110
				  "EMCTHETA:EMCPHI:"// 112
                                  "EMCWARN:EMCDEAD:EMCMCTRK1:EMCMCTRK2:EMCMCTRK3:"// 117
                                  "EMCMCEN1:EMCMCEN2:EMCMCEN3:EMCMCMEA:EMCMCCOR:"// 122
                                  "TECTRKID:TECPHI1:TECPHI2:TECDELE:TECSIDE:TECSECT:"// 128
				  "TOFISPI:TOFISPR:"// 130
				  "VTXOUTT0:VTXOUTZ0:"// 132
				  "NEMCCLUS:BBCTZERO:NDCTRACK:NPC1CLUS:NPC2CLUS:NPC3CLUS:NTCTRACK:"// 139
				  "BBCNORTH:BBCSOUTH:BBCZVERT:Z0_EVENT:B_IMPACT:EVENT"); // 145

  evaluatePairNtuple = new TNtuple("CglPair", "Central Arm Pairs, Fun4All",
				   "IPAIR:ICGL1:PTOT1:PTHETA1:PPHI1:DCQUAL1:DCALPHA1:NFILE1:"//
				   "DSG1ZPC2:DSG1PPC2:DSG1ZPC3:DSG1PPC3:DSG1ZTOF:DSG1PTOF:"//
				   "MCIDPAR1:RECMASS1:MCTOF1:RECOTOF1:PC3PHI1:PC3THETA1:PC1PRNT1:PC3PRNT1:"//
				   "DCHZED1:DCHPHI1:PCDOM1:PTOTVERT1:ANGERR1:RVERT1:ZVERT1:ISKAON1:TOFDELE1:"//
				   "DSGZEMC1:DSGPEMC1:EMCTCOR1:EMCMASS1:"//
				   "ICGL2:PTOT2:PTHETA2:PPHI2:DCQUAL2:DCALPHA2:NFILE2:"//
				   "DSG2ZPC2:DSG2PPC2:DSG2ZPC3:DSG2PPC3:DSG2ZTOF:DSG2PTOF:"//
				   "MCIDPAR2:RECMASS2:MCTOF2:RECOTOF2:PC3PHI2:PC3THETA2:PC1PRNT2:PC3PRNT2:"//
				   "DCHZED2:DCHPHI2:PCDOM2:PTOTVERT2:ANGERR2:RVERT2:ZVERT2:ISKAON2:TOFDELE2:"//
				   "DSGZEMC2:DSGPEMC2:EMCTCOR2:EMCMASS2:"//
				   "PAIRMASS:PAIRPTRAN:PAIRPTOT:PAIRTHET:PAIRPHI:VERTMASS:MASSKK:VERTPTRAN:MASSKK01:"//
				   "OPENANGLE:OPENRECO:"//
				   "NPAIRS:VTXOUTT0:VTXOUTZ0:BBCTZERO:"//
				   "NDCTRACK:NPC1CLUS:NPC2CLUS:NPC3CLUS:NTCTRACK:"//
				   "BBCNORTH:BBCSOUTH:BBCZVERT:Z0_EVENT:B_IMPACT:EVENT");

  evaluateReacPlaneNtuple->SetAutoSave(10000000); // autosave for every 10 MBytes
  evaluatePadNtuple->SetAutoSave(10000000);   // autosave for every 10 MBytes
  evaluateEmcNtuple->SetAutoSave(10000000);   // autosave for every 10 MBytes
  evaluateCglNtuple->SetAutoSave(10000000);  // autosave for every 10 MBytes
  evaluatePairNtuple->SetAutoSave(10000000);  // autosave for every 10 MBytes

  cout << endl << endl << PHWHERE << endl;
  cout << "    EvaSimreco Constructor Information, version November 24, 2007: Created evalSim.root file";
  if(getMode() >= 1)
    cout << ";  pair evaluations will be stored";

  cout << endl;
}

int EvaSimreco::Init(PHCompositeNode *topNode)
{
  return 0;
}

int EvaSimreco::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

int EvaSimreco::CreateNodeTree(PHCompositeNode *topNode)
{
  return 0;
}

int EvaSimreco::process_event(PHCompositeNode *topNode)
{

  //
  // Called at the end of the Central Arm event reconstruction
  //

  PISAEventHeader *pisaEventHeader = PISAEventHeader::GetEventHeader();
  Int_t iEvent = pisaEventHeader->GetEvent();

  if(hadronPID == 0)
     hadronPID = new utiHadPid;

  float evt_pad[NTPL_PARAMPAD];
  float evt_emc[NTPL_PARAMEMC];
  float evt_cgl[NTPL_PARAMCGL];

  for (int iPar=0; iPar<NTPL_PARAMPAD; iPar++) {
    evt_pad[iPar] = -99999.0;
  }

  evt_pad[NTPL_PARAMPAD - 1] = iEvent;  

  const float DEGRAD = 57.295779513;
  const float speedOfLight = 29.9792458;  // cm/ns

  const float kaonMass = 0.493646;
  const float kaonMassSquared = kaonMass*kaonMass;  // for K0Short simulation

  const int MAXPARTICLES = 1000;  // limit on how many particles in the event which can be stored

  Int_t nFileSave[MAXPARTICLES];
  Int_t idPartSave[MAXPARTICLES];
  Int_t idParentSavePc1[MAXPARTICLES];
  Int_t idParentSavePc3[MAXPARTICLES];
  Int_t pcDomSumSave[MAXPARTICLES];
  Float_t dSigPEMCSave[MAXPARTICLES];
  Float_t emcTofSave[MAXPARTICLES];
  Float_t emcMassSave[MAXPARTICLES];
  Float_t dSigZEMCSave[MAXPARTICLES];
  Float_t dSigZPC2Save[MAXPARTICLES];
  Float_t dSigZPC3Save[MAXPARTICLES];
  Float_t dSigZTOFSave[MAXPARTICLES];
  Float_t dSigPPC2Save[MAXPARTICLES];
  Float_t dSigPPC3Save[MAXPARTICLES];
  Float_t dSigPTOFSave[MAXPARTICLES];
  Float_t recoMassSave[MAXPARTICLES];
  Float_t mcTOFSave[MAXPARTICLES];
  Float_t recoTOFSave[MAXPARTICLES];
  Float_t isKaonSave[MAXPARTICLES];
  Float_t tofDeleSave[MAXPARTICLES];
  Float_t pc3ThetaSave[MAXPARTICLES];
  Float_t pc3PhiSave[MAXPARTICLES];
  Float_t ptotVertSave[MAXPARTICLES];
  Float_t pthetaVertSave[MAXPARTICLES];
  Float_t pphiVertSave[MAXPARTICLES];
  Float_t rVertSave[MAXPARTICLES];
  Float_t zVertSave[MAXPARTICLES];
 
  Int_t mcTrack;
  Int_t idPart;
  Int_t idParent;
  Float_t pTot;
  Float_t rVertex;
  Float_t zVertex;
  Float_t pPhi;
  Float_t pTheta;
  Int_t nFile;
  Int_t itParent;
  Int_t itOrigin;
  Int_t idOrigin;

  evt_pad[NTPL_PARAMPAD - 2] = pisaEventHeader->GetZvertex();
  cout << "\n EvaSimreco <I>: iEvent = " << iEvent << ",  Event Z0 = " << pisaEventHeader->GetZvertex();
  cout << endl;

  Float_t vtxOutZ0 = -88888.0;
  VtxOut* vtxout = NULL;
  PHTypedNodeIterator<VtxOut> vtxiter(topNode);
  PHIODataNode<VtxOut> *VtxOutNode = vtxiter.find("VtxOut");
  if (VtxOutNode) {
    vtxout = VtxOutNode->getData();
    if (!vtxout) {
      cout << PHWHERE << "VtxOut Object missing" << endl;
    }
    else {
      vtxOutZ0 = vtxout->get_ZVertex();
    }
  }
  else {
    cout << PHWHERE << "VtxOut Node missing" << endl;
  }

  Float_t t0OutT0 = -88888.0;
  T0Out* t0out = NULL;
  PHTypedNodeIterator<T0Out> t0iter(topNode);
  PHIODataNode<T0Out> *T0OutNode = t0iter.find("T0Out");
  if (T0OutNode) {
    t0out = T0OutNode->getData();
    if (!t0out) {
      cout << PHWHERE << "t0out Object missing" << endl;
    }
    else {
      t0OutT0 = t0out->get_T0();
    }
  }
  else {
    cout << PHWHERE << "T0Out Node missing" << endl;
  }

  evt_pad[NTPL_PARAMPAD - 3] = vtxOutZ0; 
  evt_pad[NTPL_PARAMPAD - 4] = t0OutT0;

  PHNodeIterator mainIter(topNode);
  PHIODataNode<PHTable>* dPc1ClusterNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc1Cluster");
  PHIODataNode<PHTable>* dPc2ClusterNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc2Cluster");
  PHIODataNode<PHTable>* dPc3ClusterNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dPc3Cluster");

  //
  // nodes for evaluating stand-alone PC reconstruction
  //

  dPadClusterWrapper* dPc1Cluster = 0;
  if(dPc1ClusterNode)
    dPc1Cluster = (dPadClusterWrapper*)dPc1ClusterNode->getData();
  else {
    cerr << PHWHERE << " dPc1ClusterNode not found" << endl;
    exit(1);
  }

  Int_t nPc1Clus = 0;
  if(dPc1Cluster) {
    nPc1Clus = dPc1Cluster->RowCount();
  }
  else {
    cerr << PHWHERE << " dPc1Cluster not found" << endl;
    exit(1);
  }

  PHIODataNode<PHTable>* dPc1GhitClusNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "dPc1GhitClus");

  dPadGhitClusWrapper* dPc1GhitClus = 0;
  if(dPc1GhitClusNode) {
    dPc1GhitClus = (dPadGhitClusWrapper*)dPc1GhitClusNode->getData();
  }
  else {
    cerr << PHWHERE << " dPc1GhitClusNode not found" << endl;
    exit(1);
  }

  PHIODataNode<PHTable>* pc1GhitNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "pc1ghit");
  pcghitWrapper* pc1ghit = 0;
  if(pc1GhitNode) {
    pc1ghit = (pcghitWrapper*)pc1GhitNode->getData();  
  }
  else {
    cerr << PHWHERE << " pc1GhitNode not found" << endl;
    exit(1);
  }

  dPadClusterWrapper* dPc2Cluster = 0;
  if(dPc2ClusterNode)
    dPc2Cluster = (dPadClusterWrapper*)dPc2ClusterNode->getData();
  else {
    cerr << PHWHERE << " dPc2ClusterNode not found" << endl;
    exit(1);
  }

  Int_t nPc2Clus = 0;
  if(dPc2Cluster) {
    nPc2Clus = dPc2Cluster->RowCount();
  }
  else {
    cerr << PHWHERE << " dPc2Cluster not found" << endl;
    exit(1);
  }

  PHIODataNode<PHTable>* dPc2GhitClusNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "dPc2GhitClus");

  dPadGhitClusWrapper* dPc2GhitClus = 0;
  if(dPc2GhitClusNode) {
    dPc2GhitClus = (dPadGhitClusWrapper*)dPc2GhitClusNode->getData();
  }
  else {
    cerr << PHWHERE << " dPc2GhitClusNode not found" << endl;
    exit(1);
  }

  PHIODataNode<PHTable>* pc2GhitNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "pc2ghit");
  pcghitWrapper* pc2ghit = 0;
  if(pc2GhitNode) {
    pc2ghit = (pcghitWrapper*)pc2GhitNode->getData();  
  }
  else {
    cerr << PHWHERE << " pc2GhitNode not found" << endl;
    exit(1);
  }

  dPadClusterWrapper* dPc3Cluster = 0;
  if(dPc3ClusterNode)
    dPc3Cluster = (dPadClusterWrapper*)dPc3ClusterNode->getData();
  else {
    cerr << PHWHERE << " dPc3ClusterNode not found" << endl;
    exit(1);
  }

  Int_t nPc3Clus = 0;
  if(dPc3Cluster) {
    nPc3Clus = dPc3Cluster->RowCount();
  }
  else {
    cerr << PHWHERE << " dPc3Cluster not found" << endl;
    exit(1);
  }

  PHIODataNode<PHTable>* dPc3GhitClusNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "dPc3GhitClus");

  dPadGhitClusWrapper* dPc3GhitClus = 0;
  if(dPc3GhitClusNode) {
    dPc3GhitClus = (dPadGhitClusWrapper*)dPc3GhitClusNode->getData();
  }
  else {
    cerr << PHWHERE << " dPc3GhitClusNode not found" << endl;
    exit(1);
  }

  PHIODataNode<PHTable>* pc3GhitNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "pc3ghit");
  pcghitWrapper* pc3ghit = 0;
  if(pc3GhitNode) {
    pc3ghit = (pcghitWrapper*)pc3GhitNode->getData();  
  }
  else {
    cerr << PHWHERE << " pc3GhitNode not found" << endl;
    exit(1);
  }

  //
  // Nodes for evaluating Cgl
  //

  PHTypedNodeIterator<BbcOut> BbcOutIter(topNode);
  PHIODataNode<BbcOut> *BbcOutNode = BbcOutIter.find("BbcOut");
  BbcOutv1* BbcOut = 0;
  if (!BbcOutNode) {
    cout << endl << endl << PHWHERE << endl;
    cout << "Exiting, could not find data node BbcOut" << endl;
    exit(1);
  }
  else {
    BbcOut = (BbcOutv1*)BbcOutNode->getData();
    if(!BbcOut) {
      cout << endl << endl << PHWHERE << endl;
      cout << "Exiting, could not find data BbcOut" << endl;
      exit(1);
    }
  }

  CglTrack *cgl = findNode::getClass<CglTrack>(topNode,"CglTrack");
  if(!cgl) {
    cerr << endl << PHWHERE << endl;
    cerr << "\n CglTrack not found" << endl;
    exit(1);
  }

  PHIODataNode<PHObject>* PHCentralTrackNode = (PHIODataNode<PHObject>*)mainIter.findFirst("PHIODataNode","PHCentralTrack");
  PHCentralTrack* PHCentralTracks = 0;
  if (!PHCentralTrackNode) {
    cerr << PHWHERE << endl;
    cerr << " Could not find data node PHCentralTrackNode" << endl;
    //exit(1);
  }
  else {
    PHCentralTracks = (PHCentralTrack*)PHCentralTrackNode->getData();
  }

  PHCentralTrack* centTrack = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if(centTrack!=0) {
    int nPart = centTrack->get_npart();
    cout << "\n\n number of particles in centTrack = " << nPart << endl << endl;
  }
  else {
    cerr << PHWHERE << endl;
    cerr << " Could not find data node centTrack" << endl;
  }

  int nCglTracks = 0;
  if(cgl) {
    nCglTracks = cgl->get_CglNTrack();
    cout << "\n\n Number of reconstructed global tracks " << nCglTracks << endl << endl;
  }
  else {
    cerr << endl << PHWHERE << endl;
    cerr << "\n cgl not found" << endl;
    exit(1);
  }

  PHIODataNode<PHObject>* DchTrackNode = (PHIODataNode<PHObject>*)mainIter.findFirst("PHIODataNode","DchTrack");
  DchTrackv1* DchTracks = 0;
  if (!DchTrackNode) {
    cerr << PHWHERE << endl;
    cerr << " Could not find data node DchTrackNode" << endl;
    exit(1);
  }
  else {
    DchTracks = (DchTrackv1*)DchTrackNode->getData();
  }

  int nDchTracks = 0;
  if(DchTracks) {
    nDchTracks = DchTracks->get_DchNTrack(); 
    cout << "\n\n Number of reconstructed Dch tracks " << nDchTracks << endl << endl;
  }
  else {
    cerr << PHWHERE << endl;
    cerr << " Could not find data DchTracks" << endl;
    exit(1);
  }
   
  PHIODataNode<PHObject>* PHTrackOutNode = (PHIODataNode<PHObject>*)mainIter.findFirst("PHIODataNode","PHTrackOut");
  PHTrackOutv3* PHTrackOut = 0;
  if (!PHTrackOutNode) {
    cerr << endl << endl << PHWHERE << endl;
    cout << " Could not find data node PHTrackOutNode" << endl;
    exit(1);
  }
  else {
    PHTrackOut = (PHTrackOutv3*)PHTrackOutNode->getData();
  }

  if(!PHTrackOut) {
    cerr << endl << endl << PHWHERE << endl;
    cout << " Could not find data PHTrackOut" << endl;
    exit(1); 
  }

  PHIODataNode<PHObject>* TecOutNode = (PHIODataNode<PHObject>*)mainIter.findFirst("PHIODataNode","TecOut");
  TecOutV2* TecOut = 0;
  if (!TecOutNode) {
    cout <<  " Could not find data node TecOutNode" << endl;
  }
  else {
    TecOut = (TecOutV2*)TecOutNode->getData();
    if(!TecOut) {
      cerr << endl << endl << PHWHERE << endl;
      cout << " Could not find data TecOut" << endl;
      exit(1);
    }
  }

  PHIODataNode<PHTable>* dTofReconstructedNode = 
    (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dTofReconstructed");
  dTofReconstructedWrapper *dTofReconstructed = 0;
  if(dTofReconstructedNode) {
    dTofReconstructed = (dTofReconstructedWrapper*)dTofReconstructedNode->getData();
  }
  else {
    cerr << endl << endl << PHWHERE << endl;
    cout << " Could not find dTofReconstructedNode " << endl;
    exit(1);
  }


  PHIODataNode<PHTable>* tofGhitNode = (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode", "tofghit");
  tofghitWrapper* tofghit = 0;
  unsigned int nTofGHit = 0;
  if(tofGhitNode) {
    tofghit = (tofghitWrapper*)tofGhitNode->getData();  
    if(tofghit)
      nTofGHit = tofghit->RowCount();
  }
  else {
    cerr << endl << endl << PHWHERE << endl;
    cout << " Could not find tofGhitNode" << endl;
    exit(1);
  }

  PHIODataNode<PHObject>* emcClusterContainerNode = (PHIODataNode<PHObject>*)mainIter.findFirst("PHIODataNode","emcClusterContainer");
  emcClusterContainer* emcClusterList = 0;
  if (!emcClusterContainerNode) {
    cout <<  " Could not find data node emcClusterListNode" << endl;
  }
  else {
    emcClusterList = (emcClusterContainer*)emcClusterContainerNode->getData();
    if(!emcClusterList) {
      cerr << endl << endl << PHWHERE << endl;
      cout << " Could not find data emcClusterList" << endl;
      exit(1);
    }
  }

  static int noEMC = 0;
  PHIODataNode<PHTable>* dEmcGeaClusterTrackNode = 
    (PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcGeaClusterTrack");
  dEmcGeaClusterTrackWrapper *dEmcGeaClusterTrack = 0;
  int nEmcCluster = -1;
  if(dEmcGeaClusterTrackNode) {
    dEmcGeaClusterTrack = (dEmcGeaClusterTrackWrapper*)dEmcGeaClusterTrackNode->getData();
    if(!dEmcGeaClusterTrack) {
      cerr << endl << endl << PHWHERE << endl;
      cout << " Could not find data dEmcGeaClusterTrack " << endl;
      exit(1);
    }
    nEmcCluster = dEmcGeaClusterTrack->RowCount();
  }
  else {
    if(noEMC == 0) {
      cerr << endl << endl << PHWHERE << endl;
      cout << " Could not find dEmcGeaClusterTrackNode " << endl;
      noEMC = 1;
    }
  }

  //
  // Fill stand-alone EMC NTUPLE
  //

  if(dEmcGeaClusterTrack && nEmcCluster>0) {

    float bbcZVert = BbcOut->get_VertexPoint();
    float z0_event = pisaEventHeader->GetZvertex();
    evt_emc[NTPL_PARAMEMC - 1] = iEvent;
    evt_emc[NTPL_PARAMEMC - 2] = z0_event;
    evt_emc[NTPL_PARAMEMC - 3] = bbcZVert;
    evt_emc[NTPL_PARAMEMC - 4] = nEmcCluster;
    evt_emc[NTPL_PARAMEMC - 5] = vtxOutZ0;
    evt_emc[NTPL_PARAMEMC - 6] = t0OutT0;
    //
    // Check for MC evaluation results
    //
    for(int iRow=0; iRow<nEmcCluster; iRow++) {
      int emcClusID = dEmcGeaClusterTrack->get_clusid(iRow);
      evt_emc[0] = emcClusID;
      evt_emc[1] = dEmcGeaClusterTrack->get_mease(iRow);
      evt_emc[2] = dEmcGeaClusterTrack->get_ecore(iRow);
      evt_emc[3] = dEmcGeaClusterTrack->get_trkno(0, iRow);
      evt_emc[4] = dEmcGeaClusterTrack->get_trkno(1, iRow);
      evt_emc[5] = dEmcGeaClusterTrack->get_trkno(2, iRow);
      evt_emc[6] = dEmcGeaClusterTrack->get_edep(0, iRow);
      evt_emc[7] = dEmcGeaClusterTrack->get_edep(1, iRow);
      evt_emc[8] = dEmcGeaClusterTrack->get_edep(2, iRow);

      double measx = dEmcGeaClusterTrack->get_measxyz(0, iRow);
      double measy = dEmcGeaClusterTrack->get_measxyz(1, iRow);
      double measz = dEmcGeaClusterTrack->get_measxyz(2, iRow);

      double measRsq = measx*measx + measy*measy;
      double emcPath = sqrt(measRsq + (measz - bbcZVert)*(measz - bbcZVert));

      double measCosThe = 0.0;
      double measSinThe = 0.0;
      double measSinTheCosPhi = 0.0;
      double measSinTheSinPhi = 0.0;
      if(emcPath > 0.0) {
	measCosThe = (measz - bbcZVert)/emcPath;
	double measR = sqrt(measRsq);
	measSinThe = measR/emcPath;
	if(measR>0.0) {
	  measSinTheSinPhi = measSinThe*measy/measR;
	  measSinTheCosPhi = measSinThe*measx/measR;
	}
      }

      Float_t phiTemp =  DEGRAD*atan2(measy, measx);
      if(phiTemp < -90.0)
	phiTemp = 360.0 + phiTemp;
    
      Float_t thetaTemp = DEGRAD*acos(measz/sqrt(measx*measx + measy*measy + measz*measz));
      evt_emc[9] = thetaTemp;
      evt_emc[10] = phiTemp;

      for(int iPar=11; iPar<21; iPar++) {
	evt_emc[iPar] = -99999.0;
      } // initialize

      float trkx1 = dEmcGeaClusterTrack->get_xyz(0, 0, iRow);
      float trky1 = dEmcGeaClusterTrack->get_xyz(1, 0, iRow);
      float trkz1 = dEmcGeaClusterTrack->get_xyz(2, 0, iRow);

      float test = trkx1*trkx1 + trky1*trky1 + trky1*trky1;

      if(test > 0.0) {
	phiTemp = DEGRAD*atan2(trky1, trkx1);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	thetaTemp = DEGRAD*acos(trkz1/sqrt(test));

	evt_emc[11] = thetaTemp;
	evt_emc[12] = phiTemp;

      }

      float trkx2 = dEmcGeaClusterTrack->get_xyz(0, 0, iRow);
      float trky2 = dEmcGeaClusterTrack->get_xyz(1, 0, iRow);
      float trkz2 = dEmcGeaClusterTrack->get_xyz(2, 0, iRow);

      test = trkx2*trkx2 + trky2*trky2 + trky2*trky2;

      if(test > 0.0) {
	phiTemp = DEGRAD*atan2(trky2, trkx2);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	thetaTemp = DEGRAD*acos(trkz2/sqrt(test));

	evt_emc[13] = thetaTemp;
	evt_emc[14] = phiTemp;

      }

      float trkx3 = dEmcGeaClusterTrack->get_xyz(0, 0, iRow);
      float trky3 = dEmcGeaClusterTrack->get_xyz(1, 0, iRow);
      float trkz3 = dEmcGeaClusterTrack->get_xyz(2, 0, iRow);

      test = trkx3*trkx3 + trky3*trky3 + trky3*trky3;

      if(test > 0.0) {
	phiTemp = DEGRAD*atan2(trky3, trkx3);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	thetaTemp = DEGRAD*acos(trkz3/sqrt(test));

	evt_emc[15] = thetaTemp;
	evt_emc[16] = phiTemp;

      }

      //
      // Check for associated track
      //

      if(nCglTracks>0) {
	for (int iCgl=0; iCgl<nCglTracks; iCgl++) {
	  evt_emc[56] = -1.0;   // default DC quality
	  evt_emc[57] = -1.0;   // default R_VERTEX
	  CglSnglTrack *sngl = cgl->get_track(iCgl);
	  if(emcClusID == sngl->get_emcclusid()) {

            if(PHCentralTracks) {
  	      Float_t px = PHCentralTracks->get_px(iCgl); // px component of reconstructed momentum
	      Float_t py = PHCentralTracks->get_py(iCgl); // py component of reconstructed momentum
	      Float_t pz = PHCentralTracks->get_pz(iCgl); // pz component of reconstructed momentum

	      evt_emc[17] = sqrt(px*px + py*py + pz*pz);
	      evt_emc[38] = px;
	      evt_emc[39] = py;
              evt_emc[40] = pz;
            }

	    evt_emc[56] = DchTracks->get_quality(iCgl);
	    CglSnglTrack *sngl = cgl->get_track(iCgl); 
	    int pc1ClusterID = sngl->get_pc1clusid();

	    if(pc1ClusterID>-1) {
	      unsigned int clusID = dPc1GhitClus->get_clusid(pc1ClusterID);
	      if(clusID < 0 || clusID>= dPc1GhitClus->RowCount()) {
		cerr << "\n PC1 clusID = " << clusID << ", max = " << dPc1GhitClus->RowCount() << endl;
		cerr << "Possible sequencing error, EMC stand-alone evaluation segment? " << endl;
	      }
	      else {
		Int_t pcghitID = dPc1GhitClus->get_ghitid(clusID);
		Int_t mcPadTrack = pc1ghit->get_mctrack(pcghitID);
		GeaTrkStack(mcPadTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
			    itParent, itOrigin, idOrigin);

		evt_emc[18] = pTot;  // MC momentum of associated track
		evt_emc[19] = mcPadTrack; // GEANT track number of associated PC1

	      }

	    } 
	    int pc3ClusterID = sngl->get_pc3clusid();

	    if(pc3ClusterID>-1) {
	      unsigned int clusID = dPc3GhitClus->get_clusid(pc3ClusterID);
	      if(clusID < 0 || clusID>= dPc3GhitClus->RowCount()) {
		cerr << "\n PC3 clusID = " << clusID << ", max = " << dPc3GhitClus->RowCount() << endl;
		cerr << "Possible sequencing error, EMC stand-alone evaluation segment? " << endl;
	      }
	      else {
		Int_t pcghitID = dPc3GhitClus->get_ghitid(clusID);
		Int_t mcPadTrack = pc3ghit->get_mctrack(pcghitID);

		evt_emc[20] = mcPadTrack; // GEANT track number of associated PC3

	      }

	    }

            short tofRecID = sngl->get_tofrecid();
	    unsigned tofRecIDu = tofRecID;
	    if(tofRecIDu < dTofReconstructed->RowCount()) { // and unsigned is how RowCount is returned (stupid isn't it)
	      float tofRecon = dTofReconstructed->get_tof(tofRecID); // and now we get the TOF after all
              evt_emc[36] = tofRecon;
            }

	    break;

	  } // check for matching cluster ID

	} // loop over Cgl tracks
	  
        evt_emc[21] = dEmcGeaClusterTrack->get_arm(iRow);
        evt_emc[22] = dEmcGeaClusterTrack->get_sector(iRow);
        evt_emc[23] = dEmcGeaClusterTrack->get_pid(0, iRow);
        evt_emc[24] = dEmcGeaClusterTrack->get_ptot(0, iRow);
        evt_emc[25] = dEmcGeaClusterTrack->get_tof(iRow);
        evt_emc[26] = dEmcGeaClusterTrack->get_twrhit(iRow);
        evt_emc[27] = emcPath;
	
	//
	// Checking the cluster container version for the "corrected TOF"
	//
	evt_emc[58] = -9999.0; // corrected TOF in EMCal (should be same as get_tof from dEmcGeaClusterTrack)
	if(emcClusterList) {
	  emcClusterContent* emcCluster = emcClusterList->findCluster(emcClusID);

	  if(emcCluster)
	    evt_emc[58] = emcCluster->tofcorr();

	} // checking on emcCluster

        //
        // More GEANT information for dominant track number
        //
        int iTrack1 = dEmcGeaClusterTrack->get_trkno(0, iRow);
        GeaTrkStack(iTrack1, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
                    itParent, itOrigin, idOrigin);
        evt_emc[28] = pTot;
        evt_emc[29] = pTheta;
        evt_emc[30] = pPhi;
        evt_emc[31] = rVertex;
        evt_emc[32] = zVertex;
	evt_emc[37] = idParent;
	evt_emc[57] = rVertex;

	evt_emc[44] = -1.0;  // partner EMCal sector
	evt_emc[45] = -1.0;  // partner EMCal particle ID
	evt_emc[46] = -1.0;  // partner TOF particle ID
	evt_emc[47] = -1.0;  // partner TOF particle vertex PTOT
	evt_emc[48] = -1.0;  // partner TOF particle vertex PX
	evt_emc[49] = -1.0;  // partner TOF particle vertex PY
	evt_emc[50] = -1.0;  // partner TOF particle vertex PZ
	evt_emc[51] = -1.0;  // partner TOF particle track number
	evt_emc[52] = -1.0;  // partner TOF particle reco PX
	evt_emc[53] = -1.0;  // partner TOF particle reco PY
	evt_emc[54] = -1.0;  // partner TOF particle reco PZ

	if((idPart==25 || idPart==15) && idParent>0 && nEmcCluster>1) {
	  //
	  // For anti-protons or anti-neutrons which are from decay of a primary particle
	  //
	  //
	  // Check if there is an EMCal Sector which has a partner particle of the same parent
	  // which has a different particle ID
	  // If so, store the EMCal Sector number and the particle ID
	  //
	  // First save values
	  //
	  int savePartID = idPart;
	  int saveParentTrack = itParent;
	  int saveParentID = idParent;
	  double savePtot = pTot;
	  for(int jRow=0; jRow<nEmcCluster; jRow++ ){
	    if(jRow==iRow)
	      continue;

	    int checkTrack = dEmcGeaClusterTrack->get_trkno(0, jRow);
	    if(checkTrack > 0) {
	      GeaTrkStack(checkTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
			  itParent, itOrigin, idOrigin);
	      if(itParent==saveParentTrack && idParent==saveParentID && idPart!=savePartID
		 && pTot>0.1) {
		//
		// matched a partner in the EMCal
		//
		evt_emc[44] = dEmcGeaClusterTrack->get_sector(jRow);
		evt_emc[45] = dEmcGeaClusterTrack->get_arm(jRow);
		evt_emc[46] = idPart;
		break;
	      } // check for parent match
	    } // safety for positive track number
	  } // loop to check for for partner particle in EMCal

	  //
	  // restore values
	  //
	  pTot = savePtot;
          idPart = savePartID;
          itParent = saveParentTrack;
	  idParent = saveParentID;

	  // Check if there is a Cgl track which has a partner particle of the same parent
	  // which has a different particle ID and the partner particle is in TOF
	  // If so, store the vertex momentum values, the particle ID, and the
	  // reconstructed momentum values
	  //
	  if(nCglTracks>0) {
	    for (int iCgl=0; iCgl<nCglTracks; iCgl++) {
	      CglSnglTrack *sngl = cgl->get_track(iCgl); 
	      int pc1ClusterID = sngl->get_pc1clusid();

	      if(pc1ClusterID>-1) {
		unsigned int clusID = dPc1GhitClus->get_clusid(pc1ClusterID);
		if(clusID < 0 || clusID>= dPc1GhitClus->RowCount()) {
		  cerr << "\n PC1 clusID = " << clusID << ", max = " << dPc1GhitClus->RowCount() << endl;
		  cerr << "Possible sequencing error, EMC stand-alone evaluation segment? " << endl;
		}
		else {
		  Int_t pcghitID = dPc1GhitClus->get_ghitid(clusID);
		  Int_t mcPadTrack = pc1ghit->get_mctrack(pcghitID);
		  GeaTrkStack(mcPadTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
			      itParent, itOrigin, idOrigin);
		  if(idParent==saveParentID && itParent==saveParentTrack && idPart!=savePartID
		     && pTot>0.1) {

		    short tofRecID = sngl->get_tofrecid();
		    unsigned tofRecIDu = tofRecID;
		    if(tofRecIDu < dTofReconstructed->RowCount()) { // and unsigned is how RowCount is return3e
		      float tofRecon = dTofReconstructed->get_tof(tofRecID); // and now we get the TOF after all
		      if(tofRecon>0.0) {

			evt_emc[47] = idPart;
	       
                        if(PHCentralTracks) {
			  Float_t pxReco = PHCentralTracks->get_px(iCgl); // px component of reconstructed momentum
			  Float_t pyReco = PHCentralTracks->get_py(iCgl); // py component of reconstructed momentum
			  Float_t pzReco = PHCentralTracks->get_pz(iCgl); // pz component of reconstructed momentum

			  evt_emc[53] = pxReco;
			  evt_emc[54] = pyReco;
			  evt_emc[55] = pzReco;
                        }

			evt_emc[48] = pTot;
			evt_emc[49] = pTot*sin(pTheta/DEGRAD)*cos(pPhi/DEGRAD);
			evt_emc[50] = pTot*sin(pTheta/DEGRAD)*sin(pPhi/DEGRAD);
			evt_emc[51] = pTot*cos(pTheta/DEGRAD);
			evt_emc[52] = mcPadTrack;

		      } /// check on tofRecon
		    } // check on tofRecIDu
		  } // check on parent track match
		}  // found PC1 match
	      } // check on Pc1 ClusterID
	    }  // loop over Cgl tracks
	  } // check on at least one Cgl track

	  //
	  // restore values
	  //
	  pTot = savePtot;
          idPart = savePartID;
          itParent = saveParentTrack;
	  idParent = saveParentID;

	} // check on anti-proton or anti-neutron from a primary particle decay

        double measTof = evt_emc[25] + emcPath/speedOfLight;
        double emcBeta = emcPath/(measTof*speedOfLight);
        evt_emc[33] = emcBeta;
        if(idPart>0 && idPart<56 && emcBeta != 1.0) {
          double trueMass = geantMass[idPart - 1];
          double trueBeta = sqrt(pTot*pTot/(trueMass*trueMass + pTot*pTot));
          evt_emc[34] = trueBeta;
          double momSquare = emcBeta*emcBeta*trueMass*trueMass/(1.0 - emcBeta*emcBeta);
          if(momSquare>0) {
	    momSquare = sqrt(momSquare);
            evt_emc[35] = momSquare;
            evt_emc[41] = momSquare*measSinTheCosPhi;
	    evt_emc[42] = momSquare*measSinTheSinPhi;
	    evt_emc[43] = momSquare*measCosThe;
	  }
        }
        
	evaluateEmcNtuple->Fill(evt_emc);

      }  // check on nCglTracks > 0
    
    } // loop over iRows   
 
  } // check for MC entries
  
  //
  // Fill stand-alone PC evaluator NTUPLE
  //

  if(nPc1Clus > 0) {
    evt_pad[0] = 1; // PC1

    for(Int_t pc1ClusterID=0; pc1ClusterID<nPc1Clus; pc1ClusterID++) {

      Float_t xPC1 = dPc1Cluster->get_xyz(0,pc1ClusterID);
      Float_t yPC1 = dPc1Cluster->get_xyz(1,pc1ClusterID);
      Float_t zPC1 = dPc1Cluster->get_xyz(2,pc1ClusterID);
      
      Float_t phiTemp =  DEGRAD*atan2(yPC1, xPC1);
      if(phiTemp < -90.0)
	phiTemp = 360.0 + phiTemp;
    
      Float_t thetaTemp = DEGRAD*acos(zPC1/sqrt(xPC1*xPC1 + yPC1*yPC1 + zPC1*zPC1));

      evt_pad[1] = pc1ClusterID;
      evt_pad[2] = phiTemp;
      evt_pad[3] = thetaTemp;
      unsigned int clusID = dPc1GhitClus->get_clusid(pc1ClusterID);

      if(clusID < 0 || clusID>= dPc1GhitClus->RowCount()) {
	cerr << "\n PC1 clusID = " << clusID << ", max = " << dPc1GhitClus->RowCount() << endl;
	cerr << "Possible sequencing error, PC1 stand-alone? " << endl;
      }

      Int_t pcghitID = dPc1GhitClus->get_ghitid(clusID);
      mcTrack = pc1ghit->get_mctrack(pcghitID);

      GeaTrkStack(mcTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
		  itParent, itOrigin, idOrigin);

      Float_t xMcPc1 = pc1ghit->get_xyzinglo(0,pcghitID);
      Float_t yMcPc1 = pc1ghit->get_xyzinglo(1,pcghitID);
      Float_t zMcPc1 = pc1ghit->get_xyzinglo(2,pcghitID);
    
      Float_t phiMcTemp =  DEGRAD*atan2(yMcPc1, xMcPc1);
      if(phiMcTemp < -90.0)
	phiMcTemp = 360.0 + phiMcTemp;
    
      Float_t thetaMcTemp = DEGRAD*acos(zMcPc1/sqrt(xMcPc1*xMcPc1 + yMcPc1*yMcPc1 + zMcPc1*zMcPc1));

      evt_pad[4] = mcTrack;
      evt_pad[5] = phiMcTemp;
      evt_pad[6] = thetaMcTemp;
      evaluatePadNtuple->Fill(evt_pad);

    }  // loop over PC1 clusters

  } /// check on at least on PC1 Cluster


  if(nPc2Clus > 0) {
    evt_pad[0] = 2; // PC2

    for(Int_t pc2ClusterID=0; pc2ClusterID<nPc2Clus; pc2ClusterID++) {

      Float_t xPC2 = dPc2Cluster->get_xyz(0,pc2ClusterID);
      Float_t yPC2 = dPc2Cluster->get_xyz(1,pc2ClusterID);
      Float_t zPC2 = dPc2Cluster->get_xyz(2,pc2ClusterID);
      
      Float_t phiTemp =  DEGRAD*atan2(yPC2, xPC2);
      if(phiTemp < -90.0)
	phiTemp = 360.0 + phiTemp;
    
      Float_t thetaTemp = DEGRAD*acos(zPC2/sqrt(xPC2*xPC2 + yPC2*yPC2 + zPC2*zPC2));

      evt_pad[1] = pc2ClusterID;
      evt_pad[2] = phiTemp;
      evt_pad[3] = thetaTemp;
      unsigned int clusID = dPc2GhitClus->get_clusid(pc2ClusterID);

      if(clusID < 0 || clusID>= dPc2GhitClus->RowCount()) {
	cerr << "\n PC2 clusID = " << clusID << ", max = " << dPc2GhitClus->RowCount() << endl;
	cerr << "Possible sequencing error, PC stand-alone " << endl;
      }

      Int_t pcghitID = dPc2GhitClus->get_ghitid(clusID);
      mcTrack = pc2ghit->get_mctrack(pcghitID);

      GeaTrkStack(mcTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
		  itParent, itOrigin, idOrigin);

      Float_t xMcPc2 = pc2ghit->get_xyzinglo(0,pcghitID);
      Float_t yMcPc2 = pc2ghit->get_xyzinglo(1,pcghitID);
      Float_t zMcPc2 = pc2ghit->get_xyzinglo(2,pcghitID);
    
      Float_t phiMcTemp =  DEGRAD*atan2(yMcPc2, xMcPc2);
      if(phiMcTemp < -90.0)
	phiMcTemp = 360.0 + phiMcTemp;
    
      Float_t thetaMcTemp = DEGRAD*acos(zMcPc2/sqrt(xMcPc2*xMcPc2 + yMcPc2*yMcPc2 + zMcPc2*zMcPc2));

      evt_pad[4] = mcTrack;
      evt_pad[5] = phiMcTemp;
      evt_pad[6] = thetaMcTemp;
      evaluatePadNtuple->Fill(evt_pad);

    }  // loop over PC2 clusters

  } /// check on at least on PC2 Cluster

  if(nPc3Clus > 0) {
    evt_pad[0] = 3; // PC3

    for(Int_t pc3ClusterID=0; pc3ClusterID<nPc3Clus; pc3ClusterID++) {

      Float_t xPC3 = dPc3Cluster->get_xyz(0,pc3ClusterID);
      Float_t yPC3 = dPc3Cluster->get_xyz(1,pc3ClusterID);
      Float_t zPC3 = dPc3Cluster->get_xyz(2,pc3ClusterID);
      
      Float_t phiTemp =  DEGRAD*atan2(yPC3, xPC3);
      if(phiTemp < -90.0)
	phiTemp = 360.0 + phiTemp;
    
      Float_t thetaTemp = DEGRAD*acos(zPC3/sqrt(xPC3*xPC3 + yPC3*yPC3 + zPC3*zPC3));

      evt_pad[1] = pc3ClusterID;
      evt_pad[2] = phiTemp;
      evt_pad[3] = thetaTemp;
      unsigned int clusID = dPc3GhitClus->get_clusid(pc3ClusterID);

      if(clusID < 0 || clusID>= dPc3GhitClus->RowCount()) {
	cerr << "\n PC3 clusID = " << clusID << ", max = " << dPc3GhitClus->RowCount() << endl;
	cerr << ", Possible sequencing error? PC stand-alone " << endl;
      }

      Int_t pcghitID = dPc3GhitClus->get_ghitid(clusID);
      mcTrack = pc3ghit->get_mctrack(pcghitID);

      GeaTrkStack(mcTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
		  itParent, itOrigin, idOrigin);

      Float_t xMcPc3 = pc3ghit->get_xyzinglo(0,pcghitID);
      Float_t yMcPc3 = pc3ghit->get_xyzinglo(1,pcghitID);
      Float_t zMcPc3 = pc3ghit->get_xyzinglo(2,pcghitID);
    
      Float_t phiMcTemp =  DEGRAD*atan2(yMcPc3, xMcPc3);
      if(phiMcTemp < -90.0)
	phiMcTemp = 360.0 + phiMcTemp;
    
      Float_t thetaMcTemp = DEGRAD*acos(zMcPc3/sqrt(xMcPc3*xMcPc3 + yMcPc3*yMcPc3 + zMcPc3*zMcPc3));

      evt_pad[4] = mcTrack;
      evt_pad[5] = phiMcTemp;
      evt_pad[6] = thetaMcTemp;
      evaluatePadNtuple->Fill(evt_pad);

    }  // loop over PC3 clusters

  } /// check on at least on PC3 Cluster

  if(nCglTracks>0) {

    //
    // Do Cgl evaluation for single tracks
    //

    utiMatch *match = new utiMatch;

    float bbcZVert = BbcOut->get_VertexPoint();
    float z0_event = pisaEventHeader->GetZvertex();
    evt_cgl[NTPL_PARAMCGL - 1] = iEvent;
    evt_cgl[NTPL_PARAMCGL - 2] = pisaEventHeader->GetImpactParameter();
    evt_cgl[NTPL_PARAMCGL - 3] = z0_event;
    evt_cgl[NTPL_PARAMCGL - 4] = bbcZVert;
    evt_cgl[NTPL_PARAMCGL - 5] = BbcOut->get_nPmt(0);
    evt_cgl[NTPL_PARAMCGL - 6] = BbcOut->get_nPmt(1);
    evt_cgl[NTPL_PARAMCGL - 7] = TecOut->getNTracks();
    evt_cgl[NTPL_PARAMCGL - 8] = nPc3Clus;
    evt_cgl[NTPL_PARAMCGL - 9] = nPc2Clus;
    evt_cgl[NTPL_PARAMCGL - 10] = nPc1Clus;
    evt_cgl[NTPL_PARAMCGL - 11] = DchTracks->get_DchNTrack();
    evt_cgl[NTPL_PARAMCGL - 12] = BbcOut->get_TimeZero();
    evt_cgl[NTPL_PARAMCGL - 13] = nEmcCluster;
    evt_cgl[NTPL_PARAMCGL - 14] = vtxOutZ0;
    evt_cgl[NTPL_PARAMCGL - 15] = t0OutT0;

    int lastPar = NTPL_PARAMCGL - 15;

    for(int iCgl=0; iCgl<nCglTracks; iCgl++) {

      for (int iPar=0; iPar<lastPar; iPar++) {
	evt_cgl[iPar] = -99999.0;
      }

      if(iCgl<MAXPARTICLES) {
	nFileSave[iCgl] = -1;
	idPartSave[iCgl] = -1;
	idParentSavePc1[iCgl] = -9999;
	idParentSavePc3[iCgl] = -9999;
	pcDomSumSave[iCgl] = -9999;
	dSigZEMCSave[iCgl] = -99999.0;
	dSigPEMCSave[iCgl] = -99999.0;
	emcMassSave[iCgl] =  -99999.0;
	emcTofSave[iCgl] =   -99999.0;
	dSigZPC2Save[iCgl] = -99999.0;
	dSigPPC2Save[iCgl] = -99999.0;
	dSigZPC3Save[iCgl] = -99999.0;
	dSigPPC3Save[iCgl] = -99999.0;
	dSigZTOFSave[iCgl] = -99999.0;
	dSigPTOFSave[iCgl] = -99999.0;
	recoMassSave[iCgl] = -99999.0;
	mcTOFSave[iCgl] = -999999.0;
	recoTOFSave[iCgl] = -99999.0;
	pc3ThetaSave[iCgl] = -99999.0;
	pc3PhiSave[iCgl] = -99999.0;
	isKaonSave[iCgl] = -99999.0;
	tofDeleSave[iCgl] = -99999.0;
      }  // initialize the save array

      Int_t pcDomSum = 0;
      Int_t pc1Multi = 0;
      Int_t pc2Multi = 0;
      Int_t pc3Multi = 0;
      Int_t pc3Track = -99999;
      float pc3Tof = -99999.0;
      float pc3PathLength = -99999.0;
      float tofMcPathLength = -99999.0;
      float ptotPc3 = -99999.0;
      float ptotDch = -99999.0;
      float alphaDch = -99999.0;
      Int_t mcDCPC1Track = -99999;

      CglSnglTrack *sngl = cgl->get_track(iCgl); 
      unsigned int dchTrack = sngl->get_dctracksid();
      evt_cgl[0] = dchTrack;

     float ptotDchsign = ptotDch;
      if(PHCentralTracks) {
        Float_t px = PHCentralTracks->get_px(iCgl); // px component of reconstructed momentum
        Float_t py = PHCentralTracks->get_py(iCgl); // py component of reconstructed momentum
        Float_t pz = PHCentralTracks->get_pz(iCgl); // pz component of reconstructed momentum

        ptotDch = sqrt(px*px + py*py + pz*pz);
        evt_cgl[1] = ptotDch;
        int alphasign     = alphaDch>0 ? 1:-1;
        ptotDchsign = -ptotDch*alphasign;
      }

      alphaDch = DchTracks->get_alpha(dchTrack);

      evt_cgl[2] = alphaDch;
      evt_cgl[3] = DchTracks->get_quality(dchTrack);
      evt_cgl[59] = DchTracks->get_side(dchTrack);
      evt_cgl[60] = DchTracks->get_arm(dchTrack);
      evt_cgl[98] = DchTracks->get_zed(dchTrack);
      evt_cgl[99] = DchTracks->get_phi(dchTrack);
      
      //
      // What Dch says is the associated PC1
      //
      Int_t pc1DchClusterID = DchTracks->get_pc1hit (dchTrack);
      mcDCPC1Track = -99999; // MC track number
      evt_cgl[61] = pc1DchClusterID;
      if(nPc1Clus>0 && pc1DchClusterID>-1 && pc1DchClusterID<nPc1Clus) {
	Float_t xPC1 = dPc1Cluster->get_xyz(0,pc1DchClusterID);
	Float_t yPC1 = dPc1Cluster->get_xyz(1,pc1DchClusterID);
	Float_t zPC1 = dPc1Cluster->get_xyz(2,pc1DchClusterID);

	Float_t phiTemp =  DEGRAD*atan2(yPC1, xPC1);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	Float_t thetaTemp = DEGRAD*acos(zPC1/sqrt(xPC1*xPC1 + yPC1*yPC1 + zPC1*zPC1));

	evt_cgl[64] = thetaTemp;
	evt_cgl[65] = phiTemp;

	unsigned int clusID = dPc1GhitClus->get_clusid(pc1DchClusterID);
	if(clusID < 0 || clusID>= dPc1GhitClus->RowCount()) {
	  cerr << "\n PC1 clusID = " << clusID << ", max = " << dPc1GhitClus->RowCount() << endl;
	  cerr << ", Possible sequencing error?, Cgl eval " << endl;
	}
	else {
	  Int_t pcghitID = dPc1GhitClus->get_ghitid(clusID);
	  mcDCPC1Track = pc1ghit->get_mctrack(pcghitID);
          GeaTrkStack(mcDCPC1Track, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
                      itParent, itOrigin, idOrigin);
	  evt_cgl[62] = mcDCPC1Track; // this is being repeated in the NTUPLE, also as [4]
          evt_cgl[63] = pTot;  // momentum of PC1 track 

	  if(iCgl<MAXPARTICLES) {
	    ptotVertSave[iCgl] = pTot;
	    pthetaVertSave[iCgl] = pTheta;
	    pphiVertSave[iCgl] = pPhi;
	    rVertSave[iCgl] = rVertex;
	    zVertSave[iCgl] = zVertex;
	  }

        }
      } // check on at least on PC1 cluster

      unsigned int clusID = dPc1GhitClus->get_clusid(pc1DchClusterID);
      if(clusID < 0 || clusID>= dPc1GhitClus->RowCount()) {
	cerr << "\n PC1 clusID = " << clusID << ", max = " << dPc1GhitClus->RowCount() << endl;
	cerr << "Possible sequencing error, Cgl eval2 " << endl;
	}
      else {
	Int_t pcghitID = dPc1GhitClus->get_ghitid(clusID);
	mcDCPC1Track = pc1ghit->get_mctrack(pcghitID);
	GeaTrkStack(mcDCPC1Track, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
		    itParent, itOrigin, idOrigin);
      } // checking clusID value
      evt_cgl[4] = mcDCPC1Track;
      evt_cgl[5] = idPart;
      evt_cgl[6] = pTot;
      evt_cgl[7] = pTheta;
      evt_cgl[8] = pPhi;
      evt_cgl[9] = idParent;
      evt_cgl[24] = nFile;
      if(iCgl<MAXPARTICLES){
	nFileSave[iCgl] = nFile;
	idPartSave[iCgl] = idPart;
	idParentSavePc1[iCgl] = idParent;
      }
      
      //
      // What CGL says is the associated PC1
      //
      int pc1ClusterID = sngl->get_pc1clusid();
      if(nPc1Clus>0 && pc1ClusterID>-1 && pc1ClusterID<nPc1Clus) {
	Float_t xPC1 = dPc1Cluster->get_xyz(0,pc1ClusterID);
	Float_t yPC1 = dPc1Cluster->get_xyz(1,pc1ClusterID);
	Float_t zPC1 = dPc1Cluster->get_xyz(2,pc1ClusterID);

	Float_t phiTemp =  DEGRAD*atan2(yPC1, xPC1);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	Float_t thetaTemp = DEGRAD*acos(zPC1/sqrt(xPC1*xPC1 + yPC1*yPC1 + zPC1*zPC1));

	evt_cgl[13] = thetaTemp;
	evt_cgl[14] = phiTemp;

	unsigned int clusID = dPc1GhitClus->get_clusid(pc1ClusterID);
	// coverity: checking unsigned int for < 0 is pointless
	//	if(clusID < 0 || clusID>= dPc1GhitClus->RowCount()) {
	if(clusID>= dPc1GhitClus->RowCount()) {
	  cerr << "\n PC1 clusID = " << clusID << ", max = " << dPc1GhitClus->RowCount() << endl;
	  cerr << " Cgl evaluation3; possible sequencing error? " << endl;
	}
	else {
	  Int_t pcghitID = dPc1GhitClus->get_ghitid(clusID);
	  Int_t mcPadTrack = pc1ghit->get_mctrack(pcghitID);
          GeaTrkStack(mcPadTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
                      itParent, itOrigin, idOrigin);
 
	  if(mcDCPC1Track == mcPadTrack)
	    pcDomSum = 1;

	  evt_cgl[19] = mcPadTrack;
	  evt_cgl[25] = pTot;  // momentum of PC1 track 
	  evt_cgl[28] = idParent;  // parent ID of PC1 track 
	  
	  evt_cgl[28] = idParent;  // parent ID of PC1 track 
	  evt_cgl[43] = rVertex;
	  evt_cgl[44] = zVertex;
	  evt_cgl[54] = idPart;
	  
	  Float_t xMcPc1 = pc1ghit->get_xyzinglo(0,pcghitID);
	  Float_t yMcPc1 = pc1ghit->get_xyzinglo(1,pcghitID);
	  Float_t zMcPc1 = pc1ghit->get_xyzinglo(2,pcghitID);
	  
	  Float_t phiMcTemp =  DEGRAD*atan2(yMcPc1, xMcPc1);
	  if(phiMcTemp < -90.0)
	    phiMcTemp = 360.0 + phiMcTemp;
	 
	  Float_t thetaMcTemp = DEGRAD*acos(zMcPc1/sqrt(xMcPc1*xMcPc1 + yMcPc1*yMcPc1 + zMcPc1*zMcPc1));

	  evt_cgl[71] = thetaMcTemp;
	  evt_cgl[72] = phiMcTemp;

	  Float_t xProPc1 = PHTrackOut->get_projectionPc1(iCgl,0);
	  Float_t yProPc1 = PHTrackOut->get_projectionPc1(iCgl,1);
	  Float_t zProPc1 = PHTrackOut->get_projectionPc1(iCgl,2);

	  Float_t phiProTemp =  DEGRAD*atan2(yProPc1, xProPc1);
	  if(phiProTemp < -90.0)
	    phiProTemp = 360.0 + phiProTemp;
	 
	  Float_t thetaProTemp = DEGRAD*acos(zProPc1/sqrt(xProPc1*xProPc1 + yProPc1*yProPc1 + zProPc1*zProPc1));
	 
	  evt_cgl[79] = thetaProTemp;
	  evt_cgl[80] = phiProTemp;

	  float pc1Tof = pc1ghit->get_tof(pcghitID);  // TOF at PC1
	  float pc1PathLength = pc1ghit->get_pathLength(pcghitID);  // path length at PC1
	  float pc1Beta = -99999.0;
	  if(pc1PathLength>0 && pc1Tof>0)
	    pc1Beta = pc1PathLength/(speedOfLight*pc1Tof);

	  evt_cgl[68] = pc1Beta;
	  float trueMass = 0;
	  if(idPart>0 && idPart<56)
	    trueMass = geantMass[idPart -1];
	 
	  if(trueMass>0.0 && pc1Beta>0.0 && pc1Beta<1.0) {
	    float gamma = 1.0/sqrt(1 - pc1Beta*pc1Beta);
	    float pc1LabMomentum = gamma*pc1Beta*trueMass;
	    evt_cgl[69] = pc1LabMomentum;
	  }

          if(nCglTracks>1 && pc1ClusterID>-1)  {
            pc1Multi++;
            for(int iCgl2=0; iCgl2<nCglTracks; iCgl2++) {
              if(DchTracks && iCgl2 != iCgl) {
		CglSnglTrack *sngl2 = cgl->get_track(iCgl2); 
                unsigned int dchTrack2 = sngl2->get_dctracksid();
		// coverity: checking unsigned int for < 0 is non sensical
		//                if(dchTrack2<0 || dchTrack2>=DchTracks->get_DchNTrack()) {
                if(dchTrack2>=DchTracks->get_DchNTrack()) {
	          cout << "\n PadTrackEvaluate <E>: dchTrack2 = " << dchTrack2;
	          cout << ", maximum value = " << DchTracks->get_DchNTrack() << endl;
                } // safety check on dchTrack index
		int pc1ClusterID2 = sngl2->get_pc1clusid();
                if(pc1ClusterID2 == pc1ClusterID)
                  pc1Multi++;

              } // check on a different iCgl2
            } // loop over CglTracks
          } // check on at least 2 tracks

	} // safety check on clusID for PC1

      } // check on at least one PC1 cluster

      //
      // Filling for PC2
      //
      int pc2ClusterID = sngl->get_pc2clusid();

      if(nPc2Clus>0 && pc2ClusterID>-1 && pc2ClusterID<nPc2Clus) {
	Float_t xPC2 = dPc2Cluster->get_xyz(0,pc2ClusterID);
	Float_t yPC2 = dPc2Cluster->get_xyz(1,pc2ClusterID);
	Float_t zPC2 = dPc2Cluster->get_xyz(2,pc2ClusterID);

	Float_t phiTemp =  DEGRAD*atan2(yPC2, xPC2);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	Float_t thetaTemp = DEGRAD*acos(zPC2/sqrt(xPC2*xPC2 + yPC2*yPC2 + zPC2*zPC2));

	evt_cgl[15] = thetaTemp;
	evt_cgl[16] = phiTemp;

	unsigned int clusID = dPc2GhitClus->get_clusid(pc2ClusterID);
	// coverity: checking unsigned int for < 0 is pointless
	//	if(clusID < 0 || clusID>= dPc2GhitClus->RowCount()) {
	if(clusID>= dPc2GhitClus->RowCount()) {
	  cerr << "\n PC2 clusID = " << clusID << ", max = " << dPc2GhitClus->RowCount() << endl;
	  cerr << "Cgl eval, possible sequencing error? " << endl;
	}
	else {
	  Int_t pcghitID = dPc2GhitClus->get_ghitid(clusID);
	  Int_t mcPadTrack = pc2ghit->get_mctrack(pcghitID);
          GeaTrkStack(mcPadTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
                      itParent, itOrigin, idOrigin);

          if(mcDCPC1Track == mcPadTrack)
            pcDomSum += 10;

	  evt_cgl[20] = mcPadTrack;
          evt_cgl[26] = pTot;  // momentum of PC2 track 
          evt_cgl[29] = idParent;  // parent ID of PC2 track 

          evt_cgl[45] = rVertex;
          evt_cgl[46] = zVertex;
          evt_cgl[55] = idPart;

	  Float_t xMcPc2 = pc2ghit->get_xyzinglo(0,pcghitID);
	  Float_t yMcPc2 = pc2ghit->get_xyzinglo(1,pcghitID);
	  Float_t zMcPc2 = pc2ghit->get_xyzinglo(2,pcghitID);

	  Float_t phiMcTemp =  DEGRAD*atan2(yMcPc2, xMcPc2);
	  if(phiMcTemp < -90.0)
	    phiMcTemp = 360.0 + phiMcTemp;

	  Float_t thetaMcTemp = DEGRAD*acos(zMcPc2/sqrt(xMcPc2*xMcPc2 + yMcPc2*yMcPc2 + zMcPc2*zMcPc2));

	  evt_cgl[73] = thetaMcTemp;
	  evt_cgl[74] = phiMcTemp;

	  Float_t xProPc2 = PHTrackOut->get_projectionPc2(iCgl,0);
	  Float_t yProPc2 = PHTrackOut->get_projectionPc2(iCgl,1);
	  Float_t zProPc2 = PHTrackOut->get_projectionPc2(iCgl,2);

	  Float_t phiProTemp =  DEGRAD*atan2(yProPc2, xProPc2);
	  if(phiProTemp < -90.0)
	    phiProTemp = 360.0 + phiProTemp;

	  Float_t thetaProTemp = DEGRAD*acos(zProPc2/sqrt(xProPc2*xProPc2 + yProPc2*yProPc2 + zProPc2*zProPc2));

	  evt_cgl[79] = thetaProTemp;
	  evt_cgl[80] = phiProTemp;

          if(PHCentralTracks) { 
            Float_t  dSigZPc2 = match->d_PC2_z_match(ptotDchsign, zPC2 - zProPc2);
            Float_t  dSigPhiPc2 = match->d_PC2_phi_match(ptotDchsign, (phiTemp - phiProTemp)/DEGRAD);
	    evt_cgl[90] = dSigZPc2;
	    evt_cgl[91] = dSigPhiPc2;

	    if(iCgl<MAXPARTICLES) {
	      dSigZPC2Save[iCgl] = dSigZPc2;
	      dSigPPC2Save[iCgl] = dSigPhiPc2;
	    }
          }

	} // safety check on clusID

      } // check on at least one PC2 cluster

      //
      // Filling for PC3 clusters
      //
      int pc3ClusterID = sngl->get_pc3clusid();

      if(nPc3Clus>0 && pc3ClusterID>-1 && pc3ClusterID<nPc3Clus) {
	Float_t xPC3 = dPc3Cluster->get_xyz(0,pc3ClusterID);
	Float_t yPC3 = dPc3Cluster->get_xyz(1,pc3ClusterID);
	Float_t zPC3 = dPc3Cluster->get_xyz(2,pc3ClusterID);

	Float_t phiTemp =  DEGRAD*atan2(yPC3, xPC3);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	Float_t thetaTemp = DEGRAD*acos(zPC3/sqrt(xPC3*xPC3 + yPC3*yPC3 + zPC3*zPC3));

	evt_cgl[17] = thetaTemp;
	evt_cgl[18] = phiTemp;

	if(iCgl<MAXPARTICLES) {
	  pc3ThetaSave[iCgl] = thetaTemp;
	  pc3PhiSave[iCgl] = phiTemp;
	}

	unsigned int clusID = dPc3GhitClus->get_clusid(pc3ClusterID);
	// coverity: checking unsigned int < 0 is pointless
	//	if(clusID < 0 || clusID>= dPc3GhitClus->RowCount()) {
	if(clusID>= dPc3GhitClus->RowCount()) {
	  cerr << "\n PC3 clusID = " << clusID << ", max = " << dPc3GhitClus->RowCount() << endl;
	  cerr << "Cgl eval, possible sequencing error? " << endl;
	}
	else {
	  Int_t pcghitID = dPc3GhitClus->get_ghitid(clusID);
	  Int_t mcPadTrack = pc3ghit->get_mctrack(pcghitID);
          GeaTrkStack(mcPadTrack, idPart, idParent, pTot, rVertex, zVertex, pTheta, pPhi, nFile,
                      itParent, itOrigin, idOrigin);

          if(mcDCPC1Track == mcPadTrack)
            pcDomSum += 100;

          pc3Track = mcPadTrack;  // use for TOF comparison
          pc3Tof = pc3ghit->get_tof(pcghitID);  // TOF at PC3
          pc3PathLength = pc3ghit->get_pathLength(pcghitID);  // path length at PC3
          ptotPc3 = pTot;
          float pc3Beta = -99999.0;
          if(pc3PathLength>0 && pc3Tof>0)
            pc3Beta = pc3PathLength/(speedOfLight*pc3Tof);

          float pc3Mass = -99999.0;
          if(pc3Beta>0.0)
            pc3Mass = pTot*pTot*(1.0 - pc3Beta*pc3Beta)/(pc3Beta*pc3Beta);

	  evt_cgl[21] = mcPadTrack;
          evt_cgl[27] = pTot;  // momentum of PC3 track 
          evt_cgl[30] = idParent;  // parent of PC3 track 
	  evt_cgl[37] = pc3PathLength;
	  evt_cgl[38] = pc3Tof;
          evt_cgl[47] = rVertex;
          evt_cgl[48] = zVertex;
          evt_cgl[49] = pc3Beta;
          evt_cgl[50] = pc3Mass;
          evt_cgl[56] = idPart;

	  if(iCgl<MAXPARTICLES)
	    idParentSavePc3[iCgl] = idParent;

	  Float_t xMcPc3 = pc3ghit->get_xyzinglo(0,pcghitID);
	  Float_t yMcPc3 = pc3ghit->get_xyzinglo(1,pcghitID);
	  Float_t zMcPc3 = pc3ghit->get_xyzinglo(2,pcghitID);

	  Float_t phiMcTemp =  DEGRAD*atan2(yMcPc3, xMcPc3);
	  if(phiMcTemp < -90.0)
	    phiMcTemp = 360.0 + phiMcTemp;

	  Float_t thetaMcTemp = DEGRAD*acos(zMcPc3/sqrt(xMcPc3*xMcPc3 + yMcPc3*yMcPc3 + zMcPc3*zMcPc3));

	  evt_cgl[75] = thetaMcTemp;
	  evt_cgl[76] = phiMcTemp;

	  Float_t xProPc3 = PHTrackOut->get_projectionPc3(iCgl,0);
	  Float_t yProPc3 = PHTrackOut->get_projectionPc3(iCgl,1);
	  Float_t zProPc3 = PHTrackOut->get_projectionPc3(iCgl,2);

	  Float_t phiProTemp =  DEGRAD*atan2(yProPc3, xProPc3);
	  if(phiProTemp < -90.0)
	    phiProTemp = 360.0 + phiProTemp;

	  Float_t thetaProTemp = DEGRAD*acos(zProPc3/sqrt(xProPc3*xProPc3 + yProPc3*yProPc3 + zProPc3*zProPc3));

	  evt_cgl[81] = thetaProTemp;
	  evt_cgl[82] = phiProTemp;

          if(phiTemp>90.0&&phiTemp<270.0) {
            Float_t  dSigZPc3 = match->d_PC3e_z_match(ptotDchsign, zPC3 - zProPc3);
            Float_t  dSigPhiPc3 = match->d_PC3e_phi_match(ptotDchsign, (phiTemp - phiProTemp)/DEGRAD);
	    evt_cgl[92] = dSigZPc3;
	    evt_cgl[93] = dSigPhiPc3;
	    if(iCgl<MAXPARTICLES) {
	      dSigZPC3Save[iCgl] = dSigZPc3;
	      dSigPPC3Save[iCgl] = dSigPhiPc3;
	    }
	  }  // PC3 East

          if(phiTemp>-90.0&&phiTemp<90.0) {
            Float_t  dSigZPc3 = match->d_PC3w_z_match(ptotDchsign, zPC3 - zProPc3);
            Float_t  dSigPhiPc3 = match->d_PC3w_phi_match(ptotDchsign, (phiTemp - phiProTemp)/DEGRAD);
	    evt_cgl[92] = dSigZPc3;
	    evt_cgl[93] = dSigPhiPc3;
	    if(iCgl<MAXPARTICLES) {
	      dSigZPC3Save[iCgl] = dSigZPc3;
	      dSigPPC3Save[iCgl] = dSigPhiPc3;
	    }
	  }  // PC3 West
	  
          float trueMass = 0;
	  if(idPart>0 && idPart<56)
	    trueMass = geantMass[idPart-1];
          if(trueMass>0.0 && pc3Beta>0.0 && pc3Beta<1.0) {
            float gamma = 1.0/sqrt(1 - pc3Beta*pc3Beta);
            float pc3LabMomentum = gamma*pc3Beta*trueMass;
            evt_cgl[70] = pc3LabMomentum;
          }

          if(nCglTracks>1 && pc3ClusterID>-1)  {
            pc3Multi++;
            for(int iCgl2=0; iCgl2<nCglTracks; iCgl2++) {
              if(DchTracks && iCgl2 != iCgl) {
		CglSnglTrack *sngl2 = cgl->get_track(iCgl2);
                unsigned int dchTrack2 = sngl2->get_dctracksid();
		// dchTrack2 is unsigned int, checking for <0 does not
		// make sense
		// if(dchTrack2<0 || dchTrack2>=DchTracks->get_DchNTrack()) {
                if(dchTrack2>=DchTracks->get_DchNTrack()) {
	          cout << "\n PadTrackEvaluate <E>: dchTrack2 = " << dchTrack2;
	          cout << ", maximum value = " << DchTracks->get_DchNTrack() << endl;
                } // safety check on dchTrack index
                int pc3ClusterID2 = sngl2->get_pc3clusid();
                if(pc3ClusterID2 == pc3ClusterID)
                  pc3Multi++;

              } // check on a different iCgl2
            } // loop over CglTracks
          } // check on at least 2 tracks

	}  // safety check on clusID

	   } // check if at least one PC3 cluster

      evt_cgl[22] = pcDomSum;  // bit pattern for Dch/PC track matching
      if(iCgl<MAXPARTICLES)
	pcDomSumSave[iCgl] = pcDomSum;

      float tofRecon = -99999.0;
      Float_t zRecoTof = -99999.0;
      Float_t thetaRecoTemp = -99999.0;
      Float_t phiRecoTemp = -99999.0;
      Float_t tofDele = -99999.0;
      if(dTofReconstructed) {
	CglSnglTrack *sngl = cgl->get_track(iCgl);
        short tofRecID = sngl->get_tofrecid();  // short is how CglTrack stores things (??)
        if(tofRecID>=0) {
	  unsigned tofRecIDu = tofRecID;

	  if(tofRecIDu < dTofReconstructed->RowCount()) { // and unsigned is how RowCount is returned (stupid isn't it)
	    tofRecon = dTofReconstructed->get_tof(tofRecID); // and now we get the TOF after all
	    Float_t xRecoTof = dTofReconstructed->get_xtof(0,tofRecID);
	    Float_t yRecoTof = dTofReconstructed->get_xtof(1,tofRecID);
	    zRecoTof = dTofReconstructed->get_xtof(2,tofRecID);
	  
	    phiRecoTemp =  DEGRAD*atan2(yRecoTof, xRecoTof);
	    if(phiRecoTemp < -90.0)
	      phiRecoTemp = 360.0 + phiRecoTemp;

	    thetaRecoTemp = DEGRAD*acos(zRecoTof/sqrt(xRecoTof*xRecoTof + yRecoTof*yRecoTof + zRecoTof*zRecoTof));
	    tofDele = dTofReconstructed->get_eloss(tofRecID);	    
	  }
	  
	}  // check for valid tofRecID

      } // check if any reconstructed TOF

      evt_cgl[40] = tofRecon;
      evt_cgl[85] = thetaRecoTemp;
      evt_cgl[86] = phiRecoTemp;
      evt_cgl[101] = tofDele;

      if(iCgl<MAXPARTICLES) {
	recoTOFSave[iCgl] = tofRecon;
	tofDeleSave[iCgl] = tofDele;
      }

      float tofPathRecon = -99999.0;
      if(PHTrackOut) {
        tofPathRecon = PHTrackOut->get_tofPathLength(iCgl);  // reconstructed TOF pathLength
	evt_cgl[41] = tofPathRecon;
      } // check on PHTrackOut

      if(tofRecon>0.0 && tofPathRecon>0.0) {
	//
	// Code to set isKaon value
	//
	hadronPID->set_TofPID();
	hadronPID->set_TofPlMom(tofRecon, tofPathRecon, ptotDch);
	int charge = 0;
	if(alphaDch<0.0&& alphaDch>-999.0)
	  charge = 1;
	if(alphaDch>0.0)
	  charge = -1;
	
	float isKaon  = -99999.0;
	if(charge != 0)
	  isKaon = hadronPID->IsKaon(charge);

	float isPion  = -99999.0;
	if(charge != 0)
	  isPion = hadronPID->IsPion(charge);

	float isProton  = -99999.0;
	if(charge != 0)
	  isProton = hadronPID->IsProton(charge);

	evt_cgl[100] = isKaon;
	evt_cgl[129] = isPion;
	evt_cgl[130] = isProton;

        float reconBeta = tofPathRecon/(speedOfLight*tofRecon);
        float massRecon = ptotDch*ptotDch*(1.0 - reconBeta*reconBeta)/(reconBeta*reconBeta);
        evt_cgl[52] = reconBeta;
        evt_cgl[53] = massRecon;
	
	if(iCgl<MAXPARTICLES) {
	  recoMassSave[iCgl] = massRecon;
	  isKaonSave[iCgl] = isKaon;
	}
        if(ptotPc3>0.0) {
          float massPerfectMomentum = ptotPc3*ptotPc3*(1.0 - reconBeta*reconBeta)/(reconBeta*reconBeta);
          evt_cgl[57] = massPerfectMomentum;
        }  // check if MC momentum available from PC3 hits
      }  // check if valid TOF and pathLength in reconstruction

      //
      // If there is no pc3Track then there will not be registered and TOF projection information
      // So if pc3Track doesn't exist, we use the mcTrack value from DCPC1Track
      //
      int tofTestTrack = pc3Track;
      if(tofTestTrack<1)
	tofTestTrack = mcTrack;

      if(tofTestTrack>0 && tofghit) {
        float tofMcTrack = 0;
        int tofMcTrackN = 0;
        float tofPtot = 0;
	float phiMcTempSum = 0.0;
	float thetaMcTempSum = 0.0;
	float distMcTempSum = 0.0;
        for(unsigned int kTof=0; kTof<nTofGHit; kTof++) {
          if(tofTestTrack==tofghit->get_mctrack(kTof)) {
            tofMcTrack += tofghit->get_tof(kTof);
            float tofPx = tofghit->get_p_m(0,kTof);
            float tofPy = tofghit->get_p_m(1,kTof);
            float tofPz = tofghit->get_p_m(2,kTof);
            tofPtot += sqrt(tofPx*tofPx + tofPy*tofPy + tofPz*tofPz);

	    Float_t xMcTof = tofghit->get_pos_m(0,kTof);
	    Float_t yMcTof = tofghit->get_pos_m(1,kTof);
	    Float_t zMcTof = tofghit->get_pos_m(2,kTof);

	    float phiMcTemp =  DEGRAD*atan2(yMcTof, xMcTof);
	    if(phiMcTemp < -90.0)
	      phiMcTemp = 360.0 + phiMcTemp;

	    Float_t distMCTemp = sqrt(xMcTof*xMcTof + yMcTof*yMcTof + zMcTof*zMcTof);
	    float thetaMcTemp = DEGRAD*acos(zMcTof/distMCTemp);

	    phiMcTempSum += phiMcTemp;
	    thetaMcTempSum += thetaMcTemp;
	    distMcTempSum += distMCTemp;

            tofMcTrackN++;
          } // check if matching track number (use PC3 track since Dch is not working ??)
        } // loop over TOF GEANT hits
        
	if(tofMcTrackN>0) {
          tofMcTrack = tofMcTrack/float(tofMcTrackN);
          tofPtot = tofPtot/float(tofMcTrackN);

          evt_cgl[39] = tofMcTrack;  // GEANT TOF average for time at TOF subsystem
          evt_cgl[67] = tofPtot;  // GEANT TOF average for momentum at TOF subsystem

	  if(iCgl<MAXPARTICLES)
	    mcTOFSave[iCgl] = tofMcTrack;

	  evt_cgl[83] = thetaMcTempSum/float(tofMcTrackN);  // true GEANT Theta angle (averaged)
	  evt_cgl[84] = phiMcTempSum/float(tofMcTrackN);    // true GEANT Phi angle (averaged)
	  evt_cgl[96] = distMcTempSum/float(tofMcTrackN);   // true GEANT total distance

	  tofMcPathLength = -99999.0;
	  if(pc3PathLength>0 && pc3Track>0 && pc3Tof>0)
	    tofMcPathLength = pc3PathLength*tofMcTrack/pc3Tof;  // scale the pathLength to the TOF position

          evt_cgl[42] = tofMcPathLength;
          float mcBetaTof = -99999.0;
          if(tofMcPathLength>0.0 && tofMcTrack>0.0)
            mcBetaTof = tofMcPathLength/(speedOfLight*tofMcTrack);

          float mcTofMass = -99999.0;
          if(mcBetaTof>0.0)
            mcTofMass = ptotPc3*ptotPc3*(1.0 - mcBetaTof*mcBetaTof)/(mcBetaTof*mcBetaTof);  // redundant check

          evt_cgl[51] = mcTofMass;

	  Float_t xProTof = PHTrackOut->get_projectionTof(iCgl,0);
	  Float_t yProTof = PHTrackOut->get_projectionTof(iCgl,1);
	  Float_t zProTof = PHTrackOut->get_projectionTof(iCgl,2);

	  Float_t phiProTemp =  DEGRAD*atan2(yProTof, xProTof);
	  if(phiProTemp < -90.0)
	    phiProTemp = 360.0 + phiProTemp;

	  Float_t distTofPro = sqrt(xProTof*xProTof + yProTof*yProTof + zProTof*zProTof);
	  Float_t thetaProTemp = DEGRAD*acos(zProTof/distTofPro);

	  evt_cgl[87] = thetaProTemp;
	  evt_cgl[88] = phiProTemp;

          Float_t  dSigZTof = match->d_TOF_z_match(ptotDchsign, zRecoTof - zProTof);
          Float_t  dSigPhiTof = match->d_TOF_phi_match(ptotDchsign, (phiRecoTemp - phiProTemp)/DEGRAD);
	  evt_cgl[94] = dSigZTof;
	  evt_cgl[95] = dSigPhiTof;
	  evt_cgl[97] = distTofPro;

	  if(iCgl < MAXPARTICLES) {
	    dSigZTOFSave[iCgl] = dSigZTof;
	    dSigPTOFSave[iCgl] = dSigPhiTof;
	  }

        } // check for at least one TOF GEANT match
      } // check for valid pc3Track and TOF hits

      if(tofRecon>0.0 && tofMcPathLength>0.0 && ptotDch>0.0) {
        float betaPerfectPathLength = tofMcPathLength/(speedOfLight*tofRecon);
        float massPerfectPathLength = ptotDch*ptotDch*(1.0 - betaPerfectPathLength*betaPerfectPathLength)/
	  (betaPerfectPathLength*betaPerfectPathLength); 
        evt_cgl[58] = massPerfectPathLength;
      } // check on valid TOF reconstructed, MC path length to TOF, and valid DC momentum

      //
      // Additions for EMCal
      //

      int emcClusID = sngl->get_emcclusid();

      //
      // Do we need to worry about separate East and West, PbGl and PbSc?
      //
      Float_t xProEmc = PHTrackOut->get_projectionEmc(iCgl,0);
      Float_t yProEmc = PHTrackOut->get_projectionEmc(iCgl,1);
      Float_t zProEmc = PHTrackOut->get_projectionEmc(iCgl,2);

      Float_t phiProTemp =  DEGRAD*atan2(yProEmc, xProEmc);
      if(phiProTemp < -90.0)
	phiProTemp = 360.0 + phiProTemp;
      
      Float_t distEmcPro = sqrt(xProEmc*xProEmc + yProEmc*yProEmc + zProEmc*zProEmc);
      Float_t thetaProTemp = DEGRAD*acos(zProEmc/distEmcPro);

      evt_cgl[106] = thetaProTemp;
      evt_cgl[107] = phiProTemp;
      double emcPathLength = PHTrackOut->get_emcPathLength(iCgl);
      evt_cgl[108] = emcPathLength;

      // cout << "\n emcClusID " << emcClusID;
      // cout << ",  emcClusterList " << emcClusterList << endl;

      if(emcClusID>=0 && emcClusterList) {

	Float_t dchBeta =  DchTracks->get_beta(iCgl);

	//
	// pick up entry in emcClusterList
	//
	emcClusterContent* emcCluster = emcClusterList->findCluster(emcClusID);

	// cout << "\n emcCluster "  << emcCluster << endl;

	if(emcCluster) {

	  Float_t xEmc = emcCluster->x();
	  Float_t yEmc = emcCluster->y();
	  Float_t zEmc = emcCluster->z();

	  Float_t emcDistSq = xEmc*xEmc + yEmc*yEmc + zEmc*zEmc;
	  Float_t thetaTemp = -88888.0;
          Float_t phiTemp = -888888.0;

	  if(emcDistSq>0.0) {
	    thetaTemp = DEGRAD*acos(zEmc/sqrt(emcDistSq));

	    phiTemp =  DEGRAD*atan2(yEmc, xEmc);
	    if(phiTemp < -90.0)
	      phiTemp = 360.0 + phiTemp;
	  }

	  Float_t dSigPhiEmc = -8888.0;
	  Float_t dSigZEmc = -8888.0;
	  if(phiTemp>90.0&&phiTemp<168.75) {
	    dSigZEmc = match->d_PBSCe_z_match(ptotDchsign, dchBeta, zEmc - zProEmc, 3);  // assume East Arm, PbSc
	    dSigPhiEmc = match->d_PBSCe_phi_match(ptotDchsign, (phiTemp - phiProTemp)/DEGRAD, 3); // East Arm, PbSc
	  } // PbSc in East

	  if(phiTemp>-90.0&&phiTemp<90.0) {
	    dSigPhiEmc = match->d_PBSCw_phi_match(ptotDchsign, (phiTemp - phiProTemp)/DEGRAD, 3); // West Arm
	    dSigZEmc = match->d_PBSCw_z_match(ptotDchsign, dchBeta, zEmc - zProEmc, 3);  // West Arm
	  }  // West Arm, PbSc

	  if(phiTemp>168.75&&phiTemp<270) {
	    dSigPhiEmc = match->d_PBGL_phi_match(ptotDchsign, (phiTemp - phiProTemp)/DEGRAD, 3);
	    dSigZEmc = match->d_PBGL_z_match(ptotDchsign, dchBeta, zEmc - zProEmc, 3);
	  } // PbGl, East Arm only

	  Float_t emcEnergyCorr = emcCluster->ecore();

	  Float_t emcTofCorr = emcCluster->tofcorr();
	  double flashTime = 0.0;
	  if(emcTofCorr>-5.0&&emcTofCorr<16.0&&emcDistSq>0.0) {

	    //
	    // PHOTON flash correction
	    //
	    float zStart = bbcZVert;
	    if(fabs(zStart)>50.0) {
	      zStart = z0_event;
	    }

	    Float_t photonPath = xEmc*xEmc + yEmc*yEmc + (zEmc-zStart)*(zEmc - zStart);

	    if(photonPath> 0.0)
	      flashTime = sqrt(photonPath)/speedOfLight;

	    emcTofCorr += flashTime;

	  }

	  // cout << "\n emcCluster tofcorr " << emcCluster->tofcorr();
	  // cout << ",  ecore " << emcCluster->ecore() << ",  e " << emcCluster->e(); 
	  // cout << ",  emcTofCorr" << emcTofCorr;
	  // cout << ",  photon flashTime " << flashTime << endl;

	  if(emcPathLength>0.0 && emcTofCorr>0.0) {
	    double emcBeta = emcPathLength/(speedOfLight*emcTofCorr);
	    evt_cgl[109] = emcBeta;
	    if(emcBeta>0.0) {
	      double emcMass = pTot*pTot*(1.0 - emcBeta*emcBeta)/(emcBeta*emcBeta);
	      evt_cgl[110] = emcMass;
	    }  // check on emcBeta
	  } // check on EMC path length and EMC tof
	  
	  evt_cgl[102] = dSigZEmc;
	  evt_cgl[103] = dSigPhiEmc;
	  evt_cgl[104] = emcEnergyCorr;
	  evt_cgl[105] = emcTofCorr;
	  evt_cgl[111] = thetaTemp;
	  evt_cgl[112] = phiTemp;
	  evt_cgl[113] = emcCluster->warnmap();
          evt_cgl[114] = emcCluster->deadmap();

	  if(dEmcGeaClusterTrack && nEmcCluster>0) {
	    //
	    // Check for MC evaluation results
	    //
	    for(int iRow=0; iRow<nEmcCluster; iRow++) {
	      if(emcClusID==dEmcGeaClusterTrack->get_clusid(iRow)) {
		evt_cgl[115] = dEmcGeaClusterTrack->get_trkno(0, iRow);
		evt_cgl[116] = dEmcGeaClusterTrack->get_trkno(1, iRow);
		evt_cgl[117] = dEmcGeaClusterTrack->get_trkno(2, iRow);
		evt_cgl[118] = dEmcGeaClusterTrack->get_edep(0, iRow);
		evt_cgl[119] = dEmcGeaClusterTrack->get_edep(1, iRow);
		evt_cgl[120] = dEmcGeaClusterTrack->get_edep(2, iRow);
		evt_cgl[121] = dEmcGeaClusterTrack->get_mease(iRow);
		evt_cgl[122] = dEmcGeaClusterTrack->get_ecore(iRow);
		break;
	      } // check for matching emcClusID

	    } // loop over MC entries

	  } // check for MC entries

	  if(iCgl < MAXPARTICLES) {
	    dSigZEMCSave[iCgl] = dSigZEmc;
	    dSigPEMCSave[iCgl] = dSigPhiEmc;
	    emcTofSave[iCgl] = emcTofCorr;
	    emcMassSave[iCgl] = evt_cgl[110];
	  }

	}  // check on existing emcCluster

      } // check on associated EMCal


      //
      // Additions for TEC
      //

      int tecTrackID = sngl->get_tectrackid();

      if(TecOut && tecTrackID>=0) {

	evt_cgl[123] = tecTrackID;

        float xTec = TecOut->getTrackXin(tecTrackID);
	float yTec = TecOut->getTrackYin(tecTrackID);

	float phiTemp =  DEGRAD*atan2(yTec, xTec);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	evt_cgl[124] = phiTemp;

	xTec = TecOut->getTrackXout(tecTrackID);
	yTec = TecOut->getTrackYout(tecTrackID);

	phiTemp =  DEGRAD*atan2(yTec, xTec);
	if(phiTemp < -90.0)
	  phiTemp = 360.0 + phiTemp;

	evt_cgl[125] = phiTemp;

	evt_cgl[126] = TecOut->getTrackdEdX(tecTrackID);
	evt_cgl[117] = TecOut->getTrackSide(tecTrackID);
	evt_cgl[128] = TecOut->getTrackSector(tecTrackID);

      } // check for TEC association

      Int_t pcMissed = 0;
      if(mcDCPC1Track>0 && pc1ghit) {
	if(pcDomSum==0 || pcDomSum==10 || pcDomSum==100 || pcDomSum==110){
	  Int_t pc1Missed = 0;
	  for(unsigned int kPc1=0; kPc1<pc1ghit->RowCount(); kPc1++) {
	    Int_t mcPadTrack = pc1ghit->get_mctrack(kPc1);
	    if(mcPadTrack == mcDCPC1Track) {
	      pc1Missed = 1;  // GEANT hit was present
	      break;
	    }
	  } // loop over GEANT hits
	  if(pc1Missed == 1) {
	    if(dPc1GhitClus) {
	      for(unsigned int kPc1=0; kPc1<dPc1GhitClus->RowCount(); kPc1++) {
		Int_t pcghitID = dPc1GhitClus->get_ghitid(kPc1);
		Int_t mcPadTrack = pc1ghit->get_mctrack(pcghitID);
		if(mcPadTrack == mcDCPC1Track) {
		  pc1Missed = 2;  // GEANT hit was present and the cluster finder found the track
		  break;
		}
	      } // loop on dPc1GhitClus
	    } // check if valid dPc1GhitClus
	  } // check cluster finder, if there was a GEANT hit
	  pcMissed = pc1Missed;
	} // PC1 not matched, check if mcDCPC1Track was actually present in GEANT, and in cluster finder
        
      } // check if there were any PC1 hits
      
      if(pc2ghit) {
	if(pcDomSum==0 || pcDomSum==1 || pcDomSum==100 || pcDomSum==101){
	  Int_t pc2Missed = 0;
	  for(unsigned int kPc2=0; kPc2<pc2ghit->RowCount(); kPc2++) {
	    Int_t mcPadTrack = pc2ghit->get_mctrack(kPc2);
	    if(mcPadTrack == mcDCPC1Track) {
	      pc2Missed = 10;  // GEANT hit was present
	      break;
	    }
	  } // loop over GEANT hits
	  if(pc2Missed == 10) {
	    if(dPc2GhitClus) {
	      for(unsigned int kPc2=0; kPc2<dPc2GhitClus->RowCount(); kPc2++) {
		Int_t pcghitID = dPc2GhitClus->get_ghitid(kPc2);
		Int_t mcPadTrack = pc2ghit->get_mctrack(pcghitID);
		if(mcPadTrack == mcDCPC1Track) {
		  pc2Missed = 20;  // GEANT hit was present and the cluster finder found the track
		  break;
		}
	      } // loop on dPc2GhitClus
	    } // check if valid dPc2GhitClus
	  } // check cluster finder, if there was a GEANT hit
	  pcMissed += pc2Missed;
	} // PC2 not matched, check if mcDCPC1Track was actually present in GEANT, and in cluster finder
        
      } // check if there were any PC2 hits

      if(pc3ghit) {
	if(pcDomSum==0 || pcDomSum==1 || pcDomSum==10 || pcDomSum==11){
	  Int_t pc3Missed = 0;
	  for(unsigned int kPc3=0; kPc3<pc3ghit->RowCount(); kPc3++) {
	    Int_t mcPadTrack = pc3ghit->get_mctrack(kPc3);
	    if(mcPadTrack == mcDCPC1Track) {
	      pc3Missed = 100;  // GEANT hit was present
	      break;
	    }
	  } // loop over GEANT hits
	  if(pc3Missed == 100) {
	    if(dPc3GhitClus) {
	      for(unsigned int kPc3=0; kPc3<dPc3GhitClus->RowCount(); kPc3++) {
		Int_t pcghitID = dPc3GhitClus->get_ghitid(kPc3);
		Int_t mcPadTrack = pc3ghit->get_mctrack(pcghitID);
		if(mcPadTrack == mcDCPC1Track) {
		  pc3Missed = 200;  // GEANT hit was present and the cluster finder found the track
		  break;
		}
	      } // loop on dPc3GhitClus
	    } // check if valid dPc3GhitClus
	  } // check cluster finder, if there was a GEANT hit
	  pcMissed += pc3Missed;
	} // PC3 not matched, check if mcDCPC1Track was actually present in GEANT, and in cluster finder
        
      } // check if there were any PC3 hits

      evt_cgl[23] = pcMissed;  // bit pattern for presence of mcDCPC1Track in PC hits and cluster finder 
      evt_cgl[31] = pc1Multi;  
      evt_cgl[32] = pc2Multi;  
      evt_cgl[33] = pc3Multi;

      evaluateCglNtuple->Fill(evt_cgl);

    } // loop over global tracks

  } // check on at least one global track

  if(getMode() >= 1 && nCglTracks>1) {

    //
    // setup a data structure system for the particle information
    // the particle structure will be filled in each event
    // the particle structure will then be scanned to get the pair information
    //
    struct storeParticleData {
      int icgl;
      int nfile;
      float px;
      float py;
      float pz;
      float pxVert;
      float pyVert;
      float pzVert;
      float ptot;
      float ptheta;
      float pphi;
      float ptotVert;
      float pthetaVert;
      float pphiVert;
      float rVert;
      float zVert;
      float emclcorr;
      float dcalpha;
      float dcqual;
      float dSigZEMC;
      float dSigPEMC;
      float emcMass;
      float emcTof;
      float dSigZPC2;
      float dSigPPC2;
      float dSigZPC3;
      float dSigPPC3;
      float dSigZTOF;
      float dSigPTOF;
      int idPart;
      float massSquared;
      float recoMassSquared;
      float mcTOF;
      float recoTOF;
      float isKaon;
      float tofDele;
      float pc3Theta;
      float pc3Phi;
      int idParentPc1;
      int idParentPc3;
      float dchZed;
      float dchPhi;
      int pcDomSum;
    };                               // design of a storeParticleData structure

    storeParticleData particleData[MAXPARTICLES];  // create an array of storeParticleData structures
  
    //
    // Loop over CglTracks for pair mass evaluation
    //
    Int_t iStore = 0;
    for(int iCgl=0; iCgl<nCglTracks; iCgl++) {
      if(iCgl< MAXPARTICLES && PHCentralTracks) {
	iStore++;
	particleData[iCgl].icgl = iCgl;  

	Float_t px = PHCentralTracks->get_px(iCgl); // px component of reconstructed momentum
        Float_t py = PHCentralTracks->get_py(iCgl); // py component of reconstructed momentum
	Float_t pz = PHCentralTracks->get_pz(iCgl); // pz component of reconstructed momentum

	Float_t pTot = sqrt(px*px + py*py + pz*pz);
	Float_t pTheta = DEGRAD*acos(pz/sqrt(px*px + py*py + pz*pz));
	Float_t pPhi = DEGRAD*atan2(py, px);

	if(pPhi < -90.0)
	  pPhi += 360.0;

	particleData[iCgl].px = px; // px component of reconstructed momentum
	particleData[iCgl].py = py; // py component of reconstructed momentum
	particleData[iCgl].pz = pz; // pz component of reconstructed momentum

	particleData[iCgl].ptot = pTot;           // total momentum
	particleData[iCgl].ptheta = pTheta;       // theta direction of momentum
	particleData[iCgl].pphi = pPhi;           // phi direction of momentum
	particleData[iCgl].nfile = nFileSave[iCgl];  // merge file source ID
	particleData[iCgl].pcDomSum = pcDomSumSave[iCgl]; // bit pattern for PC clusters

	int idPart = idPartSave[iCgl]; 
	particleData[iCgl].idPart = idPart;
	if(idPart>0 && idPart<NGEANTMASS)
	  particleData[iCgl].massSquared = geantMass[idPart-1]*geantMass[idPart-1];
	else
	  particleData[iCgl].massSquared = 0.0;

	particleData[iCgl].recoMassSquared = recoMassSave[iCgl];
	particleData[iCgl].mcTOF = mcTOFSave[iCgl];
	particleData[iCgl].recoTOF = recoTOFSave[iCgl];
	particleData[iCgl].isKaon = isKaonSave[iCgl];
	particleData[iCgl].tofDele = tofDeleSave[iCgl];

	particleData[iCgl].dSigZPC2 = dSigZPC2Save[iCgl];
	particleData[iCgl].dSigPPC2 = dSigPPC2Save[iCgl];
	particleData[iCgl].dSigZPC3 = dSigZPC3Save[iCgl];
	particleData[iCgl].dSigPPC3 = dSigPPC3Save[iCgl];
	particleData[iCgl].dSigZTOF = dSigZTOFSave[iCgl];
	particleData[iCgl].dSigPTOF = dSigPTOFSave[iCgl];
	particleData[iCgl].dSigZEMC = dSigZEMCSave[iCgl];
        particleData[iCgl].dSigPEMC = dSigPEMCSave[iCgl];
        particleData[iCgl].emcMass  = emcMassSave[iCgl];
        particleData[iCgl].emcTof   = emcTofSave[iCgl];
	Float_t dchAlpha = -9999.0;
	Float_t dchQuality = -9999.0;
        Float_t dchZed = -99999.0;
        Float_t dchPhi = -9999.0;
	CglSnglTrack *sngl = cgl->get_track(iCgl);
	if(DchTracks) {
	  unsigned int dchTrack = sngl->get_dctracksid();
	  dchAlpha = DchTracks->get_alpha(dchTrack);
	  dchQuality =  DchTracks->get_quality(dchTrack);
          dchZed = DchTracks->get_zed(dchTrack);
          dchPhi = DchTracks->get_phi(dchTrack);
	} // check on DchTracks being present
	particleData[iCgl].dcalpha = dchAlpha;
	particleData[iCgl].dcqual = dchQuality;
	particleData[iCgl].dchZed = dchZed;
	particleData[iCgl].dchPhi = dchPhi;

	particleData[iCgl].pc3Theta = pc3ThetaSave[iCgl];
	particleData[iCgl].pc3Phi = pc3PhiSave[iCgl];
	particleData[iCgl].idParentPc1 = idParentSavePc1[iCgl];
	particleData[iCgl].idParentPc3 = idParentSavePc3[iCgl];

        particleData[iCgl].ptotVert = ptotVertSave[iCgl];
        particleData[iCgl].pthetaVert = pthetaVertSave[iCgl];
        particleData[iCgl].pphiVert = pphiVertSave[iCgl];
        Float_t cosThetVert = cos(pthetaVertSave[iCgl]/DEGRAD);
        Float_t pTranVert = ptotVertSave[iCgl]*sin(pthetaVertSave[iCgl]/DEGRAD);
        particleData[iCgl].pzVert = ptotVertSave[iCgl]*cosThetVert;
        particleData[iCgl].pxVert = pTranVert*cos(pphiVertSave[iCgl]/DEGRAD);
        particleData[iCgl].pyVert = pTranVert*sin(pphiVertSave[iCgl]/DEGRAD);
	particleData[iCgl].rVert = rVertSave[iCgl];
	particleData[iCgl].zVert = zVertSave[iCgl];

      } // check on maximum number of particles
      
    } // loop over iCgl tracks

    //
    //  Now create the pair mass NTUPLE for this event
    //  Reaction plane NTUPLE is stored at the same time
    //

    Float_t evt_reacPlane[NTPL_REACPLANE];
    for(Int_t kPair=0; kPair<NTPL_REACPLANE; kPair++) {
      evt_reacPlane[kPair] = -999.0;
    }

    Float_t evt_pair[NTPL_PARAMPAIR];
    for(Int_t kPair=0; kPair<NTPL_PARAMPAIR; kPair++) {
      evt_pair[kPair] = -999.0;
    }

    evt_reacPlane[NTPL_REACPLANE - 1] = iEvent;
    evt_reacPlane[NTPL_REACPLANE - 2] = pisaEventHeader->GetReactionPlaneAngle();
    double reacPlaneRadian = (pisaEventHeader->GetReactionPlaneAngle())*3.14159/180.0;
    double cosRCP = cos(reacPlaneRadian);
    double sinRCP = sin(reacPlaneRadian);

    evt_pair[NTPL_PARAMPAIR - 1] = iEvent;
    evt_pair[NTPL_PARAMPAIR - 2] = pisaEventHeader->GetImpactParameter();
    evt_pair[NTPL_PARAMPAIR - 3] = pisaEventHeader->GetZvertex();

    evt_pair[NTPL_PARAMPAIR - 4] = BbcOut->get_VertexPoint();
    evt_pair[NTPL_PARAMPAIR - 5] = BbcOut->get_nPmt(0);
    evt_pair[NTPL_PARAMPAIR - 6] = BbcOut->get_nPmt(1);
    evt_pair[NTPL_PARAMPAIR - 7] = TecOut->getNTracks();
    evt_pair[NTPL_PARAMPAIR - 8] = nPc3Clus;
    evt_pair[NTPL_PARAMPAIR - 9] = nPc2Clus;
    evt_pair[NTPL_PARAMPAIR - 10] = nPc1Clus;
    evt_pair[NTPL_PARAMPAIR - 11] = nDchTracks;
    evt_pair[NTPL_PARAMPAIR - 12] = BbcOut->get_TimeZero();
    evt_pair[NTPL_PARAMPAIR - 13] = vtxOutZ0;
    evt_pair[NTPL_PARAMPAIR - 14] = t0OutT0;

    Int_t iPair = 1;
    int kMode = getMode();
    for(Int_t jStore=0; jStore<iStore-1; jStore++) {

      //
      // Do only Kaon pairs, or pairs from a Lambda parent, or all if kMode = 2
      //
      int jPart = particleData[jStore].idPart;
      int jParent = particleData[jStore].idParentPc3;
      if(kMode !=2 && jParent !=57 && jParent !=18 && jPart !=11 && jPart !=12 && jPart !=51 && jPart!=52)
	continue; // keep Kaons or lambda parents

      Float_t px1 = particleData[jStore].px;
      Float_t py1 = particleData[jStore].py;
      Float_t pz1 = particleData[jStore].pz;
      Float_t ptot1 = particleData[jStore].ptot;
      Float_t et1 = sqrt(particleData[jStore].massSquared + ptot1*ptot1);
      Float_t et1Kaon = sqrt(kaonMassSquared + ptot1*ptot1);
      Float_t ptot1Miss01 = 1.01*ptot1; // assume 1% miscalibration of the momentum
      Float_t et1Kaon01 = sqrt(kaonMassSquared + ptot1Miss01*ptot1Miss01);

      Float_t px1Vert = particleData[jStore].pxVert;
      Float_t py1Vert = particleData[jStore].pyVert;
      Float_t pz1Vert = particleData[jStore].pzVert;
      //
      // Following line is flagged for an uninitialized variable
      //
      Float_t ptot1Vert = particleData[jStore].ptotVert;
      Float_t et1Vert = sqrt(particleData[jStore].massSquared + ptot1Vert*ptot1Vert);

      Float_t angErr1 = -999.0;
      Float_t cosAngErr1 = (px1*px1Vert + py1*py1Vert + pz1*pz1Vert)/(ptot1*ptot1Vert);
      if(fabs(cosAngErr1)<1.0)
        angErr1 = DEGRAD*acos(cosAngErr1);

      evt_pair[1] = particleData[jStore].icgl;
      evt_pair[2] = particleData[jStore].ptot;
      evt_pair[3] = particleData[jStore].ptheta;
      evt_pair[4] = particleData[jStore].pphi;
      evt_pair[5] = particleData[jStore].dcqual;
      evt_pair[6] = particleData[jStore].dcalpha;
      evt_pair[7] = particleData[jStore].nfile;
      evt_pair[8] = particleData[jStore].dSigZPC2;
      evt_pair[9] = particleData[jStore].dSigPPC2;
      evt_pair[10] = particleData[jStore].dSigZPC3;
      evt_pair[11] = particleData[jStore].dSigPPC3;
      evt_pair[12] = particleData[jStore].dSigZTOF;
      evt_pair[13] = particleData[jStore].dSigPTOF;
      evt_pair[14] = particleData[jStore].idPart;
      evt_pair[15] = particleData[jStore].recoMassSquared;
      evt_pair[16] = particleData[jStore].mcTOF;
      evt_pair[17] = particleData[jStore].recoTOF;
      evt_pair[18] = particleData[jStore].pc3Phi;
      evt_pair[19] = particleData[jStore].pc3Theta;
      evt_pair[20] = particleData[jStore].idParentPc1;
      evt_pair[21] = particleData[jStore].idParentPc3;
      evt_pair[22] = particleData[jStore].dchZed;
      evt_pair[23] = particleData[jStore].dchPhi;
      evt_pair[24] = particleData[jStore].pcDomSum;
      evt_pair[25] = ptot1Vert;
      evt_pair[26] = angErr1;
      evt_pair[27] = particleData[jStore].rVert;
      //
      // Following line is flagged for an uninitialized variable
      //
      evt_pair[28] = particleData[jStore].zVert;
      evt_pair[29] = particleData[jStore].isKaon;
      evt_pair[30] = particleData[jStore].tofDele;
      evt_pair[31] = particleData[jStore].dSigZEMC;
      evt_pair[32] = particleData[jStore].dSigPEMC;
      evt_pair[33] = particleData[jStore].emcTof;
      evt_pair[34] = particleData[jStore].emcMass;

      //
      // reaction plane NTUPLE
      //
      evt_reacPlane[1] = particleData[jStore].ptot;
      evt_reacPlane[2] = particleData[jStore].ptheta;
      evt_reacPlane[3] = particleData[jStore].pphi;
      evt_reacPlane[4] = particleData[jStore].dcqual;
      evt_reacPlane[5] = particleData[jStore].nfile;
      evt_reacPlane[6] = ptot1Vert;
      evt_reacPlane[7] = DEGRAD*acos(pz1Vert/ptot1Vert);
      evt_reacPlane[8] = DEGRAD*atan2(py1Vert, px1Vert);

      for(Int_t kStore=jStore+1; kStore<iStore; kStore++) {
	//
	// Do only Kaon pairs, or lambda parents
	//
	int kPart = particleData[kStore].idPart;
	int kParent = particleData[kStore].idParentPc3;
	if(kMode !=2 && kParent !=57 && kParent !=18 && kPart !=11 && kPart !=12 && kPart !=51 && kPart!=52)
	  continue;

	Float_t px2 = particleData[kStore].px;
	Float_t py2 = particleData[kStore].py;
	Float_t pz2 = particleData[kStore].pz;
	Float_t ptot2 = particleData[kStore].ptot;
	Float_t et2 = sqrt(particleData[kStore].massSquared + ptot2*ptot2);
	Float_t et2Kaon = sqrt(kaonMassSquared + ptot2*ptot2);
	Float_t ptot2Miss01 = 1.01*ptot2; // assume 1% miscalibration of the momentum
	Float_t et2Kaon01 = sqrt(kaonMassSquared + ptot2Miss01*ptot2Miss01);

        Float_t openAngleReco = -999.0;
        Float_t cosOpenReco = (px1*px2 + py1*py2 + pz1*pz2)/(ptot1*ptot2);
        if(fabs(cosOpenReco)<1.0)
          openAngleReco = DEGRAD*acos(cosOpenReco);
	
        Float_t px2Vert = particleData[kStore].pxVert;
        Float_t py2Vert = particleData[kStore].pyVert;
        Float_t pz2Vert = particleData[kStore].pzVert;
	Float_t ptot2Vert = particleData[kStore].ptotVert;
	//
	// Following line is flagged for an uninitialized variable
	//
	Float_t et2Vert = sqrt(particleData[kStore].massSquared + ptot2Vert*ptot2Vert);

        Float_t angErr2 = -999.0;
        Float_t cosAngErr2 = (px2*px2Vert + py2*py2Vert + pz2*pz2Vert)/(ptot2*ptot2Vert);
        if(fabs(cosAngErr2)<1.0)
          angErr2 = DEGRAD*acos(cosAngErr2);

        Float_t openAngleVert = -999.0;
        Float_t cosOpenVert = (px1Vert*px2Vert + py1Vert*py2Vert + pz1Vert*pz2Vert)/(ptot1Vert*ptot2Vert);
        if(fabs(cosOpenVert)<1.0)
          openAngleVert = DEGRAD*acos(cosOpenVert);
	
	Float_t aMass = (et1+et2)*(et1+et2) - (px1+px2)*(px1+px2) -
	  (py1+py2)*(py1+py2) - (pz1+pz2)*(pz1+pz2); 

	Float_t aMassKK = (et1Kaon+et2Kaon)*(et1Kaon+et2Kaon) - (px1+px2)*(px1+px2) -
	  (py1+py2)*(py1+py2) - (pz1+pz2)*(pz1+pz2);  // assume pair is wrongly identified as K+K-

	Float_t aMassKK01 = (et1Kaon01+et2Kaon01)*(et1Kaon01+et2Kaon01) -
	  1.0201*((px1+px2)*(px1+px2) +(py1+py2)*(py1+py2) + (pz1+pz2)*(pz1+pz2)); // 1% miscalibration also

	Float_t aMassVert = (et1Vert+et2Vert)*(et1Vert+et2Vert) - (px1Vert+px2Vert)*(px1Vert+px2Vert) -
	  (py1Vert+py2Vert)*(py1Vert+py2Vert) - (pz1Vert+pz2Vert)*(pz1Vert+pz2Vert); 

	if(aMass>0)
	  aMass = sqrt(aMass);
	
	if(aMassVert>0)
	  aMassVert = sqrt(aMassVert);
	
	if(aMassKK>0)
	  aMassKK = sqrt(aMassKK);

	if(aMassKK01>0)
	  aMassKK01 = sqrt(aMassKK01);

	Float_t pairPxVert = px1Vert + px2Vert;
	Float_t pairPyVert = py1Vert + py2Vert;
	Float_t pairPtranVert = sqrt((px1Vert+px2Vert)*(px1Vert+px2Vert) + (py1Vert+py2Vert)*(py1Vert+py2Vert));

	Float_t pairPx = px1 + px2;
	Float_t pairPy = py1 + py2;
	Float_t pairPtran = sqrt(pairPx*pairPx + pairPy*pairPy);
	Float_t pairPz = pz1 + pz2;
	Float_t pairPtot = sqrt(pairPtran*pairPtran + pairPz*pairPz);

	Float_t pairPzVert = pz1Vert + pz2Vert;
	Float_t pairPtotVert = sqrt(pairPtranVert*pairPtranVert + pairPzVert*pairPzVert);

	Float_t pairPTheta = -9999.0;
	if(pairPtot>0.0 && fabs(pairPz)<=pairPtot)
	  pairPTheta = DEGRAD*acos(pairPz/pairPtot);

	Float_t pairPPhi = DEGRAD*atan2(pairPy, pairPx);  // should check that both are not 0
	if(pPhi < -90.0)
	  pPhi += 360.0;

	evt_reacPlane[9] =  particleData[kStore].ptot;
	evt_reacPlane[10] = particleData[kStore].ptheta;
	evt_reacPlane[11] = particleData[kStore].pphi;
	evt_reacPlane[12] = particleData[kStore].dcqual;
	evt_reacPlane[13] = particleData[kStore].nfile;
	evt_reacPlane[14] = ptot2Vert;
	evt_reacPlane[15] = DEGRAD*acos(pz2Vert/ptot2Vert);
	evt_reacPlane[16] = DEGRAD*atan2(py2Vert, px2Vert);

	evt_pair[NPAIR_DATA + 1] = particleData[kStore].icgl;
	evt_pair[NPAIR_DATA + 2] = particleData[kStore].ptot;
	evt_pair[NPAIR_DATA + 3] = particleData[kStore].ptheta;
	evt_pair[NPAIR_DATA + 4] = particleData[kStore].pphi;
	evt_pair[NPAIR_DATA + 5] = particleData[kStore].dcqual;
	evt_pair[NPAIR_DATA + 6] = particleData[kStore].dcalpha;
	evt_pair[NPAIR_DATA + 7] = particleData[kStore].nfile;
	evt_pair[NPAIR_DATA + 8] = particleData[kStore].dSigZPC2;
	evt_pair[NPAIR_DATA + 9] = particleData[kStore].dSigPPC2;
	evt_pair[NPAIR_DATA + 10] = particleData[kStore].dSigZPC3;
	evt_pair[NPAIR_DATA + 11] = particleData[kStore].dSigPPC3;
	evt_pair[NPAIR_DATA + 12] = particleData[kStore].dSigZTOF;
	evt_pair[NPAIR_DATA + 13] = particleData[kStore].dSigPTOF;
	evt_pair[NPAIR_DATA + 14] = particleData[kStore].idPart;
	evt_pair[NPAIR_DATA + 15] = particleData[kStore].recoMassSquared;
	evt_pair[NPAIR_DATA + 16] = particleData[kStore].mcTOF;
	evt_pair[NPAIR_DATA + 17] = particleData[kStore].recoTOF;
	evt_pair[NPAIR_DATA + 18] = particleData[kStore].pc3Phi;
	evt_pair[NPAIR_DATA + 19] = particleData[kStore].pc3Theta;
	evt_pair[NPAIR_DATA + 20] = particleData[kStore].idParentPc1;
	evt_pair[NPAIR_DATA + 21] = particleData[kStore].idParentPc3;
        evt_pair[NPAIR_DATA + 22] = particleData[kStore].dchZed;
        evt_pair[NPAIR_DATA + 23] = particleData[kStore].dchPhi;
        evt_pair[NPAIR_DATA + 24] = particleData[kStore].pcDomSum;
        evt_pair[NPAIR_DATA + 25] = ptot2Vert;
        evt_pair[NPAIR_DATA + 26] = angErr2;
	evt_pair[NPAIR_DATA + 27] = particleData[kStore].rVert;
	//
	// Following line is flagged for an uninitialized variable
	//
	evt_pair[NPAIR_DATA + 28] = particleData[kStore].zVert;
	//
	// Following line is flagged for an uninitialized variable
	//
	evt_pair[NPAIR_DATA + 29] = particleData[kStore].isKaon;
	evt_pair[NPAIR_DATA + 30] = particleData[kStore].tofDele;
	evt_pair[NPAIR_DATA + 31] = particleData[kStore].dSigZEMC;
	evt_pair[NPAIR_DATA + 32] = particleData[kStore].dSigPEMC;
	evt_pair[NPAIR_DATA + 33] = particleData[kStore].emcTof;
	evt_pair[NPAIR_DATA + 34] = particleData[kStore].emcMass;

	evt_pair[NPAIR_DATA + 35] = aMass;
	evt_pair[NPAIR_DATA + 36] = pairPtran;
	evt_pair[NPAIR_DATA + 37] = pairPtot;
	evt_pair[NPAIR_DATA + 38] = pairPTheta;
	evt_pair[NPAIR_DATA + 39] = pairPPhi;
	evt_pair[NPAIR_DATA + 40] = aMassVert;
	evt_pair[NPAIR_DATA + 41] = aMassKK;
	evt_pair[NPAIR_DATA + 42] = pairPtranVert;
	evt_pair[NPAIR_DATA + 43] = aMassKK01;
	evt_pair[NPAIR_DATA + 44] = openAngleVert;
	evt_pair[NPAIR_DATA + 45] = openAngleReco;

	//
	// Transformation to the rest frame
	//
	if(pairPtotVert > 0.0 && pairPtot > 0.0) {
	  double trueBeta = sqrt(pairPtotVert*pairPtotVert/
				 (aMassVert*aMassVert + pairPtotVert*pairPtotVert));

	  double recoBeta = sqrt(pairPtot*pairPtot/
				 (aMass*aMass + pairPtot*pairPtot));

	  TVector3 betaPairVert(-pairPxVert*trueBeta/pairPtotVert,
				-pairPyVert*trueBeta/pairPtotVert,
				-pairPzVert*trueBeta/pairPtotVert);

	  TVector3 betaPairReco(-pairPx*recoBeta/pairPtot,
				-pairPy*recoBeta/pairPtot,
				-pairPz*recoBeta/pairPtot);

	  TLorentzVector v1Vert(px1Vert, py1Vert, pz1Vert, et1Vert);
	  v1Vert.Boost(betaPairVert);

	  TLorentzVector v2Vert(px2Vert, py2Vert, pz2Vert, et2Vert);
	  v2Vert.Boost(betaPairVert);

	  TLorentzVector v1Reco(px1, py1, pz1, et1);
	  v1Reco.Boost(betaPairReco);

	  TLorentzVector v2Reco(px2, py2, pz2, et2);
	  v2Reco.Boost(betaPairReco);

	  double pxTemp = v1Reco[0];   
	  double pyTemp = v1Reco[1];
	  float pxReacPlane = pxTemp*cosRCP + pyTemp*sinRCP;
	  float pyReacPlane = -pxTemp*sinRCP + pyTemp*cosRCP;

	  evt_reacPlane[17] = pxReacPlane;  // Reco reaction plane px
	  evt_reacPlane[18] = pyReacPlane;  // Reco reaction plane py
	  evt_reacPlane[19] = v1Reco[2];    // Reco reaction plane pz

	  pxTemp = v1Vert[0];   
	  pyTemp = v1Vert[1];
	  pxReacPlane = pxTemp*cosRCP + pyTemp*sinRCP;
	  pyReacPlane = -pxTemp*sinRCP + pyTemp*cosRCP;

	  evt_reacPlane[20] = pxReacPlane;  // MC reaction plane px
	  evt_reacPlane[21] = pyReacPlane;  // MC reaction plane py
	  evt_reacPlane[22] = v1Vert[2];    // MC reaction plane pz

	  pxTemp = v2Reco[0];   
	  pyTemp = v2Reco[1];
	  pxReacPlane = pxTemp*cosRCP + pyTemp*sinRCP;
	  pyReacPlane = -pxTemp*sinRCP + pyTemp*cosRCP;

	  evt_reacPlane[23] = pxReacPlane;  // Reco reaction plane px
	  evt_reacPlane[24] = pyReacPlane;  // Reco reaction plane py
	  evt_reacPlane[25] = v1Reco[2];    // Reco reaction plane pz

	  pxTemp = v2Vert[0];   
	  pyTemp = v2Vert[1];
	  pxReacPlane = pxTemp*cosRCP + pyTemp*sinRCP;
	  pyReacPlane = -pxTemp*sinRCP + pyTemp*cosRCP;

	  evt_reacPlane[26] = pxReacPlane;  // MC reaction plane px
	  evt_reacPlane[27] = pyReacPlane;  // MC reaction plane py
	  evt_reacPlane[28] = v2Vert[2];    // MC reaction plane pz

	  float rcAlpha1 = DEGRAD*acos(evt_reacPlane[18]/
						sqrt(v1Reco[0]*v1Reco[0] +
						     v1Reco[1]*v1Reco[1] +
						     v1Reco[2]*v1Reco[2]));

	  float rcAlpha2 = DEGRAD*acos(evt_reacPlane[24]/
						sqrt(v2Reco[0]*v2Reco[0] +
						     v2Reco[1]*v2Reco[1] +
						     v2Reco[2]*v2Reco[2]));

	  float mcAlpha1 = DEGRAD*acos(evt_reacPlane[21]/
						sqrt(v1Vert[0]*v1Vert[0] +
						     v1Vert[1]*v1Vert[1] +
						     v1Vert[2]*v1Vert[2]));

	  float mcAlpha2 = DEGRAD*acos(evt_reacPlane[27]/
						sqrt(v2Vert[0]*v2Vert[0] +
						     v2Vert[1]*v2Vert[1] +
						     v2Vert[2]*v2Vert[2]));

	  evt_reacPlane[29] = rcAlpha1;
	  evt_reacPlane[30] = rcAlpha2;
	  evt_reacPlane[31] = mcAlpha1;
	  evt_reacPlane[32] = mcAlpha2;
	  evt_reacPlane[33] = aMass;
          evt_reacPlane[34] = aMassVert;
          evt_reacPlane[35] = particleData[jStore].idPart +
	    particleData[kStore].idPart;
	  evt_reacPlane[36] = particleData[jStore].idParentPc1 +
	    particleData[kStore].idParentPc1;
	  
	} // check on non-zero momentum at vertex for the pair

	if(iEvent<21) {
	  cout << "\n Pair call = " << iEvent;
	  cout << ", aMass = " << aMass << endl;
	}

        if((aMass>0.8 && aMass<1.6) || 
           (aMass>2.0 && aMass<4.2)) {
	  evt_pair[0] = iPair;
	  evaluatePairNtuple->Fill(evt_pair);

	  float sig1ZPC3 = fabs(particleData[jStore].dSigZPC3);
	  if(fabs(particleData[jStore].dSigZTOF) < sig1ZPC3)
	    sig1ZPC3 = fabs(particleData[jStore].dSigZTOF);

	  float sig2ZPC3 = fabs(particleData[kStore].dSigZPC3);
	  if(fabs(particleData[kStore].dSigZTOF) < sig2ZPC3)
	    sig2ZPC3 = fabs(particleData[kStore].dSigZTOF);

	  float sig1PPC3 = fabs(particleData[jStore].dSigPPC3);
	  if(fabs(particleData[jStore].dSigZTOF) < sig1PPC3)
	    sig1PPC3 = fabs(particleData[jStore].dSigZTOF);

	  float sig2PPC3 = fabs(particleData[kStore].dSigPPC3);
	  if(fabs(particleData[kStore].dSigZTOF) < sig2PPC3)
	    sig2PPC3 = fabs(particleData[kStore].dSigZTOF);

	  if(sig1ZPC3 < 3.0 && sig2ZPC3 < 3.0 &&
	     sig1PPC3 < 3.0 && sig2PPC3 < 3.0) {
	    evt_reacPlane[0] = iPair;
	  }
	  else {
	    evt_reacPlane[0] = -iPair;
	  } // check on tracking cuts in PC3 or TOF
	   
	  evaluateReacPlaneNtuple->Fill(evt_reacPlane);

	  iPair++;
        } // restrict to window around the Phi mass or the J/Psi mass

      } // inner loop over kStore;
    } // outer loop over jStore;


  }  // check on doing pair evaluations

  return 0;
}

int EvaSimreco::ResetEvent(PHCompositeNode *topNode)
{
  return 0;
}

EvaSimreco::~EvaSimreco() {

  if(evaluateFile) {
    evaluateFile->Write();
    evaluateFile->Close();
 
    cout << "\n    Closing evalSim.root file" << endl;
  }

}
