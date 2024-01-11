#include "PHIODataNode.h"
#include "PHTable.hh"
#include "mNewDchInitializer.hh"
#include "PHDchHistogrammer.hh"

#include "PHTimeStamp.h"
#include "PHDchGeometryObject.h"
#include "PHDchCalibrationObject.h"
#include "PHDchNoiseObject.h"
#include "DchHitLineLists.hh"
#include "DchPc1HitLists.hh"

#include "dDchDCMWrapper.h"
#include "dDchNibbleGhitWrapper.h"
#include "dDchUnpackParWrapper.h"
#include "dDchRawWrapper.h"
#include "dDchGhitRawWrapper.h"
#include "dDchHitWrapper.h"
#include "dDchGhitHitsWrapper.h"
#include "dDchTracksWrapper.h"
#include "dDchTracksExtWrapper.h"
#include "dcghitWrapper.h"
#include "dDchEvalParWrapper.h"
#include "dDchReconstructionParWrapper.h"
#include "dDchFEMWrapper.h"
#include "dDchPerfParWrapper.h"
#include "dDchDCMParWrapper.h"
#include "dDchFastSimParWrapper.h"

#include "fkinWrapper.h"

#include "DchRawTablev1.hh"
#include "DchHitLineTablev1.hh"
#include "DchTrackv1.h"

#include "RunToTime.hh"
#include "recoConsts.h"

#include <cstdlib>
#include <iostream>

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<dDchDCMWrapper> dDchDCMNode_t;
typedef PHIODataNode<dDchNibbleGhitWrapper> dDchNibbleGhitNode_t;
typedef PHIODataNode<dDchUnpackParWrapper> dDchUnpackParNode_t;
typedef PHIODataNode<dDchRawWrapper> dDchRawNode_t;
typedef PHIODataNode<dDchGhitRawWrapper> dDchGhitRawNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;
typedef PHIODataNode<dcghitWrapper> dcghitNode_t;
typedef PHIODataNode<dDchEvalParWrapper> dDchEvalParNode_t;
typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<dDchPerfParWrapper> dDchPerfParNode_t;
typedef PHIODataNode<dDchReconstructionParWrapper> dDchReconstructionParNode_t;
typedef PHIODataNode<dDchFEMWrapper> dDchFEMNode_t;
typedef PHIODataNode<dDchDCMParWrapper> dDchDCMParNode_t;
typedef PHIODataNode<dDchFastSimParWrapper> dDchFastSimParNode_t;
typedef PHIODataNode<PHObject> PHObjectNode_t;

typedef PHIODataNode<DchTrack> DchTrackNode_t;

using namespace std;

mNewDchInitializer::mNewDchInitializer(short flag, short histo, short dbAccess, int runnumber)
{
  flagMC = flag;
  isTree = False;
  dchGeometryObject    = 0;
  dchAddressObject     = 0;
  dchCalibrationObject = 0;
  dchNoiseObject       = 0;
  dchHistogrammer      = 0;
  readFromDB           = dbAccess;
  iniHisto             = histo;
  RunNumber            = runnumber;
  setFileNames();
}

void mNewDchInitializer::setGeometryFileNames(const char* info, const char* wire, const char* frame)
{
#ifdef DEBUG
  cout << "Update Geometry File Names " << endl;
#endif
  geoInfoFile = info;
  geoWireFile = wire;
  geoFrameFile = frame;
}

void mNewDchInitializer::setNoiseFileName(const char* noise, const char* effi)
{
#ifdef DEBUG
  cout << "Update Noise File Names " << endl;
#endif
  noiseFile = noise;
#ifdef DEBUG
  cout << "Update Efficiency File Names "<< endl;
#endif
  effiFile = effi;

}
void mNewDchInitializer::setCalibrationFileName(const char* cal,const char* slew,const char* local, const char* stereo)
{
  cout << "mNewDchInitializer::setCalibrationFileName - Update Calibration File Names " << endl;
  calFile = cal;
  slewFile = slew;
  localFile = local;
  stereoFile = stereo;
}

void mNewDchInitializer::setFileNames()
{
  geoInfoFile  = "DchGeometry.info";  // default is Monte-Carlo
  geoWireFile  = "DchGeometry.wireMc";
  geoFrameFile = "DchGeometry.frameMc";
  noiseFile    = "DchNoise.Mc";
  calFile      = "DchCalibration.Mc";
  slewFile     = "DchCalibrationSlew.Mc";
  localFile    = "DchCalibrationLocal.Mc";
  stereoFile   = "DchCalibrationStereo.Mc";
  effiFile     = "DchEfficiency.Real2001";
  addressNameDB  = "calibdch_v3_add";   // database default
  geometryNameDB = "calibdch_v3_geo";   
  calibNameDB    = "calibdch_v3_cal";    
  noiseNameDB    = "calibdch_v3_noise";
}

PHBoolean mNewDchInitializer::constructPhoolTree(PHCompositeNode* root)
{
  PHObjectNode_t *phob;
  PHNodeIterator i(root);

  if(!isTree){
    int* tmpEvent = 0;
    PHNode *n;
    TableNode_t *d;
    PHTable *w;
    PHCompositeNode *parNode, *dchNode, *dstNode, *outNode,*dcmNode, *evaNode;
    PHCompositeNode *doNode,*tmpNode;

    PHDataNode<PHDchAddressObject>*     dchDaoNode;
    PHDataNode<PHDchGeometryObject>*    dchDgoNode;
    PHDataNode<PHDchCalibrationObject>* dchDcoNode;
    PHDataNode<PHDchNoiseObject>*       dchDnoNode;
    PHDataNode<PHDchHistogrammer>*      dchHistoNode;

    PHDataNode<PHPointerList<DchCandidate> >* tmpCandiNode;
    PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandiNode;
    PHDataNode<PHPointerList<DchTrackCandidate> >* tmpPerfTrackCandiNode;
    PHDataNode<PHPointerList<DchHitInfo> >*   tmpHitNode;
    PHDataNode<PHPointerList<DchRawInfo> >*   tmpRawNode;
    PHDataNode<PHPointerList<DchTrackInfo> >* tmpTrackNode;
    PHDataNode<PHPointerList<DchTrackInfo> >* tmpTrackBestNode;
    PHDataNode<DchHitLineLists>*              tmpHitLineListsNode;
    PHDataNode<DchPc1HitLists>*              tmpPc1HitListsNode;
    
    PHDataNode<int>* eventNode;

    // Put the compositeNodes in the Tree !
    dchNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCH"));
    if (!dchNode) {
      dchNode = new PHCompositeNode("DCH");
      root->addNode(dchNode);
    }
 
    doNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCHDO"));
    if (!doNode) {
      doNode = new PHCompositeNode("DCHDO");
      root->addNode(doNode);
    }

    // For TMP stuff
    tmpNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCHTMP"));
    if (!tmpNode) {
      tmpNode = new PHCompositeNode("DCHTMP");
      root->addNode(tmpNode);
    }

    parNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "PAR"));
    if (!parNode) {
      parNode = new PHCompositeNode("PAR");
      root->addNode(parNode);
    }

    dstNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DST"));
    if (!dstNode) {
      dstNode = new PHCompositeNode("DST");
      root->addNode(dstNode);
    }

    dcmNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCM"));
    if (!dcmNode) {
      dcmNode = new PHCompositeNode("DCM");
      root->addNode(dcmNode);
    }
    // Monte-Carlo from Unpacker 
    evaNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "EVA"));
    if (!evaNode) {
      evaNode = new PHCompositeNode("EVA");
      root->addNode(evaNode);
    }
    
    // Put the data Table in the Tree
    outNode = dchNode;
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchFEM")))) {
      w = new dDchFEMWrapper("dDchFEM", 160);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dDchFEM");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchFEM Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchFEM Table already present " << endl;
#endif
    }

    outNode = dcmNode;  // Unpacker
    n = i.findFirst("PHIODataNode", "dDchDCM");
    if (!n) {
      w = new dDchDCMWrapper("dDchDCM", 160);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchDCM");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchDCM Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchDCM Table already present " << endl;
#endif
    }

    outNode = evaNode; // Unpacker MC
    n = i.findFirst("PHIODataNode", "dDchNibbleGhit");
    if (!n) {
      w = new dDchNibbleGhitWrapper("dDchNibbleGhit", 60000);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchNibbleGhit");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchNibbleGhit Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchNibbleGhit Table already present " << endl;
#endif
    }
 
    outNode = dchNode; // Unpacker-Calibrator
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchRaw")))) {
      w = new dDchRawWrapper("dDchRaw", 60000);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dDchRaw");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchRaw Table added " << endl;
#endif
    }else {
#ifdef DEBUG
      cout << "dDchRaw Table already present" << endl;
#endif
    }
  
    outNode = dchNode; // Unpacker-Calibrator MC 
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchGhitRaw")))) {
      w = new dDchGhitRawWrapper("dDchGhitRaw", 60000);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dDchGhitRaw");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchGhitRaw Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchGhitRaw Table already present " << endl;
#endif
    }
  
    outNode = tmpNode; // Calibrator-Tracker-Evaluator
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchHit")))) {
      w = new dDchHitWrapper("dDchHit", 60000);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dDchHit");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchHit Table added " << endl;
#endif
    }else {
#ifdef DEBUG
      cout << "dDchHit Table already present "<< endl;
#endif
    }
 
    outNode = dchNode; // Calibrator MC -Evaluator
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchGhitHits")))) {
      w = new dDchGhitHitsWrapper("dDchGhitHits", 60000);
      if (!w) {
	return 1;
      }
      d = new TableNode_t(w,"dDchGhitHits");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchGhitHits Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchGhitHits Table already present " << endl;
#endif
    }

    outNode = dstNode; // Tracker-Evaluator
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchTracks")))) {
      w = new dDchTracksWrapper("dDchTracks", 2000);
      if (!w) {
	 return 1;
      }
      d = new TableNode_t(w,"dDchTracks");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchTracks Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchTracks Table already present " << endl;
#endif
    }

    outNode = dchNode; // Extention to dDchTracks Table
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchTracksExt")))) {
      w = new dDchTracksExtWrapper("dDchTracksExt", 2000);
      if (!w) {
	 return 1;
      }
      d = new TableNode_t(w,"dDchTracksExt");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchTracksExt Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchTracksExt Table already present " << endl;
#endif
    } 
 
    outNode = dchNode; // Extention to dDchTracksPerf Table
    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchTracksExtPerf")))) {
      w = new dDchTracksExtWrapper("dDchTracksExtPerf", 2000);
      if (!w) {
	 return 1;
      }
      d = new TableNode_t(w,"dDchTracksExtPerf");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchTracksExtPerf Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchTracksExtPerf Table already present " << endl;
#endif
    } 

    if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchTracksPerf")))) {
      w = new dDchTracksWrapper("dDchTracksPerf", 2000);
      if (!w) {
	 return 1;
      }
      d = new TableNode_t(w,"dDchTracksPerf");
      outNode->addNode(d);
#ifdef DEBUG
      cout << "dDchTracksPerf Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dDchTracksPerf Table already present " << endl;
#endif
    } 

    DchRawTable* rawTable=0;
    DchHitLineTable *hitLineTable=0;
    DchTrack* trackTable=0;
    outNode = dchNode;
    phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchRawTablev1"));
    if (!phob){
#ifdef DEBUG
      cout << "PHIODataNode DchRawTablev1 created" << endl;
#endif
      rawTable = new DchRawTablev1();
      if (!rawTable){
	  return False;
      }
      phob = new PHObjectNode_t (rawTable,"DchRawTablev1","PHObject");
      outNode->addNode (phob);
    }  
    
    outNode = dchNode;
    phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchHitLineTablev1"));
    if (!phob){
#ifdef DEBUG
      cout << "PHIODataNode DchHitLineTablev1 created" << endl;
#endif
      hitLineTable = new DchHitLineTablev1();
      if (!hitLineTable){
	  return False;
      }
      phob = new PHObjectNode_t (hitLineTable,"DchHitLineTablev1","PHObject");
      outNode->addNode (phob);
    }

  PHTypedNodeIterator<DchHitLineTable> iter1(root);
  PHIODataNode <DchHitLineTable> * node1 = iter1.find("DchHitLineTable");
  if(node1){
    hitLineTable = node1->getData();
  }else{
    cout << PHWHERE << "DchHitLineTable Node missing" << endl;
    return False;
  }

  outNode = tmpNode;
  PHTypedNodeIterator <DchTrack> dchtrackiter(root);
  DchTrackNode_t *DchTrackNode = dchtrackiter.find("DchTrack");
  if (DchTrackNode){
    trackTable = DchTrackNode->getData();
  }else{
#ifdef DEBUG
    cout << "PHIODataNode DchTrack created" << endl;
#endif
    trackTable = new DchTrackv1();
    phob = new PHObjectNode_t (trackTable,"DchTrack","PHObject");
    outNode->addNode (phob);
  }

    outNode = parNode;  
    n = i.findFirst("PHIODataNode", "dDchUnpackPar");
    if (!n) {  
   
      w = new dDchUnpackParWrapper("dDchUnpackPar", 1);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchUnpackPar");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchUnpackPar Table added " << endl;
#endif
      if (n) {
	dDchUnpackParNode_t*  unpackNode = 
	  static_cast<dDchUnpackParNode_t*>(n);
	unpackNode->getData()->set_detIdWord(0,1);
	unpackNode->getData()->set_dc111(0,0);
#ifdef DEBUG
	cout << "dDchUnpackPar default setted " << endl;
#endif
      }
    } else {
#ifdef DEBUG
      cout << "dDchUnpackPar Table already present " << endl;
#endif
    }

    outNode = parNode; // 
    n = i.findFirst("PHIODataNode", "dDchFastSimPar");
    if (!n) {  
   
      w = new dDchFastSimParWrapper("dDchFastSimPar", 1);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchFastSimPar");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchFastSimPar Table added " << endl;
      if (n) {
	cout << "dDchFastSimPar default NOT set " << endl;
	cout << "dDchFastSimPar default NOT set " << endl;
	cout << "dDchFastSimPar default NOT set " << endl;
	cout << "dDchFastSimPar default NOT set " << endl;
	cout << "dDchFastSimPar default NOT set " << endl;
      }
#endif
    } else {
#ifdef DEBUG
      cout << "dDchFastSimPar Table already present " << endl;
#endif
    }

    outNode = parNode; // 
    n = i.findFirst("PHIODataNode", "dDchDCMPar");
    if (!n) {  
   
      w = new dDchDCMParWrapper("dDchDCMPar", 1);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchDCMPar");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchDCMPar Table added " << endl;
      if (n) {
	cout << "dDchDCMPar default NOT set " << endl;
	cout << "dDchDCMPar default NOT set " << endl;
	cout << "dDchDCMPar default NOT set " << endl;
	cout << "dDchDCMPar default NOT set " << endl;
	cout << "dDchDCMPar default NOT set " << endl;
      }
#endif
    } else {
#ifdef DEBUG
      cout << "dDchDCMPar Table already present " << endl;
#endif
    }

   outNode = parNode; // 
    n = i.findFirst("PHIODataNode", "dDchEvalPar");
    if (!n) {
#ifdef DEBUG
      cout << "WARNING: 'in' parameter dDchEvalPar not found : needed only for Evaluation" << endl;
#endif
      w = new dDchEvalParWrapper("dDchEvalPar", 1);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchEvalPar");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchEvalPar Table added " << endl;
      if (n) {
	cout << "dDchEvalPar default NOT set " << endl;
	cout << "dDchEvalPar default NOT set " << endl;
	cout << "dDchEvalPar default NOT set " << endl;
	cout << "dDchEvalPar default NOT set " << endl;
	cout << "dDchEvalPar default NOT set " << endl;
	cout << "dDchEvalPar default NOT set " << endl;
	cout << "dDchEvalPar default NOT set " << endl;
      }
#endif
    } else {
#ifdef DEBUG
      cout << "dDchEvalPar Table already present " << endl;
#endif
    }

    outNode = parNode; // 
    n = i.findFirst("PHIODataNode", "dDchPerfPar");
    if (!n) {
      w = new dDchPerfParWrapper("dDchPerfPar", 1);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchPerfPar");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchParPar Table added " << endl;
      if (n) {
	cout << "dDchPerfPar default NOT set " << endl;
	cout << "dDchPerfPar default NOT set " << endl;
	cout << "dDchPerfPar default NOT set " << endl;
	cout << "dDchPerfPar default NOT set " << endl;
	cout << "dDchPerfPar default NOT set " << endl;
      }
#endif
    } else {
#ifdef DEBUG
      cout << "dDchPerfPar Table already present " << endl;
#endif
    }

   outNode = parNode; // Tracker
    n = i.findFirst("PHIODataNode", "dDchRecoPar");
    if (!n) {
#ifdef DEBUG
      cout << "'in' parameter dDchReconstructionPar not found : needed only for Tracking" 
	   << endl;
#endif
      w = new dDchReconstructionParWrapper("dDchRecoPar", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dDchRecoPar");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dDchRecoPar Table added " << endl;
#endif
      if (n) {
	dDchReconstructionParNode_t*  recoNode = 
	  static_cast<dDchReconstructionParNode_t*>(n);
       
	recoNode->getData()->set_houghThresholdOnXCell(0,10) ;  // 
	recoNode->getData()->set_houghThresholdOnXMask(0,15) ;  //         
	recoNode->getData()->set_houghThresholdOnUVCell(0,3);  // 
	recoNode->getData()->set_houghThresholdOnUVMask(0,6) ; // 
	recoNode->getData()->set_purgeCandidateThreshold(0,15) ; // 
	recoNode->getData()->set_firstXHoughThreshold(0,10) ;  // mc is 15
	recoNode->getData()->set_secondXHoughThreshold(0,10) ; // mc is 15
	recoNode->getData()->set_minimumNumberOfXHits(0,8);
	recoNode->getData()->set_minimumNumberOfUVHits(0,0);
	recoNode->getData()->set_XHitsThreshold(0,10);
	recoNode->getData()->set_cellDifferenceCut(0,8);
	recoNode->getData()->set_delBetaCut(0,0.2);
	recoNode->getData()->set_deltaBetaCut(0,0.2);
	recoNode->getData()->set_wireResolution(0,0.015); //
	recoNode->getData()->set_initUVChi2(0,10); // 
	recoNode->getData()->set_initXChi2(0,5);  // 
	recoNode->getData()->set_deltaBetaVertexCut(0, 0.5); //  
	recoNode->getData()->set_numberOfAlphaBins(0,300);
	recoNode->getData()->set_numberOfPhiBins(0,6000);
	recoNode->getData()->set_maxAlpha(0,0.8);
	recoNode->getData()->set_minAlpha(0,-0.8);
	recoNode->getData()->set_maxPhi(0,1.);
	recoNode->getData()->set_minPhi(0,-0.65);
	recoNode->getData()->set_numberOfBetaBins(0,60);
	recoNode->getData()->set_numberOfZedBins(0,200);
	recoNode->getData()->set_maxBeta(0,2.5);
	recoNode->getData()->set_minBeta(0,0.6);
	recoNode->getData()->set_maxZed(0,100.);
	recoNode->getData()->set_minZed(0,-100.);
	recoNode->getData()->set_mirrorHitAnalysis(0,0); // 0 for data. 1 for mc
#ifdef DEBUG
	cout << "dDchRecoPar default setted " << endl;
#endif
      }
    } else {
#ifdef DEBUG
      cout << "dDchRecoPar Table already present " << endl;
#endif
    }
  
    outNode = evaNode;// Evaluator
    n = i.findFirst("PHIODataNode", "dcghit");
    if (!n) {
#ifdef DEBUG
      cout << "WARNING: 'in' parameter dcghit not found : needed only for Evaluation" << endl;
#endif
      w = new dcghitWrapper("dcghit", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"dcghit");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "dcghit Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "dcghit Table already present " << endl;
#endif
    }
  
    outNode = evaNode; //Evaluator
    n = i.findFirst("PHIODataNode", "fkin");
    if (!n) {
#ifdef DEBUG
      cout << "WARNING: 'in' parameter fkin not found : needed only for Evaluation" << endl;
#endif
      w = new fkinWrapper("fkin", 10);
      if (!w) {
	return 1;
      }
      n = new TableNode_t(w,"fkin");
      outNode->addNode(n);
#ifdef DEBUG
      cout << "fkin Table added " << endl;
#endif
    } else {
#ifdef DEBUG
      cout << "fkin Table already present " << endl;
#endif
    }
  
    // TEMPORARY (NOT PERSISTENT) OBJECT int the tree extract the candidate Node
  
    tmpCandiNode = 
      (PHDataNode<PHPointerList<DchCandidate> >*)i.findFirst("PHDataNode","DchCandidate");
    if (!tmpCandiNode) {
      candidateList = new PHPointerList<DchCandidate>(100);
      tmpCandiNode = new PHDataNode<PHPointerList<DchCandidate> >(candidateList,"DchCandidate");
      tmpNode->addNode(tmpCandiNode);
    }

    tmpPerfTrackCandiNode = 
      (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode",
								  "PerfectDchTrackCandidate");
    if (!tmpPerfTrackCandiNode) {
      perfTrackCandidateList = new PHPointerList<DchTrackCandidate>(100);
      tmpPerfTrackCandiNode = new PHDataNode<PHPointerList<DchTrackCandidate> >(perfTrackCandidateList,"PerfectDchTrackCandidate");
      tmpNode->addNode(tmpPerfTrackCandiNode);
    }

    tmpTrackCandiNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode","DchTrackCandidate");
    if (!tmpTrackCandiNode) {
      trackCandidateList = new PHPointerList<DchTrackCandidate>(100);
      tmpTrackCandiNode     = new PHDataNode<PHPointerList<DchTrackCandidate> >(trackCandidateList,"DchTrackCandidate");
      tmpNode->addNode(tmpTrackCandiNode);
    }
  
    tmpRawNode = (PHDataNode<PHPointerList<DchRawInfo> >*)i.findFirst("PHDataNode","DchRawInfo");
    if (!tmpRawNode) {
      rawTableList = new PHPointerList<DchRawInfo>;
      tmpRawNode  = new PHDataNode<PHPointerList<DchRawInfo> >(rawTableList,"DchRawInfo");
      tmpNode->addNode(tmpRawNode);
    }
    
    tmpHitNode = (PHDataNode<PHPointerList<DchHitInfo> >*)i.findFirst("PHDataNode","DchHitInfo");
    if (!tmpHitNode) {
      hitTableList = new PHPointerList<DchHitInfo>;
      tmpHitNode  = new PHDataNode<PHPointerList<DchHitInfo> >(hitTableList,"DchHitInfo");
      tmpNode->addNode(tmpHitNode);
    }
    
    tmpTrackNode = (PHDataNode<PHPointerList<DchTrackInfo> >*)i.findFirst("PHDataNode","DchTrackInfo");
    if (!tmpTrackNode) {
      trackTableList = new PHPointerList<DchTrackInfo>;
      tmpTrackNode  = new PHDataNode<PHPointerList<DchTrackInfo> >(trackTableList,"DchTrackInfo");
      tmpNode->addNode(tmpTrackNode);
    }
    
    tmpTrackBestNode = (PHDataNode<PHPointerList<DchTrackInfo> >*)i.findFirst("PHDataNode","DchTrackBestInfo");
    if (!tmpTrackBestNode) {
      trackInfoListBest = new PHPointerList<DchTrackInfo>;
      tmpTrackBestNode  = new PHDataNode<PHPointerList<DchTrackInfo> >(trackInfoListBest,"DchTrackBestInfo");
      tmpNode->addNode(tmpTrackBestNode);
    }
    
    tmpHitLineListsNode = (PHDataNode<DchHitLineLists>*)i.findFirst("PHDataNode","DchHitLineLists");
    if (!tmpHitLineListsNode) {
      hitLineLists = new DchHitLineLists(1000);
      tmpHitLineListsNode  = new PHDataNode<DchHitLineLists>(hitLineLists,"DchHitLineLists");
      tmpNode->addNode(tmpHitLineListsNode);
    }
    
    tmpPc1HitListsNode = (PHDataNode<DchPc1HitLists>*)i.findFirst("PHDataNode","DchPc1HitLists");
    if (!tmpPc1HitListsNode) {
      pc1HitLists = new DchPc1HitLists(1000);
      tmpPc1HitListsNode  = new PHDataNode<DchPc1HitLists>(pc1HitLists,"DchPc1HitLists");
      tmpNode->addNode(tmpPc1HitListsNode);
    }
    
    dchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
    if (!dchHistoNode) {
      dchHistogrammer = new PHDchHistogrammer();
      dchHistoNode = new PHDataNode<PHDchHistogrammer>(dchHistogrammer,"DchHisto");
      doNode->addNode(dchHistoNode);
    }
    
    // Insert code here to navigate node hierarchy and find
    // or create specific nodes to pass to physics module...
    PdbBankID bankID;
    bankID.setInternalValue(1);
    
    dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
    if (!dchDaoNode) 
    {
      dchAddressObject = new PHDchAddressObject();
      dchAddressObject->initialize(); 
      // reading 
      if (dchAddressObject && (readFromDB == 2 || readFromDB ==1)) 
      {
        if (!dchAddressObject->validate(Tsearch)) 
        {
          if(dchAddressObject->fetch(Tsearch,addressNameDB,bankID)) 
          {
            dchAddressObject->commit();
          }else{
            std::cout << PHWHERE
              << "NO database is matching."
              << std::endl;
            exit(1);
          }
        }
      }
      
      dchAddressObject->setFlagMC(flagMC); 
      dchDaoNode = new PHDataNode<PHDchAddressObject>(dchAddressObject,"DchDAO");
      doNode->addNode(dchDaoNode);
    }
    
    dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode", "DchDGO");
    if (!dchDgoNode)
    {
      dchGeometryObject = new PHDchGeometryObject(dchAddressObject);
      if (dchGeometryObject)
      {
        switch (readFromDB)
        {
          case 1:
          case 3:
          if (!dchGeometryObject->validate(Tsearch))
          {
            if (dchGeometryObject->fetch(Tsearch, geometryNameDB, bankID))
            {
              dchGeometryObject->commit();
            }
            else
            {
              std::cout << PHWHERE
                << "NO database is matching."
                << std::endl;
              exit(1);
            }
          }
          break;
          
          case 0:
          dchGeometryObject->setFileNames(geoInfoFile, geoWireFile, geoFrameFile);
          dchGeometryObject->fetchFromFile();
          break;
          
          default:
          break;
        }
      }
      dchDgoNode = new PHDataNode<PHDchGeometryObject>(dchGeometryObject, "DchDGO");
      doNode->addNode(dchDgoNode);
    }
    
    dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode", "DchDCO");
    if (!dchDcoNode)
    {
      dchCalibrationObject = new PHDchCalibrationObject(dchAddressObject);
      if (dchCalibrationObject)
      {
        switch (readFromDB)
        {
          case 1:
          case 4:
          if (!dchCalibrationObject->validate(Tsearch))
          {
            if (dchCalibrationObject->fetch(Tsearch, calibNameDB, bankID))
            {
              dchCalibrationObject->commit();
            }
            else
            {
              std::cout << PHWHERE
                << "NO database is matching."
                << std::endl;
              exit(1);
            }
          }
          break;
          
          case 0:
          dchCalibrationObject->setFileName(calFile, slewFile, localFile, stereoFile);
          dchCalibrationObject->fetchFromFile();
          break;
          
          default:
          break;
        }
        
        dchDcoNode = new PHDataNode<PHDchCalibrationObject>(dchCalibrationObject, "DchDCO");
        doNode->addNode(dchDcoNode);
      }
    }

    dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode", "DchDNO");
    if (!dchDnoNode)
    {
      dchNoiseObject = new PHDchNoiseObject(dchAddressObject);  // hardwired
      if (dchNoiseObject)
      {
        switch (readFromDB)
        {
          case 1:
          case 5:
          if (!dchNoiseObject->validate(Tsearch))
          {
            if (dchNoiseObject->fetch(Tsearch, noiseNameDB, bankID))
            {
              dchNoiseObject->commit();
            }
            else
            {
              std::cout << PHWHERE
                << "NO database is matching."
                << std::endl;
              exit(1);
            }
          }
          break;
          
          case 0:
          dchNoiseObject->setFileName(noiseFile, effiFile);
          dchNoiseObject->fetchNoiseFromFile(); // reading form file
          break;
          
          default:
          break;
        }
      }
          
      dchDnoNode = new PHDataNode<PHDchNoiseObject>(dchNoiseObject, "DchDNO");
      doNode->addNode(dchDnoNode);
    }

    eventNode = (PHDataNode<int>*)i.findFirst("PHDataNode","DchEvent");
    if (!eventNode) {
      tmpEvent = new int(0);
      eventNode = new PHDataNode<int>(tmpEvent,"DchEvent");
      doNode->addNode(eventNode);
    }
    
  } // is tree
  
  // if histoflag == 1 then you want to initialize the file !!! 

  if  (iniHisto) {
    dchHistogrammer->initializeFile(RunNumber);
    iniHisto = 0;
  }
  
  return True;
}

PHBoolean mNewDchInitializer::checkDetectorObjectValidity() 
{
 
  PdbBankID bankID;
  bankID.setInternalValue(1);
 
  if (dchAddressObject && (readFromDB == 2 || readFromDB ==1)) {
    if (!dchAddressObject->validate(Tsearch)) {
      if(dchAddressObject->fetch(Tsearch,addressNameDB,bankID)) {
	dchAddressObject->commit();
      }else{
	std::cout << PHWHERE
		  << "NO database is matching."
		  << std::endl;
	exit(1);
      }
    }
  }

  if(dchGeometryObject &&(readFromDB == 3 || readFromDB ==1) ) {
    if (!dchGeometryObject->validate(Tsearch)) {
      if(dchGeometryObject->fetch(Tsearch,geometryNameDB,bankID)) {
	dchGeometryObject->commit();
      }else{
	std::cout << PHWHERE
		  << "NO database is matching."
		  << std::endl;
	exit(1);
      }
    }
  }
      
  if(dchCalibrationObject && (readFromDB == 4 || readFromDB ==1)) {
    if (!dchCalibrationObject->validate(Tsearch)) {
      if(dchCalibrationObject->fetch(Tsearch,calibNameDB,bankID)) {
	dchCalibrationObject->commit();
      }else {
	std::cout << PHWHERE
		  << "NO database is matching."
		  << std::endl;
	exit(1);
      }
    }
  }

  if (dchNoiseObject && (readFromDB == 5 || readFromDB ==1)) {
    if(!dchNoiseObject->validate(Tsearch)) {
      if (dchNoiseObject->fetch(Tsearch,noiseNameDB,bankID)) {
	dchNoiseObject->commit();
      }else {
	std::cout << PHWHERE
		  << "NO database is matching."
		  << std::endl;
	exit(1);
      }
     }
  }
  
  return True;  
}

PHBoolean mNewDchInitializer::event(PHCompositeNode *root)
{
  // At this point the EventRunNumber is the Key to access the DATABASE

  if (readFromDB) {
    if (RunNumber < 0 && !flagMC) {
      std::cout << PHWHERE
		<< "No RUN information for this Run"
		<< std::endl;
      return False;
    }
  }
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------

  RunToTime *rtt = RunToTime::instance();
  PHTimeStamp *Temporary = rtt->getBeginTime(RunNumber);
  if (Temporary)
    {
      Tsearch = *Temporary;
      delete Temporary;
    }
  else
    {
      cout << PHWHERE << "RunToTime could find any information for run "<< RunNumber<<endl;
      exit(1);
    }

  // ----------------------------------------------------------
  constructPhoolTree(root); 
  //-----------------------------------------------------------

  return True;
}

PHBoolean mNewDchInitializer::updateCalibration(PHTimeStamp& start ,PHTimeStamp& stop, const char *descrip, int flag )
{
  PdbBankID bankID;
  bankID.setInternalValue(1);
  
  if(dchCalibrationObject) {
    dchCalibrationObject->setCommittingFlag(flag);
    dchCalibrationObject->update(start,stop,calibNameDB,bankID,descrip);
    dchCalibrationObject->commit();    
  }
  PHMessage("mNewDchInitializer",PHWarning,"Updating the Calibration ");

  return True;
}

PHBoolean mNewDchInitializer::updateNoise(PHTimeStamp& start ,PHTimeStamp& stop, const char *descrip )
{
  PdbBankID bankID;
  bankID.setInternalValue(1);
  
  if (dchNoiseObject) {
    dchNoiseObject->update(start,stop,noiseNameDB,bankID,descrip);
    dchNoiseObject->commit();    
  }    
  PHMessage("mNewDchInitializer",PHWarning,"Updating the Noise "); 
  return True;   
}

PHBoolean mNewDchInitializer::updateGeometry(PHTimeStamp& start ,PHTimeStamp& stop, const char *descrip ,int flag )
{
  PdbBankID bankID;
  bankID.setInternalValue(1);
  
  if(dchGeometryObject) {
    dchGeometryObject->setCommittingFlag(flag);
    dchGeometryObject->update(start,stop,geometryNameDB,bankID,descrip);
    dchGeometryObject->commit();    
  }
  PHMessage("mNewDchInitializer",PHWarning,"Updating the Geometry ");
  return True;
}

PHBoolean mNewDchInitializer::updateAddress(PHTimeStamp& start ,PHTimeStamp& stop, const char *descrip )
{
  PdbBankID bankID;
  bankID.setInternalValue(1);
  
  if (dchAddressObject) {
    dchAddressObject->update(start,stop,addressNameDB,bankID,descrip);
    dchAddressObject->commit();
  }
  PHMessage("mNewDchInitializer",PHWarning,"Updating the Address");
  return True;
}















