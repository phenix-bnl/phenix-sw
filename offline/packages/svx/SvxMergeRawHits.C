#include <cstdlib>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <math.h>

#include <SvxMergeRawHits.h>

#include <SvxRawhitList.h>
#include <SvxRawhitListv3.h>
#include <SvxRawhit.h>
#include <Bbc.hh>
#include <BbcOut.h>
#include <VtxOut.h>
#include <PHPoint.h>
#include <PHGlobal.h>

#include <headerWrapper.h>
#include <primaryWrapper.h>
#include <fkinWrapper.h>

#include <Fun4AllReturnCodes.h>

#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>

using namespace std;

ifstream finsvxmerge;
ofstream foutsvxmerge;

//----------------------------------------------------------------------------------------------------

SvxMergeRawHits::SvxMergeRawHits(const string &name) : SubsysReco(name) { 
  merge=true;
  ZVtx1 = -9999.;
  ZVtx2 = 9999.;
  BbcCut1 = -9999.; 
  BbcCut2 = 9999.; 
  CentCut1 = -1.;
  CentCut2 = 101.;
  whichVertex="DEFAULT";
  MergeFileName="svxhits.dat";
  Nskip = 0;
}

//------------------------------------------------------------------------------------------------------

SvxMergeRawHits::~SvxMergeRawHits() { }

void SvxMergeRawHits::set_mergefilename(std::string &filename) {
  MergeFileName=filename;
}

//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int SvxMergeRawHits::Init(PHCompositeNode *topNode)
{
  if(merge) {
    finsvxmerge.open(MergeFileName.c_str());
    if(!finsvxmerge) {cerr << PHWHERE << " ERROR: Can not open input text file: " << MergeFileName << endl; return -1;}
  }
  else {
    foutsvxmerge.open(MergeFileName.c_str());
    if(!foutsvxmerge) {cerr << PHWHERE << " ERROR: Can not open output text file: " << MergeFileName << endl; return -1;}
  }

  EventNumber = 0;

  return EVENT_OK;
}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxMergeRawHits::InitRun(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) { cerr << PHWHERE << " DST node missing, doing nothing." << endl; return EVENT_OK; }

  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*> (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode) { cerr << PHWHERE << " SVX node missing, doing nothing." << endl; return EVENT_OK; }

  PHIODataNode<PHObject>* SvxMergedRawhitListNode = NULL;
  SvxMergedRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxiMergedRawhitList");
  if (!SvxMergedRawhitListNode)
    {
      SvxRawhitList* svxmergedrawhits = new SvxRawhitListv3();
      SvxMergedRawhitListNode = new PHIODataNode<PHObject>(svxmergedrawhits, "SvxMergedRawhitList", "PHObject");
      svxNode->addNode(SvxMergedRawhitListNode);
      if(verbosity>0) {cout << PHWHERE << " SvxMergedRawhitList node created." << endl;} 
    }

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxMergeRawHits::process_event(PHCompositeNode *topNode)
{
  if(merge) { return process_event_merge(topNode);}
    else {return process_event_dump(topNode);}
}

//---------------------------------------------------------------------------------------------

int SvxMergeRawHits::process_event_dump(PHCompositeNode *topNode)
{
  if(verbosity>0) cout << "========== SvxMergeRawHits::process_event_dump() started..." <<endl;
  if(EventNumber<Nskip) {
    cout << "SvxMergeRawHits::process_event_dump(): Skipping event # " << EventNumber << endl;
    EventNumber++;
    return EVENT_OK;
  }

  float bbccharge=0.;
  int nbbc = 0;
  BbcOut *d_bbc = findNode::getClass<BbcOut>(topNode,"BbcOut");
  if(!d_bbc) { cerr << "SvxMergeRawHits::process_event_dump() WARNING: BbcOut node not found. No Centrality selection will be done." << endl;}
    else {
      bbccharge = d_bbc->get_ChargeSum(Bbc::North) + d_bbc->get_ChargeSum(Bbc::South);
      nbbc = d_bbc->get_nPmt(Bbc::North) + d_bbc->get_nPmt(Bbc::South);
    }
  if(verbosity>0) cout << "BBC: " << nbbc << " " << bbccharge << endl;
  if(bbccharge<BbcCut1 || bbccharge>BbcCut2) {
    if(verbosity>0) cout << "Event rejected by BBC charge: " << bbccharge << " " << BbcCut1 << " " << BbcCut1 << endl;
    return EVENT_OK;
  }

  PHGlobal *d_global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if(!d_global) { cerr << "SvxMergeRawHits::process_event_dump() WARNING: PHGlobal not found. No Centrality selection will be done." << endl;}
  float cent = d_global->getCentrality();
//  float globalvtx = d_global->getBbcZVertex();
  if(verbosity>0) cout << "   PHGlobal Centrality: " << cent << endl;
  if(cent<CentCut1 || cent>CentCut2) {
    if(verbosity>0) cout << "Event rejected by Centrality: " << cent << " " << CentCut1 << " " << CentCut1 << endl;
    return EVENT_OK;
  }

  float zvtx = 9999.;
  VtxOut *d_vtx = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if(!d_vtx) { cerr << "SvxMergeRawHits::process_event_dump() WARNING: VtxOut node not found. No Event Vertex selection will be done." << endl;}
    else {
      PHPoint vtxbbc = d_vtx->get_Vertex("BBC");
      if(verbosity>0) cout<<"VTX BBC: "<<vtxbbc.getX()<<" "<<vtxbbc.getY()<<" "<<vtxbbc.getZ()<<endl;
      PHPoint vtxsvx = d_vtx->get_Vertex("SVX_PRECISE");
      if(verbosity>0) cout<<"VTX SVX_PRECISE: "<<vtxsvx.getX()<<" "<<vtxsvx.getY()<<" "<<vtxsvx.getZ()<<endl;
      PHPoint vtx = d_vtx->get_Vertex();
      if(verbosity>0) cout<<"default VTX: "<<vtx.getX()<<" "<<vtx.getY()<<" "<<vtx.getZ()<<" "<<d_vtx->which_Vtx()<<endl;
      if(whichVertex=="DEFAULT") {
        zvtx = vtx.getZ();  // use default vertex
      }
      else if(whichVertex=="BBC") {
        zvtx = vtxbbc.getZ(); // use BBC vertex
      }
      else if(whichVertex=="SVX_PRECISE") {
        zvtx = vtxsvx.getZ(); // use precise SVX vertex
      }
      else {
         cerr << "SvxMergeRawHits::process_event_dump() ERROR: Wrong Vertex selection: " << whichVertex << endl;
         return EVENT_OK;
      }
    }

  if(zvtx>ZVtx1 && zvtx<ZVtx2) {
    if(verbosity>0) cout << "Event ACCEPTED by Zvertex: " << zvtx << " " << ZVtx1 << " " << ZVtx2 << endl;
  }
  else {
    if(verbosity>0) cout << "Event REJECTED by Zvertex: " << zvtx << " " << ZVtx1 << " " << ZVtx2 << endl;
    return EVENT_OK;
  }

  SvxRawhitList *d_rawhit = findNode::getClass<SvxRawhitList>(topNode,"SvxRawhitList");
  int nhits = (d_rawhit!=NULL) ? d_rawhit->get_nRawhits() : 0;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_dump() # of hits = " << nhits << endl;
    if(verbosity>0) cout << "Dumping to file: " << MergeFileName << endl;

  int ngoodhits=0;
  for(int ihit=0; ihit<nhits; ihit++) {
    SvxRawhit* rhit = d_rawhit->get_Rawhit(ihit);
    if(rhit->get_HotDeadFlag()==0 ) ngoodhits++;
  }

  foutsvxmerge << "evt " << EventNumber << " " << ngoodhits << " " << bbccharge << " " << zvtx << " " << d_vtx->which_Vtx() << endl;

  for(int ihit=0; ihit<nhits; ihit++) {
  //for(int ihit=0; ihit<10; ihit++) {
    SvxRawhit* rhit = d_rawhit->get_Rawhit(ihit);
    //if(ihit<20) cout << "status: " << rhit->get_HotDeadFlag() << endl;
    if(rhit->get_HotDeadFlag()!=0 ) continue;      // dont dump hot rawhits
    short r_section = rhit->get_svxSection();
    short r_layer   = rhit->get_layer();
    short r_ladder  = rhit->get_ladder();
    short r_sensor  = rhit->get_sensor();
    short sensorSection = rhit->get_sensorSection();
    short sensorReadout = rhit->get_sensorReadout();
    short sensorType = rhit->get_sensorType();
    int r_channel = rhit->get_channel();
    int r_adc = rhit->get_adc();
    short pixelModule = rhit->get_pixelModule();
    short pixelROC = rhit->get_pixelROC();
    int HDFlag = rhit->get_HotDeadFlag();
    int hitID = rhit->get_hitID();
    foutsvxmerge<<r_section<<" "<<r_layer<<" "<<r_ladder<<" "<<r_sensor<<" "<<sensorSection<<" "<<sensorReadout<<" "<<sensorType<<" "<<r_channel<<" "<<r_adc<<" "<<pixelModule<<" "<<pixelROC<<" "<<HDFlag<<" "<<hitID<<endl;
  }
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_dump() finished. " << endl;

  EventNumber++;
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxMergeRawHits::process_event_merge(PHCompositeNode *topNode)
{
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() started..." <<endl;
  if(EventNumber==0) topNode->print();

    short i_section;
    short i_layer;
    short i_ladder;
    short i_sensor;
    short i_sensorSection;
    short i_sensorReadout;
    short i_sensorType;
    int   i_channel;
    int   i_adc;
    short i_pixelModule;
    short i_pixelROC;
    int   i_HDFlag;
    int   i_hitID;
    int   i_evt;
    int   i_nhits;
    std::string tmp;

  int nmerged=0;

  SvxRawhitList *d_mergedrawhit = findNode::getClass<SvxRawhitList>(topNode,"SvxMergedRawhitList");
  if(!d_mergedrawhit) {
    cerr << "SvxMergeRawHits::process_event_merge() SvxMergedRawhitList node not found!" << endl;
    return EVENT_OK;
  }
  int nmergedrawhit = d_mergedrawhit->get_nRawhits();
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() Initial # of hits in MERGED node = " << nmergedrawhit << endl;

  SvxRawhitList *d_rawhit = findNode::getClass<SvxRawhitList>(topNode,"SvxRawhitList");
  int nrawhit = (d_rawhit!=NULL) ? d_rawhit->get_nRawhits() : 0;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() # of hits = " << nrawhit << endl;

  fkinWrapper* fkin = findNode::getClass<fkinWrapper>(topNode, "fkin");
  if (!fkin) { cout << PHWHERE << " No fkin node found." << endl;}
  if(fkin) {
    cout << "nfkin = " << fkin->RowCount() << endl;
    for(unsigned int i=0; i<fkin->RowCount(); i++) {
    cout << "fkin particle: " << fkin->get_idpart(i) << " " << fkin->get_itparent(i) << " " << fkin->get_idparent(i) << " " << fkin->get_z_vertex(i) << " " << fkin->get_true_track(i) << endl;
    }
  } 

  primaryWrapper* primary = findNode::getClass<primaryWrapper>(topNode, "primary");
  if (!primary) { cout << PHWHERE << " No primary node found." << endl;}
  if(primary) {
    cout << "nprimary = " << primary->RowCount() << endl;
    for(unsigned int i=0; i<primary->RowCount(); i++) {
      float px = primary->get_px_momentum(i);
      float py = primary->get_py_momentum(i);
      float pz = primary->get_pz_momentum(i);
      cout << "primary particle: " << primary->get_idpart(i) << " " << primary->get_true_track(i) << " " << px << " " << py << " " << pz << endl;
    }
  }

  float pisazvtx = -9876.;
  headerWrapper* header = findNode::getClass<headerWrapper>(topNode, "header");
  if (!header) { cout << PHWHERE << " No header node found." << endl;}
  if(header) {
    //cout << "header: " << header->RowCount() << endl;
    pisazvtx = header->get_vertex(2,0);
    cout << "PISA Z Vertex = " << pisazvtx << endl;
  }

//  PISAEventHeader *d_pisaheader = findNode::getClass<PISAEventHeader>(topNode,"header");
//  if(!d_pisaheader) {
//    cerr << "SvxMergeRawHits::process_event_merge() PISAEventHeader node not found!" << endl;
//    return EVENT_OK;
//  }
//  float pisazvtx = d_pisaheader->GetZvertex();
//  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() PISA Z vertex = " << pisazvtx << endl;

// copy raw hits form simulated particle to new node
  for(int i=0; i<nrawhit; i++) {
    SvxRawhit* orighit = d_rawhit->get_Rawhit(i);
    SvxRawhit* tmphit = d_mergedrawhit->addRawhit();
    //std::cout << "raw added..." << std::endl;
    short int layer  = orighit->get_layer();
    short int ladder = orighit->get_ladder();
    short int sensor = orighit->get_sensor();
    short int sensec = orighit->get_sensorSection();
    int channel = orighit->get_channel();
    short r_sensorType = orighit->get_sensorType();
    tmphit->set_svxSection(0);
    tmphit->set_layer(layer);
    tmphit->set_ladder(ladder);
    tmphit->set_sensor(sensor);
    tmphit->set_sensorSection(sensec);
    tmphit->set_channel(channel);
    tmphit->set_sensorType(r_sensorType);
    //tmphit->set_HotDeadFlag(status); // adding hot&dead flag is moved to SvxApplyHotDead
    if(layer<2) { 
      tmphit->set_sensorReadout(0);
      tmphit->set_adc(1);
      short int ichip = orighit->get_pixelROC();
      tmphit->set_pixelROC(ichip);
      short int imodule = orighit->get_pixelModule();
      tmphit->set_pixelModule(imodule);
    } else {
      short int readout = orighit->get_sensorReadout();
      tmphit->set_sensorReadout(readout);
      int adc = orighit->get_adc();
      tmphit->set_adc(adc);
    }
    //std::cout << "raw created." << std::endl;
  }
  nmergedrawhit = (d_mergedrawhit!=NULL) ? d_mergedrawhit->get_nRawhits() : 0;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() Final # of hits in MERGED node = " << nmergedrawhit << endl;


  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() reading input text file..." << endl;
  float bbccharge,zvtx;
  std::string whichVtx;

//while(true) {
//if(!finsvxmerge.eof()) {

  //finsvxmerge>>tmp>>i_evt>>i_nhits;
  finsvxmerge >> tmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx;
  if(finsvxmerge.eof()) {
    finsvxmerge.close();
    cout << PHWHERE << "Input text file with Raw Hits reached END-OF-FILE." << endl;
    finsvxmerge.open(MergeFileName.c_str());
    if(!finsvxmerge) {cerr << PHWHERE << " ERROR: Cannot open input text file: " << MergeFileName << endl; return -1;}
      else {cout << PHWHERE << "Input text file with Raw Hits re-opened." << endl;}
    finsvxmerge >> tmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx;
  }
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() # of hits to read = " << i_nhits << endl;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() BBC charge = " << bbccharge << endl;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() Event vertex: " << zvtx << " " << whichVtx << endl;
  if(fabs(zvtx-pisazvtx)>0.001) cerr << "SvxMergeRawHits::process_event_merge() VERTEX ERROR: " << zvtx << " " << pisazvtx << endl;
  for(int i=0; i<i_nhits; i++) {
    finsvxmerge>>i_section>>i_layer>>i_ladder>>i_sensor>>i_sensorSection>>i_sensorReadout>>i_sensorType>>i_channel>>i_adc>>i_pixelModule>>i_pixelROC>>i_HDFlag>>i_hitID;

// check if Rawhit with these indices already exists
    bool addhit=true;
    for(int ihit=0; ihit<nrawhit; ihit++) {

      SvxRawhit* rhit = d_rawhit->get_Rawhit(ihit);
      short r_section = rhit->get_svxSection();
      short r_layer   = rhit->get_layer();
      short r_ladder  = rhit->get_ladder();
      short r_sensor  = rhit->get_sensor();
      short r_sensorSection = rhit->get_sensorSection();
      short r_sensorReadout = rhit->get_sensorReadout();
      short r_sensorType = rhit->get_sensorType();
      int   r_channel = rhit->get_channel();
      int   r_adc = rhit->get_adc();
      short r_pixelModule = rhit->get_pixelModule();
      short r_pixelROC = rhit->get_pixelROC();
      int   r_HDFlag = rhit->get_HotDeadFlag();
      int   r_hitID = rhit->get_hitID();
      if(r_layer==i_layer && r_ladder==i_ladder && r_sensor==i_sensor && r_sensorSection==i_sensorSection && r_sensorReadout==i_sensorReadout && r_channel==i_channel) {
        if(verbosity>1) {
          cout << "MERGE THIS RAWHIT!" << endl; 
          cout<<r_section<<" "<<r_layer<<" "<<r_ladder<<" "<<r_sensor<<" "<<r_sensorSection<<" "<<r_sensorReadout<<" "<<r_sensorType<<" "<<r_channel<<" "<<r_adc<<" "<<r_pixelModule<<" "<<r_pixelROC<<" "<<r_HDFlag<<" "<<r_hitID<<endl;
          cout<<i_section<<" "<<i_layer<<" "<<i_ladder<<" "<<i_sensor<<" "<<i_sensorSection<<" "<<i_sensorReadout<<" "<<i_sensorType<<" "<<i_channel<<" "<<i_adc<<" "<<i_pixelModule<<" "<<i_pixelROC<<" "<<i_HDFlag<<" "<<i_hitID<<endl;
        }
        int newadc = i_adc + r_adc;
        rhit->set_adc(newadc);
        nmerged++;
        addhit=false;
      }
    }

      if(addhit) { // add new hit
        SvxRawhit* tmphit = d_rawhit->addRawhit();
        //std::cout << "raw added..." << std::endl;
        tmphit->set_svxSection(0);
        tmphit->set_layer(i_layer);
        tmphit->set_ladder(i_ladder);
        tmphit->set_sensor(i_sensor);
        tmphit->set_sensorSection(i_sensorSection);
        tmphit->set_sensorReadout(i_sensorReadout);
        tmphit->set_sensorType(i_sensorType);
        tmphit->set_channel(i_channel);
        tmphit->set_adc(i_adc);
        tmphit->set_pixelModule(i_pixelModule);
        tmphit->set_pixelROC(i_pixelROC);
        //tmphit->set_HotDeadFlag(status); // adding hot&dead flag is moved to SvxApplyHotDead
        //std::cout << "raw created." << std::endl;
      }

  } // end loop over reading rawhits for merging from text file

//} // end infinite loop

/*
else { // Input text file with rawhits has ended. Re-open it first.

  finsvxmerge.close();
  cout << PHWHERE << "Input text file with Raw Hits reached END-OF-FILE." << endl;
  finsvxmerge.open(MergeFileName.c_str());
  if(!finsvxmerge) {cerr << PHWHERE << " ERROR: Cannot open input text file: " << MergeFileName << endl; return -1;}
    else {cout << PHWHERE << "Input text file with Raw Hits re-opened." << endl;}

  finsvxmerge >> tmp >> i_evt >> i_nhits >> bbccharge >> zvtx >> whichVtx;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() # of hits to read = " << i_nhits << endl;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() BBC charge = " << bbccharge << endl;
  if(verbosity>0) cout << "SvxMergeRawHits::process_event_merge() Event vertex: " << zvtx << " " << whichVtx << endl;
  if(fabs(zvtx-pisazvtx)>0.001) cerr << "SvxMergeRawHits::process_event_merge() VERTEX ERROR: " << zvtx << " " << pisazvtx << endl;
  for(int i=0; i<i_nhits; i++) {
    finsvxmerge>>i_section>>i_layer>>i_ladder>>i_sensor>>i_sensorSection>>i_sensorReadout>>i_sensorType>>i_channel>>i_adc>>i_pixelModule>>i_pixelROC>>i_HDFlag>>i_hitID;

// check if Rawhit with these indices already exists
    bool addhit=true;
    for(int ihit=0; ihit<nrawhit; ihit++) {

      SvxRawhit* rhit = d_rawhit->get_Rawhit(ihit);
      short r_section = rhit->get_svxSection();
      short r_layer   = rhit->get_layer();
      short r_ladder  = rhit->get_ladder();
      short r_sensor  = rhit->get_sensor();
      short r_sensorSection = rhit->get_sensorSection();
      short r_sensorReadout = rhit->get_sensorReadout();
      short r_sensorType = rhit->get_sensorType();
      int   r_channel = rhit->get_channel();
      int   r_adc = rhit->get_adc();
      short r_pixelModule = rhit->get_pixelModule();
      short r_pixelROC = rhit->get_pixelROC();
      int   r_HDFlag = rhit->get_HotDeadFlag();
      int   r_hitID = rhit->get_hitID();
      if(r_layer==i_layer && r_ladder==i_ladder && r_sensor==i_sensor && r_sensorSection==i_sensorSection && r_sensorReadout==i_sensorReadout && r_channel==i_channel) {
        if(verbosity>0) {
          cout << "MERGE THIS RAWHIT!" << endl;
          cout<<r_section<<" "<<r_layer<<" "<<r_ladder<<" "<<r_sensor<<" "<<r_sensorSection<<" "<<r_sensorReadout<<" "<<r_sensorType<<" "<<r_channel<<" "<<r_adc<<" "<<r_pixelModule<<" "<<r_pixelROC<<" "<<r_HDFlag<<" "<<r_hitID<<endl;
          cout<<i_section<<" "<<i_layer<<" "<<i_ladder<<" "<<i_sensor<<" "<<i_sensorSection<<" "<<i_sensorReadout<<" "<<i_sensorType<<" "<<i_channel<<" "<<i_adc<<" "<<i_pixelModule<<" "<<i_pixelROC<<" "<<i_HDFlag<<" "<<i_hitID<<endl;
        }
        int newadc = i_adc + r_adc;
        rhit->set_adc(newadc);
        nmerged++;
        addhit=false;
      }
    }

      if(addhit) { // add new hit
        SvxRawhit* tmphit = d_rawhit->addRawhit();
        //std::cout << "raw added..." << std::endl;
        tmphit->set_svxSection(0);
        tmphit->set_layer(i_layer);
        tmphit->set_ladder(i_ladder);
        tmphit->set_sensor(i_sensor);
        tmphit->set_sensorSection(i_sensorSection);
        tmphit->set_sensorReadout(i_sensorReadout);
        tmphit->set_sensorType(i_sensorType);
        tmphit->set_channel(i_channel);
        tmphit->set_adc(i_adc);
        tmphit->set_pixelModule(i_pixelModule);
        tmphit->set_pixelROC(i_pixelROC);
        //tmphit->set_HotDeadFlag(status); // adding hot&dead flag is moved to SvxApplyHotDead
        //std::cout << "raw created." << std::endl;
      }

  } // end loop over reading rawhits for merging from text file

}
*/

  if(verbosity>0) {cout << "SvxMergeRawHits::process_event_merge() Event processed." << endl;}
  if(verbosity>0) {cout << "SvxMergeRawHits::process_event_merge() Number of merged hits = " << nmerged << endl;}
  if(verbosity>0) {cout << "SvxMergeRawHits::process_event_merge() Final number of hits = " << d_rawhit->get_nRawhits() <<endl;}

  EventNumber++;
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

int SvxMergeRawHits::End(PHCompositeNode *topNode)
{
  if(merge) {
    finsvxmerge.close();
    cout << "SvxMergeRawHits::End(): Closing input text file." << endl;
  }
  else {
//    foutsvxmerge.close();
    cout << "SvxMergeRawHits::End(): NOT Closing output text file: " << MergeFileName << endl;
  }
  return EVENT_OK;
}

