
#include "fkinWrapper.h"
#include "tecghitWrapper.h"
#include "dTecGhitRawWrapper.h"
#include "TecOutV1.hh"
#include "TecOutV3.hh"
#include "mTecUtilities.h"
#include "mTecTrackEvalModule.h"
#include "DchTrack.h"
#include "PHGlobal.h"
#include "EventHeader.h"
#include "RunHeader.h"

#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"

#include "TFile.h"
#include "TNtuple.h"

#include "gsl/gsl_math.h"

#include <iostream>
#include <fstream>
#include <algorithm>
#include <map>

using namespace std;

typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<tecghitWrapper> tecghitNode_t;
typedef PHIODataNode<dTecGhitRawWrapper> dTecGhitRawNode_t;
typedef PHIODataNode<TecOut> TecOutNode_t;

ofstream OutputFile;
ifstream InputFile;

static int init_eval = 0;

using namespace TecUtilities;

//-------------------------------------------------------------------

mTecTrackEvalModule::mTecTrackEvalModule(){
Verbose=0;
Chi2Max=0.40;
Merge=1;
Ecut=0.100;
LineCut=0.04;
Write2File=0;
OutFileName="tectrackeval.txt";
InFileName="tectrackeval0.txt";
EvalFile=0;
EvalFileName="eval.root";
currentTrack=0;
}

//-----------------------------------------------------------------------

int mTecTrackEvalModule::ReadSingleTracks(PHCompositeNode* topNode) {

cout << "mTecTrackEvalModule::ReadSingleTracks Started." << endl;

  InputFile.open(InFileName); 
  if(!InputFile) {cerr << PHWHERE << " Can not open input file " << InFileName << endl; return 0;}

  int nRead = 0;
  int runnumber,evtnumber,nhits,trkindex;
  float xin,xout,yin,yout;
  int aindex[1000]; for(int k=0; k<1000; k++) {aindex[k]=0;}
  int awire[1000]; for(int k=0; k<1000; k++) {awire[k]=0;}
  int abin[1000]; for(int k=0; k<1000; k++) {abin[k]=0;}
  int aadc[1000]; for(int k=0; k<1000; k++) {aadc[k]=0;}
  float acharge[1000]; for(int k=0; k<1000; k++) {acharge[k]=0;}

  for(int k=0; k<999999; k++) {

    InputFile >> runnumber >> evtnumber >> trkindex >> xin >> yin >> xout >> yout >> nhits;
    if(InputFile.eof()) return nRead;
    for(int k=0; k<nhits; k++) {
      InputFile >> aindex[k] >> awire[k] >> abin[k] >> aadc[k] >> acharge[k];
    } 
    cout << "TRACK: " << trkindex << " " << xin << " " << yin << " " << xout << " " << yout 
                      << " " << nhits << endl;
    for(int i=0; i<10; i++) {cout << i << " " << aindex[i] << " " << awire[i] << " " << abin[i] 
  	                          << " " << aadc[i] << endl;}

    float xyz[3],xyzin[3],xyzout[3];
    for(int i=0; i<3; i++) {xyz[i]=0.;}
    xyzin[0]=xin;
    xyzin[1]=yin;
    xyzin[2]=-1.; if(trkindex%2==1) {xyzin[2]=1.;}
    xyzout[0]=xout;
    xyzout[1]=yout;
    xyzout[2]=xyzin[2];
    TecTrackV2 tr1 = TecTrackV2(xyzin,xyzout);
    tr1.setSector(trkindex/2);
    tr1.setSide(trkindex%2);
    tr1.setNhits(nhits);

    TecTrackHit tr2 = TecTrackHit(tr1);
    for(int i =0; i<nhits; i++) {
       TecHit h1 = TecHit(aindex[i], awire[i], abin[i], aadc[i], acharge[i], xyz, nRead);
       tr2.AddHit(h1);
    }

    TRACKS.push_back(tr2);
    nRead++;
    cout << "mTecTrackEvalModule::ReadSingleTracks: Read total " << nRead << " tracks." << endl;

  }

  return nRead;
}

//-----------------------------------------------------------------------

void mTecTrackEvalModule::SaveEvalFile() {

cout << "Writing Tec tracking evaluation file." << endl;
  EvalFile->Write("ntpeval");
  //EvalFile->Write();
  EvalFile->Close();
  delete EvalFile;
  
  return;
}

//-----------------------------------------------------------------------

void mTecTrackEvalModule::MergeSimulatedEvent(PHCompositeNode* topNode) {

cout << "mTecTrackEvalModule::MergeSimulatedEvent Started." << endl;

  PHTypedNodeIterator<TecOut> teciter(topNode);

  PHIODataNode<TecOut> *TecEvalNode = teciter.find("TecOutEVAL");
  if(!TecEvalNode) { cerr << "Can't find TecOutEVAL." << endl; return;}

  TecOut* tec = 0;
  PHIODataNode<TecOut> *TecNode = teciter.find("TecOut");
  if(TecNode) { tec = (TecOut*)TecNode->getData(); }
    else {cerr << "Can't find TecOut." << endl; return;}

  TecOut* techit = 0;
  PHIODataNode<TecOut> *TecHitNode = teciter.find("TecHitOut");
  if(TecHitNode) { techit = (TecOut*)TecHitNode->getData(); }
    else {cerr << "Can't find TecHitOut." << endl; return;}

  PHNodeIterator iter(topNode);

  fkinWrapper* fkin = 0;
  fkinNode_t* fkN = static_cast<fkinNode_t*>(iter.findFirst("PHIODataNode", "fkin"));
  if (fkN) { fkin = fkN->getData(); }
    else { cerr << "fkin table not found." << endl; return; }

  tecghitWrapper* tecghit = 0;
  tecghitNode_t* tgN = static_cast<tecghitNode_t*>(iter.findFirst("PHIODataNode", "tecghit"));
  if (tgN) { tecghit = tgN->getData(); }
    else { cerr << "tecghit table not found." << endl; return; }

  dTecGhitRawWrapper* dTecGhitRaw = 0;
  dTecGhitRawNode_t* GRN = static_cast<dTecGhitRawNode_t*>(iter.findFirst("PHIODataNode", "dTecGhitRaw"));
  if (GRN) { dTecGhitRaw = GRN->getData(); }
    else { cerr << "dTecGhitRaw table not found." << endl; return; }

  cout << " Number of Tracks in EVAL         = " << tec->getNTracks() << endl;
  cout << " Number of Hits in EVAL           = " << techit->getNHits() << endl;
  cout << " Number of Tracks                 = " << tec->getNTracks() << endl;
  cout << " Number of Hits                   = " << techit->getNHits() << endl;
  cout << " Number of entries in dTecGhitRaw = " << dTecGhitRaw->RowCount() << endl;
  cout << " Number of entries in tecghit     = " << tecghit->RowCount() << endl;
  cout << " Number of entries in fkin        = " << fkin->RowCount() << endl;

  return;
}

//-----------------------------------------------------------------------

void mTecTrackEvalModule::MergeSingleTracks(PHCompositeNode* topNode) {

cout << "mTecTrackEvalModule::MergeSingleTracks Started." << endl;

  if(!init_eval) {

    ReadSingleTracks(topNode);
    cout << "Read " << TRACKS.size() << " single tracks from file." << endl;

    EvalFile = new TFile(EvalFileName,"RECREATE");
    ntpeval = new TNtuple("ntpeval","tracking evaluation","tecmult:techitmult:distin:distout:sector:side:dchmult");

    init_eval=1;
  }

  TecOutV1* tec = 0;
  PHTypedNodeIterator<TecOutV1> teciter(topNode);
  PHIODataNode<TecOutV1> *TecNode = teciter.find("TecOutV1");
  if(TecNode) { tec = (TecOutV1*)TecNode->getData(); }
    else {cerr << PHWHERE << "Can't find TecOutV1." << endl; return;}

  TecTrackHit TH;
  if(currentTrack>-1 && currentTrack< (int) TRACKS.size()) {
    TH = TRACKS[currentTrack];
  }
  else {cerr << PHWHERE << " Wrong track number: " << currentTrack << endl; return;}

  float xyz[3]; for(int i=0; i<3; i++) {xyz[i]=0.;}
  int trkid = -1;
  int nadded = 0;
  int nmerged = 0;

  cout << "INITIAL number of hits in TecOutV1: " << tec->getNHits() << endl;
  cout << "  Current test track: " << currentTrack << endl;
  cout << "    Number of hits in test track: " << TH.getNhits() << endl;

  for(int i=0; i<TH.getNhits(); i++) {

    int iindex = (TH.getHit(i))->get_index();
    int iwire = (TH.getHit(i))->get_wire();
    int ibin = (TH.getHit(i))->get_bin();
    int found=0;

    for(int j=0; j<tec->getNHits(); j++) {
      int jindex = tec->getHitIndex(j);
      int jwire = tec->getHitWire(j);
      int jbin = tec->getHitTimeBin(j);
      if(iindex==jindex && iwire==jwire && ibin==jbin) {
        // Hit with these indices exist. Merge.
        found=1;
        float icharge = (TH.getHit(i))->get_charge();
        float jcharge = tec->getHitCharge(j);
        float newcharge=icharge+jcharge;
        int newadc = Charge2Ampl(newcharge);
        tec->setHitADC(j,newadc);
        tec->setHitCharge(j,newcharge);
        nmerged++;
      }
    }

    if(!found) {
      // Add new hit.
       float icharge = (TH.getHit(i))->get_charge();
       int iadc = (TH.getHit(i))->get_adc();
       tec->AddTecHit(iindex, iwire, ibin, iadc, icharge, xyz, trkid);
         nadded++;
    }

  }

  cout << "FINAL number of hits in TecOutV1: " << tec->getNHits() << endl;
  cout << "   MERGED: " << nmerged << endl;
  cout << "   ADDED : " << nadded << endl;

  return;
}

//-----------------------------------------------------------------------

void mTecTrackEvalModule::Evaluate(PHCompositeNode* topNode) {

  cout << "mTecTrackEvalModule::Evaluate Started." << endl;

  TecOutV1* tec = 0;
  PHTypedNodeIterator<TecOutV1> teciter(topNode);
  PHIODataNode<TecOutV1> *TecNode = teciter.find("TecOutV1");
  if(TecNode) { tec = (TecOutV1*)TecNode->getData(); }
    else {cerr << PHWHERE << "Can't find TecOutV1." << endl; return;}

  DchTrack* dch = 0;
  PHTypedNodeIterator<DchTrack> dchiter(topNode);
  PHIODataNode<DchTrack> *DchNode = dchiter.find("DchTrack");
  if(DchNode) { dch = (DchTrack*)DchNode->getData(); }
    else {cerr << PHWHERE << "Can't find DchTrack." << endl; return;}

  int NTRK = tec->getNTracks();
  int NHIT = tec->getNHits();
  cout << "Number of tracks in TecOutV1: " << NTRK << endl;
  cout << "Number of hits in TecOutV1: "   << NHIT << endl;

  TecTrackHit TH;
  if(currentTrack>-1 && currentTrack< (int) TRACKS.size()) {
    TH = TRACKS[currentTrack];
  }
  else {cerr << PHWHERE << " Wrong track number: " << currentTrack << endl; return;}

  cout << "mTecTrackEvalModule::Evaluate track # " << currentTrack << endl;

  float xin0 = (TH.getTrack())->getXin();
  float yin0 = (TH.getTrack())->getYin();
  float xout0 = (TH.getTrack())->getXout();
  float yout0 = (TH.getTrack())->getYout();
  int sector = (TH.getTrack())->getSector();
  int side = (TH.getTrack())->getSide();

  float mindistin = 999.;
  float mindistout = 999.;

  for(int i=0; i<tec->getNTracks(); i++) {

    float xin = tec->getTrackXin(i);
    float yin = tec->getTrackYin(i);
    float xout = tec->getTrackXout(i);
    float yout = tec->getTrackYout(i);

      float distin = sqrt((xin-xin0)*(xin-xin0)+(yin-yin0)*(yin-yin0));
      float distout = sqrt((xout-xout0)*(xout-xout0)+(yout-yout0)*(yout-yout0));
      if(distin<mindistin) {mindistin=distin;}   
      if(distout<mindistout) {mindistout=distout;}   

  }

  cout << "Closest distance: " << mindistin << " " << mindistout << endl;

  float ntpart[99];
  ntpart[0]=float(NTRK);
  ntpart[1]=float(NHIT);
  ntpart[2]=mindistin;
  ntpart[3]=mindistout;
  ntpart[4]=float(sector);
  ntpart[5]=float(side);
  ntpart[6]=float(dch->get_DchNTrack());
  ntpeval->Fill(ntpart);

  currentTrack++;
  if(currentTrack>= (int) TRACKS.size()) {currentTrack=0;}
  return;
}

//-----------------------------------------------------------------------

int mTecTrackEvalModule::WriteSimulatedEvent(PHCompositeNode* topNode) {
using namespace TecUtilities;

  int iret=-1;

  cout << "###### mTecTrackEvalModule::WriteSimulatedEvent Started." << endl;

  PHTypedNodeIterator<TecOut> teciter(topNode);
  PHTypedNodeIterator<TecOutV1> teciterv1(topNode);

  TecOut* teceval = 0;
  PHIODataNode<TecOut> *TecEvalNode = teciter.find("TecOutEVAL");
  if(TecEvalNode) { teceval = (TecOut*)TecEvalNode->getData(); }
    else {cerr << "Can't find TecOutEVAL." << endl; return -1;}

  TecOutV1* tec = 0;
  PHIODataNode<TecOutV1> *TecNode = teciterv1.find("TecOutV1");
  if(TecNode) { tec = (TecOutV1*)TecNode->getData(); }
    else {cerr << "Can't find TecOutV1." << endl; return -1;}

  PHNodeIterator iter(topNode);
  tecghitWrapper* tecghit = 0;
  tecghitNode_t* tgN = static_cast<tecghitNode_t*>(iter.findFirst("PHIODataNode", "tecghit"));
  if (tgN) { tecghit = tgN->getData(); }
    else { cerr << "tecghit table not found." << endl; return -1; }

  cout << "Number of Tracks in EVAL         = " << teceval->getNTracks() << endl;
  cout << "Number of Hits in EVAL           = " << teceval->getNHits() << endl;
  cout << "Number of Tracks                 = " << tec->getNTracks() << endl;
  cout << "Number of Hits                   = " << tec->getNHits() << endl;
  cout << "Number of GEANT Hits             = " << tecghit->RowCount() << endl;

// Copy TecOut to TecOutEVAL

if(tecghit->RowCount()>5) {

  iret=0;
  int runnumber = 0;
  runnumber = tec->getRunNumber();
  teceval->setRunNumber(runnumber);

  float charge = 0.0;
  float xyz[2];
  xyz[0] = 0.0;
  xyz[1] = 0.0;

// Copy hits
  for (int i = 0; i < tec->getNHits(); i++) {
    int index = tec->getHitIndex(i);
    int wire = tec->getHitWire(i);
    int bin = tec->getHitTimeBin(i);
    int adc = tec->getHitADC(i);
    int trackid = tec->getHitTrackID(i);
      teceval->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);
  }

// Copy tracks
  for (int i = 0; i < tec->getNTracks(); i++) {
      TecTrack* tectrack = (TecTrack*)((tec->GetTecTracks())->UncheckedAt(i));
      teceval->AddTecTrack(*tectrack);
  }

  cout << "  Number of Tracks in EVAL         = " << teceval->getNTracks() << endl;
  cout << "  Number of Hits in EVAL           = " << teceval->getNHits() << endl;
  cout << "  Number of Tracks                 = " << tec->getNTracks() << endl;
  cout << "  Number of Hits                   = " << tec->getNHits() << endl;

} // if there are enough hits

  return iret;
}
  
//-----------------------------------------------------------------------

void mTecTrackEvalModule::WriteSingleTracks(PHCompositeNode* topNode) {

using namespace TecUtilities;
	
cout << "mTecTrackEvalModule::WriteSingleTracks Started." << endl;
  static int firstevent=1;
  if(firstevent) { firstevent=0; OutputFile.open(OutFileName); }

  TecOut* tec = 0;
  PHTypedNodeIterator<TecOut> teciter(topNode);
  PHIODataNode<TecOut> *TecNode = teciter.find("TecOut");
  if(TecNode) { tec = (TecOut*)TecNode->getData(); }
    else {cerr << PHWHERE << "Can't find TecOut." << endl; return;}

  TecOut* techit = 0;
  PHTypedNodeIterator<TecOut> techititer(topNode);
  PHIODataNode<TecOut> *TecHitNode = techititer.find("TecHitOut");
  if(TecHitNode) { techit = (TecOut*)TecHitNode->getData(); }
    else {cerr << PHWHERE << "Can't find TecHitOut." << endl; return;}

  DchTrack* dch = 0;
  PHTypedNodeIterator<DchTrack> dchiter(topNode);
  PHIODataNode<DchTrack> *DchNode = dchiter.find("DchTrack");
  if(DchNode) { dch = (DchTrack*)DchNode->getData(); }
    else {cerr << PHWHERE << "Can't find DchTrack." << endl; return;}

  EventHeader* evt = 0;
  PHTypedNodeIterator<EventHeader> evtiter(topNode);
  PHIODataNode<EventHeader> *EvtNode = evtiter.find("EventHeader");
  if(EvtNode) { evt = (EventHeader*)EvtNode->getData(); }
    else {cerr << PHWHERE << "Can't find EventHeader." << endl; return;}

  RunHeader* run = 0;
  PHTypedNodeIterator<RunHeader> runiter(topNode);
  PHIODataNode<RunHeader> *RunNode = runiter.find("RunHeader");
  if(RunNode) { run = (RunHeader*)RunNode->getData(); }
    else {cerr << PHWHERE << "Can't find RunHeader." << endl; return;}

// select low multiplicity events without much noise
  cout << "####### " << tec->getNTracks() << " " << techit->getNHits() << " " << dch->get_DchNTrack() << endl;
  //if(tec->getNTracks()>4 || techit->getNHits()>4000 || dch->get_DchNTrack()>20) {return;}
  if(tec->getNTracks()>6 || techit->getNHits()>6000 || dch->get_DchNTrack()>30) {return;}

  int akuku[8]; for(int i=0; i<8; i++) {akuku[i]=0;}
  for(int i=0; i<tec->getNTracks(); i++) {
    int index = tec->getTrackIndex(i);
    akuku[index]++;
  }
  for(int i=0; i<8; i++) {cout << akuku[i] << " ";} cout << endl;

  for(int i=0; i<tec->getNTracks(); i++) {

     int aindex[1000]; for(int k=0; k<1000; k++) {aindex[k]=0;}
     int awire[1000]; for(int k=0; k<1000; k++) {awire[k]=0;}
     int abin[1000]; for(int k=0; k<1000; k++) {abin[k]=0;}
     int aadc[1000]; for(int k=0; k<1000; k++) {aadc[k]=0;}
     float acharge[1000]; for(int k=0; k<1000; k++) {acharge[k]=0;}
     int itmp=0;
     int runnumber = run->get_RunNumber();
     int evtnumber = evt->get_EvtSequence();
     int nhits = tec->getTrackNhits(i);
     int trkindex = tec->getTrackIndex(i);
     float xin = tec->getTrackXin(i);
     float xout = tec->getTrackXout(i);
     float yin = tec->getTrackYin(i);
     float yout = tec->getTrackYout(i);

     for(int j=0; j<techit->getNHits(); j++) {
	int trkid=techit->getHitTrackID(j);
	if(trkid==i) {
          int index = techit->getHitIndex(j);
	  int wire = techit->getHitWire(j);
	  int bin = techit->getHitTimeBin(j);
	  int adc = techit->getHitADC(j);
          float charge = Ampl2Charge(adc);
	  aindex[itmp]=index;
	  awire[itmp]=wire;
	  abin[itmp]=bin;
	  aadc[itmp]=adc;
	  acharge[itmp]=charge;
	  itmp++;
	}
     }

     cout << i << " ############# hits: " << itmp << " " << nhits << endl;
     if(itmp!=nhits) {return;} // track crossings
     if(akuku[tec->getTrackIndex(i)]!=1) {return;} // 1 track per sector/side

     OutputFile << runnumber << " " << evtnumber << " " << trkindex << " " << xin << " " << yin  
                << " " << xout << " " << yout << " " << nhits << endl;
     for(int k=0; k<itmp; k++) {
        OutputFile << aindex[k] << " " << awire[k] << " " << abin[k] << " " << aadc[k] 
                   << " " << acharge[k] << endl;
     }

  } // end loop over tracks
  

  return;
}

//-----------------------------------------------------------------------

PHBoolean mTecTrackEvalModule::event(PHCompositeNode *root) {
  return True;
}

int mTecTrackEvalModule::findDominantContributor(PHCompositeNode* root, int itrk, float& NHITS) {

  int dcPtr = -1;

  PHTypedNodeIterator<fkinWrapper> iFKINN(root);
  PHTypedNodeIterator<tecghitWrapper> iTGHN(root);
  PHTypedNodeIterator<TecOut> iTECN(root);
  PHTypedNodeIterator<dTecGhitRawWrapper> iGRN(root);

  fkinNode_t *FKINN = iFKINN.find("fkin");
  tecghitNode_t *TGHN = iTGHN.find("tecghit");
  dTecGhitRawNode_t *GRN = iGRN.find("dTecGhitRaw");
  TecOutNode_t *TECN = iTECN.find("TecOutV1");

  if(!GRN) {cerr << "dTecGhitRaw not found." << endl; return -1;}
  if(!TECN) {cerr << "TecOut not found." << endl; return -1;}
  if(!FKINN) {cerr << "fkin not found." << endl; return -1;}
  if(!TGHN) {cerr << "tecghit not found." << endl; return -1;}

  dTecGhitRawWrapper* dTecGhitRaw = GRN->getData();
  TecOut* tecout = TECN->getData();
  fkinWrapper* fkin = FKINN->getData();
  tecghitWrapper* tecghit = TGHN->getData();
  if(Verbose>0) {
    cout << "mTecTrackEvalModule::findDominantContributor: dTecGhitRaw: " << dTecGhitRaw->RowCount() << endl;
    cout << "mTecTrackEvalModule::findDominantContributor: TecOutV1: " << tecout->getNTracks() << " " << tecout->getNHits() << endl;
    cout << "mTecTrackEvalModule::findDominantContributor: fkin: " << fkin->RowCount() << endl;
    cout << "mTecTrackEvalModule::findDominantContributor: tecghit: " << tecghit->RowCount() << endl;
  }
//  for(int i=0; i<dTecGhitRaw->RowCount(); i++) {
//    cout << "dTecGhitRaw: " << i << " " << dTecGhitRaw->get_rawid(i) << " "
//         << dTecGhitRaw->get_binnum(i) << endl;
//  }

 
  static int eventNumber = 0;

    vector<int> INDEX;
    vector<int> SECTOR;
    vector<int> PLANE;
    vector<int> SIDE;
    vector<int> WIRE;
    vector<int> TIMEBIN;
    vector<int> RAWID;

      vector<int> ID;
      vector<float> SHARED;
      vector<int> FKINID;
      vector<int> IDPARENT;
      vector<int> IDPART;
      vector<float> PTOT;
      vector<float> ZORIG;
      vector<float> RORIG;
      vector<float> THETA;
      vector<float> PHI;
      vector<int> ITPARENT;

// Find all hits (fired time bins) associated with this track

      for(int ih=0; ih<tecout->getNHits(); ih++) {
        if(tecout->getHitTrackID(ih)==itrk) {
          int plane = tecout->getHitPlane(ih);
          int wire = tecout->getHitWire(ih);
          int bin = tecout->getHitTimeBin(ih);
          int sector = tecout->getHitSector(ih);
          int side = tecout->getHitSide(ih);
          int index = (sector*12+plane*2+side)*100000+wire*100+bin;
          INDEX.push_back(index);
          SECTOR.push_back(sector);
          PLANE.push_back(plane);
          SIDE.push_back(side);
          WIRE.push_back(wire);
          TIMEBIN.push_back(bin);
          RAWID.push_back(ih);
        }
      }
      if(Verbose>0) {
        cout << "Found " << INDEX.size() << " hits for track # " << itrk << endl;
      }

//------------------ Find Geant particle for each hit -----------------

      vector<int> tmpFKINID;
      vector<float> tmpSHARED;

      for(int i=0; i<(int)INDEX.size(); i++) { // start loop over hits

// Find all Geant contributors to current hit (could be more than 1)

//	if(Verbose>0) {
//          cout << "hit # " << i << ": " << RAWID[i] << " " << TIMEBIN[i] << endl;
//        }

        int found1=0;
        for(int igr=0; igr<(int)dTecGhitRaw->RowCount(); igr++) {
          if(RAWID[i]==dTecGhitRaw->get_rawid(igr) &&
             TIMEBIN[i]==dTecGhitRaw->get_binnum(igr) ) {
               int fkinid = dTecGhitRaw->get_ghitid(igr);
               float shared = dTecGhitRaw->get_fraction(igr);
               tmpFKINID.push_back(fkinid);
               tmpSHARED.push_back(shared);
                found1++;
          }
        }
        if(found1==0 && Verbose>0) {
          cerr << PHWHERE << "ERROR: Geant origin not found !!!" << endl;
        }
        else {
          if(Verbose>2) {
            cout << "Number of contributors for hit # " << i << " is " << found1 << endl;
          }
        }

// For each Geant contributor find Geant particle in fkin table

        for(int j=0; j<(int)tmpFKINID.size(); j++) {

          for(int k=0; k<(int)fkin->RowCount(); k++) {
            if(fkin->get_true_track(k)==tmpFKINID[j]) {
              int idpart = fkin->get_idpart(k);
              int idparent = fkin->get_idparent(k);
              float ptot = fkin->get_ptot(k); 
              float rorig = fkin->get_r_vertex(k); 
              float zorig = fkin->get_z_vertex(k); 
              float theta = fkin->get_pthet(k); 
              float phi = fkin->get_pphi(k); 
              int itparent = fkin->get_itparent(k); 
              ID.push_back(INDEX[i]);
              FKINID.push_back(tmpFKINID[j]);
              SHARED.push_back(tmpSHARED[j]);
              IDPART.push_back(idpart);
              IDPARENT.push_back(idparent);
              PTOT.push_back(ptot);
              RORIG.push_back(rorig);
              ZORIG.push_back(zorig);
              THETA.push_back(theta);
              PHI.push_back(phi);
              ITPARENT.push_back(itparent);
              break;
            }
          }
                   
        } // end j loop over tmpFKINID
   
        tmpFKINID.clear();
        tmpSHARED.clear();

      } // end i loop over INDEX

// ----------------- Find all Contributors to this track ---------------------

        int Contributors[100],Idpart[100],Idparent[100],Itparent[100];
        float Ptot[100],Rorig[100],Zorig[100],Theta[100],Phi[100];
        int ncontrib=0;
        float Total[100]; for(int i=0; i<100; i++) Total[i]=0.;
        int current = -1;

        for(int i=0; i<(int)ID.size(); i++) {
          int found2=0;
          for(int j=0; j<ncontrib; j++) {
            if(Contributors[j]==FKINID[i]) {
              found2=1;
              current=j;
            }
          }
          if(found2!=0) {
            Total[current] += SHARED[i];
          }
          else { // new contributor
            Contributors[ncontrib] = FKINID[i];
            Idpart[ncontrib] = IDPART[i];
            Idparent[ncontrib] = IDPARENT[i];
            Ptot[ncontrib] = PTOT[i];
            Rorig[ncontrib] = RORIG[i];
            Zorig[ncontrib] = ZORIG[i];
            Theta[ncontrib] = THETA[i];
            Phi[ncontrib] = PHI[i];
            Itparent[ncontrib] = ITPARENT[i];
            Total[ncontrib] += SHARED[i];
            ncontrib++;
          }

        }
	if(Verbose>0) cout << ncontrib << " contributors for track # " << itrk << endl;

//----------------- Find Dominant Contributor to this track -------------------
        
        int dcIdpart,dcIdparent,dcItparent;
        float dcPtot,dcRorig,dcZorig,dcTheta,dcPhi,dcTotal,dcFraction;

        if(ncontrib==0) {
	  if(Verbose>0) cerr << PHWHERE << "ERROR: NO CONTRIBUTORS !!!" << endl;
          return -1;
        }

         dcIdpart = Idpart[0];
         dcIdparent = Idparent[0];
         dcItparent = Itparent[0];
         dcPtot = Ptot[0];
         dcRorig = Rorig[0];
         dcZorig = Zorig[0];
         dcTheta = Theta[0];
         dcPhi = Phi[0];
         dcTotal = Total[0];
         dcFraction = Total[0]/INDEX.size();
         dcPtr=0;
          for(int i=1; i<ncontrib; i++) {
            if(Total[i]>dcTotal) {
              dcIdpart = Idpart[i];
              dcIdparent = Idparent[i];
              dcItparent = Itparent[i];
              dcPtot = Ptot[i];
              dcRorig = Rorig[i];
              dcZorig = Zorig[i];
              dcTheta = Theta[i];
              dcPhi = Phi[i];
              dcTotal = Total[i];
              dcFraction = Total[i]/INDEX.size();
              dcPtr=i;
            }
          }

	  NHITS = dcTotal;

// Print track statistics
 
    if(Verbose>0) {
      cout << "************** TRACK # " << itrk << " " << INDEX.size() 
           << " " << ID.size() << " " << ncontrib << endl;
//      for(int i=0; i<ID.size(); i++) {
//        cout << ID[i] << " " << IDPART[i] << " " << IDPARENT[i] << " "
//             << PTOT[i] << " " << RORIG[i] << " " << ZORIG[i] << " "
//             << FKINID[i] << " " << SHARED[i] << endl;
//      }
      for(int i=0; i<ncontrib; i++) {
        cout << "contributor: " << Contributors[i] << " " << Total[i] << " "
             << Idpart[i] << " " << Idparent[i] << " " << Ptot[i] << " "
             << Rorig[i] << " " << Zorig[i] << endl;
      }
      cout << "Dominant contributor: " <<  dcIdpart << " " << dcIdparent
           << " " << dcPtot << " " << dcRorig << " " << dcZorig << " "
           << dcTheta<< " " << dcPhi << " " << dcItparent << " " 
           << dcFraction << endl;
    }

// Reset vectors

      INDEX.clear();
      SECTOR.clear();
      PLANE.clear();
      SIDE.clear();
      WIRE.clear();
      TIMEBIN.clear();
      RAWID.clear();

        ID.clear();
        SHARED.clear();
        FKINID.clear();
        IDPARENT.clear();
        IDPART.clear();
        PTOT.clear();
        ZORIG.clear();
        RORIG.clear();
        THETA.clear();
        PHI.clear();
        ITPARENT.clear();

  eventNumber++;

  return dcPtr;
}




