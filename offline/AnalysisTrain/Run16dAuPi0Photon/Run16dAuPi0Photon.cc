#include "Run16dAuPi0Photon.h"
#include <iostream>
#include <fstream>
#include <cmath>

#include "TFile.h"
#include "TNtuple.h"
#include "TTree.h"
#include "Diagnostic.h"

#include <algorithm>
#include "TrigLvl1.h"
#include "ErtOut.h"
#include "RunHeader.h"
#include "EventHeader.h"
#include "utiCentrality.h"
#include "TriggerHelper.h"
#include "PHGlobal.h"
#include "PHCentralTrack.h"
#include "recoConsts.h"

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"

#include "mEmcGeometryModule.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emCalibMaster.h"


#include "KCluster.h"
#include "KTofCutter.h"
#include "Combination.h"
#include "KEvent.h"
#include "THmulf.h"
#include "getClass.h"
#include "TOAD.h"
#include "TH2.h"



//#include "HotTwrList.h" // Run7
//#include "TowerHelper.h"
// #include "Warnmap.h" // Run8

using namespace std;
using namespace findNode;

const double DEG_PER_RAD = 180.0 / M_PI;

const double Run16dAuPi0Photon::pi = M_PI;

//const int Run16dAuPi0Photon::_ASYMBINS = 12;             /// ASYMMETRY BINNING
//double Run16dAuPi0Photon::_ASYMLOWEDGES[_ASYMBINS+1] = 
//      {0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.90, 0.95, 0.99}; 
const int Run16dAuPi0Photon::_ASYMBINS = 4;             /// ASYMMETRY BINNING
double Run16dAuPi0Photon::_ASYMLOWEDGES[_ASYMBINS+1] = 
  {0.0,0.7,0.8, 0.90, 0.99}; 



const int Run16dAuPi0Photon::_ASYM2BINS = 2;             /// ASYMMETRY BINNING
double Run16dAuPi0Photon::_ASYM2LOWEDGES[_ASYM2BINS+1] = {0.0, 0.6, 0.8}; 


ClassImp(Run16dAuPi0Photon);

Run16dAuPi0Photon::Run16dAuPi0Photon(const char* outfile, const int Vorder) : 
  SubsysReco("PI04ALL Analyzer"),
  _TrigNodeName("TrigLvl1"),
  _ErtNodeName("ErtOut"), 
  _phGlobalNodeName("PHGlobal"),
  _emcClusterContainerNodeName("emcClusterContainer"),//"PhPhotonList"),
  _phCentralTrackNodeName("PHCentralTrack"),//PhCglList"),
  _neventsGammaCluster(0),
  _currentSegNumber(-1),
  _numMissingSegments(0),
  se(0),
  centReco(0),
  rc(0),
  _diagfile(0),
 
 _ggDiagNtuple(0),
 
  OutFileName(outfile)
#ifdef HAVE_EVBAVERAGER_H
  ,_eventTimeAvg(1000),
  _mixTimeAvg(1000)
#endif
{
  rc =  recoConsts::instance();   /// read inmacro constatnts
  debug = rc->get_IntFlag("EMCNEW_DEBUG", 0);
  is_pp = rc->get_IntFlag("EMCTP_IS_PP", 0);
  is_ert = rc->get_IntFlag("EMCTP_IS_ERT", 0);
  is_patag = rc->get_IntFlag("PA_TAG", 0);
  rpflag = rc->get_IntFlag("RP_METHOD", 1);
  trigsel = rc->get_IntFlag("TRIG_SEL", 0);
  shuffle = rc->get_IntFlag("SHUFFLE", 0);
  

  //
  // vn order. If it is 1, the RP_CLASS RANGE should be doubled
  //  and phi range should be extended 
  //
  xing_shift = -9999;
  xing = -9999;
  
  vorder = Vorder;

  _phGlobal_ptr = NULL;
  _emcClusterContainer_ptr = NULL;
  _phCentTrack_ptr = NULL;
  _Trig_ptr = NULL;
  _Ert_ptr = NULL;
  _CP_Calib = new emCalibMaster;
  mixed = 0;
  _nclus = 0;
  _nevents = 0;
  _prevevent = 0;
  runnumber = 0;
  EmcGeo = NULL;
  event_counter = 0;
  cluster_counter = 0;  //  added by sunzd
  event_live_erta = NULL;
  event_live_ertb = NULL;
  event_live_ertc = NULL;
  evts_cent = NULL;
  evts_mult = NULL;
  evts_ert  = NULL;
  gamma1 = NULL;
  gammarap = NULL;
  tower = NULL;
  towertof = NULL;
  towertof_3ns = NULL;
  tower_gghasym= NULL;
  gghrp = NULL;
  bbczdc = NULL;
  reacpl = NULL;
  reacpl2 = NULL;
  gghboxes = NULL;
  gghasym = NULL;
  ggh_sysErr_Asym = NULL;
  ggh_sysErr_Chi2 = NULL;
  gghspecial = NULL;
  gghrap = NULL;
  gghshuffle = NULL;
  gghcheck = NULL;
  Asym = NULL;
  htrigger = NULL;
  hlumi = NULL;
  hpatternBlue = NULL;
  hpatternYellow = NULL;
  hAcc = NULL;
  hRelative = NULL;
  _gammaDiagNtuple = NULL;
  _evRateTree = NULL;
  inputFlag = 0;
  bbcz = 0;

  pair_timing_sec[0] = NULL;
  pair_timing_sec[1] = NULL;
  pair_timing_sec[2] = NULL;
  pair_timing_sec[3] = NULL;
  pair_timing_sec[4] = NULL;
  pair_timing_sec[5] = NULL;
  pair_timing_sec[6] = NULL;
  pair_timing_sec[7] = NULL;
  centrality = NULL;
  
  bbcqs_ecoreMax = NULL;
  bbcqs_ecoreTotal = NULL;
  bbcqs_ecoreAverage = NULL;
  bbcqn_ecoreMax = NULL;
  bbcqn_ecoreTotal = NULL;
  bbcqn_ecoreAverage = NULL;

  cent_fw_corr = NULL;
  
  if(is_patag==1) cout <<"P-A tagging"<<endl;

//  is_bookNt = 0;
  is_bookNt = 0;
//  is_bookNt =  rc->get_IntFlag("EMCNEW_IS_BOOKNT",0);
  
  if ( is_ert == 0 ) {
      TriggerBit = TT_BBC; 
  }
  if ( is_ert == 1 ) {
      TriggerBit = TT_ERT; 
  }  

  pTcut = rc->get_FloatFlag("EMC_NTUPLE_PTCUT", 5.0);
//  pTcut = 0.5;
  cout << " *** pT cut for ntuples = " << pTcut << " GeV" <<endl;

  tofcutter = NULL;//KTofCutter::getInstance(); 
  //tofcutter->init(is_pp);
  
  /////////////////////////////////////
  /// read in ToF cuts for run 8 
  /////////////////////////////////////
  const int MAXY[2][4] = { {36, 36, 36, 36}, {48, 48, 36, 36}};
  const int MAXZ[2][4] = { {72, 72, 72, 72}, {96, 96, 72, 72}};

  std::ifstream status_file;
  std::ifstream low_limit_file;
  std::ifstream high_limit_file;

  TOAD *toad_loader = new TOAD("Run16dAuPi0Photon");
  std::string file_status = toad_loader->location("tof_status_arrays");
  std::string file_low = toad_loader->location("tof_low_limit_arrays");
  std::string file_high = toad_loader->location("tof_high_limit_arrays");

  std::cout << " file_status = " << file_status << std::endl;
  std::cout << " file_low = " << file_low << std::endl;
  std::cout << " file_high = " << file_high << std::endl;

  delete toad_loader;

  status_file.open(file_status.c_str());
  low_limit_file.open(file_low.c_str());
  high_limit_file.open(file_high.c_str());

  std::string read_str;

  for (int a = 0; a < 2; a++) {
    for (int s = 0; s < 4; s++) {
      for (int y = 0; y < MAXY[a][s]; y++) {
        for (int z = 0; z < MAXZ[a][s]; z++) {
          getline(status_file, read_str);
          _status_array[a][s][y][z] = 0;//atoi(read_str.c_str());
          getline(low_limit_file, read_str);
          _low_limit_array[a][s][y][z] = 0;//atof(read_str.c_str());
          getline(high_limit_file, read_str);
          _high_limit_array[a][s][y][z] = 0;//atof(read_str.c_str());
        }
      }
    }
  }

  status_file.close();
  low_limit_file.close();
  high_limit_file.close();

  return;
}

Run16dAuPi0Photon::~Run16dAuPi0Photon()
{

  std::cout << "Run16dAuPi0Photon::~Run16dAuPi0Photon: Here's looking at you, kid..." << std::endl;

  for (int i=0; i<NUM_CENT_CLASSES; i++ )
    for (int j=0; j<NUM_VTX_CLASSES; j++ )
      for (int k=0; k<NUM_RP_CLASSES; k++ )
	    evarray[i][j][k].clear();

  delete EmcGeo;
  EmcGeo = 0;

  delete _diagfile;

  delete _CP_Calib;
}
#define HERE(x) if(debug>0) std::cout << __PRETTY_FUNCTION__ << " " << __LINE__ << x << std::endl

int Run16dAuPi0Photon::process_event(PHCompositeNode *topNode)
{



  //  _CP_Calib->InitTables(topNode);
  _CP_Calib->LoadGlobals(topNode);

  
  //  _CP_Calib->InitTables(topNode);
  //  _CP_Calib->LoadGlobals(topNode);

 
#ifdef HAVE_EVBTIMER_H
  _eventTimer.restart();
#endif
  // WGH: I don't think this is needed for Run7
  checkSegNumber();
  EventHeader *eh = getClass<EventHeader>(topNode, "EventHeader");
  if ( !eh ) {
    std::cout << PHWHERE << "Failed to find EventHeader Node" << std::endl;
    //
    // Not fatal (yet), so just continue
    //
  }
  
// cout << PHWHERE << "  OK " << endl;
  int iret = EVENT_OK;
  if( (_nevents>0) && (_nevents % 1000 == 0) && (_nevents!=_prevevent)) std::cout << "TP8::Events "<< _nevents << std::endl;
  _prevevent = _nevents;

	//cout << "nevents= " << _nevents << endl;

  bool nodeLookup = getNodes(topNode);
  if ( nodeLookup == false ) {
    std::cout << PHWHERE << "WARNING: Failed to get one or more critical Nodes!" << std::endl;
  }
    
  if ( !_phGlobal_ptr ) return -1;
  if ( !_Trig_ptr ) return -1;
  if ( !_Ert_ptr ) return -1;
//ondrejch  if ( !_reacPlane_ptr ) return -1;
//
  
//  std::cout <<"Pass node test!"<<std::endl;
  
  if(!((is_ert==0)||(is_ert==1))) { 
    std::cout << PHWHERE << "Trigger flag ill-defined" << std::endl;
    return -229;
  }
  //std::cout << "Test1" << std::endl;
  /// Fill the event rate tree 
  _evtN   = eh->get_EvtSequence(); // event number
  _evTime = eh->get_TimeStamp();   // time stamp
  _runN   = runnumber;

  
  if(is_bookNt>0) { _evRateTree->Fill(); }

  TriggerHelper thelp(topNode);

  string TrigName[6] = {"BBCLL1(>0 tubes)",
                       "BBCLL1(>0 tubes) novertex",
                       "BBCLL1(>0 tubes) narrowvtx",
                       "ERT_4x4a&BBCLL1",
                       "ERT_4x4b",
			"ERT_4x4c&BBCLL1(narrow)"};
  if(runnumber <= 422085) TrigName[5] = "ERT_4x4c&BBCLL1";	   

  int PickTrig[6] = { 0, 1, 4, 6, 7, 8}; /// p+p
						   
  if(is_pp != 1) {                    /// p+Au
     PickTrig[0] = 0;		//  0 - BBCLL! >0tubes
     PickTrig[1] = 1; 		//  1 - BBCLL! no vtx cut 
     PickTrig[2] = 4; 		//  4 - BBCLL! narrow vtx 
     PickTrig[3] = 6; 		//  6 - 4x4b 
     PickTrig[4] = 3; 		//  3 - Central trigger
     PickTrig[5] = 8; 		//  8 - 4x4c & BBCLL1
  }

  if(is_pp == 1) {                    /// p+p
     PickTrig[0] = 0;		//  0 - BBCLL! >0tubes
     PickTrig[1] = 1; 		//  1 - BBCLL! no vtx cut 
     PickTrig[2] = 4; 		//  4 - BBCLL! narrow vtx 
     PickTrig[3] = 6; 		//  6 - 4x4b & BBCLL1
     PickTrig[4] = 7; 		//  7 - 4x4a & BBCLL1
     PickTrig[5] = 8; 		//  8 - 4x4c & BBCLL1
  }

  int trigscaled_on[6]={0,0,0,0,0,0};
  int trigscaledown[6]={0,0,0,0,0,0};
  int triglive_on[6]={0,0,0,0,0,0};
  int wanted_trig[6]={0,0,0,0,0,0};

  // Trigger information - we process all trigger information but wite out only 4x4c
  for(int i=0;i<6;i++){

     triglive_on[i] = _Trig_ptr->get_lvl1_triglive_bit(PickTrig[i]);
     trigscaled_on[i] = _Trig_ptr->get_lvl1_trigscaled_bit(PickTrig[i]);
	 trigscaledown[i] = thelp.getLevel1Scaledown(TrigName[i]);
	 //cout << "Trigger  " << PickTrig[2] << "\t live: "<< triglive_on[2] << "\t scaled: "<< trigscaled_on[2] <<"\t"<<trigscaledown[2]<<endl;
	 //cout <<"\n\n\n"<< runnumber << " " << i<<" "<<trigscaledown[i] << endl;
	 
  }
// cout << PHWHERE << "  OK " << endl;
  UInt_t crossorg   = _Trig_ptr->get_lvl1_clock_cross(); 
  Int_t cross = ( crossorg + xing_shift ) % 120;   
  xing = cross;
 
 
  // Trigger wanted (BBC). Consolidated
  if(TriggerBit == TT_BBC) { /// MinBias 
     wanted_trig[0] = trigscaled_on[0];
     wanted_trig[1] = trigscaled_on[1];
     wanted_trig[2] = trigscaled_on[2];
     wanted_trig[3] = _Trig_ptr->get_lvl1_triglive_bit(5); //ZDC
     wanted_trig[4] = trigscaled_on[0] | trigscaled_on[1] | trigscaled_on[2]; //all BBC triggers
	 wanted_trig[5] = _Trig_ptr->get_lvl1_trigscaled_bit(3); //Central triggter
  }

  //
  // Trigger wanted (ERT). Consolidated
  if(TriggerBit == TT_ERT) {  /// ERT
     wanted_trig[0] = trigscaled_on[4]; //ERTA
     wanted_trig[1] = trigscaled_on[3]; //ERTB
     wanted_trig[2] = trigscaled_on[5]; //ERTC
     wanted_trig[3] = _Trig_ptr->get_lvl1_triglive_bit(5); //ZDC
     wanted_trig[4] = trigscaled_on[4] | trigscaled_on[3] | trigscaled_on[5]; //all ERT triggers
  }

   
  //cout << "Triggers: isERT: " << TriggerBit << ", TriggerBit: " << trigscaled_on[3] << ", WantedTrig:  " << wanted_trig[trigsel] << endl;
  if(TriggerBit == TT_BBC){
     //if(!wanted_trig[0] && !wanted_trig[1] && !wanted_trig[2]) return iret;
     if(!wanted_trig[trigsel]) return iret;
  }

 

  if(TriggerBit == TT_ERT){
     if(!wanted_trig[trigsel]) return iret;
  }

  
 
  for ( int itrig = 0; itrig < 32; itrig++){
	  if (_Trig_ptr->get_lvl1_trigscaled_bit(itrig)){
		  htrigger->Fill(itrig, xing);
	  }
  }
  if( _currentSegNumber == 0) htrigger->SetBinContent(45, 50, xing_shift);
  hlumi->Fill(xing);
  
  float bbct0 = _phGlobal_ptr->getBbcTimeZero();
  if (bbct0 < -900)     {
    if(debug>0) cout << " BBC T0 off, exitting"<<endl;
    return iret;
  }
 

  //below lines are used to run Carlo's calibration and the reason I am adding BBC time as it is subtracted eeprately in this code adn thus to avoid double subtraction. 

 for(uint i=0; i!= _emcClusterContainer_ptr->size(); ++i)
  {
    emcClusterContent* emc=  _emcClusterContainer_ptr->getCluster(int(i));
    double tof = _CP_Calib->TOF(emc, -1) +bbct0;
    if(TriggerBit == TT_ERT)
      tof=tof+0.2;
    emc->set_tofcorr(tof);
    double ecore_new = _CP_Calib->ECoreCorr(emc);
    emc->set_ecore(ecore_new);
    
  }
  

  float bbcqs = _phGlobal_ptr->getBbcChargeS();
  float bbcqn = _phGlobal_ptr->getBbcChargeN();

  float zdces = _phGlobal_ptr->getZdcEnergyS();
  float zdcen = _phGlobal_ptr->getZdcEnergyN();

  if(is_patag){
//     cout <<"PA In"<<endl;
     if(zdcen<60 || zdcen>180) return iret;
//     cout <<"PA passed"<<endl;
  }


  bbcz = _phGlobal_ptr->getBbcZVertex();
  
  if (fabs(bbcz) > VTX_Z_CUT)  { 
   if(debug>0)cout << "<Run16dAuPi0Photon::process_event> bbcz " << bbcz << endl;
      return iret;
  }

  

  if ( ! _emcClusterContainer_ptr ) { 
    cout << "<Run16dAuPi0Photon::process_event>  no _emcClusterContainer_ptr" <<endl;
    return -1;
  }
 

  double vtxBinWidth = 60.0 / NUM_VTX_CLASSES;
  int ivtx = int( ( bbcz + 30. ) / vtxBinWidth );

  if ( ivtx < 0 || ivtx >= NUM_VTX_CLASSES )  { 
    if(debug>0) cout << "<Run16dAuPi0Photon::process_event> ivtx " << ivtx <<endl;
    return iret;
  }


  float percent = _phGlobal_ptr->getCentrality();
  if(is_pp==1) percent=1;   /// centrality "fix" to use the same code for p+p Run8 as for d+Au  OCH 
  int MBT  = trigscaled_on[trigsel];
  int MBTl = triglive_on[trigsel];
  int MBTla = ((trigscaledown[0]+1)/(trigscaledown[4]+1));
  int MBTlb = ((trigscaledown[0]+1)/(trigscaledown[3]+1));
  int MBTlc = 0;//((trigscaledown[0]+1)/(trigscaledown[5]+1));
  int ERTa = trigscaled_on[trigsel] & trigscaled_on[3]; // just to check if the first condition is redundant or not
  int ERTb = trigscaled_on[3] & triglive_on[2]; //ERTb and BBC narrow vertex live
  int ERTc = trigscaled_on[trigsel] & triglive_on[5];
  // int ERTb_n_MBnl = trigscaled_on[3]
  
  if (!(percent > 0 && percent <= 100)) {
    HERE("");
    HERE(" percent = " << percent);
    return iret;
  }
  centrality->Fill(percent); // Fill the cnetrality histogram in persentage

  //std::cout <<"6: centrality passed!"<<std::endl;

  _nclus = _emcClusterContainer_ptr->size();
  int cent = evCent.getCentBin( percent, NUM_CENT_CLASSES);
  bbczdc->Fill(1.0,(float)cent,bbcqs,bbcqn,zdces,zdcen,bbcz);
  evts_ert->Fill(1.0, (int)MBT, (int)MBTl, (int)ERTa, (int)ERTb, (int)ERTc, (float)cent);

//  for(int ntrig=0;ntrig<NUM_TRIG_CLASSES;ntrig++)
//  if(wanted_trig[0]) event_counter->Fill(cent);
  
  event_counter->Fill((float)cent);
  if( trigscaledown[0] ){
 	  event_live_erta->Fill((float)cent,MBTla);
	  event_live_ertb->Fill((float)cent,MBTlb);
	  event_live_ertc->Fill((float)cent,MBTlc);
  }
  //std::cout << "Test6" << std::endl;

  if (!(cent >= 0))
    {//    cout << "<Run16dAuPi0Photon::process_event> cent " << cent <<endl;
       return iret;
    }


  //
  // Getting Event plane information through RecoConsts
  //
  float theta;

  rc =  recoConsts::instance();   /// read inmacro constatnts
  theta = rc->get_FloatFlag("BBCEP2_VALUE",11.0*3.141592/180.0);

  //cout << "2: "<<theta<<endl;

  float thetaDEG = theta * DEG_PER_RAD;
  int   rpBin;

  double rpBinWidth;

  if(vorder==1){
     rpBinWidth = 2*180.0/NUM_RP_CLASSES;
  }
  else{
     rpBinWidth = 2*90.0/NUM_RP_CLASSES;
  }

  if (thetaDEG <= 0.) rpBin = int((thetaDEG-rpBinWidth)/rpBinWidth) + NUM_RP_CLASSES/2;
  else rpBin = int(thetaDEG/rpBinWidth) + NUM_RP_CLASSES/2;

  //if(cent==0) cout<<thetaDEG<<", " <<rpBinWidth<<", "<<rpBin<<endl; 

  //reacpl->Fill(1.0, thetaDEG, (float)cent);

  //std::cout <<"7: theta passed!"<<std::endl;

  //if(cent>=0&& cent<4) reacpl2->Fill(theta);

///beg
  KEVENT_PTR kp;
  int nc=0;

  KEVENT_PTR dumkp(new KEvent());
  kp= dumkp;
  kp->setCentralityBin(cent);
  kp->setReacBin(rpBin);
  kp->setTheta(theta);
  //std::cout << "Test7" << std::endl;
  
/// Fom taxi206 up, we only fill ntrig==0 for minbias and ntrig==3 (4x4c) for ERT to save memory // ondrej
  kp->getEvent( _emcClusterContainer_ptr, _phCentTrack_ptr,
                bbct0, bbcz, EmcGeo, gamma1, gammarap, tower, towertof,towertof_3ns,        _Ert_ptr, runnumber, _gammaDiagNtuple,is_bookNt, pTcut,ERTb,
                //_status_array,_low_limit_array,_high_limit_array,
                bbcqn,bbcqs,_nevents,cluster_counter,
				bbcqs_ecoreMax, bbcqs_ecoreTotal, bbcqs_ecoreAverage, bbcqn_ecoreMax, bbcqn_ecoreTotal, bbcqn_ecoreAverage,cent_fw_corr );
//                _Ert_ptr, runnumber, _gammaDiagNtuple,is_bookNt);
//                _Ert_ptr, runnumber, _gammaDiagNtuple,is_bookNt);
  
  nc = kp->getGammaMultiplicity();
  //std::cout << "Test8" << std::endl;
        
  evts_mult->Fill(1.0,(float)nc,(float)rpBin,(float)percent);
  evts_cent->Fill(1.0,(float)nc,(float)rpBin,(float)cent);
  
  _nevents++;
   
  cmbReal(kp,ERTb);

  evarray[cent][ivtx][rpBin].push_back(kp);
  // cout<<"\n size"<< evarray[cent][ivtx][rpBin].size() << "maxDepth"<<MAX_EVBUF_DEPTH;  
  if ( evarray[cent][ivtx][rpBin].size() == MAX_EVBUF_DEPTH ) 
    {

#if defined(HAVE_EVBTIMER_H)
    EvBTimer mixTimer;
#endif

    // make combinations in mixed events
    cmbMixedERT(cent, ivtx, rpBin,ERTb);
    //std::cout << "Test9" << std::endl;

    // move remaining events down in buffer by 1 index
    //
    evarray[cent][ivtx][rpBin].pop_front();

#if defined(HAVE_EVBTIMER_H)&&defined(HAVE_EVBAVERAGER_H)
    _mixTimeAvg.insert(mixTimer.getElapsedTime());
#endif
  }

#if defined(HAVE_EVBTIMER_H)&&defined(HAVE_EVBAVERAGER_H)
     _eventTimeAvg.insert(_eventTimer.getElapsedTime());
#endif

  return EVENT_OK;

}


int Run16dAuPi0Photon::InitRun(PHCompositeNode *topNode)
{

 
  //std::cout << __FILE__ << ":" << __LINE__  << " in InitRun" << std::endl;
  _CP_Calib->InitTables(topNode);
  runnumber = 0;
 
  RunHeader *runheader = getClass<RunHeader>(topNode, "RunHeader");
  if ( !runheader ) {
    std::cout << PHWHERE << "Failed to find RunHeader Node" << std::endl;
    //
    // Not fatal (yet), so just continue
    //
  }
 
  runnumber = runheader->get_RunNumber();
  std::cout << "Run16dAuPi0Photon::InitRun: Run Number = " << runnumber <<std::endl;
  
  
 
  if(is_bookNt>0) {
//#ifdef BOOK_GG_NTUPLE
//#warning Diagnostic Ntuple code is being included
  // We open the special root file once and book the ntuple
  //
    
   _ggDiagNtuple = NULL;
   if ( _ggDiagNtuple == 0 ) 
    {
     
      int thisSegNumber = se->SegmentNumber();
      std::ostringstream ss;
      ss << "diag_" << runnumber << "_" << thisSegNumber << ".root";
      _diagfilename = ss.str();
      std::cout << "Run16dAuPi0Photon::InitRun: " << "Create output file for diagnostic ntuple: " << _diagfilename 
		<< std::endl;
      _diagfile = new TFile(OutFileName.c_str(),"RECREATE");
      _diagfile->UseCache(0,0); // nocache ondrejch
//      _diagfile = new TFile(_diagfilename.c_str(),"RECREATE");
      std::cout << "Run16dAuPi0Photon::InitRun: " << "Book gghntuple Ntuple for diagnostics" << std::endl;
      _ggDiagNtuple = bookGGNtuple();
      _gammaDiagNtuple = bookGammaNtuple();
      _evRateTree = new TTree("evrt","Event rates tree");
      _evRateTree->Branch("evn",&_evtN,"evn/i");
      _evRateTree->Branch("run",&_runN,"run/i");
      _evRateTree->Branch("time",&_evTime,"time/l");
    }
//#endif
  }
  
  
  return 0;
}

int Run16dAuPi0Photon::Init(PHCompositeNode *topNode)
{
   std::cout << __FILE__ << ":" <<__LINE__ << " in Init, flags "<<is_pp << " "<<is_ert << std::endl;

 

   _nevents = 0;
  _neventsGammaCluster = 0;
  _currentSegNumber = -1;
  _numMissingSegments = 0;
  
#ifdef SIMPLE_MIXING
  evCent.setCentBins(0, 0., 92.); 
#else
/*
  evCent.setCentBins(0, 0., 20.);   /// run8
  evCent.setCentBins(1, 20., 40.); 
  evCent.setCentBins(2, 40., 60.); 
  evCent.setCentBins(3, 60., 88.); 
*/

// For EP study
  evCent.setCentBins(0, 0., 5.);   /// run8
  evCent.setCentBins(1, 5., 10.);   /// run8
  evCent.setCentBins(2, 10., 15.);   /// run8
  evCent.setCentBins(3, 15., 20.);   /// run8
  evCent.setCentBins(4, 20., 40.); 
  evCent.setCentBins(5, 40., 60.); 
  evCent.setCentBins(6, 60., 88.); 
#endif


  EmcGeo = new mEmcGeometryModule();

  se = Fun4AllServer::instance();

  const float upper = NUM_CENT_CLASSES; 
  int num_cent      = NUM_CENT_CLASSES;

  
  std::cout << "Run16dAuPi0Photon::Init: " << "Book event_counter histogram" << std::endl;
  event_counter = new TH1F("event_counter","Event counter histogram", 7, -0.5, 6.5);
  se->registerHisto(event_counter->GetName(), event_counter );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book cluster_counter histogram" << std::endl;
  cluster_counter = new TH1F("cluster_counter","Cluster counter histogram", 200, -100, 100);
  se->registerHisto(cluster_counter->GetName(), cluster_counter );



  std::cout << "Run16dAuPi0Photon::Init: " << "Book event_live_erta histogram" << std::endl;
  event_live_erta = new TH1F("event_live_erta","Event live histogram", 7, -0.5, 6.5);
  se->registerHisto(event_live_erta->GetName(), event_live_erta );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book event_live_ertb histogram" << std::endl;
  event_live_ertb = new TH1F("event_live_ertb","Event live histogram", 7, -0.5, 6.5);
  se->registerHisto(event_live_ertb->GetName(), event_live_ertb );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book event_live_ertc histogram" << std::endl;
  event_live_ertc = new TH1F("event_live_ertc","Event live histogram", 7, -0.5, 6.5);
  se->registerHisto(event_live_ertc->GetName(), event_live_ertc );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book htrigger histogram" << std::endl;
  htrigger = new TH2F("htrigger","Trigger counter histogram", 50, -0.5, 49.5, 120, -0.5, 119.5);
  se->registerHisto(htrigger->GetName(), htrigger );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book hlumi histogram" << std::endl;
  hlumi = new TH1F("hlumi","Trigger counter histogram", 120, -0.5, 119.5);
  se->registerHisto(hlumi->GetName(), hlumi );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book hpatternBlue histogram" << std::endl;
 
  std::cout << "Run16dAuPi0Photon::Init: " << "Book hAcc histogram" << std::endl;
  hAcc = new TH2F("hAcc","Acceptance histogram", 100, -1.0, 4.0, 100, -0.5, 0.5);
  se->registerHisto(hAcc->GetName(), hAcc );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book hRelative histogram" << std::endl;
  hRelative = new TH1D("hRelative","Trigger counter histogram", 5, -1.5, 3.5);
  se->registerHisto(hRelative->GetName(), hRelative );


  char buffer[128];
  for(int i=0;i<8;i++) {  
    std::cout << "Run16dAuPi0Photon::Init: " << "Book pair_timing_sec_" << i << "histogram" << std::endl;
    sprintf(buffer,"pair_timing_sec_%d",i);
    pair_timing_sec[i] = new TH2F(buffer,buffer, 500, -50., 80.,500, -50., 80.);
    se->registerHisto(pair_timing_sec[i]->GetName(), pair_timing_sec[i] );
  }

  std::cout << "Run16dAuPi0Photon::Init: " << "Book centrality histogram" << std::endl;
  centrality = new TH1D("centrality","centrality distribution",100,-0.5,99.5);
  se->registerHisto(centrality->GetName(), centrality);

  
  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbcqs_ecoreMax histogram" << std::endl;
  bbcqs_ecoreMax = new TH2F("bbcqs_ecoreMax","bbcqs_ecoreMax",250,0,250,150,0,30);
  se->registerHisto(bbcqs_ecoreMax->GetName(), bbcqs_ecoreMax);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbcqs_ecoreTotal histogram" << std::endl;
  bbcqs_ecoreTotal = new TH2F("bbcqs_ecoreTotal","bbcqs_ecoreTotal",250,0,250,150,0,30);
  se->registerHisto(bbcqs_ecoreTotal->GetName(), bbcqs_ecoreTotal);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbcqs_ecoreAverage histogram" << std::endl;
  bbcqs_ecoreAverage = new TH2F("bbcqs_ecoreAverage","bbcqs_ecoreAverage",250,0,250,200,0,5);
  se->registerHisto(bbcqs_ecoreAverage->GetName(), bbcqs_ecoreAverage);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbcqn_ecoreMax histogram" << std::endl;
  bbcqn_ecoreMax = new TH2F("bbcqn_ecoreMax","bbcqn_ecoreMax",250,0,250,150,0,30);
  se->registerHisto(bbcqn_ecoreMax->GetName(), bbcqn_ecoreMax);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbcqn_ecoreTotal histogram" << std::endl;
    bbcqn_ecoreTotal = new TH2F("bbcqn_ecoreTotal","bbcqn_ecoreTotal",250,0,250,150,0,30);
  se->registerHisto(bbcqn_ecoreTotal->GetName(), bbcqn_ecoreTotal);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbcqn_ecoreAverage histogram" << std::endl;
  bbcqn_ecoreAverage = new TH2F("bbcqn_ecoreAverage","bbcqn_ecoreAverage",250,0,250,200,0,5);
  se->registerHisto(bbcqn_ecoreAverage->GetName(), bbcqn_ecoreAverage);



  //std::cout << "Run16dAuPi0Photon::Init: " << "Book reacpl multi-histogram" << std::endl;
  //reacpl = new THmulf("reacpl","Reaction plane distribution");
//if(vorder==1)  reacpl->AddAxis("reac","reaction plane",48,-180.0,180.0);
//else  reacpl->AddAxis("reac","reaction plane",48,-90.0,90.0);
  //reacpl->AddAxis("cent","Centrality Class", num_cent, -0.5, (upper-0.5));
  //se->registerHisto(reacpl->GetName(), reacpl );

  //std::cout << "Run16dAuPi0Photon::Init: " << "Book reacpl2 simple-histogram" << std::endl;
//if(vorder==1) reacpl2 = new TH1D("reacpl2","Simple Reaction plane distribution",48,-1*pi,pi);
//else reacpl2 = new TH1D("reacpl2","Simple Reaction plane distribution",48,-1*pi/2.0,pi/2.0);
  //se->registerHisto(reacpl2->GetName(), reacpl2 );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book cluster mult multi-histogram" << std::endl;
  evts_mult = new THmulf("evts_mult","Cluster Multiplicity");
  evts_mult->AddAxis("mult","EMC cluster mult", 5, -0.5, 20.); 
  evts_mult->AddAxis("rpBin","reaction plane Bin",NUM_RP_CLASSES,-0.5,NUM_RP_CLASSES-0.5);
  evts_mult->AddAxis("cent","cent", 102,-1.5,100.5);
  se->registerHisto(evts_mult->GetName(), evts_mult );
  
  evts_cent = new THmulf("evts_cent","Cluster Centrality");
  evts_cent->AddAxis("mult","EMC cluster multiplicity", 20, -0.5, 100.); 
  evts_cent->AddAxis("rpBin","reaction plane Bin",NUM_RP_CLASSES,-0.5,NUM_RP_CLASSES-0.5);
  evts_cent->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  se->registerHisto(evts_cent->GetName(), evts_cent );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book ert trigger signal" << std::endl;
  evts_ert = new THmulf("evts_ert","Trigger normalization");
  evts_ert->AddAxis("minb","Min. bias trigg", 3, -0.5, 2.5); 
  evts_ert->AddAxis("minblive","Min. bias trigg", 3, -0.5, 2.5); 
  evts_ert->AddAxis("erta","ERTA trigg", 3, -0.5, 2.5); 
  evts_ert->AddAxis("ertb","ERTB trigg", 3, -0.5, 2.5); 
  evts_ert->AddAxis("ertc","ERTC trigg", 3, -0.5, 2.5); 
  evts_ert->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  se->registerHisto(evts_ert->GetName(), evts_ert );
  
  //
  // Pair asymmetry histogram for studying the asymmetry dependence
  //
  std::cout << "Run16dAuPi0Photon::Init: " << "Book gghasym multi-histogram" << std::endl;
  gghasym = new THmulf("gghasym","Pair asymmetry");
  gghasym->AddAxis("mass","Invariant Mass",160,0.,0.8);
  gghasym->AddAxis("pt","Pair Pt",55,0.5,28.0);
  gghasym->AddAxis("mix","Real or mixed",2,-0.5,1.5); 
  gghasym->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  gghasym->AddAxis("chi2","Chi2 cut",4,-0.5,3.5);
  // gghasym->AddAxis("chi2","Chi2 cut",2,-0.5,1.5);
  gghasym->AddAxis("sec","sector",8,-0.5,7.5);
//  gghasym->AddAxis("asym","Energy Asymmetry Bin",_ASYMBINS,_ASYMLOWEDGES);
  gghasym->AddAxis("tof","tof cut",7,-1.5,5.5);
  //gghasym->AddAxis("tof","tof cut",5,-1.5,3.5);
  //gghasym->AddAxis("stoch","Stoch cut",2,-0.5,1.5);
  gghasym->AddAxis("ertb","ERT4x4b",2,-0.5,1.5);
  se->registerHisto(gghasym->GetName(), gghasym);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book ggh_sysErr_Asym multi-histogram" << std::endl;
  ggh_sysErr_Asym = new THmulf("ggh_sysErr_asym","Pair asymmetry");
  ggh_sysErr_Asym->AddAxis("mass","Invariant Mass",160,0.,0.8);
  ggh_sysErr_Asym->AddAxis("pt","Pair Pt",55,0.5,28.0);
 
  ggh_sysErr_Asym->AddAxis("mix","Real or mixed",2,-0.5,1.5); 
  ggh_sysErr_Asym->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  ggh_sysErr_Asym->AddAxis("chi2","Chi2 cut",4,-0.5,3.5);
  //ggh_sysErr_asym->AddAxis("sec","sector",8,-0.5,7.5);
  ggh_sysErr_Asym->AddAxis("asym","Energy Asymmetry Bin",_ASYMBINS,_ASYMLOWEDGES);
  ggh_sysErr_Asym->AddAxis("tof","tof cut",7,-1.5,5.5);
  ggh_sysErr_Asym->AddAxis("ertb","ERT4x4b",2,-0.5,1.5);
  se->registerHisto(ggh_sysErr_Asym->GetName(), ggh_sysErr_Asym);






  std::cout << "Run16dAuPi0Photon::Init: " << "Book gghspecial multi-histogram" << std::endl;
  gghspecial = new THmulf("gghspecial","Pair asymmetry");
  gghspecial->AddAxis("mass","Invariant Mass",160,0.,0.4);
  gghspecial->AddAxis("pt","Pair Pt",50,0.0,10.0);
  gghspecial->AddAxis("mix","Real or mixed",2,-0.5,1.5); 
  gghspecial->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  gghspecial->AddAxis("chi2","Chi2 cut",2,-0.5,1.5);
  gghspecial->AddAxis("sec","sector",8,-0.5,7.5);
  gghspecial->AddAxis("asym","Energy Asymmetry Bin",_ASYMBINS,_ASYMLOWEDGES);
  gghspecial->AddAxis("tof","tof cut",5,-1.5,3.5);
  se->registerHisto(gghspecial->GetName(), gghspecial);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book gghrap multi-histogram" << std::endl;
  gghrap = new THmulf("gghrap","dN/drap");
  gghrap->AddAxis("mass","Invariant Mass",60,0.,0.3);
  gghrap->AddAxis("pt","Pair Pt",19,0.5,19.5);
  gghrap->AddAxis("rap","Rapidity",50,-0.5,0.5);
  gghrap->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  gghrap->AddAxis("tof","tof cut",5,-1.5,3.5);
  se->registerHisto(gghrap->GetName(), gghrap);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book gghcheck multi-histogram" << std::endl;
  gghcheck = new THmulf("gghcheck","Pair asymmetry");
  gghcheck->AddAxis("mass","Invariant Mass",160,0.,0.8);
  gghcheck->AddAxis("mix","Real or mixed",2,-0.5,1.5); 
  gghcheck->AddAxis("arm","Arm",2,-0.5,1.5); 
  gghcheck->AddAxis("pt","Pair Pt",19,1.0,20.0);
  se->registerHisto(gghcheck->GetName(), gghcheck);

  //std::cout << "Run16dAuPi0Photon::Init: " << "Book gghrp1 multi-histogram" << std::endl;
  //gghrp = new THmulf("gghrp","pair histograms w.r.t. RP");
  //gghrp->AddAxis("mass","Invariant Mass",80,0.,0.4);
  //gghrp->AddAxis("pt","Pair Pt",30,1.,16.0);
  //gghrp->AddAxis("mix","Real or mixed",2,-0.5,1.5); 
  //gghrp->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  //gghrp->AddAxis("pid","chi2 + stoch cut",3,-0.5,2.5);
  //gghrp->AddAxis("reac","reaction plane",12,-0.5,11.5);
  //gghrp->AddAxis("sec","sector",4,-0.5,7.5);
  //gghrp->AddAxis("pc3","pc3 reject cut",2,-0.5,1.5);
  //gghrp->AddAxis("asym","Energy Asymmetry Bin",_ASYM2BINS,_ASYM2LOWEDGES);
  //se->registerHisto(gghrp->GetName(), gghrp);

  std::cout << "Run16dAuPi0Photon::Init: " << "Book gamma1 multi-histogram" << std::endl;
  gamma1 = new THmulf("gamma1","gamma pT distribution");
  gamma1->AddAxis("pt","pt of cluster",120,0.,30.);
  gamma1->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  gamma1->AddAxis("sec","sector",8,-0.5,7.5);
  gamma1->AddAxis("tof","tof cut",7,-1.5,5.5);
//  gamma1->AddAxis("tof","tof cut",2,-0.5,1.5);
  gamma1->AddAxis("PID","stoch cut",4,-0.5,3.5); 
//  gamma1->AddAxis("reac","reaction plane w/MPC",24,-0.5,11.5);
  gamma1->AddAxis("reac","reaction plane w/MPC",24,-0.5,23.5);
  gamma1->AddAxis("pc3","pc3 rejection",3,-0.5,2.5);
  gamma1->AddAxis("ertb","ERT4x4b",2,-0.5,1.5); 
//  gamma1->AddAxis("chi2","Chi2 cut",2,-0.5,1.5);
  se->registerHisto(gamma1->GetName(), gamma1 );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book gammarap multi-histogram" << std::endl;
  gammarap = new THmulf("gammarap","gamma pT distribution");
  gammarap->AddAxis("pt","pt of cluster",30,0.,30.);
  gammarap->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  gammarap->AddAxis("rap","Rapidity",50,-0.5,0.5);
  gammarap->AddAxis("tof","tof cut",2,-0.5,1.5);
  se->registerHisto(gammarap->GetName(), gammarap );
  
  std::cout << "Run16dAuPi0Photon::Init: " << "Book tower multi-histogram" << std::endl;
  tower = new THmulf("tower","tower hit distribution");
  tower->AddAxis("energy","cluster energy",120, 0.,30.);
  //tower->AddAxis("energy","central tower energy",120, 0.,30.);
  tower->AddAxis("sec","sector",8,-0.5,7.5);
  tower->AddAxis("iy","iy",48,-0.5,47.5);
  tower->AddAxis("iz","iz",96,-0.5,95.5);
  tower->AddAxis("Tighty","Tight rejectmap cut",2,-0.5,1.5);
  se->registerHisto(tower->GetName(), tower );


 std::cout << "Run16dAuPi0Photon::Init: " << "Book tower multi-histogram" << std::endl;
  tower_gghasym = new THmulf("tower_gghasym","tower hit distribution");
  tower_gghasym->AddAxis("sec","sector",8,-0.5,7.5);
  tower_gghasym->AddAxis("iy","iy",48,-0.5,47.5);
  tower_gghasym->AddAxis("iz","iz",96,-0.5,95.5);
  se->registerHisto(tower_gghasym->GetName(), tower_gghasym );
  
  std::cout << "Run16dAuPi0Photon::Init: " << "Book towertof multi-histogram" << std::endl;
  towertof = new THmulf("towertof","tower tof hit distribution");
  // towertof->AddAxis("tof","tof tower",50,-2.5,2.5);
  towertof->AddAxis("tof","tof tower",50,-10.,10.);
  towertof->AddAxis("energy","central tower energy",40, 0.,20.);
//towertof->AddAxis("energy","central tower energy",20, 0.,20.);
  towertof->AddAxis("sec","sector",8,-0.5,7.5);
  towertof->AddAxis("iy","iy",48,-0.5,47.5);
  towertof->AddAxis("iz","iz",96,-0.5,95.5);
  se->registerHisto(towertof->GetName(), towertof );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book towertof multi-histogram" << std::endl;
  towertof_3ns = new THmulf("towertof_3ns","tower tof hit distribution");
  towertof_3ns->AddAxis("tof","tof tower",50,-2.5,2.5);
  //towertof->AddAxis("tof","tof tower",50,-10.,10.);
  towertof_3ns->AddAxis("energy","central tower energy",40, 0.,20.);
//towertof->AddAxis("energy","central tower energy",20, 0.,20.);
  towertof_3ns->AddAxis("sec","sector",8,-0.5,7.5);
  towertof_3ns->AddAxis("iy","iy",48,-0.5,47.5);
  towertof_3ns->AddAxis("iz","iz",96,-0.5,95.5);
  se->registerHisto(towertof_3ns->GetName(), towertof_3ns );


  std::cout << "Run16dAuPi0Photon::Init: " << "Book bbczdc multi-histogram" << std::endl;
  bbczdc = new THmulf("bbczdc","Bbc Zdc Distr");
  bbczdc->AddAxis("cent","Centrality Class",num_cent,-0.5,(upper-0.5));
  bbczdc->AddAxis("bbcs","bbc S charge",10,0.,300.);
  bbczdc->AddAxis("bbcn","bbc N charge",10,0.,300.);
  bbczdc->AddAxis("zdcs","zdc S energy",5,0.,5000.);
  bbczdc->AddAxis("zdcn","zdc N energy",10,0.,400.);
  bbczdc->AddAxis("vtxz","vertex z position",60,-30.,30.);
  se->registerHisto(bbczdc->GetName(), bbczdc );

  std::cout << "Run16dAuPi0Photon::Init: " << "Book gghboxes multi-histogram" << std::endl;
  gghboxes = new THmulf("gghboxes","Pair in timing boxes");

  gghboxes->AddAxis("mass","Invariant Mass",160,0.,0.8);
  gghboxes->AddAxis("pt","Pair Pt",55,0.5,28.0);
  gghboxes->AddAxis("sec","sector",8,-0.5,7.5);
  gghboxes->AddAxis("box","box cut",13,-1.5,11.5);
  //  gghboxes->AddAxis("mix","Real or mixed",2,-0.5,1.5);
  se->registerHisto(gghboxes->GetName(), gghboxes);

  std::cout << "Run15dAuPi0Photon::Init: " << "Book cent_fw_corr multi-histogram " << std::endl;
  cent_fw_corr = new THmulf("cent_fw_corr","central forward correlation");
  cent_fw_corr->AddAxis("bbcq","BBC charge ", 60,0,240);
  cent_fw_corr->AddAxis("nhclus","number of high clusters(>0.5GeV)", 10,0,20);
  cent_fw_corr->AddAxis("nclus","number of clusters(>0.05GeV)", 30,0,240);
  cent_fw_corr->AddAxis("ecoreMax","ecoreMax", 30,0,30);
  cent_fw_corr->AddAxis("ecoreTotal","ecoreTotal", 20,0,40);
  cent_fw_corr->AddAxis("NS","North-0, South-1", 2,-0.5,1.5);
  se->registerHisto(cent_fw_corr->GetName(),cent_fw_corr);

  
  return 0;
}

void Run16dAuPi0Photon::cmbReal(KEVENT_PTR kevent_ptr,int ERTb)
{
 
  //cout<<"\n\n\n____________STEP 5___________\n\n\n";
  float ev_theta = kevent_ptr->getTheta();
  int nc = kevent_ptr->getGammaMultiplicity();
  int cent = kevent_ptr->getCentralityBin();
  int req_trig=-1; 

  for( int k = 0; k<nc-1 ; k++ ) 
    {
                 
      for( int j = k+1; j<nc ; j++ ) 
	{
          // Trigger wanted (BBC). Consolidated
          if(TriggerBit == TT_BBC) req_trig=-1; 

          // Trigger wanted (ERT). Consolidated
          if(TriggerBit == TT_ERT){
             if(trigsel==0) req_trig=0;
             if(trigsel==1) req_trig=1;
             if(trigsel==2) req_trig=2;
          }
          //if(TriggerBit == TT_ERT) req_trig=0;
	
	  const KCluster* clusa = kevent_ptr->cluster_element(k);	  
	  const KCluster* clusb = kevent_ptr->cluster_element(j);
	  assert(clusa != 0);
	  assert(clusb != 0);
          int seca = clusa->getSec();  /// both gammas hit the same sector
          int secb = clusb->getSec();
          if(seca != secb) continue;

	  if( cmb.calcCombination( clusa, clusb, ev_theta, req_trig, vorder, pair_timing_sec) ) // 5/6/2010 ondrejch 
	    {
	      tower_gghasym->Fill(1.0,seca,clusa->getIy(),clusa->getIz());
	      mixed = 0;
 	      cmb.fillAsymHist(gghasym, mixed, cent, ERTb);  
 	      cmb.fillAsymHist(gghspecial, mixed, cent,ERTb);  
              cmb.fillBoxesHist(gghboxes, mixed, cent);
	      cmb.fill_sysErr_asym(ggh_sysErr_Asym, mixed, cent, ERTb);
   
 	      cmb.fillRapHist(gghrap, cent);
	      //cmb.fillPHist(gghrp, mixed, cent);

	      // #ifdef BOOK_GG_NTUPLE
 	      if(is_bookNt>0) cmb.fillNtuple(_ggDiagNtuple, cent, pTcut,runnumber);
// #endif
	    }
	} 
    }
}


void Run16dAuPi0Photon::cmbMixedERT(int cent, int ivtx, int rpBin, int ERTb) 
{

  //cout<<"\n\n\n____________STEP 6___________\n\n\n";
  int e1 = MAX_EVBUF_DEPTH-1;
  int n1 = evarray[cent][ivtx][rpBin][e1]->getGammaMultiplicity();
  float ev1_theta = evarray[cent][ivtx][rpBin][e1]->getTheta();

  assert((unsigned int)n1 == evarray[cent][ivtx][rpBin][e1]->clusterVector.size());
  for ( int e2=0; e2<(MAX_EVBUF_DEPTH-1); e2++) 
    {
      int n2 = evarray[cent][ivtx][rpBin][e2]->getGammaMultiplicity();
      assert((unsigned int)n2 == evarray[cent][ivtx][rpBin][e2]->clusterVector.size());

      float ev2_theta = evarray[cent][ivtx][rpBin][e2]->getTheta();
      float dev_theta = fabs(ev1_theta - ev2_theta);
      float avg_theta;

      if (ev1_theta > ev2_theta) 
        {
          avg_theta = ev2_theta + fabs(dev_theta)/2.;
        }
      else
        {
          avg_theta = ev1_theta + fabs(dev_theta)/2.;
        }
      
      if (avg_theta > pi/2.) avg_theta = avg_theta - pi;
      if (avg_theta < -1.*pi/2.) avg_theta = avg_theta + pi;
      

      for( int subi = 0; subi<n1 ; subi++ ) 
	{
	  for( int subj = 0; subj<n2 ; subj++ ) 
	    {
	      
	     
	      const KCluster* clusb = evarray[cent][ivtx][rpBin][e2]->cluster_element(subj);
	      const KCluster* clusa = evarray[cent][ivtx][rpBin][e1]->cluster_element(subi);
	      assert(clusa != 0);
	      assert(clusb != 0);
              int seca = clusa->getSec();  /// both gammas hit the same sector
              int secb = clusb->getSec();
              if(seca != secb) continue;

              int req_trig = -1; 

              if(TriggerBit == TT_BBC) req_trig=-1;

              // Trigger wanted (ERT). Consolidated
              if(TriggerBit == TT_ERT){
                 if(trigsel==0) req_trig=0;
                 if(trigsel==1) req_trig=1;
                 if(trigsel==2) req_trig=2;
              }

	      //cout<<"\nTrigger Bit" <<TriggerBit<< "\t"<<clusa->getErtBit(req_trig)<<"\t"<<clusb->getErtBit(req_trig);
              //if(TriggerBit == TT_ERT) req_trig=0;

              /// test if either cluster A has the trigger bit, while cluster B has no triggers, or the other way around
              int takepair = 0;
              if( req_trig>=0 && (clusa->getErtBit(req_trig) >0) && (clusb->getErtBit(req_trig)==0) ) takepair++;
              if( req_trig>=0 && (clusa->getErtBit(req_trig)==0) && (clusb->getErtBit(req_trig) >0) ) takepair++;
              if((takepair==0) && (TriggerBit == TT_ERT))  continue; // take all pairs for MinBias data, ondrej Jul 15 2010
              
	      
	      if( cmb.calcCombination( clusa, clusb, avg_theta, req_trig, vorder,pair_timing_sec) ) // 5/6/2010 ondrejch 
	      {
		
		mixed = 1;
		cmb.fillAsymHist(gghasym, mixed, cent, ERTb);
		cmb.fill_sysErr_asym(ggh_sysErr_Asym, mixed, cent, ERTb);
		cmb.fillAsymHist(gghspecial, mixed, cent, ERTb);
		      //cmb.fillBoxesHist(gghboxes, mixed, cent);
	      }//end if

	      }  // loop over second event clusters

	    }  // loop over first event clusters

    }  // loop over events for second clusters in combination

}

int Run16dAuPi0Photon::End(PHCompositeNode *topNode) {

   if (is_bookNt){
    if ( _ggDiagNtuple ) {
    std::cout << "Run16dAuPi0Photon::End: Writing out diagnostic ntuple" << std::endl;
    _diagfile->Write();
    _diagfile->Close();
    }
  }

  summary();

  return 0;
}

void Run16dAuPi0Photon::summary() 
{
  std::cout << "Number of events processed = " << _nevents << " (" << _neventsGammaCluster << " with gamma clusters)" 
	    << std::endl;
#if defined(HAVE_EVBTIMER_H) && defined(HAVE_EVBAVERAGER_H)
  std::cout << "process_event time (Avg/Min/Max) = " << _eventTimeAvg.getAverage()
	    << " / " << _eventTimeAvg.getMin() << " / " << _eventTimeAvg.getMax() << " sec (for " 
	    <<  _eventTimeAvg.getTotalCount() << " total counts)" << std::endl;
  std::cout << "Event mixing time (Avg/Min/Max) = " << _mixTimeAvg.getAverage()
	    << " / " << _mixTimeAvg.getMin() << " / " << _mixTimeAvg.getMax() << " sec (for " 
	    << _mixTimeAvg.getTotalCount() << " total counts)" << std::endl;
  std::cout << "Total time in process_event = " << _eventTimeAvg.getTotalCount() * _eventTimeAvg.getAverage()
	    << " sec" << std::endl;
#endif
}

bool Run16dAuPi0Photon::getNodes(PHCompositeNode* topNode)
{
  bool retVal = true; // set to false if we fail to get a needed node
  
  _Trig_ptr = getClass<TrigLvl1>(topNode, _TrigNodeName.c_str());
  if ( !_Trig_ptr )
    {
      std::cout << PHWHERE << "Could not find " << _TrigNodeName << " Node" << std::endl;
      retVal = false;
    }

  _Ert_ptr = getClass<ErtOut>(topNode, _ErtNodeName.c_str());
  if ( !_Ert_ptr )
    {
      std::cout << PHWHERE << "Could not find " << _ErtNodeName << " Node" << std::endl;
      retVal = false;
    }

  _phGlobal_ptr = getClass<PHGlobal>(topNode, _phGlobalNodeName.c_str());
  if ( !_phGlobal_ptr )
    {
      std::cout << PHWHERE << "Could not find " << _phGlobalNodeName << " Node" << std::endl;
      retVal = false;
    }

  _emcClusterContainer_ptr = getClass<emcClusterContainer>(topNode, _emcClusterContainerNodeName.c_str());
  if ( !_emcClusterContainer_ptr ) {
    std::cout << PHWHERE << "Could not find " << _emcClusterContainerNodeName << " Node" << std::endl;
    retVal = false;
  }

  _phCentTrack_ptr = getClass<PHCentralTrack>(topNode, _phCentralTrackNodeName.c_str());
  if ( !_phCentTrack_ptr ) {
    std::cout << PHWHERE << "Failed to find " << _phCentralTrackNodeName << " Node" << std::endl;
    //
    // Not fatal (yet), so just continue
    //
  }

  return retVal;
}

void Run16dAuPi0Photon::checkSegNumber()
{
  if ( se ) 
    {
      int thisSegNumber = se->SegmentNumber();
      // This assumes segments are coming in order...
      //
      if ( thisSegNumber != _currentSegNumber ) 
	{
	  std::cout << "Run16dAuPi0Photon::process_event: New Segment detected: old = " << _currentSegNumber << ", "
		    << "new = " << thisSegNumber << std::endl;
	  if ( thisSegNumber - _currentSegNumber != 1 )
	    {
	      std::cout << "Run16dAuPi0Photon::process_event: WARNING: Segment sequence problem: old = " << _currentSegNumber << ", "
			<< "new = " << thisSegNumber << std::endl;
	      _numMissingSegments++;
	    }
	}
      _currentSegNumber = thisSegNumber;
    }
}

TNtuple* Run16dAuPi0Photon::bookGGNtuple() const
{
  return new TNtuple("ggntuple","gg pair ntuple",Diagnostic::getDescription().c_str());
}

TNtuple* Run16dAuPi0Photon::bookGammaNtuple() const
{
  return new TNtuple("gnt","gammma ntuple",Diagamma::getDescription().c_str());
}

