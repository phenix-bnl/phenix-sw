#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>

#include <glob.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TString.h>
#include <TROOT.h>
#include <TTree.h>
#include <TBranch.h>
#include <TProfile.h>

#include <fileEventiterator.h>
#include <Event.h>
#include <EventTypes.h>
#include <packet.h>
#include <ZdcRaw.h>
#include <ZdcRawv1.h>
#include <ZdcOut.h>
#include <ZdcOutv2.h>
#include <SmdOut.h>
#include <SmdOutv1.h>
#include <BbcOut.h>
#include <BbcOutv1.h>
//#include <NtcRaw.h>
//#include <NtcRawv1.h>
//#include <FclRaw.h>
//#include <FclRawv1.h>
//#include <FclCompactRawv1.h>
//#include <FclCalib.h>
//#include <FclOut.h>
//#include <FclOutv1.h>
//#include <FclIndexer.h>
#include <BbcEvent.hh>
#include <ZdcEvent.hh>
#include <BbcCalib.hh>
#include <ZdcCalib.hh>
#include <ZdcCalOverflow.h>
#include <ZdcCalPedestal.h>
#include <ZdcCalEnergy.h>
#include <ZdcCalTiming.h>
#include <ZdcCalSlew.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include <TriggerUtilities.h>
#include <TriggerHelper.h>
#include <TrigLvl1.h>

using namespace std;

//static TriggerUtilities tu;

// local definition of trigger bits
static const unsigned int BBCLL1 = 0x1;
static const unsigned int ZDCNS = 0x2;
//static const unsigned int NTCNSwide = 0x4;
static const unsigned int BBCLL1_noVtx = 0x8;
static const unsigned int ZDCS = 0x10;
static const unsigned int ZDCNorS = 0x20;
static const unsigned int CLOCK = 0x40;
static const unsigned int ZDCNSnarrow = 0x80;
static const unsigned int BBCLL1andZDCNorS = 0x100;
static const unsigned int ZDCLL1wide = 0x200;
static const unsigned int ZDCLL1narrow = 0x400;

static const unsigned int PPGpedestal = 0x10000000;
static const unsigned int PPGtestpulse = 0x20000000;
static const unsigned int PPGlaser = 0x40000000;

static const int alltrigs_flag = 0;

static unsigned int GetL1Trig(Packet *gl1p, const char *type)
{
  int trigword = gl1p->iValue(0,type);

  unsigned int temp_trig = 0;
  if ( (trigword&0x2)!=0 ) temp_trig |= CLOCK;
  if ( (trigword&0x4)!=0 ) temp_trig |= BBCLL1;
  if ( (trigword&0x8)!=0 ) temp_trig |= ZDCNS;
  if ( (trigword&0x10)!=0 ) temp_trig |= ZDCLL1wide;
  if ( (trigword&0x20)!=0 ) temp_trig |= ZDCLL1narrow;
  if ( (trigword&0x10000000)!=0 ) temp_trig |= PPGpedestal;
  if ( (trigword&0x40000000)!=0 ) temp_trig |= PPGlaser;

  return temp_trig;
}

int zdcprdf2tree(const char *prdfname, const int pass, const unsigned int nevents,
                 const char *zdctreefname)
{
  int runNumber = 0;

  // parse filename wildcards
  glob_t globbuf;
  glob( prdfname, 0, NULL, &globbuf );
  
  if ( globbuf.gl_pathc == 0 )
    {
      cout << "no match to " << prdfname << endl;
      return 0;
    }

  // need to figure out how many channels automatically
  const unsigned int numch = 8*5;	// number of channels

  // Instantiate Output Objects
  ZdcRaw *zdcraw = new ZdcRawv1();
  //  NtcRaw *ntcraw = new NtcRawv1();
  BbcOut *bbcout = new BbcOutv1();
  ZdcOut *zdcout = new ZdcOutv2();// for evaluation
  SmdOut *smdout = new SmdOutv1();// for evaluation
  /*
  FclOut *fclout[2];
  fclout[0] = new FclOutv1();	// north fcal
  fclout[1] = new FclOutv1();	// south fcal
  FclIndexer* fclindexer = FclIndexer::Instance();

  TClonesArray *fcl = new TClonesArray("FclCompactRawv1",2);
  new ((*fcl)[0]) FclCompactRawv1();	// north
  new ((*fcl)[1]) FclCompactRawv1();	// south
  */

  unsigned int scaledtrig = 0;
  unsigned int livetrig = 0;
  unsigned int rawtrig = 0;

  unsigned int scaledgl1trig = 0;
  unsigned int livegl1trig = 0;
  unsigned int rawgl1trig = 0;

  float fcale[2] = { 0. };
  unsigned int prdf_eventnum = 0;

  const int compression = 3;
  TFile *savefile = new TFile(zdctreefname,"RECREATE","ZDC Tree",compression);

  // tree for general use
  TBranch *b[100];
  int ibr = 0;
  TTree *zdctree = new TTree("T","ZDC Tree");
  b[ibr++] = zdctree->Branch("event",&prdf_eventnum,"prdf_eventnum/i");
  b[ibr++] = zdctree->Branch("scaledtrig",&scaledtrig,"scaledtrig/i");
  b[ibr++] = zdctree->Branch("livetrig",&livetrig,"livetrig/i");
  b[ibr++] = zdctree->Branch("rawtrig",&rawtrig,"rawtrig/i");
  b[ibr++] = zdctree->Branch("scaledgl1trig",&scaledgl1trig,"scaledgl1trig/i");
  b[ibr++] = zdctree->Branch("livegl1trig",&livegl1trig,"livegl1trig/i");
  b[ibr++] = zdctree->Branch("rawgl1trig",&rawgl1trig,"rawgl1trig/i");
  b[ibr++] = zdctree->Branch("ZdcRaw","ZdcRawv1",&zdcraw);
  b[ibr++] = zdctree->Branch("BbcOut","BbcOutv1",&bbcout);
  b[ibr++] = zdctree->Branch("ZdcOut","ZdcOutv2",&zdcout);// for evaluation
  b[ibr++] = zdctree->Branch("SmdOut","SmdOutv1",&smdout);// for evaluation
  //  b[ibr++] = zdctree->Branch("FclOutNorth","FclOutv1",&(fclout[0]));
  //  b[ibr++] = zdctree->Branch("FclOutSouth","FclOutv1",&(fclout[1]));

  // test of 
  TH2F *fcaltowers[2];
  fcaltowers[0] = new TH2F("fcaltowers0","fcaltowers0",9,-0.5,8.5,10,-0.5,9.5);
  fcaltowers[1] = new TH2F("fcaltowers1","fcaltowers1",9,-0.5,8.5,10,-0.5,9.5);

  ZdcCalOverflow zdcoverflow;
  ZdcCalPedestal zdcpedestal;

  // for evaluation of zdc vertex (pass 1)
/*
  TH1F *zvtxbvtxcheck = new TH1F("zvtxbvtxcheck","ZDC-BBC vertex",400,-20.,20.);
  TH1F *zt0bt0check = new TH1F("zt0bt0check","ZDC-BBC T0",3000,-10.,20.);
  TH1F *ztdiff[8][2];
  for (int itdc=0; itdc<2; itdc++)
    {
      for (int ich=0; ich<8; ich++)
        {
  	  name = "ztdiffch"; name += ich; name += "tdc"; name += itdc;
          ztdiff[ich][itdc] = new TH1F(name,name,2000,-10.,10.);
        }
    }
*/

  TString calibdir;

  bool objyflag = true;	// whether to get constants from database
  bool bbcobjyflag = true;	// whether to get constants from database
  BbcCalib *bbccalib = new BbcCalib();

  /*
  FclCalib fclcalibnorth;
  FclCalib fclcalibsouth;
  fclcalibnorth.setSide(0);
  fclcalibsouth.setSide(1);
  */

  calibdir = getenv("HOME");

  BbcEvent *bbcevent = new BbcEvent();

  ZdcCalib zdccalib;
  if (pass==0)
    {
      calibdir += "/run04commissioning/pass0/ZdcCalib";
      zdccalib.restore( (char *)calibdir.Data() );
    }
  else if (pass==1)
    {
      calibdir += "/run04commissioning/pass1/ZdcCalib";
      zdccalib.restore( (char *)calibdir.Data() );
    }

  ZdcCalSlew   zdcslew( &zdccalib );

  ZdcEvent *zdcevent = new ZdcEvent();

  if ( pass==0 || pass==1 ) zdcevent->setCalibDataAll(zdccalib);

  const int zdc_packetid = 13001;         // ZDC Packet Id

  // loop over all files and process them for pedestals
  // this can be considered pass 0
  unsigned int eventnum = 0;

  for (size_t ifile=0; (ifile<globbuf.gl_pathc)&&(pass==0); ifile++)
    {
      if ( (nevents!=0) && (eventnum==nevents) ) break;

      char *prdfname = globbuf.gl_pathv[ifile];
      cout << "processing " << prdfname << endl;

      int status = 0;
      Eventiterator *it =  new fileEventiterator(prdfname, status);
      if (status)
	{
	  cout << "Couldn't open input file " << prdfname << endl;
	  exit(1);
	}
 
      Event *evt = 0;
      while ( (nevents==0||eventnum<nevents) && ((evt = it->getNextEvent())!=0) )
	{
	  if ( evt->getEvtType() != DATAEVENT )
            {
              delete evt;
              continue;
            }

	  runNumber = evt->getRunNumber();
	  unsigned int strig = 0;

	  /*
	  TriggerHelper *zdc_TH = tu.getTriggerHelper(runNumber, evt);

          if ( zdc_TH == NULL)
            {
              cerr << "Can't get TriggerHelper." << endl;
              delete evt;
	      if (eventnum!=0) continue;
	      else             return -1;
            }

	  // exclude laser events
	  if ( zdc_TH->didLevel1TriggerFire("PPG(Laser)") )
            {
              delete evt;
              continue;
            }
	    */

	  //-** temporary kludge until triggerhelper works
	  /*
	  Packet *gl1p = evt->getPacket( 14001 );
          if (gl1p)
	    {
              strig = GetL1Trig( gl1p, "SCALEDTRIG" );

	      //-** exclude laser events
	      if ( (strig&PPGlaser)!=0 )
	        {
	          delete evt;
		  continue;
		}
	    }
	    */

          if ( bbcobjyflag )
            {
	      /*
              RunToTime *runTime = RunToTime::instance();
              PHTimeStamp *ts = runTime->getBeginTime(evt->getRunNumber());
              cout << "Getting calibration constants from objectivity ..." << endl;
              cout << "Run Number is " << evt->getRunNumber() << " TimeStamp is ";
              PHTimeStamp BeginRunTimeStamp(0);
              if ( ts )
                {
                  BeginRunTimeStamp = (*ts) + 5; //magic 5
                }
              else
                {
                  cout << "Using event TimeStamp since Run TimeStamp was not in database!!!!"
                       << endl;
                  BeginRunTimeStamp.setTics(evt->getTime());
                }
              BeginRunTimeStamp.print(); cout << endl;
	      */

              PHTimeStamp BeginRunTimeStamp(2003,2,1,0,0,0);

              //bbccalib->restore( BeginRunTimeStamp, 1004 );
bbccalib->restore("/home/phnxbb/Y4/calib/BbcCalib");
              bbcevent->setCalibDataAll( bbccalib );

              bbcobjyflag = false;
            }

	  bbcevent->Clear();
	  bbcevent->setRawData(evt);
	  bbcevent->calculate();

	  prdf_eventnum = evt->getEvtSequence();
          if ( (eventnum%1000)==0 ) cout << "event " << eventnum << endl;

	  Packet *p = evt->getPacket(zdc_packetid);

	  if (p)
	    {
	      //-** fill overflow for pedestal events
	      if ( (strig&PPGpedestal)!=0 ) zdcoverflow.FillFromPacket(p);

	      zdcpedestal.FillFromPacket(p);
              if ( fabs(bbcevent->getZVertex())<50. )
                {
                  float bbcntime = bbcevent->getArmHitTime(Bbc::North);
                  float bbcstime = bbcevent->getArmHitTime(Bbc::South);
                  zdcslew.FillFromPacket(p,bbcntime,bbcstime);
                }

              delete p;
	    }

	  eventnum++;
	  delete evt;
	}

      delete it;
    }

  cout << " processed " << eventnum << " events for pedestals" << endl;

  if ( pass==0 )
    {
      zdcoverflow.CalculateConstants();
      zdccalib.restore("ZdcCalib.overflow0.temp","overflow0");
      zdccalib.restore("ZdcCalib.overflow1.temp","overflow1");

      zdcpedestal.CalculateConstants();
      zdcpedestal.SaveToFile();
      zdccalib.restore("ZdcCalib.pedestal.temp","pedestal");
    }

  //-*** end of pass 0

  //-*** start of pass1
  // loop over all files again and process them again (ie, pass1)
  eventnum = 0;
  int nlaser = 0;

  //  bool fclcalibflag = false;	// whether fcl has good calibrations

  ZdcCalEnergy   zdcenergy( &zdccalib );
  ZdcCalTiming   zdctiming( &zdccalib );

  for (size_t ifile=0; (ifile<globbuf.gl_pathc)&&(pass==1||pass==2); ifile++)
    {
      if ( (nevents!=0) && (eventnum==nevents) ) break;

      char *prdfname = globbuf.gl_pathv[ifile];
      cout << "processing " << prdfname << endl;

      int status = 0;
      Eventiterator *it =  new fileEventiterator(prdfname, status);
      if (status)
	{
	  cout << "Couldn't open input file " << prdfname << endl;
	  exit(1);
	}
      
      
      Event *evt = 0;
      while ( ((evt = it->getNextEvent())!=0) && (nevents==0||eventnum<nevents) )
	{
	  if ( evt->getEvtType() != DATAEVENT )
            {
              delete evt;
              continue;
            }

	  prdf_eventnum = evt->getEvtSequence();
          if ( (eventnum%1000)==1 ) cout << "event " << eventnum << endl;

	  runNumber = evt->getRunNumber();

	  // get trigger information
	  //TriggerHelper *myTH = tu.getTriggerHelper(evt->getRunNumber(), evt);

	  scaledtrig = 0;
	  livetrig = 0;
	  rawtrig = 0;
	  scaledgl1trig = 0;
	  livegl1trig = 0;
	  rawgl1trig = 0;

	  //-** temporary kludge until triggerhelper works
	  Packet *gl1p = evt->getPacket( 14001 );
          if (gl1p)
	    {
              scaledtrig = GetL1Trig(gl1p,"SCALEDTRIG");
              livetrig = GetL1Trig(gl1p,"LIVETRIG");
              rawtrig = GetL1Trig(gl1p,"RAWTRIG");
	      scaledgl1trig = gl1p->iValue(0,"SCALEDTRIG");
	      livegl1trig = gl1p->iValue(0,"LIVETRIG");
	      rawgl1trig = gl1p->iValue(0,"RAWTRIG");
	    }

	  /*
	  if ( myTH!=0 )
	    {
	      if ( myTH->trigScaled("BBCLL1>=1") ) scaledtrig |= BBCLL1;
	      if ( myTH->trigScaled("ZDCNS") ) scaledtrig |= ZDCNS;
	      if ( myTH->trigScaled("ZDCNSwide") ) scaledtrig |= ZDCNS;
	      if ( myTH->trigScaled("NTCNSwide") ) scaledtrig |= NTCNSwide;
	      if ( myTH->trigScaled("BBCLL1>=1(noVertexCut)") ) scaledtrig |= BBCLL1_noVtx;
	      if ( myTH->trigScaled("ZDCS") ) scaledtrig |= ZDCS;
	      if ( myTH->trigScaled("ZDCS|ZDCN") ) scaledtrig |= ZDCNorS;
	      if ( myTH->trigScaled("Clock") ) scaledtrig |= CLOCK;
	      if ( myTH->trigScaled("PPG(Pedestal)") ) scaledtrig |= PPGpedestal;
	      if ( myTH->trigScaled("PPG(TestPulse)") ) scaledtrig |= PPGtestpulse;
	      if ( myTH->trigScaled("PPG(Laser)") ) scaledtrig |= PPGlaser;
	      if ( myTH->trigScaled("ZDCLL1") ) scaledtrig |= ZDCLL1wide;
	      if ( myTH->trigScaled("ZDCLL1wide") ) scaledtrig |= ZDCLL1wide;
	      if ( myTH->trigScaled("ZDCLL1narrow") ) scaledtrig |= ZDCLL1narrow;
	      if ( myTH->trigScaled("BBCLL1&(ZDCN|ZDCS)") ) scaledtrig |= BBCLL1andZDCNorS;

	      if ( myTH->trigLive("BBCLL1>=1") ) livetrig |= BBCLL1;
	      if ( myTH->trigLive("ZDCNS") ) livetrig |= ZDCNS;
	      if ( myTH->trigLive("ZDCNSwide") ) livetrig |= ZDCNS;
	      if ( myTH->trigLive("NTCNSwide") ) livetrig |= NTCNSwide;
	      if ( myTH->trigLive("BBCLL1>=1(noVertexCut)") ) livetrig |= BBCLL1_noVtx;
	      if ( myTH->trigLive("ZDCS") ) livetrig |= ZDCS;
	      if ( myTH->trigLive("ZDCS|ZDCN") ) livetrig |= ZDCNorS;
	      if ( myTH->trigLive("Clock") ) livetrig |= CLOCK;
	      if ( myTH->trigLive("PPG(Pedestal)") ) livetrig |= PPGpedestal;
	      if ( myTH->trigLive("PPG(TestPulse)") ) livetrig |= PPGtestpulse;
	      if ( myTH->trigLive("PPG(Laser)") ) livetrig |= PPGlaser;
	      if ( myTH->trigLive("ZDCLL1") ) livetrig |= ZDCLL1wide;
	      if ( myTH->trigLive("ZDCLL1wide") ) livetrig |= ZDCLL1wide;
	      if ( myTH->trigLive("ZDCLL1narrow") ) livetrig |= ZDCLL1narrow;
	      if ( myTH->trigLive("BBCLL1&(ZDCN|ZDCS)") ) livetrig |= BBCLL1andZDCNorS;

	      if ( myTH->trigRaw("BBCLL1>=1") ) rawtrig |= BBCLL1;
	      if ( myTH->trigRaw("ZDCNS") ) rawtrig |= ZDCNS;
	      if ( myTH->trigRaw("ZDCNSwide") ) rawtrig |= ZDCNS;
	      if ( myTH->trigRaw("NTCNSwide") ) rawtrig |= NTCNSwide;
	      if ( myTH->trigRaw("BBCLL1>=1(noVertexCut)") ) rawtrig |= BBCLL1_noVtx;
	      if ( myTH->trigRaw("ZDCS") ) rawtrig |= ZDCS;
	      if ( myTH->trigRaw("ZDCS|ZDCN") ) rawtrig |= ZDCNorS;
	      if ( myTH->trigRaw("Clock") ) rawtrig |= CLOCK;
	      if ( myTH->trigRaw("PPG(Pedestal)") ) rawtrig |= PPGpedestal;
	      if ( myTH->trigRaw("PPG(TestPulse)") ) rawtrig |= PPGtestpulse;
	      if ( myTH->trigRaw("PPG(Laser)") ) rawtrig |= PPGlaser;
	      if ( myTH->trigRaw("ZDCLL1") ) rawtrig |= ZDCLL1wide;
	      if ( myTH->trigRaw("ZDCLL1wide") ) rawtrig |= ZDCLL1wide;
	      if ( myTH->trigRaw("ZDCLL1narrow") ) rawtrig |= ZDCLL1narrow;
	      if ( myTH->trigRaw("BBCLL1&(ZDCN|ZDCS)") ) rawtrig |= BBCLL1andZDCNorS;

	      scaledgl1trig = myTH->get_trigLvl1()->get_lvl1_trigscaled();
	      livegl1trig = myTH->get_trigLvl1()->get_lvl1_triglive();
	      rawgl1trig = myTH->get_trigLvl1()->get_lvl1_trigraw();
	      rawgl1trig = myTH->get_trigLvl1()->get_lvl1_trigraw();

	      // temporary kludge to get at ZDC-LL1 reduced bits
	      Packet *gl1p = evt->getPacket( 14001 );
              if (gl1p)
                {
		  unsigned int rbits1 = gl1p->iValue(0,"RBITS1");
		  if ( (rbits1&0x2000) != 0 )
		    {
		      rawtrig |= ZDCLL1narrow;
		      livetrig |= ZDCLL1narrow;
		    }
                  delete gl1p;
		}
	    }
	  else
	    {
	      cout << "could not instantiate triggerhelper" << endl;
	    }
	  */

          // only keep the interesting events
	  /*
          if ( scaledtrig == 0 )
            {
              delete evt;
              continue;
            }
	    */

	  // on the 1st pass through the data, set the calibration
	  // constants based on the timestamp from the run number
	  if ( objyflag )
	    {
	      /*
	      RunToTime *runTime = RunToTime::instance();
	      PHTimeStamp *ts = runTime->getBeginTime(evt->getRunNumber());
              cout << "Getting calibration constants from objectivity ..." << endl;
	      cout << "Run Number is " << evt->getRunNumber() << " TimeStamp is ";
	      PHTimeStamp BeginRunTimeStamp(0);
	      if ( ts )
		{
		  BeginRunTimeStamp = (*ts) + 5; //magic 5
		}
	      else
		{
		  cout << "Using event TimeStamp since Run TimeStamp was not in database!!!!"
		       << endl;
		  BeginRunTimeStamp.setTics(evt->getTime());
		}
              BeginRunTimeStamp.print(); cout << endl;
	      */

PHTimeStamp BeginRunTimeStamp(2003,2,1,0,0,0);
              if ( bbcobjyflag )
                {
                  //bbccalib->restore( BeginRunTimeStamp, 1004 );
                  bbccalib->restore( "/home/phnxbb/Y4/calib/BbcCalib");
                  bbcevent->setCalibDataAll( bbccalib );
                }

/*
              if ( fclcalibnorth.getDatabaseInfo( runNumber ) &&
                   fclcalibsouth.getDatabaseInfo( runNumber ) )
                {
                  fclcalibflag = true;
                }
              else
                {
                  cout << " *** FCAL calibrations missing from run "
                       << runNumber << ", skipping fclout" << endl;
                }
*/

              if ( pass==2 )
                {
		  cout << "restoring zdc, pass2" << endl;
                  calibdir += "/run04commissioning/final_calibrations/ZdcCalib";
                  zdccalib.restore( (char *)calibdir.Data() );
		  /*
                  zdccalib.restore( BeginRunTimeStamp );
                  zdcevent->setCalibDataAll( zdccalib );
		  */
		  
                }

	      objyflag = false;
	    }
 
	  zdcraw->Reset();
	  //	  ntcraw->Reset();
	  bbcout->Reset();
	  zdcout->Reset();
	  smdout->Reset();

	  /*
	  fclout[0]->Reset();
	  fclout[1]->Reset();

          FclRaw *fclraw[2];
          fclraw[0] = (FclCompactRawv1 *)fcl->At(0);
          fclraw[1] = (FclCompactRawv1 *)fcl->At(1);
	  fclraw[0]->Reset();
	  fclraw[1]->Reset();
	  */

	  bbcevent->Clear();
	  bbcevent->setRawData(evt);
	  bbcevent->calculate();

	  zdcevent->Clear();
	  zdcevent->setRawData(evt);
	  zdcevent->calculate();

	  // this part shouldn't have to be done by me
	  bbcout->set_TimeVertex( bbcevent->getTimeZero(),
				  bbcevent->getTimeZeroError(),
				  bbcevent->getZVertex(),
				  bbcevent->getZVertexError() );
	  bbcout->AddBbcNS( bbcevent->getnHitPmt(Bbc::North),
			    bbcevent->getChargeSum(Bbc::North),
			    bbcevent->getArmHitTime(Bbc::North), Bbc::North);
	  bbcout->AddBbcNS( bbcevent->getnHitPmt(Bbc::South),
			    bbcevent->getChargeSum(Bbc::South),
			    bbcevent->getArmHitTime(Bbc::South), Bbc::South);

	  for (int ipmt = 0; ipmt<8; ipmt++)
	    {
	      zdcout->AddZdcHit(zdcevent->getCharge(ipmt),
				zdcevent->getTime0(ipmt),
				zdcevent->getTime1(ipmt),(short)ipmt);

	      float btime = -99999.;
	      if ( ipmt<4 ) btime = bbcevent->getArmHitTime(Bbc::South);
	      else if ( ipmt>=4 ) btime = bbcevent->getArmHitTime(Bbc::North);

/*
	      if ( btime>-999. )
	        {
		  if ( zdcevent->getTime0(ipmt)>-999. )
		    {
		      float tdiff = zdcevent->getTime0(ipmt) - btime;
		      ztdiff[ipmt][0]->Fill( tdiff );
		    }
		  if ( zdcevent->getTime1(ipmt)>-999. )
		    {
		      float tdiff = zdcevent->getTime1(ipmt) - btime;
		      ztdiff[ipmt][1]->Fill( tdiff );
		    }
		}
*/
	    }
	  zdcout->AddZdcNS(zdcevent->getEnergy(0),zdcevent->getHitTime(0),0);
	  zdcout->AddZdcNS(zdcevent->getEnergy(1),zdcevent->getHitTime(1),1);
	  zdcout->set_TimeVertex(zdcevent->getTimeZero(),
				 zdcevent->getTimeZeroError(),
				 zdcevent->getZvertex(),
				 zdcevent->getZvertexError());

          // copy smd information to the SmdOut object
          for (int ipmt=8; ipmt<40; ipmt++)
            {
              float charge = zdcevent->getCharge(ipmt);
              float time0 = zdcevent->getTime0(ipmt);
              float time1 = zdcevent->getTime1(ipmt);
              smdout->AddSmdHit(charge,time0,time1,(short)(ipmt-8));
            }
          int arm = Zdc::South;
          smdout->AddSmdNS(zdcevent->getSmdPosX(arm),zdcevent->getSmdPosY(arm),
             zdcevent->getSmdEnergy(arm),arm);
          arm = Zdc::North;
          smdout->AddSmdNS(zdcevent->getSmdPosX(arm),zdcevent->getSmdPosY(arm),
             zdcevent->getSmdEnergy(arm),arm);

	  if ( fabs(zdcevent->getZvertex())<1000. && fabs(bbcevent->getZVertex())<1000. &&
	       (rawtrig&PPGlaser)!=PPGlaser )
	    {
	      zdctiming.Fill( zdcevent->getZvertex() - bbcevent->getZVertex(),
	                      zdcevent->getTimeZero() - bbcevent->getTimeZero() );
	    }

	  // fill NTC raw packet
	  /*
          static const int ntc_packetid = 15001;
	  Packet *p = evt->getPacket( ntc_packetid );
	  if (p)
	    {
	      unsigned int num_ntc_ch = 16;
	      for (unsigned int ich=0; ich<num_ntc_ch; ich++)
		{
		  int q  = p->iValue(ich);
		  int t0 = p->iValue(ich, "T1");
		  int t1 = p->iValue(ich, "T2");
	          ntcraw->AddNtcRawHit(ich,q,t0,t1);
		}

	      delete p;
	    }
	  */
          // fill FCAL output object

	  // fill FCAL raw packet (16001 = south = 1, 16002 = north = 0)
	  /*
	  fcale[0] = 0.;
	  fcale[1] = 0.;
          static const int fcl_packetids[] = { 16002, 16001 };
          for (int iarm=0; iarm<2; iarm++)
            {
	      p = evt->getPacket( fcl_packetids[iarm] );
              if (p)
                {
                  fclraw[iarm]->readData(p,iarm);

                  if ( fclcalibflag )
                    {
		      fclout[iarm]->readData(p,iarm);
		      if (iarm==0) fclout[iarm]->calibrateData(fclcalibnorth);
		      else         fclout[iarm]->calibrateData(fclcalibsouth);
		      fclout[iarm]->computeSums();
                    }
                  delete p;
                }
            }
	  */

	  Packet *p = evt->getPacket(zdc_packetid);
	  if (p)
	    {
	      // fill the ntuple (just one board for now)
	      for (unsigned int ich=0; ich<numch; ich++)
		{
		  int q  = p->iValue(ich);
		  int t0 = p->iValue(ich, "T1");
		  int t1 = p->iValue(ich, "T2");

		  zdcraw->AddZdcRawHit(q,t0,t1,ich);

	          if ( (rawtrig&PPGlaser)==PPGlaser )
                    {
		      if (nlaser<10&&ich==0) cout << "found laser " << eventnum << endl;
		      if (ich==0) nlaser++;
		      continue;
		    }
		}

              //-** fill ADC Spectrum for zdc
              //-** if we want zdc no bbc use
              //-** (bbcout->get_nPmt(0)+bbcout->get_nPmt(1))==0)
//              if ( (rawtrig&PPGlaser)!=PPGlaser )
		{
                  if ( pass==1 ) zdcenergy.FillFromPacket( p );
                }
	      
	      delete p;   // we are done with p and delete it again.
	      
	      // Fill only if there is a zdc packet
	      eventnum++;
	      zdctree->Fill();
	    }

	  delete evt;
	}

      delete it;
    }

  if ( pass==1 )
    {
      zdcenergy.CalculateConstants();
      zdcenergy.SaveToFile();

      zdctiming.CalculateConstants();
      zdctiming.SaveToFile();
    }

  //-*** end of pass 1

  cout << " processed " << eventnum << " events" << endl;
  cout << " there were " << nlaser << " laser events" << endl;

  savefile->Write();
  savefile->Close();

  globfree( &globbuf );	// free glob memory

  return 1;
}

