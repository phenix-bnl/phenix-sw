/*
 * dstqa.C
 * $Id: dstqa.C,v 1.39 2007/11/02 13:06:12 hpereira Exp $
 *
 * Ed Jackson <ejackson@iastate.edu>
 *
 * This is the main source file for the DST QA library.
 *
 * Updated by G.Mishra, H.Qu and X.He 04/13/2004
 * Updated by Cesar Luiz da Silva 2003/08/01
 * 
 */

#include <unistd.h>

// For ROOT classes used
#include "TObjString.h"
#include "TFile.h"
#include "TROOT.h"
#include "TKey.h"
#include "TObject.h"

// PHOOL headers
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"

// Local includes
#include <ctime>
#include "dstqa.h"
#include "qaDstReader.h"
#include "histFcl.h"
#include "histZdc.h"
#include "histBbc.h"
#include "histNtc.h"
#include "histNtcp.h"
#include "histTzr.h"
#include "histMvd.h"
#include "histDch.h"
#include "histPad.h"
#include "histTec.h"
#include "histTof.h"
#include "histEmc.h"
#include "histCrk.h"
#include "QAMut.h"
#include "QAMui.h"
#include "histErt.h"
#include "histElectron.h"

/*
 * We will not do QA now.  xhe  04-29-2004

#include "summaryQA.h"
#include "summaryTec.h"
#include "summaryTof.h"
#include "summaryZdc.h"
#include "summaryBbc.h"
#include "summaryDch.h"
#include "summaryMuid.h"
#include "summaryMutr.h"
#include "summaryEmc.h"
#include "summaryPad.h"
#include "summaryNtcp.h"
#include "summaryMvd.h"
#include "summaryFcl.h"
#include "summaryCrk.h"
#include "summaryElectron.h"

*/

using namespace std;

qaDstReader *in = NULL;

TString *optstr = NULL;
TString *outputName = NULL;

TFile *outfile = NULL;

int numevents = 0;

int runNumber = 0;
time_t begintime = 0;

//----------------------------------------------------------------------
/** 
		QAReco(char *QAoutputFile, char *option)

		option = 
		L -- Forward Calorimeter
		Z -- Zero Degree Calorimeter
		B -- Beam-Beam Counter
		N -- NTC Counter (pp dataset only)
		O -- NTCP Counter (run3, next to "N")
		S -- T-zero Counter (pp dataset only)
		V -- Multiplicity Vertex Detector (AuAu dataset only)
		D -- Drift Chamber
		P -- Pad Chamber
		T -- Time Expansion Chamber
		F -- Time of Flight Detector
		E -- Electromagnetic Calorimeter
		C -- Ring Imaging Cherenkov Counter
		M -- Muon Tracker
		U -- Muon Identifier
		R -- EMC-RICH (ERT) trigger
		electron -- EWG 
		min_bias -- loop only into Minimum Bias Events
		prdf -- Run QA from prdf file input
    
    Note that QA for subsystems other than the BBC rely on having BBC t0
    available, so if BBC information is missing or corrupt, the QA run will
    probably fail even if BBC QA is omitted.  This function is visible from the
    ROOT interpreter so it can be called before drun().
*/

//_____________________________________________________
QAReco::QAReco(): SubsysReco("QA")
{
  outputName = new TString("dstout.root");	//default
  optstr = new TString("ZBNOSVDPTFECMURelectron");  //default
}

//_____________________________________________________
QAReco::QAReco(const char *name): SubsysReco("QA")
{
  outputName = new TString(name);
  optstr = new TString("ZBNOSVDPTFECMURelectron");  //default
}

//_____________________________________________________
QAReco::QAReco(const char *name, const char *opt): SubsysReco("QA")
{
  outputName = new TString(name);
  optstr = new TString(opt);
}

//_____________________________________________________
int QAReco::Init(PHCompositeNode *n)
{
	in = new qaDstReader();
	
  //Open the output file.
	cout << "Writing QA histograms to " << outputName->Data() << endl;
  outfile = new TFile(outputName->Data(), "RECREATE");
  outfile->cd();

  //Book all histograms
  if (optstr->Contains("L")) fclHistBook();
  if (optstr->Contains("Z")) zdcHistBook();
  if (optstr->Contains("B")) bbcHistBook();
  if (optstr->Contains("N")) ntcHistBook();
  if (optstr->Contains("O")) ntcpHistBook();
  if (optstr->Contains("S")) tzrHistBook();
  if (optstr->Contains("V")) mvdHistBook();
  if (optstr->Contains("D")) dchHistBook();
  if (optstr->Contains("P")) padHistBook();
  if (optstr->Contains("T")) tecHistBook();
  if (optstr->Contains("F")) tofHistBook();
  if (optstr->Contains("E")) emcHistBook();
  if (optstr->Contains("C")) crkHistBook();
  if (optstr->Contains("M")) mutHistBook();
  if (optstr->Contains("U")) muiHistBook();
  if (optstr->Contains("R")) ertHistBook();
  if (optstr->Contains("electron")) elHistBook();
  
	return 0;
}

//_____________________________________________________
int QAReco::InitRun(PHCompositeNode *n)
{
  numevents = 0;
  TriggerHelper trighelper(n);
  if (trighelper.IsRunPP()) 
    {
      cout << "INFO  : Run is identified as pp" << endl << flush;
    }
  else if (trighelper.IsRunAuAu()) 
    {
      cout << "INFO  : Run is identified as AuAu" << endl << flush;
    }
  else
    {
      cout << "ERROR : Run is not identified as either AuAu or pp" << endl 
	   << flush;
    }

  PHNodeIterator *iter;
  iter = new PHNodeIterator(n);
  cout << "QA : input optstr = " << optstr->Data() << endl;
  in->update(iter,optstr);  // take out from optstr missed subsystems
  cout << "QA : working optstr = " << optstr->Data() << endl;

  runNumber = in->getRunHeader()->get_RunNumber();
  begintime = in->getRunHeader()->get_TimeStart();

  delete iter;
  return 0;
}

//_____________________________________________________
int QAReco::End(PHCompositeNode *n)
{

  TString sbegintime = TString(ctime(&begintime));
  sbegintime.ReplaceAll(" ","_");
  sbegintime.ReplaceAll(":","_");
  sbegintime.Resize(strlen(sbegintime)-1);
  
  if (optstr->Contains("M")) mutHistFinish(n);
  outfile->cd();
  outfile->Write();
  
  outfile->Close();

  /*
  
  QASummary *qasummary = new QASummary();
  char inputname[100];
  sprintf(inputname,"%s",outputName->Data());  // convert const char* to char*
  qasummary->setInputFileName(inputname);
  delete outputName;
  
  char summaryfilename[100];
  sprintf(summaryfilename,"summary_%d_at_%s.txt",runNumber,sbegintime.Data());
  char statusfilename[100];
  sprintf(statusfilename,"status_%d_at_%s.txt",runNumber, sbegintime.Data());
  
  qasummary->setOutputTextFileName(summaryfilename);
  qasummary->setOutputStatusFileName(statusfilename);
  qasummary->setRunNumber(runNumber);
  
  qasummary->Init();
  if (optstr->Contains("L")) qasummary->processFcl();
  if (optstr->Contains("Z")) qasummary->processZdc();
  if (optstr->Contains("B")) qasummary->processBbc();
  //  if (optstr->Contains("N")) qasummary->processNtc();
  if (optstr->Contains("O")) qasummary->processNtcp();
  //  if (optstr->Contains("S")) qasummary->processTzr();
  if (optstr->Contains("V")) qasummary->processMvd();
  if (optstr->Contains("D")) qasummary->processDch();
  if (optstr->Contains("P")) qasummary->processPad();
  if (optstr->Contains("T")) qasummary->processTec();
  if (optstr->Contains("F")) qasummary->processTof();
  if (optstr->Contains("E")) qasummary->processEmc();
  if (optstr->Contains("C")) qasummary->processCrk();
  if (optstr->Contains("M")) qasummary->processMut();
  if (optstr->Contains("U")) qasummary->processMui();
  //  if (optstr->Contains("R")) qasummary->processErt();
  if (optstr->Contains("electron")) qasummary->processElectron();  
  qasummary->End();

  */

  delete optstr;
  
  return 0;
}

//_____________________________________________________
int QAReco::process_event(PHCompositeNode *n)
{
  PHNodeIterator *iter;
  iter = new PHNodeIterator(n);
  
  // 1 -- create the trigger helper object 

	// Allow select only min bias to avoid mix triggers disturbing the
	// histograms distributions
  TriggerHelper trighelper(n);
  bool answer = trighelper.IsEventMinBias();
	if (!answer && optstr->Contains("min_bias")) return 0;
  in->updateTriggerHelper(&trighelper); // store trighelper pointer 'cause
  in->update(iter);                     // by the update() function.
  
  // 3 -- update the histograms
  if (optstr->Contains("L")) fclHistFill(in);
  if (optstr->Contains("Z")) zdcHistFill(in);
  if (optstr->Contains("B")) bbcHistFill(in);
  if (optstr->Contains("N")) ntcHistFill(in);
  if (optstr->Contains("O")) ntcpHistFill(in);
  if (optstr->Contains("S")) tzrHistFill(in);
  if (optstr->Contains("V")) mvdHistFill(in);
  if (optstr->Contains("D")) dchHistFill(in);
  if (optstr->Contains("P")) padHistFill(in);
  if (optstr->Contains("T")) tecHistFill(in);
  if (optstr->Contains("F")) tofHistFill(in);
  if (optstr->Contains("E")) emcHistFill(in);
  if (optstr->Contains("C")) crkHistFill(in);
  if (trighelper.IsRun4_AuAu_63GeV())
    {
    if (optstr->Contains("M")) mutHistFill(n);
    if (optstr->Contains("U")) muiHistFill(n);
    }
  else if (trighelper.IsRun4_AuAu_200GeV())
    {
    if (optstr->Contains("M")) mutHistFill(n);
    if (optstr->Contains("U")) muiHistFill(n);
    }
  else if (trighelper.IsRun4_PP())
    {
    if (optstr->Contains("M")) mutHistFill(n);
    if (optstr->Contains("U")) muiHistFill(n);
    }
 else {}
  if (optstr->Contains("R")) ertHistFill(in);
  if (optstr->Contains("electron")) elHistFill(in);  

  // 4 -- produce some output every 1000 events

  numevents++;
  if ( (numevents % 100) == 0 )
		{
			cout << "QA has been processed " << numevents << " events ..." << endl << flush;
		}
  
  // 5 -- delete all created objects to avoid a memory leak

  delete iter;
  
  // 6 -- that's all folks, return to invoker

  return 0;

} /* end of routine process_event() */

//----------------------------------------------------------------------
/*
 * This is just a hack right now until I figure out what the deal
 * is with TFileIter.
 *
 * Before this function is called, gROOT->SetMacroPath() should be 
 * called, so ROOT can find printall.C.
 */


// EOF




