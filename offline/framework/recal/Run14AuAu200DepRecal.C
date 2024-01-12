#include "Run14AuAu200DepRecal.h"

#include <Fun4AllReturnCodes.h>
#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>

#include <PHGlobal.h>

#include <PHCompositeNode.h>
#include <recoConsts.h>

#include <PHString.h>
#include <TH2.h>
#include <TMath.h>

#include <RunNumberRanges.h>

#include <iostream>

using namespace std;
using namespace findNode;

Run14AuAu200DepRecal::Run14AuAu200DepRecal(const string &name):
Recalibrator("Run14AuAu200DepRecal"),
runNumber(-9999),
d_cnt(NULL),
d_global(NULL)
{
  baseclasses.insert("PHCentralTrack");

  initialize_parameters();
}

int Run14AuAu200DepRecal::Init(PHCompositeNode *topNode)
{
  cout << "Run14AuAu200DepRecal::Init()" << endl;

  return 0;
}

int Run14AuAu200DepRecal::InitRun(PHCompositeNode *topNode)
{
  RunHeader *d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (!d_run)
  {
    cout << PHWHERE << " RunHeader not found" << endl;
    return 0;
  }
  
  runNumber = d_run->get_RunNumber();
  
  return 0;
}

int Run14AuAu200DepRecal::isValidRun(const int runno) const
{
  if(runno >= BEGIN_OF_RUN14AUAU200 && runno <= END_OF_RUN14AUAU200)
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

void Run14AuAu200DepRecal::help()
{
    cout << "===================================================================" << endl;
    cout << "            Run14AuAu200DepRecal::help method output               " << endl;
    cout << "          Author: Wenqing Fan (wenqing.fan@stonybrook.edu)         " << endl;
    cout << "     This recalibrator updates the Dep values for PHCentralTrack   " << endl;
    cout << "                 mom range is 0.2 < mom < 10 GeV/c                 " << endl;
    cout << " ( Need a new recalibraor for Dep since both energy recalibration  " << endl;
    cout << "  and momentum recalibration are updated for Run14 Au+Au dataset ) " << endl;
    cout << "===================================================================" << endl;
}

int Run14AuAu200DepRecal::process_event(PHCompositeNode *topNode)
{
  if(isValidRun(runNumber) == 0)
  {
    return 0;
  }

  static int evtno = 0;
  //if(evtno%1000==0) std::cout << "Run14AuAu200DepRecal::process_event() "<< evtno << std::endl;

  evtno++;

  //Get central tracks
  d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt || !d_global)
    {
      return 0;
    }

  Calibrate_Run14AuAu200GeV();
  
  return EVENT_OK;
}

int Run14AuAu200DepRecal::Calibrate_Run14AuAu200GeV()
{
  //Iterate over tracks
  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
  {
    PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
    sngltrk->ShutUp();

    if ( sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_dcarm()) &&
	  sngltrk->isImplemented(sngltrk->get_sect()) &&
	  sngltrk->isImplemented(sngltrk->get_ecore()) )
    {
	   if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
    }
    else
    {
	   sngltrk->ShutUp(1);
	   if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	   continue;
    }

    sngltrk->ShutUp(1); // enable virtual warnings again

    if (!isfinite(sngltrk->get_mom()))
    {
	   continue;
    }

    //Extract track parameters
    float mom  = -9999.;
    float ecore = -9999.;
    float eop = -9999.;
    float dep = -9999.;
    int dcarm = -9999.;
    int emcsect = -9999.;
    int sect = -9999.;

    mom  = sngltrk->get_mom();
    ecore = sngltrk->get_ecore();
    dcarm = (int) sngltrk->get_dcarm();
    emcsect = (int)sngltrk->get_sect();
    dcarm == 0 ? sect = 7 - emcsect : sect = emcsect; // convert to EMCal convention

    float px = sngltrk->get_px();
    float py = sngltrk->get_py();
    float pt = sqrt(px*px+py*py);

    if(ecore < 0 || mom < 0)
  	{
  	  continue;
  	}

    if(mom < 0.2 || mom > 10)
  	{
  	  continue;
  	}      

    eop = ecore/mom;

    if(sect<0 || sect>7)
  	{
  	  continue;
  	}

    //Standardize E/p
    dep = calculate_dep(sect,pt,eop);

    //Set the new variables
    sngltrk->set_dep(dep);
  }

  return EVENT_OK;
}

float Run14AuAu200DepRecal::meanFunc(float pt, int sect)
{
  if (sect<6)
  { // PbSc
    return meanPbSc_params[sect][0]*pow(1+exp((meanPbSc_params[sect][1]-pt)/meanPbSc_params[sect][2]),-1);
  }
  else
  {
    return (meanPbGl_params[sect-6][0]+meanPbGl_params[sect-6][1]*pt+meanPbGl_params[sect-6][2]*pt*pt)/(1+meanPbGl_params[sect-6][3]*pt+meanPbGl_params[sect-6][4]*pt*pt);
  }
}

float Run14AuAu200DepRecal::sigmaFunc(float pt, int sect)
{
  if (sect<6)
  { // PbSc
    return sigmaPbSc_params[sect][3]*sqrt((sigmaPbSc_params[sect][0]/sqrt(pt)*sigmaPbSc_params[sect][0]/sqrt(pt)) + sigmaPbSc_params[sect][1]*pt + sigmaPbSc_params[sect][2]*sigmaPbSc_params[sect][2]);
  }
  else
  {
    return (sigmaPbGl_params[sect-6][0]+sigmaPbGl_params[sect-6][1]*pt+sigmaPbGl_params[sect-6][2]*pt*pt)/(1+sigmaPbGl_params[sect-6][3]*pt+sigmaPbGl_params[sect-6][4]*pt*pt);
  }
}

float Run14AuAu200DepRecal::calculate_dep(int sector, float pt, float eop)
{ 
  float mean = -9999.;
  float sigma = -9999.;
 	  
  mean = meanFunc(pt,sector);
  sigma = sigmaFunc(pt,sector);

  if (mean<-999 || sigma<-999) return -9999.;
  else return (eop-mean)/sigma;
}

void Run14AuAu200DepRecal::initialize_parameters()
{
  meanPbSc_params[0][0] = 0.933852;
  meanPbSc_params[0][1] = -0.524203;
  meanPbSc_params[0][2] = 0.292352;

  meanPbSc_params[1][0] = 0.91943;
  meanPbSc_params[1][1] = -0.646051;
  meanPbSc_params[1][2] = 0.437654;

  meanPbSc_params[2][0] = 0.93053;
  meanPbSc_params[2][1] = -0.269929;
  meanPbSc_params[2][2] = 0.263158;

  meanPbSc_params[3][0] = 0.939889;
  meanPbSc_params[3][1] = -0.473038;
  meanPbSc_params[3][2] = 0.285999;

  meanPbSc_params[4][0] = 0.944391;
  meanPbSc_params[4][1] = -0.597766;
  meanPbSc_params[4][2] = 0.342229;

  meanPbSc_params[5][0] = 0.943378;
  meanPbSc_params[5][1] = -0.516728;
  meanPbSc_params[5][2] = 0.326514;

  meanPbGl_params[0][0] = 0.753861;
  meanPbGl_params[0][1] = 0.515105;
  meanPbGl_params[0][2] = 9.57388;
  meanPbGl_params[0][3] = 1.02742;
  meanPbGl_params[0][4] = 10.0386;

  meanPbGl_params[1][0] = -0.0414852;
  meanPbGl_params[1][1] = 21.9119;
  meanPbGl_params[1][2] = 14.1471;
  meanPbGl_params[1][3] = 24.0883;
  meanPbGl_params[1][4] = 14.7064;

  sigmaPbSc_params[0][0] = 0.0830352;
  sigmaPbSc_params[0][1] = 0.000909991;
  sigmaPbSc_params[0][2] = 0.0410683;
  sigmaPbSc_params[0][3] = 0.900891;

  sigmaPbSc_params[1][0] = 0.0899572;
  sigmaPbSc_params[1][1] = 0.0010949;
  sigmaPbSc_params[1][2] = 0.0340413;
  sigmaPbSc_params[1][3] = 0.929485;

  sigmaPbSc_params[2][0] = 0.0863989;
  sigmaPbSc_params[2][1] = 0.00134776;
  sigmaPbSc_params[2][2] = 0.0303385;
  sigmaPbSc_params[2][3] = 0.910618;

  sigmaPbSc_params[3][0] = 0.0861372;
  sigmaPbSc_params[3][1] = 0.00129565;
  sigmaPbSc_params[3][2] = 0.0193427;
  sigmaPbSc_params[3][3] = 0.903529;

  sigmaPbSc_params[4][0] = 0.085615;
  sigmaPbSc_params[4][1] = 0.00105683;
  sigmaPbSc_params[4][2] = 0.017538;
  sigmaPbSc_params[4][3] = 0.896315;

  sigmaPbSc_params[5][0] = 0.0846969;
  sigmaPbSc_params[5][1] = 0.000789427;
  sigmaPbSc_params[5][2] = 0.0400465;
  sigmaPbSc_params[5][3] = 0.89107;

  sigmaPbGl_params[0][0] = 0.147539;
  sigmaPbGl_params[0][1] = -0.121329;
  sigmaPbGl_params[0][2] = 0.288375;
  sigmaPbGl_params[0][3] = -1.1896;
  sigmaPbGl_params[0][4] = 3.68693;

  sigmaPbGl_params[1][0] = 0.174377;
  sigmaPbGl_params[1][1] = -0.135625;
  sigmaPbGl_params[1][2] = 0.224303;
  sigmaPbGl_params[1][3] = -0.457599;
  sigmaPbGl_params[1][4] = 2.39171;
}
