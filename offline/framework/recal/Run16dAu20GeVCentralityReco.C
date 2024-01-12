#include "Run16dAu20GeVCentralityReco.h"

#include <PHGlobal.h>
#include <RunHeader.h>
#include <PHPoint.h>
#include <VtxOut.h>


#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <PHCompositeNode.h>

#include <iostream>


using namespace std;

enum  QAstatus_types {QAstatus_PASS = 0, QAstatus_FAIL = 1, QAstatus_NOCHECK = 2};
static const int MINRUN = 456652; // first Run-16 d+Au @ 20 GeV physics run, May 31, 2016
static const int MAXRUN = 457298; // ended on June 08, 2016

Run16dAu20GeVCentralityReco::Run16dAu20GeVCentralityReco() : Recalibrator("Run16dAu20GeVCentralityReco")
{
  memset(bbcCutZDep, 0, sizeof(bbcCutZDep));
  memset(BBCScaleFactor, 0, sizeof(BBCScaleFactor));
  fill(QAstatus, QAstatus + sizeof(QAstatus) / sizeof(int), QAstatus_PASS);
  baseclasses.insert("PHGlobal");
  return ;
}

int
Run16dAu20GeVCentralityReco::isValidRun(const int runno) const
{
  if (runno < MINRUN || runno > MAXRUN)
    {
      return 0;
    }

  return 1;
}

int Run16dAu20GeVCentralityReco::Init(PHCompositeNode *topNode)
{

  help(); // just for test !!!
  InitArray1();
  InitScaleFactor();
  return 0;

}

void
Run16dAu20GeVCentralityReco::help() const
{
  cout << "===================================================================" << endl;
  cout << "Run16dAu20GeVCentralityReco::help method output"                    << endl;
  cout << "Author: J.L. Nagle (jamie.nagle@colorado.edu)." << endl;
  cout << "Comment: Run-16 dAu@20 GeV Centrality Using BBC Only uses 61% "             << endl;
  cout << "           of the inelastic cross section."                          << endl;
  cout << "         This method updates the value in PHGlobal for"              << endl;
  cout << "           BbcPercentile to go from 1 to 61."                        << endl;
  cout << "         This is applicable for runs " << MINRUN << " - " << MAXRUN                << endl;
  cout << "         NOTE: First pass version only, no run-to-run QA / Scaling"                << endl;
  cout << "         NOTE: The calibration has been performed in 1 cm wide z-vertex bins" << endl;
  cout << ": Run16 dAu 20 GeV with minimum bias defined by FVTX_NS Level-1, an offline FVTX z-vertex and BBC Charge South > 0" << endl;
  cout << "  The calibration is based upon the BBC south charge distribution." << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int
Run16dAu20GeVCentralityReco::process_event(PHCompositeNode *topNode)
{

  int intcentral = getCentralityByBBCRun16dAu(topNode);
  if (intcentral < 0)
    {
      if ( Verbosity() > 0 )
	cout << PHWHERE << " centrality < 0" << endl;
      return EVENT_OK;
    }

  PHGlobal* global = findNode::getClass<PHGlobal>(topNode, inputnodename.c_str());
  global->setCentrality((float)intcentral);

  return EVENT_OK;

}

int
Run16dAu20GeVCentralityReco::getCentralityByBBCRun16dAu(PHCompositeNode* topNode) const
{
  PHGlobal* d_global = findNode::getClass<PHGlobal>(topNode, inputnodename.c_str());
  RunHeader *run = findNode::getClass<RunHeader>(topNode, "RunHeader");

  // Need the FVTX z-vertex 
  VtxOut *vertexes = findNode::getClass<VtxOut>(topNode, "VtxOut");

  if (!d_global || !run || !vertexes) return -999;

  float bbc1 = d_global->getBbcChargeN();
  float bbc2 = d_global->getBbcChargeS();
  float zdc1 = d_global->getZdcEnergyN();
  float zdc2 = d_global->getZdcEnergyS();

  float zvertex = d_global->getBbcZVertex();

  // look for FVTX z-vertex first...
  PHPoint fvtx_vertex = vertexes->get_Vertex("FVTX");
  float  FVTXz = fvtx_vertex.getZ();
  if(FVTXz!=FVTXz) {
    FVTXz=-9999;
  } else {
    zvertex = FVTXz;  // use FVTXz if valid, otherwise keep BBCz
  }

  int   runno   = run->get_RunNumber();

  // For d+Au @ 20 GeV, calibration is only for +/- 10 cm...
  // require a non-zero BBC charge south and FVTX zvertex within +/-10 cm

  // in d+Au @ 20 GeV, we also want to return a centrality value even if the event
  // did not fire the FVTX trigger (for studies)...
  // thus we could also use the BBC zvertex if the FVTX zvertex does not exist...

  bool isEventOK = fabs(zvertex) < 10; // +- 10 (cm)
  if (! (bbc2>0)) isEventOK = false;  // require non-zero charge in BBC south for min.bias definition!!!

  if(!isEventOK) return -999;

  return getCentralityByBBCRun16dAu(bbc1, bbc2, zdc1, zdc2, zvertex, runno);

}

int
Run16dAu20GeVCentralityReco::getCentralityByBBCRun16dAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const
{

  // Returned value of 1 means that this event is from 0% to 1% most central,
  // Returned value of 50 means that this event is from 49% to 50% most central, etc.
  //
  //-------------------------------------------------------------------------
  //-------------------------------------------------------------------------

  // NOTE SPECIAL CASE WITH ONLY CALIBRATION FOR +/- 10 CM.. (NOT THE USUAL +/- 30 CM)
  // only include the range -10 : +10 cm
  int zbin = (int) ((zvertex + 10.0) / 1.0);
  if (zvertex < -10.0 || zvertex > +10.0) return -999;
  if (zbin < 0 || zbin > 19) return -999;

  float bbcsum = bbc2;  // Only use the BBC south charge

  // apply a run-to-run scale factor correction to the bbcsum (south)
  float scalefactor = GetScaleFactor(runno);
  if (scalefactor <= 0.0)
    {
      cout << "Run16dAu20GeVCentralityReco::getCentralityByBBCRun16dAu - ERROR with run " <<
	runno << " and retrieved scale factor " << scalefactor << endl;
      return -999;
    }
  bbcsum = bbcsum / scalefactor;

  int centrality = -1;
  // use lookup table to determine centrality of this event using clock method
  for (int jjj = 0; jjj < 61; jjj++)
    {
      if (bbcsum < bbcCutZDep[jjj][zbin] && bbcsum >= bbcCutZDep[jjj + 1][zbin])
	{
	  centrality = jjj + 1;
	  break;
	}
    }

  return centrality;

}

void
Run16dAu20GeVCentralityReco::InitArray1()
{

  // Lookup Table (BBC South Charge Cut Values) for Run16dAu20-Centrality
  // (-10<bbcz<10 cm, 1 cm step)
  // table generated by J.L. Nagle - first trial table (not yet final)
  // generated from all runs on 6/17/2016

  // --> J.NAGLE - INSERT HERE....

  // June 16, 2016 - pass on d+Au @ 20 GeV using 81 of 84 runs (online production)
  // June 20, 2016 - updated with restricted range Scale Factors and only on good run list


  // October 19, 2016 - use all runs and quote some bad run group - no real difference

bbcCutZDep[ 0][ 0] = 999.983337;
bbcCutZDep[ 1][ 0] = 53.483334;
bbcCutZDep[ 2][ 0] = 47.283333;
bbcCutZDep[ 3][ 0] = 43.650002;
bbcCutZDep[ 4][ 0] = 41.016666;
bbcCutZDep[ 5][ 0] = 38.950001;
bbcCutZDep[ 6][ 0] = 37.216667;
bbcCutZDep[ 7][ 0] = 35.683334;
bbcCutZDep[ 8][ 0] = 34.349998;
bbcCutZDep[ 9][ 0] = 33.150002;
bbcCutZDep[10][ 0] = 32.016666;
bbcCutZDep[11][ 0] = 31.016666;
bbcCutZDep[12][ 0] = 30.049999;
bbcCutZDep[13][ 0] = 29.150000;
bbcCutZDep[14][ 0] = 28.283333;
bbcCutZDep[15][ 0] = 27.483334;
bbcCutZDep[16][ 0] = 26.716667;
bbcCutZDep[17][ 0] = 25.983334;
bbcCutZDep[18][ 0] = 25.250000;
bbcCutZDep[19][ 0] = 24.583334;
bbcCutZDep[20][ 0] = 23.916666;
bbcCutZDep[21][ 0] = 23.250000;
bbcCutZDep[22][ 0] = 22.616667;
bbcCutZDep[23][ 0] = 22.016666;
bbcCutZDep[24][ 0] = 21.416666;
bbcCutZDep[25][ 0] = 20.816668;
bbcCutZDep[26][ 0] = 20.250000;
bbcCutZDep[27][ 0] = 19.683332;
bbcCutZDep[28][ 0] = 19.116667;
bbcCutZDep[29][ 0] = 18.583334;
bbcCutZDep[30][ 0] = 18.049999;
bbcCutZDep[31][ 0] = 17.483334;
bbcCutZDep[32][ 0] = 16.983334;
bbcCutZDep[33][ 0] = 16.450001;
bbcCutZDep[34][ 0] = 15.916667;
bbcCutZDep[35][ 0] = 15.416667;
bbcCutZDep[36][ 0] = 14.883333;
bbcCutZDep[37][ 0] = 14.383333;
bbcCutZDep[38][ 0] = 13.883333;
bbcCutZDep[39][ 0] = 13.350000;
bbcCutZDep[40][ 0] = 12.850000;
bbcCutZDep[41][ 0] = 12.350000;
bbcCutZDep[42][ 0] = 11.850000;
bbcCutZDep[43][ 0] = 11.316667;
bbcCutZDep[44][ 0] = 10.816667;
bbcCutZDep[45][ 0] = 10.316667;
bbcCutZDep[46][ 0] = 9.816667;
bbcCutZDep[47][ 0] = 9.283334;
bbcCutZDep[48][ 0] = 8.783334;
bbcCutZDep[49][ 0] = 8.250000;
bbcCutZDep[50][ 0] = 7.716667;
bbcCutZDep[51][ 0] = 7.183333;
bbcCutZDep[52][ 0] = 6.650000;
bbcCutZDep[53][ 0] = 6.116667;
bbcCutZDep[54][ 0] = 5.516667;
bbcCutZDep[55][ 0] = 4.950000;
bbcCutZDep[56][ 0] = 4.350000;
bbcCutZDep[57][ 0] = 3.750000;
bbcCutZDep[58][ 0] = 3.116667;
bbcCutZDep[59][ 0] = 2.450000;
bbcCutZDep[60][ 0] = 1.750000;
bbcCutZDep[61][ 0] = -0.500000;
bbcCutZDep[ 0][ 1] = 999.983337;
bbcCutZDep[ 1][ 1] = 53.549999;
bbcCutZDep[ 2][ 1] = 47.316666;
bbcCutZDep[ 3][ 1] = 43.716667;
bbcCutZDep[ 4][ 1] = 41.116665;
bbcCutZDep[ 5][ 1] = 39.016666;
bbcCutZDep[ 6][ 1] = 37.283333;
bbcCutZDep[ 7][ 1] = 35.750000;
bbcCutZDep[ 8][ 1] = 34.416668;
bbcCutZDep[ 9][ 1] = 33.216667;
bbcCutZDep[10][ 1] = 32.083332;
bbcCutZDep[11][ 1] = 31.049999;
bbcCutZDep[12][ 1] = 30.116667;
bbcCutZDep[13][ 1] = 29.216667;
bbcCutZDep[14][ 1] = 28.350000;
bbcCutZDep[15][ 1] = 27.549999;
bbcCutZDep[16][ 1] = 26.783333;
bbcCutZDep[17][ 1] = 26.016666;
bbcCutZDep[18][ 1] = 25.316668;
bbcCutZDep[19][ 1] = 24.616667;
bbcCutZDep[20][ 1] = 23.950001;
bbcCutZDep[21][ 1] = 23.283333;
bbcCutZDep[22][ 1] = 22.650000;
bbcCutZDep[23][ 1] = 22.049999;
bbcCutZDep[24][ 1] = 21.450001;
bbcCutZDep[25][ 1] = 20.850000;
bbcCutZDep[26][ 1] = 20.250000;
bbcCutZDep[27][ 1] = 19.683332;
bbcCutZDep[28][ 1] = 19.116667;
bbcCutZDep[29][ 1] = 18.583334;
bbcCutZDep[30][ 1] = 18.016666;
bbcCutZDep[31][ 1] = 17.483334;
bbcCutZDep[32][ 1] = 16.950001;
bbcCutZDep[33][ 1] = 16.416666;
bbcCutZDep[34][ 1] = 15.916667;
bbcCutZDep[35][ 1] = 15.383333;
bbcCutZDep[36][ 1] = 14.883333;
bbcCutZDep[37][ 1] = 14.350000;
bbcCutZDep[38][ 1] = 13.850000;
bbcCutZDep[39][ 1] = 13.350000;
bbcCutZDep[40][ 1] = 12.850000;
bbcCutZDep[41][ 1] = 12.316667;
bbcCutZDep[42][ 1] = 11.816667;
bbcCutZDep[43][ 1] = 11.316667;
bbcCutZDep[44][ 1] = 10.783334;
bbcCutZDep[45][ 1] = 10.283334;
bbcCutZDep[46][ 1] = 9.783334;
bbcCutZDep[47][ 1] = 9.250000;
bbcCutZDep[48][ 1] = 8.750000;
bbcCutZDep[49][ 1] = 8.216666;
bbcCutZDep[50][ 1] = 7.683333;
bbcCutZDep[51][ 1] = 7.150000;
bbcCutZDep[52][ 1] = 6.616667;
bbcCutZDep[53][ 1] = 6.050000;
bbcCutZDep[54][ 1] = 5.483333;
bbcCutZDep[55][ 1] = 4.916667;
bbcCutZDep[56][ 1] = 4.350000;
bbcCutZDep[57][ 1] = 3.750000;
bbcCutZDep[58][ 1] = 3.083333;
bbcCutZDep[59][ 1] = 2.416667;
bbcCutZDep[60][ 1] = 1.716667;
bbcCutZDep[61][ 1] = -0.500000;
bbcCutZDep[ 0][ 2] = 999.983337;
bbcCutZDep[ 1][ 2] = 53.616665;
bbcCutZDep[ 2][ 2] = 47.383335;
bbcCutZDep[ 3][ 2] = 43.783333;
bbcCutZDep[ 4][ 2] = 41.183334;
bbcCutZDep[ 5][ 2] = 39.083332;
bbcCutZDep[ 6][ 2] = 37.349998;
bbcCutZDep[ 7][ 2] = 35.849998;
bbcCutZDep[ 8][ 2] = 34.483334;
bbcCutZDep[ 9][ 2] = 33.283333;
bbcCutZDep[10][ 2] = 32.183334;
bbcCutZDep[11][ 2] = 31.150000;
bbcCutZDep[12][ 2] = 30.183332;
bbcCutZDep[13][ 2] = 29.283333;
bbcCutZDep[14][ 2] = 28.416666;
bbcCutZDep[15][ 2] = 27.616667;
bbcCutZDep[16][ 2] = 26.816668;
bbcCutZDep[17][ 2] = 26.083334;
bbcCutZDep[18][ 2] = 25.350000;
bbcCutZDep[19][ 2] = 24.650000;
bbcCutZDep[20][ 2] = 23.983334;
bbcCutZDep[21][ 2] = 23.316668;
bbcCutZDep[22][ 2] = 22.683332;
bbcCutZDep[23][ 2] = 22.083334;
bbcCutZDep[24][ 2] = 21.483334;
bbcCutZDep[25][ 2] = 20.883333;
bbcCutZDep[26][ 2] = 20.283333;
bbcCutZDep[27][ 2] = 19.716667;
bbcCutZDep[28][ 2] = 19.150000;
bbcCutZDep[29][ 2] = 18.583334;
bbcCutZDep[30][ 2] = 18.049999;
bbcCutZDep[31][ 2] = 17.516666;
bbcCutZDep[32][ 2] = 16.950001;
bbcCutZDep[33][ 2] = 16.450001;
bbcCutZDep[34][ 2] = 15.916667;
bbcCutZDep[35][ 2] = 15.383333;
bbcCutZDep[36][ 2] = 14.850000;
bbcCutZDep[37][ 2] = 14.350000;
bbcCutZDep[38][ 2] = 13.816667;
bbcCutZDep[39][ 2] = 13.316667;
bbcCutZDep[40][ 2] = 12.816667;
bbcCutZDep[41][ 2] = 12.283334;
bbcCutZDep[42][ 2] = 11.783334;
bbcCutZDep[43][ 2] = 11.283334;
bbcCutZDep[44][ 2] = 10.783334;
bbcCutZDep[45][ 2] = 10.250000;
bbcCutZDep[46][ 2] = 9.750000;
bbcCutZDep[47][ 2] = 9.250000;
bbcCutZDep[48][ 2] = 8.716666;
bbcCutZDep[49][ 2] = 8.216666;
bbcCutZDep[50][ 2] = 7.683333;
bbcCutZDep[51][ 2] = 7.150000;
bbcCutZDep[52][ 2] = 6.583333;
bbcCutZDep[53][ 2] = 6.050000;
bbcCutZDep[54][ 2] = 5.483333;
bbcCutZDep[55][ 2] = 4.916667;
bbcCutZDep[56][ 2] = 4.316667;
bbcCutZDep[57][ 2] = 3.716667;
bbcCutZDep[58][ 2] = 3.083333;
bbcCutZDep[59][ 2] = 2.416667;
bbcCutZDep[60][ 2] = 1.716667;
bbcCutZDep[61][ 2] = -0.500000;
bbcCutZDep[ 0][ 3] = 999.983337;
bbcCutZDep[ 1][ 3] = 53.716667;
bbcCutZDep[ 2][ 3] = 47.549999;
bbcCutZDep[ 3][ 3] = 43.950001;
bbcCutZDep[ 4][ 3] = 41.316666;
bbcCutZDep[ 5][ 3] = 39.250000;
bbcCutZDep[ 6][ 3] = 37.516666;
bbcCutZDep[ 7][ 3] = 35.983334;
bbcCutZDep[ 8][ 3] = 34.650002;
bbcCutZDep[ 9][ 3] = 33.416668;
bbcCutZDep[10][ 3] = 32.316666;
bbcCutZDep[11][ 3] = 31.283333;
bbcCutZDep[12][ 3] = 30.316668;
bbcCutZDep[13][ 3] = 29.383333;
bbcCutZDep[14][ 3] = 28.516666;
bbcCutZDep[15][ 3] = 27.716667;
bbcCutZDep[16][ 3] = 26.916666;
bbcCutZDep[17][ 3] = 26.183332;
bbcCutZDep[18][ 3] = 25.450001;
bbcCutZDep[19][ 3] = 24.750000;
bbcCutZDep[20][ 3] = 24.083334;
bbcCutZDep[21][ 3] = 23.416666;
bbcCutZDep[22][ 3] = 22.783333;
bbcCutZDep[23][ 3] = 22.150000;
bbcCutZDep[24][ 3] = 21.516666;
bbcCutZDep[25][ 3] = 20.916666;
bbcCutZDep[26][ 3] = 20.350000;
bbcCutZDep[27][ 3] = 19.750000;
bbcCutZDep[28][ 3] = 19.183332;
bbcCutZDep[29][ 3] = 18.650000;
bbcCutZDep[30][ 3] = 18.083334;
bbcCutZDep[31][ 3] = 17.549999;
bbcCutZDep[32][ 3] = 17.016666;
bbcCutZDep[33][ 3] = 16.483334;
bbcCutZDep[34][ 3] = 15.950000;
bbcCutZDep[35][ 3] = 15.416667;
bbcCutZDep[36][ 3] = 14.883333;
bbcCutZDep[37][ 3] = 14.350000;
bbcCutZDep[38][ 3] = 13.850000;
bbcCutZDep[39][ 3] = 13.316667;
bbcCutZDep[40][ 3] = 12.816667;
bbcCutZDep[41][ 3] = 12.316667;
bbcCutZDep[42][ 3] = 11.783334;
bbcCutZDep[43][ 3] = 11.283334;
bbcCutZDep[44][ 3] = 10.783334;
bbcCutZDep[45][ 3] = 10.250000;
bbcCutZDep[46][ 3] = 9.750000;
bbcCutZDep[47][ 3] = 9.216666;
bbcCutZDep[48][ 3] = 8.716666;
bbcCutZDep[49][ 3] = 8.183333;
bbcCutZDep[50][ 3] = 7.650000;
bbcCutZDep[51][ 3] = 7.116667;
bbcCutZDep[52][ 3] = 6.583333;
bbcCutZDep[53][ 3] = 6.016667;
bbcCutZDep[54][ 3] = 5.483333;
bbcCutZDep[55][ 3] = 4.916667;
bbcCutZDep[56][ 3] = 4.316667;
bbcCutZDep[57][ 3] = 3.716667;
bbcCutZDep[58][ 3] = 3.083333;
bbcCutZDep[59][ 3] = 2.416667;
bbcCutZDep[60][ 3] = 1.716667;
bbcCutZDep[61][ 3] = -0.500000;
bbcCutZDep[ 0][ 4] = 999.983337;
bbcCutZDep[ 1][ 4] = 53.750000;
bbcCutZDep[ 2][ 4] = 47.650002;
bbcCutZDep[ 3][ 4] = 44.016666;
bbcCutZDep[ 4][ 4] = 41.383335;
bbcCutZDep[ 5][ 4] = 39.283333;
bbcCutZDep[ 6][ 4] = 37.549999;
bbcCutZDep[ 7][ 4] = 36.016666;
bbcCutZDep[ 8][ 4] = 34.683334;
bbcCutZDep[ 9][ 4] = 33.483334;
bbcCutZDep[10][ 4] = 32.349998;
bbcCutZDep[11][ 4] = 31.316668;
bbcCutZDep[12][ 4] = 30.350000;
bbcCutZDep[13][ 4] = 29.450001;
bbcCutZDep[14][ 4] = 28.583334;
bbcCutZDep[15][ 4] = 27.750000;
bbcCutZDep[16][ 4] = 26.950001;
bbcCutZDep[17][ 4] = 26.216667;
bbcCutZDep[18][ 4] = 25.483334;
bbcCutZDep[19][ 4] = 24.783333;
bbcCutZDep[20][ 4] = 24.116667;
bbcCutZDep[21][ 4] = 23.450001;
bbcCutZDep[22][ 4] = 22.816668;
bbcCutZDep[23][ 4] = 22.183332;
bbcCutZDep[24][ 4] = 21.583334;
bbcCutZDep[25][ 4] = 20.983334;
bbcCutZDep[26][ 4] = 20.383333;
bbcCutZDep[27][ 4] = 19.816668;
bbcCutZDep[28][ 4] = 19.250000;
bbcCutZDep[29][ 4] = 18.683332;
bbcCutZDep[30][ 4] = 18.150000;
bbcCutZDep[31][ 4] = 17.583334;
bbcCutZDep[32][ 4] = 17.049999;
bbcCutZDep[33][ 4] = 16.516666;
bbcCutZDep[34][ 4] = 15.983334;
bbcCutZDep[35][ 4] = 15.450000;
bbcCutZDep[36][ 4] = 14.916667;
bbcCutZDep[37][ 4] = 14.416667;
bbcCutZDep[38][ 4] = 13.883333;
bbcCutZDep[39][ 4] = 13.350000;
bbcCutZDep[40][ 4] = 12.850000;
bbcCutZDep[41][ 4] = 12.350000;
bbcCutZDep[42][ 4] = 11.816667;
bbcCutZDep[43][ 4] = 11.316667;
bbcCutZDep[44][ 4] = 10.816667;
bbcCutZDep[45][ 4] = 10.283334;
bbcCutZDep[46][ 4] = 9.750000;
bbcCutZDep[47][ 4] = 9.250000;
bbcCutZDep[48][ 4] = 8.716666;
bbcCutZDep[49][ 4] = 8.183333;
bbcCutZDep[50][ 4] = 7.683333;
bbcCutZDep[51][ 4] = 7.150000;
bbcCutZDep[52][ 4] = 6.583333;
bbcCutZDep[53][ 4] = 6.050000;
bbcCutZDep[54][ 4] = 5.483333;
bbcCutZDep[55][ 4] = 4.916667;
bbcCutZDep[56][ 4] = 4.316667;
bbcCutZDep[57][ 4] = 3.716667;
bbcCutZDep[58][ 4] = 3.083333;
bbcCutZDep[59][ 4] = 2.416667;
bbcCutZDep[60][ 4] = 1.716667;
bbcCutZDep[61][ 4] = -0.500000;
bbcCutZDep[ 0][ 5] = 999.983337;
bbcCutZDep[ 1][ 5] = 53.816666;
bbcCutZDep[ 2][ 5] = 47.716667;
bbcCutZDep[ 3][ 5] = 44.049999;
bbcCutZDep[ 4][ 5] = 41.450001;
bbcCutZDep[ 5][ 5] = 39.383335;
bbcCutZDep[ 6][ 5] = 37.650002;
bbcCutZDep[ 7][ 5] = 36.116665;
bbcCutZDep[ 8][ 5] = 34.783333;
bbcCutZDep[ 9][ 5] = 33.549999;
bbcCutZDep[10][ 5] = 32.416668;
bbcCutZDep[11][ 5] = 31.383333;
bbcCutZDep[12][ 5] = 30.416666;
bbcCutZDep[13][ 5] = 29.516666;
bbcCutZDep[14][ 5] = 28.650000;
bbcCutZDep[15][ 5] = 27.816668;
bbcCutZDep[16][ 5] = 27.016666;
bbcCutZDep[17][ 5] = 26.283333;
bbcCutZDep[18][ 5] = 25.549999;
bbcCutZDep[19][ 5] = 24.850000;
bbcCutZDep[20][ 5] = 24.150000;
bbcCutZDep[21][ 5] = 23.483334;
bbcCutZDep[22][ 5] = 22.850000;
bbcCutZDep[23][ 5] = 22.216667;
bbcCutZDep[24][ 5] = 21.616667;
bbcCutZDep[25][ 5] = 21.016666;
bbcCutZDep[26][ 5] = 20.416666;
bbcCutZDep[27][ 5] = 19.816668;
bbcCutZDep[28][ 5] = 19.250000;
bbcCutZDep[29][ 5] = 18.683332;
bbcCutZDep[30][ 5] = 18.150000;
bbcCutZDep[31][ 5] = 17.583334;
bbcCutZDep[32][ 5] = 17.049999;
bbcCutZDep[33][ 5] = 16.516666;
bbcCutZDep[34][ 5] = 15.983334;
bbcCutZDep[35][ 5] = 15.450000;
bbcCutZDep[36][ 5] = 14.950000;
bbcCutZDep[37][ 5] = 14.416667;
bbcCutZDep[38][ 5] = 13.883333;
bbcCutZDep[39][ 5] = 13.383333;
bbcCutZDep[40][ 5] = 12.850000;
bbcCutZDep[41][ 5] = 12.350000;
bbcCutZDep[42][ 5] = 11.850000;
bbcCutZDep[43][ 5] = 11.316667;
bbcCutZDep[44][ 5] = 10.816667;
bbcCutZDep[45][ 5] = 10.316667;
bbcCutZDep[46][ 5] = 9.783334;
bbcCutZDep[47][ 5] = 9.250000;
bbcCutZDep[48][ 5] = 8.750000;
bbcCutZDep[49][ 5] = 8.216666;
bbcCutZDep[50][ 5] = 7.683333;
bbcCutZDep[51][ 5] = 7.150000;
bbcCutZDep[52][ 5] = 6.583333;
bbcCutZDep[53][ 5] = 6.050000;
bbcCutZDep[54][ 5] = 5.483333;
bbcCutZDep[55][ 5] = 4.916667;
bbcCutZDep[56][ 5] = 4.316667;
bbcCutZDep[57][ 5] = 3.716667;
bbcCutZDep[58][ 5] = 3.083333;
bbcCutZDep[59][ 5] = 2.416667;
bbcCutZDep[60][ 5] = 1.716667;
bbcCutZDep[61][ 5] = -0.500000;
bbcCutZDep[ 0][ 6] = 999.983337;
bbcCutZDep[ 1][ 6] = 53.916668;
bbcCutZDep[ 2][ 6] = 47.783333;
bbcCutZDep[ 3][ 6] = 44.183334;
bbcCutZDep[ 4][ 6] = 41.549999;
bbcCutZDep[ 5][ 6] = 39.450001;
bbcCutZDep[ 6][ 6] = 37.716667;
bbcCutZDep[ 7][ 6] = 36.216667;
bbcCutZDep[ 8][ 6] = 34.849998;
bbcCutZDep[ 9][ 6] = 33.650002;
bbcCutZDep[10][ 6] = 32.516666;
bbcCutZDep[11][ 6] = 31.483334;
bbcCutZDep[12][ 6] = 30.516666;
bbcCutZDep[13][ 6] = 29.583334;
bbcCutZDep[14][ 6] = 28.716667;
bbcCutZDep[15][ 6] = 27.883333;
bbcCutZDep[16][ 6] = 27.116667;
bbcCutZDep[17][ 6] = 26.350000;
bbcCutZDep[18][ 6] = 25.616667;
bbcCutZDep[19][ 6] = 24.883333;
bbcCutZDep[20][ 6] = 24.216667;
bbcCutZDep[21][ 6] = 23.549999;
bbcCutZDep[22][ 6] = 22.916666;
bbcCutZDep[23][ 6] = 22.283333;
bbcCutZDep[24][ 6] = 21.683332;
bbcCutZDep[25][ 6] = 21.049999;
bbcCutZDep[26][ 6] = 20.483334;
bbcCutZDep[27][ 6] = 19.883333;
bbcCutZDep[28][ 6] = 19.316668;
bbcCutZDep[29][ 6] = 18.750000;
bbcCutZDep[30][ 6] = 18.183332;
bbcCutZDep[31][ 6] = 17.650000;
bbcCutZDep[32][ 6] = 17.083334;
bbcCutZDep[33][ 6] = 16.549999;
bbcCutZDep[34][ 6] = 16.016666;
bbcCutZDep[35][ 6] = 15.483334;
bbcCutZDep[36][ 6] = 14.950000;
bbcCutZDep[37][ 6] = 14.450000;
bbcCutZDep[38][ 6] = 13.916667;
bbcCutZDep[39][ 6] = 13.416667;
bbcCutZDep[40][ 6] = 12.883333;
bbcCutZDep[41][ 6] = 12.383333;
bbcCutZDep[42][ 6] = 11.850000;
bbcCutZDep[43][ 6] = 11.350000;
bbcCutZDep[44][ 6] = 10.816667;
bbcCutZDep[45][ 6] = 10.316667;
bbcCutZDep[46][ 6] = 9.783334;
bbcCutZDep[47][ 6] = 9.283334;
bbcCutZDep[48][ 6] = 8.750000;
bbcCutZDep[49][ 6] = 8.216666;
bbcCutZDep[50][ 6] = 7.683333;
bbcCutZDep[51][ 6] = 7.150000;
bbcCutZDep[52][ 6] = 6.616667;
bbcCutZDep[53][ 6] = 6.050000;
bbcCutZDep[54][ 6] = 5.516667;
bbcCutZDep[55][ 6] = 4.916667;
bbcCutZDep[56][ 6] = 4.350000;
bbcCutZDep[57][ 6] = 3.716667;
bbcCutZDep[58][ 6] = 3.083333;
bbcCutZDep[59][ 6] = 2.416667;
bbcCutZDep[60][ 6] = 1.716667;
bbcCutZDep[61][ 6] = -0.500000;
bbcCutZDep[ 0][ 7] = 999.983337;
bbcCutZDep[ 1][ 7] = 53.716667;
bbcCutZDep[ 2][ 7] = 47.750000;
bbcCutZDep[ 3][ 7] = 44.116665;
bbcCutZDep[ 4][ 7] = 41.516666;
bbcCutZDep[ 5][ 7] = 39.450001;
bbcCutZDep[ 6][ 7] = 37.683334;
bbcCutZDep[ 7][ 7] = 36.150002;
bbcCutZDep[ 8][ 7] = 34.816666;
bbcCutZDep[ 9][ 7] = 33.583332;
bbcCutZDep[10][ 7] = 32.450001;
bbcCutZDep[11][ 7] = 31.416666;
bbcCutZDep[12][ 7] = 30.450001;
bbcCutZDep[13][ 7] = 29.516666;
bbcCutZDep[14][ 7] = 28.650000;
bbcCutZDep[15][ 7] = 27.850000;
bbcCutZDep[16][ 7] = 27.049999;
bbcCutZDep[17][ 7] = 26.283333;
bbcCutZDep[18][ 7] = 25.549999;
bbcCutZDep[19][ 7] = 24.850000;
bbcCutZDep[20][ 7] = 24.183332;
bbcCutZDep[21][ 7] = 23.516666;
bbcCutZDep[22][ 7] = 22.883333;
bbcCutZDep[23][ 7] = 22.250000;
bbcCutZDep[24][ 7] = 21.616667;
bbcCutZDep[25][ 7] = 21.016666;
bbcCutZDep[26][ 7] = 20.416666;
bbcCutZDep[27][ 7] = 19.850000;
bbcCutZDep[28][ 7] = 19.283333;
bbcCutZDep[29][ 7] = 18.716667;
bbcCutZDep[30][ 7] = 18.150000;
bbcCutZDep[31][ 7] = 17.583334;
bbcCutZDep[32][ 7] = 17.049999;
bbcCutZDep[33][ 7] = 16.516666;
bbcCutZDep[34][ 7] = 15.983334;
bbcCutZDep[35][ 7] = 15.450000;
bbcCutZDep[36][ 7] = 14.950000;
bbcCutZDep[37][ 7] = 14.416667;
bbcCutZDep[38][ 7] = 13.883333;
bbcCutZDep[39][ 7] = 13.383333;
bbcCutZDep[40][ 7] = 12.850000;
bbcCutZDep[41][ 7] = 12.350000;
bbcCutZDep[42][ 7] = 11.850000;
bbcCutZDep[43][ 7] = 11.316667;
bbcCutZDep[44][ 7] = 10.816667;
bbcCutZDep[45][ 7] = 10.283334;
bbcCutZDep[46][ 7] = 9.783334;
bbcCutZDep[47][ 7] = 9.250000;
bbcCutZDep[48][ 7] = 8.750000;
bbcCutZDep[49][ 7] = 8.216666;
bbcCutZDep[50][ 7] = 7.683333;
bbcCutZDep[51][ 7] = 7.150000;
bbcCutZDep[52][ 7] = 6.616667;
bbcCutZDep[53][ 7] = 6.050000;
bbcCutZDep[54][ 7] = 5.483333;
bbcCutZDep[55][ 7] = 4.916667;
bbcCutZDep[56][ 7] = 4.316667;
bbcCutZDep[57][ 7] = 3.716667;
bbcCutZDep[58][ 7] = 3.083333;
bbcCutZDep[59][ 7] = 2.416667;
bbcCutZDep[60][ 7] = 1.716667;
bbcCutZDep[61][ 7] = -0.500000;
bbcCutZDep[ 0][ 8] = 999.983337;
bbcCutZDep[ 1][ 8] = 53.716667;
bbcCutZDep[ 2][ 8] = 47.716667;
bbcCutZDep[ 3][ 8] = 44.116665;
bbcCutZDep[ 4][ 8] = 41.483334;
bbcCutZDep[ 5][ 8] = 39.383335;
bbcCutZDep[ 6][ 8] = 37.650002;
bbcCutZDep[ 7][ 8] = 36.116665;
bbcCutZDep[ 8][ 8] = 34.783333;
bbcCutZDep[ 9][ 8] = 33.549999;
bbcCutZDep[10][ 8] = 32.416668;
bbcCutZDep[11][ 8] = 31.383333;
bbcCutZDep[12][ 8] = 30.416666;
bbcCutZDep[13][ 8] = 29.483334;
bbcCutZDep[14][ 8] = 28.616667;
bbcCutZDep[15][ 8] = 27.783333;
bbcCutZDep[16][ 8] = 27.016666;
bbcCutZDep[17][ 8] = 26.250000;
bbcCutZDep[18][ 8] = 25.516666;
bbcCutZDep[19][ 8] = 24.816668;
bbcCutZDep[20][ 8] = 24.150000;
bbcCutZDep[21][ 8] = 23.483334;
bbcCutZDep[22][ 8] = 22.850000;
bbcCutZDep[23][ 8] = 22.216667;
bbcCutZDep[24][ 8] = 21.583334;
bbcCutZDep[25][ 8] = 20.983334;
bbcCutZDep[26][ 8] = 20.416666;
bbcCutZDep[27][ 8] = 19.816668;
bbcCutZDep[28][ 8] = 19.250000;
bbcCutZDep[29][ 8] = 18.683332;
bbcCutZDep[30][ 8] = 18.150000;
bbcCutZDep[31][ 8] = 17.583334;
bbcCutZDep[32][ 8] = 17.049999;
bbcCutZDep[33][ 8] = 16.516666;
bbcCutZDep[34][ 8] = 15.983334;
bbcCutZDep[35][ 8] = 15.450000;
bbcCutZDep[36][ 8] = 14.916667;
bbcCutZDep[37][ 8] = 14.383333;
bbcCutZDep[38][ 8] = 13.883333;
bbcCutZDep[39][ 8] = 13.350000;
bbcCutZDep[40][ 8] = 12.850000;
bbcCutZDep[41][ 8] = 12.350000;
bbcCutZDep[42][ 8] = 11.816667;
bbcCutZDep[43][ 8] = 11.316667;
bbcCutZDep[44][ 8] = 10.783334;
bbcCutZDep[45][ 8] = 10.283334;
bbcCutZDep[46][ 8] = 9.783334;
bbcCutZDep[47][ 8] = 9.250000;
bbcCutZDep[48][ 8] = 8.716666;
bbcCutZDep[49][ 8] = 8.216666;
bbcCutZDep[50][ 8] = 7.683333;
bbcCutZDep[51][ 8] = 7.150000;
bbcCutZDep[52][ 8] = 6.616667;
bbcCutZDep[53][ 8] = 6.050000;
bbcCutZDep[54][ 8] = 5.516667;
bbcCutZDep[55][ 8] = 4.950000;
bbcCutZDep[56][ 8] = 4.350000;
bbcCutZDep[57][ 8] = 3.716667;
bbcCutZDep[58][ 8] = 3.083333;
bbcCutZDep[59][ 8] = 2.416667;
bbcCutZDep[60][ 8] = 1.716667;
bbcCutZDep[61][ 8] = -0.500000;
bbcCutZDep[ 0][ 9] = 999.983337;
bbcCutZDep[ 1][ 9] = 53.549999;
bbcCutZDep[ 2][ 9] = 47.549999;
bbcCutZDep[ 3][ 9] = 43.983334;
bbcCutZDep[ 4][ 9] = 41.416668;
bbcCutZDep[ 5][ 9] = 39.316666;
bbcCutZDep[ 6][ 9] = 37.583332;
bbcCutZDep[ 7][ 9] = 36.049999;
bbcCutZDep[ 8][ 9] = 34.716667;
bbcCutZDep[ 9][ 9] = 33.483334;
bbcCutZDep[10][ 9] = 32.383335;
bbcCutZDep[11][ 9] = 31.316668;
bbcCutZDep[12][ 9] = 30.350000;
bbcCutZDep[13][ 9] = 29.450001;
bbcCutZDep[14][ 9] = 28.583334;
bbcCutZDep[15][ 9] = 27.750000;
bbcCutZDep[16][ 9] = 26.950001;
bbcCutZDep[17][ 9] = 26.216667;
bbcCutZDep[18][ 9] = 25.483334;
bbcCutZDep[19][ 9] = 24.783333;
bbcCutZDep[20][ 9] = 24.116667;
bbcCutZDep[21][ 9] = 23.450001;
bbcCutZDep[22][ 9] = 22.816668;
bbcCutZDep[23][ 9] = 22.183332;
bbcCutZDep[24][ 9] = 21.549999;
bbcCutZDep[25][ 9] = 20.950001;
bbcCutZDep[26][ 9] = 20.383333;
bbcCutZDep[27][ 9] = 19.783333;
bbcCutZDep[28][ 9] = 19.216667;
bbcCutZDep[29][ 9] = 18.650000;
bbcCutZDep[30][ 9] = 18.083334;
bbcCutZDep[31][ 9] = 17.549999;
bbcCutZDep[32][ 9] = 17.016666;
bbcCutZDep[33][ 9] = 16.483334;
bbcCutZDep[34][ 9] = 15.950000;
bbcCutZDep[35][ 9] = 15.416667;
bbcCutZDep[36][ 9] = 14.916667;
bbcCutZDep[37][ 9] = 14.383333;
bbcCutZDep[38][ 9] = 13.850000;
bbcCutZDep[39][ 9] = 13.350000;
bbcCutZDep[40][ 9] = 12.850000;
bbcCutZDep[41][ 9] = 12.316667;
bbcCutZDep[42][ 9] = 11.816667;
bbcCutZDep[43][ 9] = 11.316667;
bbcCutZDep[44][ 9] = 10.783334;
bbcCutZDep[45][ 9] = 10.283334;
bbcCutZDep[46][ 9] = 9.783334;
bbcCutZDep[47][ 9] = 9.250000;
bbcCutZDep[48][ 9] = 8.750000;
bbcCutZDep[49][ 9] = 8.216666;
bbcCutZDep[50][ 9] = 7.683333;
bbcCutZDep[51][ 9] = 7.150000;
bbcCutZDep[52][ 9] = 6.616667;
bbcCutZDep[53][ 9] = 6.050000;
bbcCutZDep[54][ 9] = 5.516667;
bbcCutZDep[55][ 9] = 4.950000;
bbcCutZDep[56][ 9] = 4.350000;
bbcCutZDep[57][ 9] = 3.750000;
bbcCutZDep[58][ 9] = 3.116667;
bbcCutZDep[59][ 9] = 2.450000;
bbcCutZDep[60][ 9] = 1.750000;
bbcCutZDep[61][ 9] = -0.500000;
bbcCutZDep[ 0][10] = 999.983337;
bbcCutZDep[ 1][10] = 53.416668;
bbcCutZDep[ 2][10] = 47.416668;
bbcCutZDep[ 3][10] = 43.849998;
bbcCutZDep[ 4][10] = 41.250000;
bbcCutZDep[ 5][10] = 39.183334;
bbcCutZDep[ 6][10] = 37.416668;
bbcCutZDep[ 7][10] = 35.916668;
bbcCutZDep[ 8][10] = 34.583332;
bbcCutZDep[ 9][10] = 33.349998;
bbcCutZDep[10][10] = 32.216667;
bbcCutZDep[11][10] = 31.183332;
bbcCutZDep[12][10] = 30.216667;
bbcCutZDep[13][10] = 29.316668;
bbcCutZDep[14][10] = 28.450001;
bbcCutZDep[15][10] = 27.650000;
bbcCutZDep[16][10] = 26.850000;
bbcCutZDep[17][10] = 26.116667;
bbcCutZDep[18][10] = 25.383333;
bbcCutZDep[19][10] = 24.683332;
bbcCutZDep[20][10] = 24.016666;
bbcCutZDep[21][10] = 23.350000;
bbcCutZDep[22][10] = 22.716667;
bbcCutZDep[23][10] = 22.083334;
bbcCutZDep[24][10] = 21.483334;
bbcCutZDep[25][10] = 20.883333;
bbcCutZDep[26][10] = 20.283333;
bbcCutZDep[27][10] = 19.716667;
bbcCutZDep[28][10] = 19.150000;
bbcCutZDep[29][10] = 18.583334;
bbcCutZDep[30][10] = 18.049999;
bbcCutZDep[31][10] = 17.483334;
bbcCutZDep[32][10] = 16.950001;
bbcCutZDep[33][10] = 16.416666;
bbcCutZDep[34][10] = 15.916667;
bbcCutZDep[35][10] = 15.383333;
bbcCutZDep[36][10] = 14.850000;
bbcCutZDep[37][10] = 14.350000;
bbcCutZDep[38][10] = 13.816667;
bbcCutZDep[39][10] = 13.316667;
bbcCutZDep[40][10] = 12.816667;
bbcCutZDep[41][10] = 12.283334;
bbcCutZDep[42][10] = 11.783334;
bbcCutZDep[43][10] = 11.283334;
bbcCutZDep[44][10] = 10.783334;
bbcCutZDep[45][10] = 10.250000;
bbcCutZDep[46][10] = 9.750000;
bbcCutZDep[47][10] = 9.250000;
bbcCutZDep[48][10] = 8.716666;
bbcCutZDep[49][10] = 8.183333;
bbcCutZDep[50][10] = 7.683333;
bbcCutZDep[51][10] = 7.150000;
bbcCutZDep[52][10] = 6.616667;
bbcCutZDep[53][10] = 6.050000;
bbcCutZDep[54][10] = 5.516667;
bbcCutZDep[55][10] = 4.950000;
bbcCutZDep[56][10] = 4.350000;
bbcCutZDep[57][10] = 3.750000;
bbcCutZDep[58][10] = 3.116667;
bbcCutZDep[59][10] = 2.450000;
bbcCutZDep[60][10] = 1.750000;
bbcCutZDep[61][10] = -0.500000;
bbcCutZDep[ 0][11] = 999.983337;
bbcCutZDep[ 1][11] = 53.283333;
bbcCutZDep[ 2][11] = 47.316666;
bbcCutZDep[ 3][11] = 43.750000;
bbcCutZDep[ 4][11] = 41.150002;
bbcCutZDep[ 5][11] = 39.049999;
bbcCutZDep[ 6][11] = 37.283333;
bbcCutZDep[ 7][11] = 35.783333;
bbcCutZDep[ 8][11] = 34.450001;
bbcCutZDep[ 9][11] = 33.216667;
bbcCutZDep[10][11] = 32.083332;
bbcCutZDep[11][11] = 31.049999;
bbcCutZDep[12][11] = 30.116667;
bbcCutZDep[13][11] = 29.183332;
bbcCutZDep[14][11] = 28.350000;
bbcCutZDep[15][11] = 27.516666;
bbcCutZDep[16][11] = 26.750000;
bbcCutZDep[17][11] = 25.983334;
bbcCutZDep[18][11] = 25.283333;
bbcCutZDep[19][11] = 24.583334;
bbcCutZDep[20][11] = 23.916666;
bbcCutZDep[21][11] = 23.250000;
bbcCutZDep[22][11] = 22.616667;
bbcCutZDep[23][11] = 21.983334;
bbcCutZDep[24][11] = 21.383333;
bbcCutZDep[25][11] = 20.783333;
bbcCutZDep[26][11] = 20.216667;
bbcCutZDep[27][11] = 19.650000;
bbcCutZDep[28][11] = 19.083334;
bbcCutZDep[29][11] = 18.516666;
bbcCutZDep[30][11] = 17.983334;
bbcCutZDep[31][11] = 17.416666;
bbcCutZDep[32][11] = 16.883333;
bbcCutZDep[33][11] = 16.383333;
bbcCutZDep[34][11] = 15.850000;
bbcCutZDep[35][11] = 15.316667;
bbcCutZDep[36][11] = 14.816667;
bbcCutZDep[37][11] = 14.283334;
bbcCutZDep[38][11] = 13.783334;
bbcCutZDep[39][11] = 13.250000;
bbcCutZDep[40][11] = 12.750000;
bbcCutZDep[41][11] = 12.250000;
bbcCutZDep[42][11] = 11.750000;
bbcCutZDep[43][11] = 11.250000;
bbcCutZDep[44][11] = 10.750000;
bbcCutZDep[45][11] = 10.216666;
bbcCutZDep[46][11] = 9.716666;
bbcCutZDep[47][11] = 9.216666;
bbcCutZDep[48][11] = 8.683333;
bbcCutZDep[49][11] = 8.183333;
bbcCutZDep[50][11] = 7.650000;
bbcCutZDep[51][11] = 7.116667;
bbcCutZDep[52][11] = 6.583333;
bbcCutZDep[53][11] = 6.050000;
bbcCutZDep[54][11] = 5.516667;
bbcCutZDep[55][11] = 4.950000;
bbcCutZDep[56][11] = 4.350000;
bbcCutZDep[57][11] = 3.750000;
bbcCutZDep[58][11] = 3.116667;
bbcCutZDep[59][11] = 2.450000;
bbcCutZDep[60][11] = 1.750000;
bbcCutZDep[61][11] = -0.500000;
bbcCutZDep[ 0][12] = 999.983337;
bbcCutZDep[ 1][12] = 52.983334;
bbcCutZDep[ 2][12] = 47.083332;
bbcCutZDep[ 3][12] = 43.516666;
bbcCutZDep[ 4][12] = 40.950001;
bbcCutZDep[ 5][12] = 38.849998;
bbcCutZDep[ 6][12] = 37.083332;
bbcCutZDep[ 7][12] = 35.583332;
bbcCutZDep[ 8][12] = 34.216667;
bbcCutZDep[ 9][12] = 33.016666;
bbcCutZDep[10][12] = 31.883333;
bbcCutZDep[11][12] = 30.850000;
bbcCutZDep[12][12] = 29.883333;
bbcCutZDep[13][12] = 28.983334;
bbcCutZDep[14][12] = 28.150000;
bbcCutZDep[15][12] = 27.316668;
bbcCutZDep[16][12] = 26.549999;
bbcCutZDep[17][12] = 25.816668;
bbcCutZDep[18][12] = 25.083334;
bbcCutZDep[19][12] = 24.416666;
bbcCutZDep[20][12] = 23.750000;
bbcCutZDep[21][12] = 23.083334;
bbcCutZDep[22][12] = 22.450001;
bbcCutZDep[23][12] = 21.850000;
bbcCutZDep[24][12] = 21.250000;
bbcCutZDep[25][12] = 20.650000;
bbcCutZDep[26][12] = 20.083334;
bbcCutZDep[27][12] = 19.483334;
bbcCutZDep[28][12] = 18.950001;
bbcCutZDep[29][12] = 18.383333;
bbcCutZDep[30][12] = 17.850000;
bbcCutZDep[31][12] = 17.316668;
bbcCutZDep[32][12] = 16.783333;
bbcCutZDep[33][12] = 16.250000;
bbcCutZDep[34][12] = 15.716666;
bbcCutZDep[35][12] = 15.216666;
bbcCutZDep[36][12] = 14.683333;
bbcCutZDep[37][12] = 14.183333;
bbcCutZDep[38][12] = 13.683333;
bbcCutZDep[39][12] = 13.183333;
bbcCutZDep[40][12] = 12.683333;
bbcCutZDep[41][12] = 12.183333;
bbcCutZDep[42][12] = 11.683333;
bbcCutZDep[43][12] = 11.183333;
bbcCutZDep[44][12] = 10.683333;
bbcCutZDep[45][12] = 10.183333;
bbcCutZDep[46][12] = 9.683333;
bbcCutZDep[47][12] = 9.150000;
bbcCutZDep[48][12] = 8.650000;
bbcCutZDep[49][12] = 8.150000;
bbcCutZDep[50][12] = 7.616667;
bbcCutZDep[51][12] = 7.116667;
bbcCutZDep[52][12] = 6.583333;
bbcCutZDep[53][12] = 6.016667;
bbcCutZDep[54][12] = 5.483333;
bbcCutZDep[55][12] = 4.916667;
bbcCutZDep[56][12] = 4.350000;
bbcCutZDep[57][12] = 3.750000;
bbcCutZDep[58][12] = 3.116667;
bbcCutZDep[59][12] = 2.450000;
bbcCutZDep[60][12] = 1.750000;
bbcCutZDep[61][12] = -0.500000;
bbcCutZDep[ 0][13] = 999.983337;
bbcCutZDep[ 1][13] = 52.916668;
bbcCutZDep[ 2][13] = 46.916668;
bbcCutZDep[ 3][13] = 43.316666;
bbcCutZDep[ 4][13] = 40.716667;
bbcCutZDep[ 5][13] = 38.583332;
bbcCutZDep[ 6][13] = 36.849998;
bbcCutZDep[ 7][13] = 35.349998;
bbcCutZDep[ 8][13] = 34.016666;
bbcCutZDep[ 9][13] = 32.783333;
bbcCutZDep[10][13] = 31.683332;
bbcCutZDep[11][13] = 30.683332;
bbcCutZDep[12][13] = 29.716667;
bbcCutZDep[13][13] = 28.816668;
bbcCutZDep[14][13] = 27.950001;
bbcCutZDep[15][13] = 27.150000;
bbcCutZDep[16][13] = 26.383333;
bbcCutZDep[17][13] = 25.650000;
bbcCutZDep[18][13] = 24.950001;
bbcCutZDep[19][13] = 24.250000;
bbcCutZDep[20][13] = 23.583334;
bbcCutZDep[21][13] = 22.950001;
bbcCutZDep[22][13] = 22.316668;
bbcCutZDep[23][13] = 21.716667;
bbcCutZDep[24][13] = 21.116667;
bbcCutZDep[25][13] = 20.549999;
bbcCutZDep[26][13] = 19.950001;
bbcCutZDep[27][13] = 19.383333;
bbcCutZDep[28][13] = 18.850000;
bbcCutZDep[29][13] = 18.283333;
bbcCutZDep[30][13] = 17.750000;
bbcCutZDep[31][13] = 17.250000;
bbcCutZDep[32][13] = 16.716667;
bbcCutZDep[33][13] = 16.183332;
bbcCutZDep[34][13] = 15.683333;
bbcCutZDep[35][13] = 15.183333;
bbcCutZDep[36][13] = 14.650000;
bbcCutZDep[37][13] = 14.150000;
bbcCutZDep[38][13] = 13.650000;
bbcCutZDep[39][13] = 13.150000;
bbcCutZDep[40][13] = 12.650000;
bbcCutZDep[41][13] = 12.150000;
bbcCutZDep[42][13] = 11.650000;
bbcCutZDep[43][13] = 11.150000;
bbcCutZDep[44][13] = 10.650000;
bbcCutZDep[45][13] = 10.150000;
bbcCutZDep[46][13] = 9.650000;
bbcCutZDep[47][13] = 9.150000;
bbcCutZDep[48][13] = 8.650000;
bbcCutZDep[49][13] = 8.116667;
bbcCutZDep[50][13] = 7.616667;
bbcCutZDep[51][13] = 7.083333;
bbcCutZDep[52][13] = 6.550000;
bbcCutZDep[53][13] = 6.016667;
bbcCutZDep[54][13] = 5.483333;
bbcCutZDep[55][13] = 4.916667;
bbcCutZDep[56][13] = 4.350000;
bbcCutZDep[57][13] = 3.750000;
bbcCutZDep[58][13] = 3.116667;
bbcCutZDep[59][13] = 2.450000;
bbcCutZDep[60][13] = 1.750000;
bbcCutZDep[61][13] = -0.500000;
bbcCutZDep[ 0][14] = 999.983337;
bbcCutZDep[ 1][14] = 52.616665;
bbcCutZDep[ 2][14] = 46.683334;
bbcCutZDep[ 3][14] = 43.083332;
bbcCutZDep[ 4][14] = 40.450001;
bbcCutZDep[ 5][14] = 38.383335;
bbcCutZDep[ 6][14] = 36.650002;
bbcCutZDep[ 7][14] = 35.116665;
bbcCutZDep[ 8][14] = 33.783333;
bbcCutZDep[ 9][14] = 32.583332;
bbcCutZDep[10][14] = 31.483334;
bbcCutZDep[11][14] = 30.450001;
bbcCutZDep[12][14] = 29.516666;
bbcCutZDep[13][14] = 28.616667;
bbcCutZDep[14][14] = 27.783333;
bbcCutZDep[15][14] = 26.983334;
bbcCutZDep[16][14] = 26.216667;
bbcCutZDep[17][14] = 25.483334;
bbcCutZDep[18][14] = 24.783333;
bbcCutZDep[19][14] = 24.116667;
bbcCutZDep[20][14] = 23.450001;
bbcCutZDep[21][14] = 22.816668;
bbcCutZDep[22][14] = 22.183332;
bbcCutZDep[23][14] = 21.583334;
bbcCutZDep[24][14] = 20.983334;
bbcCutZDep[25][14] = 20.416666;
bbcCutZDep[26][14] = 19.850000;
bbcCutZDep[27][14] = 19.316668;
bbcCutZDep[28][14] = 18.750000;
bbcCutZDep[29][14] = 18.216667;
bbcCutZDep[30][14] = 17.683332;
bbcCutZDep[31][14] = 17.150000;
bbcCutZDep[32][14] = 16.650000;
bbcCutZDep[33][14] = 16.116667;
bbcCutZDep[34][14] = 15.616667;
bbcCutZDep[35][14] = 15.116667;
bbcCutZDep[36][14] = 14.583333;
bbcCutZDep[37][14] = 14.083333;
bbcCutZDep[38][14] = 13.583333;
bbcCutZDep[39][14] = 13.083333;
bbcCutZDep[40][14] = 12.616667;
bbcCutZDep[41][14] = 12.116667;
bbcCutZDep[42][14] = 11.616667;
bbcCutZDep[43][14] = 11.116667;
bbcCutZDep[44][14] = 10.616667;
bbcCutZDep[45][14] = 10.116667;
bbcCutZDep[46][14] = 9.616667;
bbcCutZDep[47][14] = 9.116667;
bbcCutZDep[48][14] = 8.616667;
bbcCutZDep[49][14] = 8.116667;
bbcCutZDep[50][14] = 7.583333;
bbcCutZDep[51][14] = 7.083333;
bbcCutZDep[52][14] = 6.550000;
bbcCutZDep[53][14] = 6.016667;
bbcCutZDep[54][14] = 5.483333;
bbcCutZDep[55][14] = 4.916667;
bbcCutZDep[56][14] = 4.350000;
bbcCutZDep[57][14] = 3.716667;
bbcCutZDep[58][14] = 3.116667;
bbcCutZDep[59][14] = 2.450000;
bbcCutZDep[60][14] = 1.750000;
bbcCutZDep[61][14] = -0.500000;
bbcCutZDep[ 0][15] = 999.983337;
bbcCutZDep[ 1][15] = 52.383335;
bbcCutZDep[ 2][15] = 46.450001;
bbcCutZDep[ 3][15] = 42.849998;
bbcCutZDep[ 4][15] = 40.250000;
bbcCutZDep[ 5][15] = 38.183334;
bbcCutZDep[ 6][15] = 36.450001;
bbcCutZDep[ 7][15] = 34.983334;
bbcCutZDep[ 8][15] = 33.616665;
bbcCutZDep[ 9][15] = 32.416668;
bbcCutZDep[10][15] = 31.316668;
bbcCutZDep[11][15] = 30.316668;
bbcCutZDep[12][15] = 29.350000;
bbcCutZDep[13][15] = 28.483334;
bbcCutZDep[14][15] = 27.650000;
bbcCutZDep[15][15] = 26.850000;
bbcCutZDep[16][15] = 26.083334;
bbcCutZDep[17][15] = 25.350000;
bbcCutZDep[18][15] = 24.650000;
bbcCutZDep[19][15] = 23.983334;
bbcCutZDep[20][15] = 23.316668;
bbcCutZDep[21][15] = 22.683332;
bbcCutZDep[22][15] = 22.083334;
bbcCutZDep[23][15] = 21.483334;
bbcCutZDep[24][15] = 20.883333;
bbcCutZDep[25][15] = 20.316668;
bbcCutZDep[26][15] = 19.750000;
bbcCutZDep[27][15] = 19.216667;
bbcCutZDep[28][15] = 18.650000;
bbcCutZDep[29][15] = 18.116667;
bbcCutZDep[30][15] = 17.583334;
bbcCutZDep[31][15] = 17.083334;
bbcCutZDep[32][15] = 16.549999;
bbcCutZDep[33][15] = 16.049999;
bbcCutZDep[34][15] = 15.550000;
bbcCutZDep[35][15] = 15.016666;
bbcCutZDep[36][15] = 14.516666;
bbcCutZDep[37][15] = 14.050000;
bbcCutZDep[38][15] = 13.550000;
bbcCutZDep[39][15] = 13.050000;
bbcCutZDep[40][15] = 12.550000;
bbcCutZDep[41][15] = 12.050000;
bbcCutZDep[42][15] = 11.550000;
bbcCutZDep[43][15] = 11.083333;
bbcCutZDep[44][15] = 10.583333;
bbcCutZDep[45][15] = 10.083333;
bbcCutZDep[46][15] = 9.583333;
bbcCutZDep[47][15] = 9.083333;
bbcCutZDep[48][15] = 8.583333;
bbcCutZDep[49][15] = 8.083333;
bbcCutZDep[50][15] = 7.583333;
bbcCutZDep[51][15] = 7.050000;
bbcCutZDep[52][15] = 6.550000;
bbcCutZDep[53][15] = 6.016667;
bbcCutZDep[54][15] = 5.483333;
bbcCutZDep[55][15] = 4.916667;
bbcCutZDep[56][15] = 4.350000;
bbcCutZDep[57][15] = 3.750000;
bbcCutZDep[58][15] = 3.116667;
bbcCutZDep[59][15] = 2.450000;
bbcCutZDep[60][15] = 1.750000;
bbcCutZDep[61][15] = -0.500000;
bbcCutZDep[ 0][16] = 999.983337;
bbcCutZDep[ 1][16] = 52.250000;
bbcCutZDep[ 2][16] = 46.283333;
bbcCutZDep[ 3][16] = 42.716667;
bbcCutZDep[ 4][16] = 40.116665;
bbcCutZDep[ 5][16] = 38.049999;
bbcCutZDep[ 6][16] = 36.316666;
bbcCutZDep[ 7][16] = 34.849998;
bbcCutZDep[ 8][16] = 33.516666;
bbcCutZDep[ 9][16] = 32.316666;
bbcCutZDep[10][16] = 31.216667;
bbcCutZDep[11][16] = 30.216667;
bbcCutZDep[12][16] = 29.283333;
bbcCutZDep[13][16] = 28.383333;
bbcCutZDep[14][16] = 27.583334;
bbcCutZDep[15][16] = 26.783333;
bbcCutZDep[16][16] = 26.016666;
bbcCutZDep[17][16] = 25.283333;
bbcCutZDep[18][16] = 24.616667;
bbcCutZDep[19][16] = 23.950001;
bbcCutZDep[20][16] = 23.283333;
bbcCutZDep[21][16] = 22.650000;
bbcCutZDep[22][16] = 22.049999;
bbcCutZDep[23][16] = 21.450001;
bbcCutZDep[24][16] = 20.883333;
bbcCutZDep[25][16] = 20.316668;
bbcCutZDep[26][16] = 19.750000;
bbcCutZDep[27][16] = 19.183332;
bbcCutZDep[28][16] = 18.650000;
bbcCutZDep[29][16] = 18.116667;
bbcCutZDep[30][16] = 17.583334;
bbcCutZDep[31][16] = 17.083334;
bbcCutZDep[32][16] = 16.549999;
bbcCutZDep[33][16] = 16.049999;
bbcCutZDep[34][16] = 15.550000;
bbcCutZDep[35][16] = 15.050000;
bbcCutZDep[36][16] = 14.550000;
bbcCutZDep[37][16] = 14.050000;
bbcCutZDep[38][16] = 13.550000;
bbcCutZDep[39][16] = 13.050000;
bbcCutZDep[40][16] = 12.550000;
bbcCutZDep[41][16] = 12.050000;
bbcCutZDep[42][16] = 11.583333;
bbcCutZDep[43][16] = 11.083333;
bbcCutZDep[44][16] = 10.583333;
bbcCutZDep[45][16] = 10.116667;
bbcCutZDep[46][16] = 9.616667;
bbcCutZDep[47][16] = 9.116667;
bbcCutZDep[48][16] = 8.616667;
bbcCutZDep[49][16] = 8.116667;
bbcCutZDep[50][16] = 7.616667;
bbcCutZDep[51][16] = 7.083333;
bbcCutZDep[52][16] = 6.550000;
bbcCutZDep[53][16] = 6.016667;
bbcCutZDep[54][16] = 5.483333;
bbcCutZDep[55][16] = 4.916667;
bbcCutZDep[56][16] = 4.350000;
bbcCutZDep[57][16] = 3.750000;
bbcCutZDep[58][16] = 3.150000;
bbcCutZDep[59][16] = 2.450000;
bbcCutZDep[60][16] = 1.750000;
bbcCutZDep[61][16] = -0.500000;
bbcCutZDep[ 0][17] = 999.983337;
bbcCutZDep[ 1][17] = 52.150002;
bbcCutZDep[ 2][17] = 46.283333;
bbcCutZDep[ 3][17] = 42.683334;
bbcCutZDep[ 4][17] = 40.083332;
bbcCutZDep[ 5][17] = 38.016666;
bbcCutZDep[ 6][17] = 36.283333;
bbcCutZDep[ 7][17] = 34.783333;
bbcCutZDep[ 8][17] = 33.450001;
bbcCutZDep[ 9][17] = 32.250000;
bbcCutZDep[10][17] = 31.183332;
bbcCutZDep[11][17] = 30.183332;
bbcCutZDep[12][17] = 29.250000;
bbcCutZDep[13][17] = 28.383333;
bbcCutZDep[14][17] = 27.549999;
bbcCutZDep[15][17] = 26.750000;
bbcCutZDep[16][17] = 25.983334;
bbcCutZDep[17][17] = 25.283333;
bbcCutZDep[18][17] = 24.583334;
bbcCutZDep[19][17] = 23.916666;
bbcCutZDep[20][17] = 23.283333;
bbcCutZDep[21][17] = 22.650000;
bbcCutZDep[22][17] = 22.016666;
bbcCutZDep[23][17] = 21.450001;
bbcCutZDep[24][17] = 20.850000;
bbcCutZDep[25][17] = 20.283333;
bbcCutZDep[26][17] = 19.750000;
bbcCutZDep[27][17] = 19.183332;
bbcCutZDep[28][17] = 18.650000;
bbcCutZDep[29][17] = 18.116667;
bbcCutZDep[30][17] = 17.616667;
bbcCutZDep[31][17] = 17.083334;
bbcCutZDep[32][17] = 16.583334;
bbcCutZDep[33][17] = 16.049999;
bbcCutZDep[34][17] = 15.550000;
bbcCutZDep[35][17] = 15.050000;
bbcCutZDep[36][17] = 14.550000;
bbcCutZDep[37][17] = 14.050000;
bbcCutZDep[38][17] = 13.583333;
bbcCutZDep[39][17] = 13.083333;
bbcCutZDep[40][17] = 12.583333;
bbcCutZDep[41][17] = 12.116667;
bbcCutZDep[42][17] = 11.616667;
bbcCutZDep[43][17] = 11.116667;
bbcCutZDep[44][17] = 10.650000;
bbcCutZDep[45][17] = 10.150000;
bbcCutZDep[46][17] = 9.650000;
bbcCutZDep[47][17] = 9.150000;
bbcCutZDep[48][17] = 8.650000;
bbcCutZDep[49][17] = 8.150000;
bbcCutZDep[50][17] = 7.650000;
bbcCutZDep[51][17] = 7.150000;
bbcCutZDep[52][17] = 6.616667;
bbcCutZDep[53][17] = 6.083333;
bbcCutZDep[54][17] = 5.550000;
bbcCutZDep[55][17] = 4.983333;
bbcCutZDep[56][17] = 4.383333;
bbcCutZDep[57][17] = 3.783333;
bbcCutZDep[58][17] = 3.150000;
bbcCutZDep[59][17] = 2.483333;
bbcCutZDep[60][17] = 1.783333;
bbcCutZDep[61][17] = -0.500000;
bbcCutZDep[ 0][18] = 999.983337;
bbcCutZDep[ 1][18] = 52.049999;
bbcCutZDep[ 2][18] = 46.150002;
bbcCutZDep[ 3][18] = 42.616665;
bbcCutZDep[ 4][18] = 40.016666;
bbcCutZDep[ 5][18] = 37.950001;
bbcCutZDep[ 6][18] = 36.216667;
bbcCutZDep[ 7][18] = 34.750000;
bbcCutZDep[ 8][18] = 33.416668;
bbcCutZDep[ 9][18] = 32.250000;
bbcCutZDep[10][18] = 31.150000;
bbcCutZDep[11][18] = 30.150000;
bbcCutZDep[12][18] = 29.216667;
bbcCutZDep[13][18] = 28.350000;
bbcCutZDep[14][18] = 27.516666;
bbcCutZDep[15][18] = 26.750000;
bbcCutZDep[16][18] = 25.983334;
bbcCutZDep[17][18] = 25.283333;
bbcCutZDep[18][18] = 24.583334;
bbcCutZDep[19][18] = 23.916666;
bbcCutZDep[20][18] = 23.283333;
bbcCutZDep[21][18] = 22.650000;
bbcCutZDep[22][18] = 22.049999;
bbcCutZDep[23][18] = 21.450001;
bbcCutZDep[24][18] = 20.883333;
bbcCutZDep[25][18] = 20.316668;
bbcCutZDep[26][18] = 19.783333;
bbcCutZDep[27][18] = 19.216667;
bbcCutZDep[28][18] = 18.683332;
bbcCutZDep[29][18] = 18.150000;
bbcCutZDep[30][18] = 17.650000;
bbcCutZDep[31][18] = 17.116667;
bbcCutZDep[32][18] = 16.616667;
bbcCutZDep[33][18] = 16.116667;
bbcCutZDep[34][18] = 15.616667;
bbcCutZDep[35][18] = 15.116667;
bbcCutZDep[36][18] = 14.616667;
bbcCutZDep[37][18] = 14.116667;
bbcCutZDep[38][18] = 13.650000;
bbcCutZDep[39][18] = 13.150000;
bbcCutZDep[40][18] = 12.650000;
bbcCutZDep[41][18] = 12.183333;
bbcCutZDep[42][18] = 11.683333;
bbcCutZDep[43][18] = 11.183333;
bbcCutZDep[44][18] = 10.716666;
bbcCutZDep[45][18] = 10.216666;
bbcCutZDep[46][18] = 9.716666;
bbcCutZDep[47][18] = 9.216666;
bbcCutZDep[48][18] = 8.716666;
bbcCutZDep[49][18] = 8.216666;
bbcCutZDep[50][18] = 7.716667;
bbcCutZDep[51][18] = 7.183333;
bbcCutZDep[52][18] = 6.683333;
bbcCutZDep[53][18] = 6.116667;
bbcCutZDep[54][18] = 5.583333;
bbcCutZDep[55][18] = 5.016667;
bbcCutZDep[56][18] = 4.450000;
bbcCutZDep[57][18] = 3.816667;
bbcCutZDep[58][18] = 3.183333;
bbcCutZDep[59][18] = 2.516667;
bbcCutZDep[60][18] = 1.783333;
bbcCutZDep[61][18] = -0.500000;
bbcCutZDep[ 0][19] = 999.983337;
bbcCutZDep[ 1][19] = 51.983334;
bbcCutZDep[ 2][19] = 46.183334;
bbcCutZDep[ 3][19] = 42.650002;
bbcCutZDep[ 4][19] = 40.049999;
bbcCutZDep[ 5][19] = 38.016666;
bbcCutZDep[ 6][19] = 36.283333;
bbcCutZDep[ 7][19] = 34.816666;
bbcCutZDep[ 8][19] = 33.483334;
bbcCutZDep[ 9][19] = 32.316666;
bbcCutZDep[10][19] = 31.216667;
bbcCutZDep[11][19] = 30.216667;
bbcCutZDep[12][19] = 29.316668;
bbcCutZDep[13][19] = 28.416666;
bbcCutZDep[14][19] = 27.616667;
bbcCutZDep[15][19] = 26.816668;
bbcCutZDep[16][19] = 26.083334;
bbcCutZDep[17][19] = 25.350000;
bbcCutZDep[18][19] = 24.683332;
bbcCutZDep[19][19] = 24.016666;
bbcCutZDep[20][19] = 23.383333;
bbcCutZDep[21][19] = 22.750000;
bbcCutZDep[22][19] = 22.150000;
bbcCutZDep[23][19] = 21.549999;
bbcCutZDep[24][19] = 20.983334;
bbcCutZDep[25][19] = 20.416666;
bbcCutZDep[26][19] = 19.850000;
bbcCutZDep[27][19] = 19.316668;
bbcCutZDep[28][19] = 18.783333;
bbcCutZDep[29][19] = 18.250000;
bbcCutZDep[30][19] = 17.716667;
bbcCutZDep[31][19] = 17.216667;
bbcCutZDep[32][19] = 16.716667;
bbcCutZDep[33][19] = 16.216667;
bbcCutZDep[34][19] = 15.683333;
bbcCutZDep[35][19] = 15.183333;
bbcCutZDep[36][19] = 14.716666;
bbcCutZDep[37][19] = 14.216666;
bbcCutZDep[38][19] = 13.716666;
bbcCutZDep[39][19] = 13.216666;
bbcCutZDep[40][19] = 12.750000;
bbcCutZDep[41][19] = 12.250000;
bbcCutZDep[42][19] = 11.750000;
bbcCutZDep[43][19] = 11.250000;
bbcCutZDep[44][19] = 10.783334;
bbcCutZDep[45][19] = 10.283334;
bbcCutZDep[46][19] = 9.783334;
bbcCutZDep[47][19] = 9.283334;
bbcCutZDep[48][19] = 8.783334;
bbcCutZDep[49][19] = 8.283334;
bbcCutZDep[50][19] = 7.783333;
bbcCutZDep[51][19] = 7.250000;
bbcCutZDep[52][19] = 6.716667;
bbcCutZDep[53][19] = 6.183333;
bbcCutZDep[54][19] = 5.650000;
bbcCutZDep[55][19] = 5.083333;
bbcCutZDep[56][19] = 4.483333;
bbcCutZDep[57][19] = 3.883333;
bbcCutZDep[58][19] = 3.250000;
bbcCutZDep[59][19] = 2.550000;
bbcCutZDep[60][19] = 1.816667;
bbcCutZDep[61][19] = -0.500000;

}

void
Run16dAu20GeVCentralityReco::InitScaleFactor ()
{

  // scale factors determined from <bbc-south charge> on a run-by-run basis
  // all set to 1.0 for now
  for (int i=0;i<5000;i++) {
    BBCScaleFactor[i] = 1.00000;
  }

  // scale factors determined from <bbc-south charge> on a run-by-run basis
  // for d+Au @ 20 GeV, initial pass from online production (6/20/2016)
  // try 2nd pass with restricted range BBC south charge 5-75 (to remove background regions)
  // just use scale factors of 1.0 - too much background otherwise and quote bad run list
  

  // set all missing values to the value of the run before it ....
  // now enter list of runs that were QA checked, but failed

  for (int i = 0; i < 5000; i++)
    {

      if (BBCScaleFactor[i] == 0.0)
	{

	  // this implies that the run was not QA checked (because no scale factor exists above)
	  QAstatus[i] = QAstatus_NOCHECK; //

	  if (i == 0)
	    {
	      BBCScaleFactor[i] = 1.00;
	    }
	  else
	    {
	      // search for last run number with non-zero value
	      for (int j = i - 1; j >= 0; j--)
		{
		  if (BBCScaleFactor[j] != 0.0)
		    {
		      BBCScaleFactor[i] = BBCScaleFactor[j];
		      break;
		    }
		}
	    }

	}

    } // end loop over checking all run-numbers

  return;

}

float
Run16dAu20GeVCentralityReco::GetScaleFactor(const int runnumber) const
{

  if (! (runnumber >= MINRUN && runnumber <= MAXRUN))
    {
      cout << "MyGetScaleFactor error in run number range " << runnumber << endl;
      return 0.0;
    }

  float scalefactor = BBCScaleFactor[runnumber - MINRUN]; // subtracting reference offset index

  return scalefactor;

}

int
Run16dAu20GeVCentralityReco::GetQAStatus(const int runnumber) const
{

  if (! (runnumber >= MINRUN && runnumber <= MAXRUN))
    {
      cout << "Run16dAu20GeVCentralityReco::GetQAStatus - ERROR in run number range (Not a Run-16 dAu 20 GeV run number) = " << runnumber << endl;
      return 0;
    }

  int qastatus = QAstatus[runnumber - MINRUN];

  cout << "Run16dAu20GeVCentralityReco::GetQAStatus - Run " << runnumber << " Centrality QA = ";
  if (qastatus == QAstatus_PASS)
    {
      cout << "Passed" << endl;
    }
  else if (qastatus == QAstatus_FAIL)
    {
      cout << "Failed" << endl;
    }
  else
    {
      cout << "Never Checked (Not available at time of QA)" << endl;
    }

  return qastatus;
}
