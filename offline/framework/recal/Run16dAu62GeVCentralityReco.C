#include "Run16dAu62GeVCentralityReco.h"

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
static const int MINRUN = 455792; // first Run-16 d+Au @ 62 GeV physics run, May 21, 2016
static const int MAXRUN = 456283; // end of running time on 5/27/2016

Run16dAu62GeVCentralityReco::Run16dAu62GeVCentralityReco() : Recalibrator("Run16dAu62GeVCentralityReco")
{
  memset(bbcCutZDep, 0, sizeof(bbcCutZDep));
  memset(BBCScaleFactor, 0, sizeof(BBCScaleFactor));
  fill(QAstatus, QAstatus + sizeof(QAstatus) / sizeof(int), QAstatus_PASS);
  baseclasses.insert("PHGlobal");
  return ;
}

int
Run16dAu62GeVCentralityReco::isValidRun(const int runno) const
{
  if (runno < MINRUN || runno > MAXRUN)
    {
      return 0;
    }

  return 1;
}

int Run16dAu62GeVCentralityReco::Init(PHCompositeNode *topNode)
{

  help(); // just for test !!!
  InitArray1();
  InitScaleFactor();
  return 0;

}

void
Run16dAu62GeVCentralityReco::help() const
{
  cout << "===================================================================" << endl;
  cout << "Run16dAu62GeVCentralityReco::help method output"                    << endl;
  cout << "Author: J.L. Nagle (jamie.nagle@colorado.edu)." << endl;
  cout << "Comment: Run-16 dAu@62 GeV Centrality Using BBC South Charge Only uses 78% "             << endl;
  cout << "           of the inelastic cross section."                          << endl;
  cout << "         This method updates the value in PHGlobal for"              << endl;
  cout << "           BbcPercentile to go from 1 to 78."                        << endl;
  cout << "         This is applicable for runs " << MINRUN << " - " << MAXRUN                << endl;
  cout << "         NOTE: First pass version only, no run-to-run QA / Scaling"                << endl;
  cout << "         NOTE: The calibration has been performed in 1 cm wide z-vertex bins" << endl;
  cout << ": Run16 dAu 62 GeV with minimum bias defined by FVTX_NS_BBCS>0 Level-1, an offline FVTX z-vertex and BBC Charge South > 0" << endl;
  cout << "  The calibration is based upon the BBC south charge distribution." << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int
Run16dAu62GeVCentralityReco::process_event(PHCompositeNode *topNode)
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
Run16dAu62GeVCentralityReco::getCentralityByBBCRun16dAu(PHCompositeNode* topNode) const
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
    // leave zvertex from the BBC
  } else {
    zvertex = FVTXz; 
  }

  int   runno   = run->get_RunNumber();

  // For d+Au @ 62 GeV, calibration is only for +/- 10 cm...
  // require a non-zero BBC charge south and FVTX zvertex or BBC zvertex within +/-10 cm

  bool isEventOK = fabs(zvertex) < 10; // +- 10 (cm)
  if (! (bbc2>0)) isEventOK = false;  // require non-zero charge in BBC south for min.bias definition!!!

  if(!isEventOK) return -999;

  return getCentralityByBBCRun16dAu(bbc1, bbc2, zdc1, zdc2, zvertex, runno);

}

int
Run16dAu62GeVCentralityReco::getCentralityByBBCRun16dAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const
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
      cout << "Run16dAu62GeVCentralityReco::getCentralityByBBCRun16dAu - ERROR with run " <<
	runno << " and retrieved scale factor " << scalefactor << endl;
      return -999;
    }
  bbcsum = bbcsum / scalefactor;

  int centrality = -1;
  // use lookup table to determine centrality of this event
  for (int jjj = 0; jjj < 78; jjj++)
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
Run16dAu62GeVCentralityReco::InitArray1()
{

  // Lookup Table (BBC South Charge Cut Values) for Run16dAu62-Centrality
  // (-10<bbcz<10 cm, 1 cm step)
  // table generated by J.L. Nagle - first trial table (not yet final)
  // generated from just two runs on 05/24/2016
  // now complete from the full taxi 10/17/2016

  // --> J.NAGLE - INSERT HERE....

bbcCutZDep[ 0][ 0] = 999.983337;
bbcCutZDep[ 1][ 0] = 72.750000;
bbcCutZDep[ 2][ 0] = 64.750000;
bbcCutZDep[ 3][ 0] = 60.049999;
bbcCutZDep[ 4][ 0] = 56.349998;
bbcCutZDep[ 5][ 0] = 53.450001;
bbcCutZDep[ 6][ 0] = 51.016666;
bbcCutZDep[ 7][ 0] = 48.983334;
bbcCutZDep[ 8][ 0] = 47.049999;
bbcCutZDep[ 9][ 0] = 45.216667;
bbcCutZDep[10][ 0] = 43.650002;
bbcCutZDep[11][ 0] = 42.183334;
bbcCutZDep[12][ 0] = 40.916668;
bbcCutZDep[13][ 0] = 39.683334;
bbcCutZDep[14][ 0] = 38.549999;
bbcCutZDep[15][ 0] = 37.450001;
bbcCutZDep[16][ 0] = 36.349998;
bbcCutZDep[17][ 0] = 35.316666;
bbcCutZDep[18][ 0] = 34.416668;
bbcCutZDep[19][ 0] = 33.549999;
bbcCutZDep[20][ 0] = 32.683334;
bbcCutZDep[21][ 0] = 31.850000;
bbcCutZDep[22][ 0] = 31.049999;
bbcCutZDep[23][ 0] = 30.250000;
bbcCutZDep[24][ 0] = 29.483334;
bbcCutZDep[25][ 0] = 28.716667;
bbcCutZDep[26][ 0] = 28.016666;
bbcCutZDep[27][ 0] = 27.316668;
bbcCutZDep[28][ 0] = 26.650000;
bbcCutZDep[29][ 0] = 25.983334;
bbcCutZDep[30][ 0] = 25.283333;
bbcCutZDep[31][ 0] = 24.583334;
bbcCutZDep[32][ 0] = 23.916666;
bbcCutZDep[33][ 0] = 23.283333;
bbcCutZDep[34][ 0] = 22.683332;
bbcCutZDep[35][ 0] = 22.150000;
bbcCutZDep[36][ 0] = 21.583334;
bbcCutZDep[37][ 0] = 20.983334;
bbcCutZDep[38][ 0] = 20.416666;
bbcCutZDep[39][ 0] = 19.883333;
bbcCutZDep[40][ 0] = 19.350000;
bbcCutZDep[41][ 0] = 18.783333;
bbcCutZDep[42][ 0] = 18.250000;
bbcCutZDep[43][ 0] = 17.750000;
bbcCutZDep[44][ 0] = 17.283333;
bbcCutZDep[45][ 0] = 16.750000;
bbcCutZDep[46][ 0] = 16.250000;
bbcCutZDep[47][ 0] = 15.716666;
bbcCutZDep[48][ 0] = 15.250000;
bbcCutZDep[49][ 0] = 14.750000;
bbcCutZDep[50][ 0] = 14.283334;
bbcCutZDep[51][ 0] = 13.816667;
bbcCutZDep[52][ 0] = 13.283334;
bbcCutZDep[53][ 0] = 12.783334;
bbcCutZDep[54][ 0] = 12.316667;
bbcCutZDep[55][ 0] = 11.850000;
bbcCutZDep[56][ 0] = 11.383333;
bbcCutZDep[57][ 0] = 10.883333;
bbcCutZDep[58][ 0] = 10.450000;
bbcCutZDep[59][ 0] = 9.983334;
bbcCutZDep[60][ 0] = 9.516666;
bbcCutZDep[61][ 0] = 9.016666;
bbcCutZDep[62][ 0] = 8.516666;
bbcCutZDep[63][ 0] = 8.083333;
bbcCutZDep[64][ 0] = 7.650000;
bbcCutZDep[65][ 0] = 7.150000;
bbcCutZDep[66][ 0] = 6.683333;
bbcCutZDep[67][ 0] = 6.183333;
bbcCutZDep[68][ 0] = 5.716667;
bbcCutZDep[69][ 0] = 5.250000;
bbcCutZDep[70][ 0] = 4.783333;
bbcCutZDep[71][ 0] = 4.250000;
bbcCutZDep[72][ 0] = 3.716667;
bbcCutZDep[73][ 0] = 3.216667;
bbcCutZDep[74][ 0] = 2.683333;
bbcCutZDep[75][ 0] = 2.116667;
bbcCutZDep[76][ 0] = 1.550000;
bbcCutZDep[77][ 0] = 0.983333;
bbcCutZDep[78][ 0] = -0.500000;
bbcCutZDep[ 0][ 1] = 999.983337;
bbcCutZDep[ 1][ 1] = 72.216667;
bbcCutZDep[ 2][ 1] = 64.416664;
bbcCutZDep[ 3][ 1] = 59.616665;
bbcCutZDep[ 4][ 1] = 55.849998;
bbcCutZDep[ 5][ 1] = 52.950001;
bbcCutZDep[ 6][ 1] = 50.583332;
bbcCutZDep[ 7][ 1] = 48.349998;
bbcCutZDep[ 8][ 1] = 46.516666;
bbcCutZDep[ 9][ 1] = 44.883335;
bbcCutZDep[10][ 1] = 43.383335;
bbcCutZDep[11][ 1] = 41.983334;
bbcCutZDep[12][ 1] = 40.583332;
bbcCutZDep[13][ 1] = 39.450001;
bbcCutZDep[14][ 1] = 38.383335;
bbcCutZDep[15][ 1] = 37.250000;
bbcCutZDep[16][ 1] = 36.183334;
bbcCutZDep[17][ 1] = 35.183334;
bbcCutZDep[18][ 1] = 34.250000;
bbcCutZDep[19][ 1] = 33.349998;
bbcCutZDep[20][ 1] = 32.450001;
bbcCutZDep[21][ 1] = 31.583334;
bbcCutZDep[22][ 1] = 30.783333;
bbcCutZDep[23][ 1] = 30.016666;
bbcCutZDep[24][ 1] = 29.250000;
bbcCutZDep[25][ 1] = 28.450001;
bbcCutZDep[26][ 1] = 27.750000;
bbcCutZDep[27][ 1] = 27.116667;
bbcCutZDep[28][ 1] = 26.416666;
bbcCutZDep[29][ 1] = 25.750000;
bbcCutZDep[30][ 1] = 25.116667;
bbcCutZDep[31][ 1] = 24.483334;
bbcCutZDep[32][ 1] = 23.883333;
bbcCutZDep[33][ 1] = 23.283333;
bbcCutZDep[34][ 1] = 22.683332;
bbcCutZDep[35][ 1] = 22.116667;
bbcCutZDep[36][ 1] = 21.549999;
bbcCutZDep[37][ 1] = 20.983334;
bbcCutZDep[38][ 1] = 20.416666;
bbcCutZDep[39][ 1] = 19.883333;
bbcCutZDep[40][ 1] = 19.316668;
bbcCutZDep[41][ 1] = 18.783333;
bbcCutZDep[42][ 1] = 18.216667;
bbcCutZDep[43][ 1] = 17.716667;
bbcCutZDep[44][ 1] = 17.216667;
bbcCutZDep[45][ 1] = 16.683332;
bbcCutZDep[46][ 1] = 16.183332;
bbcCutZDep[47][ 1] = 15.716666;
bbcCutZDep[48][ 1] = 15.216666;
bbcCutZDep[49][ 1] = 14.716666;
bbcCutZDep[50][ 1] = 14.216666;
bbcCutZDep[51][ 1] = 13.683333;
bbcCutZDep[52][ 1] = 13.183333;
bbcCutZDep[53][ 1] = 12.716666;
bbcCutZDep[54][ 1] = 12.216666;
bbcCutZDep[55][ 1] = 11.750000;
bbcCutZDep[56][ 1] = 11.283334;
bbcCutZDep[57][ 1] = 10.816667;
bbcCutZDep[58][ 1] = 10.350000;
bbcCutZDep[59][ 1] = 9.850000;
bbcCutZDep[60][ 1] = 9.350000;
bbcCutZDep[61][ 1] = 8.883333;
bbcCutZDep[62][ 1] = 8.416667;
bbcCutZDep[63][ 1] = 7.950000;
bbcCutZDep[64][ 1] = 7.516667;
bbcCutZDep[65][ 1] = 7.016667;
bbcCutZDep[66][ 1] = 6.516667;
bbcCutZDep[67][ 1] = 6.083333;
bbcCutZDep[68][ 1] = 5.616667;
bbcCutZDep[69][ 1] = 5.116667;
bbcCutZDep[70][ 1] = 4.616667;
bbcCutZDep[71][ 1] = 4.116667;
bbcCutZDep[72][ 1] = 3.583333;
bbcCutZDep[73][ 1] = 3.083333;
bbcCutZDep[74][ 1] = 2.583333;
bbcCutZDep[75][ 1] = 2.050000;
bbcCutZDep[76][ 1] = 1.516667;
bbcCutZDep[77][ 1] = 0.950000;
bbcCutZDep[78][ 1] = -0.500000;
bbcCutZDep[ 0][ 2] = 999.983337;
bbcCutZDep[ 1][ 2] = 71.883331;
bbcCutZDep[ 2][ 2] = 64.116669;
bbcCutZDep[ 3][ 2] = 59.216667;
bbcCutZDep[ 4][ 2] = 55.650002;
bbcCutZDep[ 5][ 2] = 52.849998;
bbcCutZDep[ 6][ 2] = 50.349998;
bbcCutZDep[ 7][ 2] = 48.283333;
bbcCutZDep[ 8][ 2] = 46.549999;
bbcCutZDep[ 9][ 2] = 44.950001;
bbcCutZDep[10][ 2] = 43.316666;
bbcCutZDep[11][ 2] = 41.883335;
bbcCutZDep[12][ 2] = 40.549999;
bbcCutZDep[13][ 2] = 39.349998;
bbcCutZDep[14][ 2] = 38.183334;
bbcCutZDep[15][ 2] = 37.083332;
bbcCutZDep[16][ 2] = 36.083332;
bbcCutZDep[17][ 2] = 35.083332;
bbcCutZDep[18][ 2] = 34.150002;
bbcCutZDep[19][ 2] = 33.283333;
bbcCutZDep[20][ 2] = 32.383335;
bbcCutZDep[21][ 2] = 31.516666;
bbcCutZDep[22][ 2] = 30.683332;
bbcCutZDep[23][ 2] = 29.883333;
bbcCutZDep[24][ 2] = 29.083334;
bbcCutZDep[25][ 2] = 28.350000;
bbcCutZDep[26][ 2] = 27.549999;
bbcCutZDep[27][ 2] = 26.883333;
bbcCutZDep[28][ 2] = 26.183332;
bbcCutZDep[29][ 2] = 25.516666;
bbcCutZDep[30][ 2] = 24.850000;
bbcCutZDep[31][ 2] = 24.216667;
bbcCutZDep[32][ 2] = 23.616667;
bbcCutZDep[33][ 2] = 23.016666;
bbcCutZDep[34][ 2] = 22.383333;
bbcCutZDep[35][ 2] = 21.750000;
bbcCutZDep[36][ 2] = 21.150000;
bbcCutZDep[37][ 2] = 20.583334;
bbcCutZDep[38][ 2] = 19.983334;
bbcCutZDep[39][ 2] = 19.450001;
bbcCutZDep[40][ 2] = 18.850000;
bbcCutZDep[41][ 2] = 18.316668;
bbcCutZDep[42][ 2] = 17.783333;
bbcCutZDep[43][ 2] = 17.216667;
bbcCutZDep[44][ 2] = 16.716667;
bbcCutZDep[45][ 2] = 16.216667;
bbcCutZDep[46][ 2] = 15.716666;
bbcCutZDep[47][ 2] = 15.216666;
bbcCutZDep[48][ 2] = 14.750000;
bbcCutZDep[49][ 2] = 14.250000;
bbcCutZDep[50][ 2] = 13.783334;
bbcCutZDep[51][ 2] = 13.316667;
bbcCutZDep[52][ 2] = 12.816667;
bbcCutZDep[53][ 2] = 12.350000;
bbcCutZDep[54][ 2] = 11.883333;
bbcCutZDep[55][ 2] = 11.416667;
bbcCutZDep[56][ 2] = 10.916667;
bbcCutZDep[57][ 2] = 10.450000;
bbcCutZDep[58][ 2] = 9.950000;
bbcCutZDep[59][ 2] = 9.516666;
bbcCutZDep[60][ 2] = 9.083333;
bbcCutZDep[61][ 2] = 8.616667;
bbcCutZDep[62][ 2] = 8.116667;
bbcCutZDep[63][ 2] = 7.683333;
bbcCutZDep[64][ 2] = 7.183333;
bbcCutZDep[65][ 2] = 6.716667;
bbcCutZDep[66][ 2] = 6.250000;
bbcCutZDep[67][ 2] = 5.816667;
bbcCutZDep[68][ 2] = 5.316667;
bbcCutZDep[69][ 2] = 4.816667;
bbcCutZDep[70][ 2] = 4.350000;
bbcCutZDep[71][ 2] = 3.883333;
bbcCutZDep[72][ 2] = 3.416667;
bbcCutZDep[73][ 2] = 2.916667;
bbcCutZDep[74][ 2] = 2.416667;
bbcCutZDep[75][ 2] = 1.916667;
bbcCutZDep[76][ 2] = 1.383333;
bbcCutZDep[77][ 2] = 0.883333;
bbcCutZDep[78][ 2] = -0.500000;
bbcCutZDep[ 0][ 3] = 999.983337;
bbcCutZDep[ 1][ 3] = 71.949997;
bbcCutZDep[ 2][ 3] = 63.716667;
bbcCutZDep[ 3][ 3] = 59.016666;
bbcCutZDep[ 4][ 3] = 55.316666;
bbcCutZDep[ 5][ 3] = 52.416668;
bbcCutZDep[ 6][ 3] = 50.083332;
bbcCutZDep[ 7][ 3] = 47.916668;
bbcCutZDep[ 8][ 3] = 45.883335;
bbcCutZDep[ 9][ 3] = 44.216667;
bbcCutZDep[10][ 3] = 42.750000;
bbcCutZDep[11][ 3] = 41.349998;
bbcCutZDep[12][ 3] = 40.049999;
bbcCutZDep[13][ 3] = 38.883335;
bbcCutZDep[14][ 3] = 37.750000;
bbcCutZDep[15][ 3] = 36.750000;
bbcCutZDep[16][ 3] = 35.716667;
bbcCutZDep[17][ 3] = 34.683334;
bbcCutZDep[18][ 3] = 33.716667;
bbcCutZDep[19][ 3] = 32.816666;
bbcCutZDep[20][ 3] = 31.983334;
bbcCutZDep[21][ 3] = 31.116667;
bbcCutZDep[22][ 3] = 30.250000;
bbcCutZDep[23][ 3] = 29.450001;
bbcCutZDep[24][ 3] = 28.683332;
bbcCutZDep[25][ 3] = 27.983334;
bbcCutZDep[26][ 3] = 27.250000;
bbcCutZDep[27][ 3] = 26.583334;
bbcCutZDep[28][ 3] = 25.916666;
bbcCutZDep[29][ 3] = 25.283333;
bbcCutZDep[30][ 3] = 24.650000;
bbcCutZDep[31][ 3] = 24.016666;
bbcCutZDep[32][ 3] = 23.350000;
bbcCutZDep[33][ 3] = 22.716667;
bbcCutZDep[34][ 3] = 22.150000;
bbcCutZDep[35][ 3] = 21.616667;
bbcCutZDep[36][ 3] = 21.016666;
bbcCutZDep[37][ 3] = 20.450001;
bbcCutZDep[38][ 3] = 19.883333;
bbcCutZDep[39][ 3] = 19.350000;
bbcCutZDep[40][ 3] = 18.816668;
bbcCutZDep[41][ 3] = 18.316668;
bbcCutZDep[42][ 3] = 17.783333;
bbcCutZDep[43][ 3] = 17.250000;
bbcCutZDep[44][ 3] = 16.783333;
bbcCutZDep[45][ 3] = 16.283333;
bbcCutZDep[46][ 3] = 15.750000;
bbcCutZDep[47][ 3] = 15.250000;
bbcCutZDep[48][ 3] = 14.683333;
bbcCutZDep[49][ 3] = 14.216666;
bbcCutZDep[50][ 3] = 13.750000;
bbcCutZDep[51][ 3] = 13.283334;
bbcCutZDep[52][ 3] = 12.783334;
bbcCutZDep[53][ 3] = 12.316667;
bbcCutZDep[54][ 3] = 11.883333;
bbcCutZDep[55][ 3] = 11.383333;
bbcCutZDep[56][ 3] = 10.916667;
bbcCutZDep[57][ 3] = 10.450000;
bbcCutZDep[58][ 3] = 9.983334;
bbcCutZDep[59][ 3] = 9.516666;
bbcCutZDep[60][ 3] = 9.016666;
bbcCutZDep[61][ 3] = 8.616667;
bbcCutZDep[62][ 3] = 8.116667;
bbcCutZDep[63][ 3] = 7.650000;
bbcCutZDep[64][ 3] = 7.150000;
bbcCutZDep[65][ 3] = 6.683333;
bbcCutZDep[66][ 3] = 6.216667;
bbcCutZDep[67][ 3] = 5.783333;
bbcCutZDep[68][ 3] = 5.283333;
bbcCutZDep[69][ 3] = 4.816667;
bbcCutZDep[70][ 3] = 4.350000;
bbcCutZDep[71][ 3] = 3.916667;
bbcCutZDep[72][ 3] = 3.483333;
bbcCutZDep[73][ 3] = 2.983333;
bbcCutZDep[74][ 3] = 2.483333;
bbcCutZDep[75][ 3] = 1.983333;
bbcCutZDep[76][ 3] = 1.416667;
bbcCutZDep[77][ 3] = 0.916667;
bbcCutZDep[78][ 3] = -0.500000;
bbcCutZDep[ 0][ 4] = 999.983337;
bbcCutZDep[ 1][ 4] = 71.383331;
bbcCutZDep[ 2][ 4] = 63.383335;
bbcCutZDep[ 3][ 4] = 58.349998;
bbcCutZDep[ 4][ 4] = 54.816666;
bbcCutZDep[ 5][ 4] = 51.983334;
bbcCutZDep[ 6][ 4] = 49.716667;
bbcCutZDep[ 7][ 4] = 47.616665;
bbcCutZDep[ 8][ 4] = 45.883335;
bbcCutZDep[ 9][ 4] = 44.283333;
bbcCutZDep[10][ 4] = 42.816666;
bbcCutZDep[11][ 4] = 41.383335;
bbcCutZDep[12][ 4] = 40.083332;
bbcCutZDep[13][ 4] = 38.883335;
bbcCutZDep[14][ 4] = 37.716667;
bbcCutZDep[15][ 4] = 36.616665;
bbcCutZDep[16][ 4] = 35.616665;
bbcCutZDep[17][ 4] = 34.583332;
bbcCutZDep[18][ 4] = 33.616665;
bbcCutZDep[19][ 4] = 32.683334;
bbcCutZDep[20][ 4] = 31.783333;
bbcCutZDep[21][ 4] = 30.916666;
bbcCutZDep[22][ 4] = 30.150000;
bbcCutZDep[23][ 4] = 29.316668;
bbcCutZDep[24][ 4] = 28.583334;
bbcCutZDep[25][ 4] = 27.850000;
bbcCutZDep[26][ 4] = 27.083334;
bbcCutZDep[27][ 4] = 26.350000;
bbcCutZDep[28][ 4] = 25.650000;
bbcCutZDep[29][ 4] = 25.016666;
bbcCutZDep[30][ 4] = 24.416666;
bbcCutZDep[31][ 4] = 23.783333;
bbcCutZDep[32][ 4] = 23.150000;
bbcCutZDep[33][ 4] = 22.516666;
bbcCutZDep[34][ 4] = 21.950001;
bbcCutZDep[35][ 4] = 21.350000;
bbcCutZDep[36][ 4] = 20.750000;
bbcCutZDep[37][ 4] = 20.183332;
bbcCutZDep[38][ 4] = 19.650000;
bbcCutZDep[39][ 4] = 19.116667;
bbcCutZDep[40][ 4] = 18.583334;
bbcCutZDep[41][ 4] = 18.016666;
bbcCutZDep[42][ 4] = 17.483334;
bbcCutZDep[43][ 4] = 16.983334;
bbcCutZDep[44][ 4] = 16.483334;
bbcCutZDep[45][ 4] = 15.983334;
bbcCutZDep[46][ 4] = 15.516666;
bbcCutZDep[47][ 4] = 15.050000;
bbcCutZDep[48][ 4] = 14.516666;
bbcCutZDep[49][ 4] = 14.016666;
bbcCutZDep[50][ 4] = 13.550000;
bbcCutZDep[51][ 4] = 13.050000;
bbcCutZDep[52][ 4] = 12.550000;
bbcCutZDep[53][ 4] = 12.083333;
bbcCutZDep[54][ 4] = 11.650000;
bbcCutZDep[55][ 4] = 11.183333;
bbcCutZDep[56][ 4] = 10.683333;
bbcCutZDep[57][ 4] = 10.250000;
bbcCutZDep[58][ 4] = 9.816667;
bbcCutZDep[59][ 4] = 9.383333;
bbcCutZDep[60][ 4] = 8.950000;
bbcCutZDep[61][ 4] = 8.483334;
bbcCutZDep[62][ 4] = 7.983333;
bbcCutZDep[63][ 4] = 7.550000;
bbcCutZDep[64][ 4] = 7.116667;
bbcCutZDep[65][ 4] = 6.650000;
bbcCutZDep[66][ 4] = 6.183333;
bbcCutZDep[67][ 4] = 5.716667;
bbcCutZDep[68][ 4] = 5.250000;
bbcCutZDep[69][ 4] = 4.783333;
bbcCutZDep[70][ 4] = 4.350000;
bbcCutZDep[71][ 4] = 3.883333;
bbcCutZDep[72][ 4] = 3.383333;
bbcCutZDep[73][ 4] = 2.883333;
bbcCutZDep[74][ 4] = 2.383333;
bbcCutZDep[75][ 4] = 1.916667;
bbcCutZDep[76][ 4] = 1.416667;
bbcCutZDep[77][ 4] = 0.916667;
bbcCutZDep[78][ 4] = -0.500000;
bbcCutZDep[ 0][ 5] = 999.983337;
bbcCutZDep[ 1][ 5] = 71.650002;
bbcCutZDep[ 2][ 5] = 63.483334;
bbcCutZDep[ 3][ 5] = 58.616665;
bbcCutZDep[ 4][ 5] = 54.916668;
bbcCutZDep[ 5][ 5] = 51.983334;
bbcCutZDep[ 6][ 5] = 49.383335;
bbcCutZDep[ 7][ 5] = 47.349998;
bbcCutZDep[ 8][ 5] = 45.416668;
bbcCutZDep[ 9][ 5] = 43.683334;
bbcCutZDep[10][ 5] = 42.183334;
bbcCutZDep[11][ 5] = 40.816666;
bbcCutZDep[12][ 5] = 39.516666;
bbcCutZDep[13][ 5] = 38.316666;
bbcCutZDep[14][ 5] = 37.250000;
bbcCutZDep[15][ 5] = 36.150002;
bbcCutZDep[16][ 5] = 35.116665;
bbcCutZDep[17][ 5] = 34.083332;
bbcCutZDep[18][ 5] = 33.150002;
bbcCutZDep[19][ 5] = 32.183334;
bbcCutZDep[20][ 5] = 31.316668;
bbcCutZDep[21][ 5] = 30.483334;
bbcCutZDep[22][ 5] = 29.683332;
bbcCutZDep[23][ 5] = 28.950001;
bbcCutZDep[24][ 5] = 28.283333;
bbcCutZDep[25][ 5] = 27.549999;
bbcCutZDep[26][ 5] = 26.850000;
bbcCutZDep[27][ 5] = 26.150000;
bbcCutZDep[28][ 5] = 25.483334;
bbcCutZDep[29][ 5] = 24.783333;
bbcCutZDep[30][ 5] = 24.116667;
bbcCutZDep[31][ 5] = 23.516666;
bbcCutZDep[32][ 5] = 22.883333;
bbcCutZDep[33][ 5] = 22.216667;
bbcCutZDep[34][ 5] = 21.616667;
bbcCutZDep[35][ 5] = 21.049999;
bbcCutZDep[36][ 5] = 20.516666;
bbcCutZDep[37][ 5] = 19.916666;
bbcCutZDep[38][ 5] = 19.383333;
bbcCutZDep[39][ 5] = 18.816668;
bbcCutZDep[40][ 5] = 18.316668;
bbcCutZDep[41][ 5] = 17.783333;
bbcCutZDep[42][ 5] = 17.283333;
bbcCutZDep[43][ 5] = 16.750000;
bbcCutZDep[44][ 5] = 16.216667;
bbcCutZDep[45][ 5] = 15.683333;
bbcCutZDep[46][ 5] = 15.150000;
bbcCutZDep[47][ 5] = 14.683333;
bbcCutZDep[48][ 5] = 14.216666;
bbcCutZDep[49][ 5] = 13.750000;
bbcCutZDep[50][ 5] = 13.316667;
bbcCutZDep[51][ 5] = 12.850000;
bbcCutZDep[52][ 5] = 12.350000;
bbcCutZDep[53][ 5] = 11.916667;
bbcCutZDep[54][ 5] = 11.450000;
bbcCutZDep[55][ 5] = 10.983334;
bbcCutZDep[56][ 5] = 10.516666;
bbcCutZDep[57][ 5] = 10.083333;
bbcCutZDep[58][ 5] = 9.616667;
bbcCutZDep[59][ 5] = 9.183333;
bbcCutZDep[60][ 5] = 8.716666;
bbcCutZDep[61][ 5] = 8.250000;
bbcCutZDep[62][ 5] = 7.783333;
bbcCutZDep[63][ 5] = 7.350000;
bbcCutZDep[64][ 5] = 6.916667;
bbcCutZDep[65][ 5] = 6.450000;
bbcCutZDep[66][ 5] = 6.016667;
bbcCutZDep[67][ 5] = 5.550000;
bbcCutZDep[68][ 5] = 5.083333;
bbcCutZDep[69][ 5] = 4.683333;
bbcCutZDep[70][ 5] = 4.216667;
bbcCutZDep[71][ 5] = 3.750000;
bbcCutZDep[72][ 5] = 3.250000;
bbcCutZDep[73][ 5] = 2.783333;
bbcCutZDep[74][ 5] = 2.350000;
bbcCutZDep[75][ 5] = 1.916667;
bbcCutZDep[76][ 5] = 1.350000;
bbcCutZDep[77][ 5] = 0.916667;
bbcCutZDep[78][ 5] = -0.500000;
bbcCutZDep[ 0][ 6] = 999.983337;
bbcCutZDep[ 1][ 6] = 70.216667;
bbcCutZDep[ 2][ 6] = 62.316666;
bbcCutZDep[ 3][ 6] = 57.916668;
bbcCutZDep[ 4][ 6] = 54.283333;
bbcCutZDep[ 5][ 6] = 51.349998;
bbcCutZDep[ 6][ 6] = 49.049999;
bbcCutZDep[ 7][ 6] = 47.016666;
bbcCutZDep[ 8][ 6] = 45.216667;
bbcCutZDep[ 9][ 6] = 43.683334;
bbcCutZDep[10][ 6] = 42.183334;
bbcCutZDep[11][ 6] = 40.783333;
bbcCutZDep[12][ 6] = 39.516666;
bbcCutZDep[13][ 6] = 38.250000;
bbcCutZDep[14][ 6] = 37.183334;
bbcCutZDep[15][ 6] = 36.083332;
bbcCutZDep[16][ 6] = 34.983334;
bbcCutZDep[17][ 6] = 33.983334;
bbcCutZDep[18][ 6] = 33.016666;
bbcCutZDep[19][ 6] = 32.183334;
bbcCutZDep[20][ 6] = 31.283333;
bbcCutZDep[21][ 6] = 30.416666;
bbcCutZDep[22][ 6] = 29.616667;
bbcCutZDep[23][ 6] = 28.883333;
bbcCutZDep[24][ 6] = 28.150000;
bbcCutZDep[25][ 6] = 27.416666;
bbcCutZDep[26][ 6] = 26.716667;
bbcCutZDep[27][ 6] = 25.983334;
bbcCutZDep[28][ 6] = 25.350000;
bbcCutZDep[29][ 6] = 24.716667;
bbcCutZDep[30][ 6] = 24.116667;
bbcCutZDep[31][ 6] = 23.516666;
bbcCutZDep[32][ 6] = 22.916666;
bbcCutZDep[33][ 6] = 22.316668;
bbcCutZDep[34][ 6] = 21.716667;
bbcCutZDep[35][ 6] = 21.116667;
bbcCutZDep[36][ 6] = 20.583334;
bbcCutZDep[37][ 6] = 20.049999;
bbcCutZDep[38][ 6] = 19.516666;
bbcCutZDep[39][ 6] = 18.950001;
bbcCutZDep[40][ 6] = 18.383333;
bbcCutZDep[41][ 6] = 17.816668;
bbcCutZDep[42][ 6] = 17.283333;
bbcCutZDep[43][ 6] = 16.783333;
bbcCutZDep[44][ 6] = 16.283333;
bbcCutZDep[45][ 6] = 15.750000;
bbcCutZDep[46][ 6] = 15.250000;
bbcCutZDep[47][ 6] = 14.783334;
bbcCutZDep[48][ 6] = 14.250000;
bbcCutZDep[49][ 6] = 13.816667;
bbcCutZDep[50][ 6] = 13.316667;
bbcCutZDep[51][ 6] = 12.816667;
bbcCutZDep[52][ 6] = 12.350000;
bbcCutZDep[53][ 6] = 11.883333;
bbcCutZDep[54][ 6] = 11.450000;
bbcCutZDep[55][ 6] = 10.983334;
bbcCutZDep[56][ 6] = 10.516666;
bbcCutZDep[57][ 6] = 10.083333;
bbcCutZDep[58][ 6] = 9.650000;
bbcCutZDep[59][ 6] = 9.183333;
bbcCutZDep[60][ 6] = 8.750000;
bbcCutZDep[61][ 6] = 8.283334;
bbcCutZDep[62][ 6] = 7.850000;
bbcCutZDep[63][ 6] = 7.416667;
bbcCutZDep[64][ 6] = 6.983333;
bbcCutZDep[65][ 6] = 6.516667;
bbcCutZDep[66][ 6] = 6.083333;
bbcCutZDep[67][ 6] = 5.616667;
bbcCutZDep[68][ 6] = 5.183333;
bbcCutZDep[69][ 6] = 4.716667;
bbcCutZDep[70][ 6] = 4.283333;
bbcCutZDep[71][ 6] = 3.850000;
bbcCutZDep[72][ 6] = 3.350000;
bbcCutZDep[73][ 6] = 2.883333;
bbcCutZDep[74][ 6] = 2.383333;
bbcCutZDep[75][ 6] = 1.950000;
bbcCutZDep[76][ 6] = 1.416667;
bbcCutZDep[77][ 6] = 0.916667;
bbcCutZDep[78][ 6] = -0.500000;
bbcCutZDep[ 0][ 7] = 999.983337;
bbcCutZDep[ 1][ 7] = 70.616669;
bbcCutZDep[ 2][ 7] = 62.583332;
bbcCutZDep[ 3][ 7] = 57.883335;
bbcCutZDep[ 4][ 7] = 54.216667;
bbcCutZDep[ 5][ 7] = 51.250000;
bbcCutZDep[ 6][ 7] = 48.983334;
bbcCutZDep[ 7][ 7] = 46.916668;
bbcCutZDep[ 8][ 7] = 45.083332;
bbcCutZDep[ 9][ 7] = 43.416668;
bbcCutZDep[10][ 7] = 41.916668;
bbcCutZDep[11][ 7] = 40.483334;
bbcCutZDep[12][ 7] = 39.250000;
bbcCutZDep[13][ 7] = 38.116665;
bbcCutZDep[14][ 7] = 37.049999;
bbcCutZDep[15][ 7] = 35.950001;
bbcCutZDep[16][ 7] = 34.883335;
bbcCutZDep[17][ 7] = 33.883335;
bbcCutZDep[18][ 7] = 32.916668;
bbcCutZDep[19][ 7] = 32.016666;
bbcCutZDep[20][ 7] = 31.150000;
bbcCutZDep[21][ 7] = 30.316668;
bbcCutZDep[22][ 7] = 29.516666;
bbcCutZDep[23][ 7] = 28.683332;
bbcCutZDep[24][ 7] = 27.983334;
bbcCutZDep[25][ 7] = 27.283333;
bbcCutZDep[26][ 7] = 26.549999;
bbcCutZDep[27][ 7] = 25.883333;
bbcCutZDep[28][ 7] = 25.216667;
bbcCutZDep[29][ 7] = 24.549999;
bbcCutZDep[30][ 7] = 23.950001;
bbcCutZDep[31][ 7] = 23.350000;
bbcCutZDep[32][ 7] = 22.750000;
bbcCutZDep[33][ 7] = 22.150000;
bbcCutZDep[34][ 7] = 21.516666;
bbcCutZDep[35][ 7] = 20.916666;
bbcCutZDep[36][ 7] = 20.350000;
bbcCutZDep[37][ 7] = 19.816668;
bbcCutZDep[38][ 7] = 19.250000;
bbcCutZDep[39][ 7] = 18.716667;
bbcCutZDep[40][ 7] = 18.216667;
bbcCutZDep[41][ 7] = 17.716667;
bbcCutZDep[42][ 7] = 17.183332;
bbcCutZDep[43][ 7] = 16.650000;
bbcCutZDep[44][ 7] = 16.150000;
bbcCutZDep[45][ 7] = 15.650000;
bbcCutZDep[46][ 7] = 15.116667;
bbcCutZDep[47][ 7] = 14.650000;
bbcCutZDep[48][ 7] = 14.183333;
bbcCutZDep[49][ 7] = 13.683333;
bbcCutZDep[50][ 7] = 13.216666;
bbcCutZDep[51][ 7] = 12.750000;
bbcCutZDep[52][ 7] = 12.250000;
bbcCutZDep[53][ 7] = 11.783334;
bbcCutZDep[54][ 7] = 11.316667;
bbcCutZDep[55][ 7] = 10.850000;
bbcCutZDep[56][ 7] = 10.383333;
bbcCutZDep[57][ 7] = 9.916667;
bbcCutZDep[58][ 7] = 9.516666;
bbcCutZDep[59][ 7] = 9.050000;
bbcCutZDep[60][ 7] = 8.650000;
bbcCutZDep[61][ 7] = 8.183333;
bbcCutZDep[62][ 7] = 7.750000;
bbcCutZDep[63][ 7] = 7.283333;
bbcCutZDep[64][ 7] = 6.850000;
bbcCutZDep[65][ 7] = 6.416667;
bbcCutZDep[66][ 7] = 5.983333;
bbcCutZDep[67][ 7] = 5.516667;
bbcCutZDep[68][ 7] = 5.116667;
bbcCutZDep[69][ 7] = 4.650000;
bbcCutZDep[70][ 7] = 4.216667;
bbcCutZDep[71][ 7] = 3.783333;
bbcCutZDep[72][ 7] = 3.316667;
bbcCutZDep[73][ 7] = 2.850000;
bbcCutZDep[74][ 7] = 2.383333;
bbcCutZDep[75][ 7] = 1.883333;
bbcCutZDep[76][ 7] = 1.350000;
bbcCutZDep[77][ 7] = 0.916667;
bbcCutZDep[78][ 7] = -0.500000;
bbcCutZDep[ 0][ 8] = 999.983337;
bbcCutZDep[ 1][ 8] = 70.349998;
bbcCutZDep[ 2][ 8] = 62.483334;
bbcCutZDep[ 3][ 8] = 57.650002;
bbcCutZDep[ 4][ 8] = 53.783333;
bbcCutZDep[ 5][ 8] = 50.783333;
bbcCutZDep[ 6][ 8] = 48.316666;
bbcCutZDep[ 7][ 8] = 46.383335;
bbcCutZDep[ 8][ 8] = 44.583332;
bbcCutZDep[ 9][ 8] = 42.950001;
bbcCutZDep[10][ 8] = 41.450001;
bbcCutZDep[11][ 8] = 40.116665;
bbcCutZDep[12][ 8] = 38.849998;
bbcCutZDep[13][ 8] = 37.683334;
bbcCutZDep[14][ 8] = 36.516666;
bbcCutZDep[15][ 8] = 35.416668;
bbcCutZDep[16][ 8] = 34.383335;
bbcCutZDep[17][ 8] = 33.383335;
bbcCutZDep[18][ 8] = 32.416668;
bbcCutZDep[19][ 8] = 31.516666;
bbcCutZDep[20][ 8] = 30.716667;
bbcCutZDep[21][ 8] = 29.916666;
bbcCutZDep[22][ 8] = 29.150000;
bbcCutZDep[23][ 8] = 28.416666;
bbcCutZDep[24][ 8] = 27.650000;
bbcCutZDep[25][ 8] = 26.916666;
bbcCutZDep[26][ 8] = 26.250000;
bbcCutZDep[27][ 8] = 25.549999;
bbcCutZDep[28][ 8] = 24.883333;
bbcCutZDep[29][ 8] = 24.183332;
bbcCutZDep[30][ 8] = 23.516666;
bbcCutZDep[31][ 8] = 22.883333;
bbcCutZDep[32][ 8] = 22.250000;
bbcCutZDep[33][ 8] = 21.650000;
bbcCutZDep[34][ 8] = 21.049999;
bbcCutZDep[35][ 8] = 20.483334;
bbcCutZDep[36][ 8] = 19.916666;
bbcCutZDep[37][ 8] = 19.416666;
bbcCutZDep[38][ 8] = 18.883333;
bbcCutZDep[39][ 8] = 18.350000;
bbcCutZDep[40][ 8] = 17.816668;
bbcCutZDep[41][ 8] = 17.316668;
bbcCutZDep[42][ 8] = 16.816668;
bbcCutZDep[43][ 8] = 16.350000;
bbcCutZDep[44][ 8] = 15.850000;
bbcCutZDep[45][ 8] = 15.350000;
bbcCutZDep[46][ 8] = 14.850000;
bbcCutZDep[47][ 8] = 14.383333;
bbcCutZDep[48][ 8] = 13.916667;
bbcCutZDep[49][ 8] = 13.416667;
bbcCutZDep[50][ 8] = 12.950000;
bbcCutZDep[51][ 8] = 12.450000;
bbcCutZDep[52][ 8] = 11.983334;
bbcCutZDep[53][ 8] = 11.516666;
bbcCutZDep[54][ 8] = 11.050000;
bbcCutZDep[55][ 8] = 10.583333;
bbcCutZDep[56][ 8] = 10.150000;
bbcCutZDep[57][ 8] = 9.683333;
bbcCutZDep[58][ 8] = 9.250000;
bbcCutZDep[59][ 8] = 8.816667;
bbcCutZDep[60][ 8] = 8.383333;
bbcCutZDep[61][ 8] = 7.983333;
bbcCutZDep[62][ 8] = 7.516667;
bbcCutZDep[63][ 8] = 7.050000;
bbcCutZDep[64][ 8] = 6.650000;
bbcCutZDep[65][ 8] = 6.216667;
bbcCutZDep[66][ 8] = 5.783333;
bbcCutZDep[67][ 8] = 5.350000;
bbcCutZDep[68][ 8] = 4.950000;
bbcCutZDep[69][ 8] = 4.516667;
bbcCutZDep[70][ 8] = 4.050000;
bbcCutZDep[71][ 8] = 3.583333;
bbcCutZDep[72][ 8] = 3.150000;
bbcCutZDep[73][ 8] = 2.683333;
bbcCutZDep[74][ 8] = 2.250000;
bbcCutZDep[75][ 8] = 1.816667;
bbcCutZDep[76][ 8] = 1.316667;
bbcCutZDep[77][ 8] = 0.883333;
bbcCutZDep[78][ 8] = -0.500000;
bbcCutZDep[ 0][ 9] = 999.983337;
bbcCutZDep[ 1][ 9] = 69.750000;
bbcCutZDep[ 2][ 9] = 61.883335;
bbcCutZDep[ 3][ 9] = 57.349998;
bbcCutZDep[ 4][ 9] = 53.816666;
bbcCutZDep[ 5][ 9] = 50.849998;
bbcCutZDep[ 6][ 9] = 48.416668;
bbcCutZDep[ 7][ 9] = 46.216667;
bbcCutZDep[ 8][ 9] = 44.349998;
bbcCutZDep[ 9][ 9] = 42.683334;
bbcCutZDep[10][ 9] = 41.183334;
bbcCutZDep[11][ 9] = 39.816666;
bbcCutZDep[12][ 9] = 38.483334;
bbcCutZDep[13][ 9] = 37.283333;
bbcCutZDep[14][ 9] = 36.150002;
bbcCutZDep[15][ 9] = 35.116665;
bbcCutZDep[16][ 9] = 34.083332;
bbcCutZDep[17][ 9] = 33.150002;
bbcCutZDep[18][ 9] = 32.283333;
bbcCutZDep[19][ 9] = 31.416666;
bbcCutZDep[20][ 9] = 30.583334;
bbcCutZDep[21][ 9] = 29.750000;
bbcCutZDep[22][ 9] = 28.916666;
bbcCutZDep[23][ 9] = 28.183332;
bbcCutZDep[24][ 9] = 27.483334;
bbcCutZDep[25][ 9] = 26.783333;
bbcCutZDep[26][ 9] = 26.116667;
bbcCutZDep[27][ 9] = 25.450001;
bbcCutZDep[28][ 9] = 24.750000;
bbcCutZDep[29][ 9] = 24.116667;
bbcCutZDep[30][ 9] = 23.450001;
bbcCutZDep[31][ 9] = 22.883333;
bbcCutZDep[32][ 9] = 22.250000;
bbcCutZDep[33][ 9] = 21.616667;
bbcCutZDep[34][ 9] = 21.016666;
bbcCutZDep[35][ 9] = 20.416666;
bbcCutZDep[36][ 9] = 19.883333;
bbcCutZDep[37][ 9] = 19.350000;
bbcCutZDep[38][ 9] = 18.816668;
bbcCutZDep[39][ 9] = 18.283333;
bbcCutZDep[40][ 9] = 17.750000;
bbcCutZDep[41][ 9] = 17.283333;
bbcCutZDep[42][ 9] = 16.750000;
bbcCutZDep[43][ 9] = 16.283333;
bbcCutZDep[44][ 9] = 15.783334;
bbcCutZDep[45][ 9] = 15.316667;
bbcCutZDep[46][ 9] = 14.816667;
bbcCutZDep[47][ 9] = 14.350000;
bbcCutZDep[48][ 9] = 13.883333;
bbcCutZDep[49][ 9] = 13.416667;
bbcCutZDep[50][ 9] = 12.916667;
bbcCutZDep[51][ 9] = 12.450000;
bbcCutZDep[52][ 9] = 12.016666;
bbcCutZDep[53][ 9] = 11.550000;
bbcCutZDep[54][ 9] = 11.116667;
bbcCutZDep[55][ 9] = 10.683333;
bbcCutZDep[56][ 9] = 10.250000;
bbcCutZDep[57][ 9] = 9.783334;
bbcCutZDep[58][ 9] = 9.350000;
bbcCutZDep[59][ 9] = 8.916667;
bbcCutZDep[60][ 9] = 8.483334;
bbcCutZDep[61][ 9] = 8.050000;
bbcCutZDep[62][ 9] = 7.616667;
bbcCutZDep[63][ 9] = 7.150000;
bbcCutZDep[64][ 9] = 6.716667;
bbcCutZDep[65][ 9] = 6.250000;
bbcCutZDep[66][ 9] = 5.816667;
bbcCutZDep[67][ 9] = 5.383333;
bbcCutZDep[68][ 9] = 4.950000;
bbcCutZDep[69][ 9] = 4.516667;
bbcCutZDep[70][ 9] = 4.083333;
bbcCutZDep[71][ 9] = 3.683333;
bbcCutZDep[72][ 9] = 3.250000;
bbcCutZDep[73][ 9] = 2.816667;
bbcCutZDep[74][ 9] = 2.316667;
bbcCutZDep[75][ 9] = 1.850000;
bbcCutZDep[76][ 9] = 1.350000;
bbcCutZDep[77][ 9] = 0.883333;
bbcCutZDep[78][ 9] = -0.500000;
bbcCutZDep[ 0][10] = 999.983337;
bbcCutZDep[ 1][10] = 69.883331;
bbcCutZDep[ 2][10] = 61.716667;
bbcCutZDep[ 3][10] = 57.049999;
bbcCutZDep[ 4][10] = 53.716667;
bbcCutZDep[ 5][10] = 50.849998;
bbcCutZDep[ 6][10] = 48.483334;
bbcCutZDep[ 7][10] = 46.316666;
bbcCutZDep[ 8][10] = 44.349998;
bbcCutZDep[ 9][10] = 42.716667;
bbcCutZDep[10][10] = 41.250000;
bbcCutZDep[11][10] = 39.783333;
bbcCutZDep[12][10] = 38.516666;
bbcCutZDep[13][10] = 37.316666;
bbcCutZDep[14][10] = 36.183334;
bbcCutZDep[15][10] = 35.183334;
bbcCutZDep[16][10] = 34.183334;
bbcCutZDep[17][10] = 33.183334;
bbcCutZDep[18][10] = 32.283333;
bbcCutZDep[19][10] = 31.416666;
bbcCutZDep[20][10] = 30.549999;
bbcCutZDep[21][10] = 29.750000;
bbcCutZDep[22][10] = 28.983334;
bbcCutZDep[23][10] = 28.183332;
bbcCutZDep[24][10] = 27.450001;
bbcCutZDep[25][10] = 26.716667;
bbcCutZDep[26][10] = 26.049999;
bbcCutZDep[27][10] = 25.383333;
bbcCutZDep[28][10] = 24.716667;
bbcCutZDep[29][10] = 24.116667;
bbcCutZDep[30][10] = 23.416666;
bbcCutZDep[31][10] = 22.850000;
bbcCutZDep[32][10] = 22.216667;
bbcCutZDep[33][10] = 21.616667;
bbcCutZDep[34][10] = 21.049999;
bbcCutZDep[35][10] = 20.483334;
bbcCutZDep[36][10] = 19.916666;
bbcCutZDep[37][10] = 19.383333;
bbcCutZDep[38][10] = 18.816668;
bbcCutZDep[39][10] = 18.283333;
bbcCutZDep[40][10] = 17.750000;
bbcCutZDep[41][10] = 17.250000;
bbcCutZDep[42][10] = 16.750000;
bbcCutZDep[43][10] = 16.250000;
bbcCutZDep[44][10] = 15.750000;
bbcCutZDep[45][10] = 15.283334;
bbcCutZDep[46][10] = 14.783334;
bbcCutZDep[47][10] = 14.283334;
bbcCutZDep[48][10] = 13.816667;
bbcCutZDep[49][10] = 13.350000;
bbcCutZDep[50][10] = 12.850000;
bbcCutZDep[51][10] = 12.383333;
bbcCutZDep[52][10] = 11.950000;
bbcCutZDep[53][10] = 11.483334;
bbcCutZDep[54][10] = 11.016666;
bbcCutZDep[55][10] = 10.616667;
bbcCutZDep[56][10] = 10.150000;
bbcCutZDep[57][10] = 9.716666;
bbcCutZDep[58][10] = 9.250000;
bbcCutZDep[59][10] = 8.783334;
bbcCutZDep[60][10] = 8.350000;
bbcCutZDep[61][10] = 7.916667;
bbcCutZDep[62][10] = 7.516667;
bbcCutZDep[63][10] = 7.050000;
bbcCutZDep[64][10] = 6.650000;
bbcCutZDep[65][10] = 6.183333;
bbcCutZDep[66][10] = 5.750000;
bbcCutZDep[67][10] = 5.350000;
bbcCutZDep[68][10] = 4.950000;
bbcCutZDep[69][10] = 4.516667;
bbcCutZDep[70][10] = 4.083333;
bbcCutZDep[71][10] = 3.650000;
bbcCutZDep[72][10] = 3.216667;
bbcCutZDep[73][10] = 2.750000;
bbcCutZDep[74][10] = 2.316667;
bbcCutZDep[75][10] = 1.850000;
bbcCutZDep[76][10] = 1.350000;
bbcCutZDep[77][10] = 0.883333;
bbcCutZDep[78][10] = -0.500000;
bbcCutZDep[ 0][11] = 999.983337;
bbcCutZDep[ 1][11] = 69.516670;
bbcCutZDep[ 2][11] = 61.616665;
bbcCutZDep[ 3][11] = 56.816666;
bbcCutZDep[ 4][11] = 53.216667;
bbcCutZDep[ 5][11] = 50.349998;
bbcCutZDep[ 6][11] = 47.916668;
bbcCutZDep[ 7][11] = 45.916668;
bbcCutZDep[ 8][11] = 44.216667;
bbcCutZDep[ 9][11] = 42.549999;
bbcCutZDep[10][11] = 41.049999;
bbcCutZDep[11][11] = 39.716667;
bbcCutZDep[12][11] = 38.416668;
bbcCutZDep[13][11] = 37.216667;
bbcCutZDep[14][11] = 36.150002;
bbcCutZDep[15][11] = 35.116665;
bbcCutZDep[16][11] = 34.116665;
bbcCutZDep[17][11] = 33.116665;
bbcCutZDep[18][11] = 32.250000;
bbcCutZDep[19][11] = 31.316668;
bbcCutZDep[20][11] = 30.483334;
bbcCutZDep[21][11] = 29.683332;
bbcCutZDep[22][11] = 28.883333;
bbcCutZDep[23][11] = 28.116667;
bbcCutZDep[24][11] = 27.383333;
bbcCutZDep[25][11] = 26.616667;
bbcCutZDep[26][11] = 25.916666;
bbcCutZDep[27][11] = 25.216667;
bbcCutZDep[28][11] = 24.583334;
bbcCutZDep[29][11] = 23.883333;
bbcCutZDep[30][11] = 23.250000;
bbcCutZDep[31][11] = 22.683332;
bbcCutZDep[32][11] = 22.049999;
bbcCutZDep[33][11] = 21.450001;
bbcCutZDep[34][11] = 20.883333;
bbcCutZDep[35][11] = 20.350000;
bbcCutZDep[36][11] = 19.783333;
bbcCutZDep[37][11] = 19.250000;
bbcCutZDep[38][11] = 18.683332;
bbcCutZDep[39][11] = 18.150000;
bbcCutZDep[40][11] = 17.616667;
bbcCutZDep[41][11] = 17.150000;
bbcCutZDep[42][11] = 16.616667;
bbcCutZDep[43][11] = 16.116667;
bbcCutZDep[44][11] = 15.616667;
bbcCutZDep[45][11] = 15.150000;
bbcCutZDep[46][11] = 14.683333;
bbcCutZDep[47][11] = 14.216666;
bbcCutZDep[48][11] = 13.750000;
bbcCutZDep[49][11] = 13.283334;
bbcCutZDep[50][11] = 12.816667;
bbcCutZDep[51][11] = 12.383333;
bbcCutZDep[52][11] = 11.916667;
bbcCutZDep[53][11] = 11.483334;
bbcCutZDep[54][11] = 11.016666;
bbcCutZDep[55][11] = 10.583333;
bbcCutZDep[56][11] = 10.150000;
bbcCutZDep[57][11] = 9.683333;
bbcCutZDep[58][11] = 9.250000;
bbcCutZDep[59][11] = 8.816667;
bbcCutZDep[60][11] = 8.383333;
bbcCutZDep[61][11] = 7.916667;
bbcCutZDep[62][11] = 7.483333;
bbcCutZDep[63][11] = 7.016667;
bbcCutZDep[64][11] = 6.616667;
bbcCutZDep[65][11] = 6.183333;
bbcCutZDep[66][11] = 5.750000;
bbcCutZDep[67][11] = 5.316667;
bbcCutZDep[68][11] = 4.850000;
bbcCutZDep[69][11] = 4.416667;
bbcCutZDep[70][11] = 3.983333;
bbcCutZDep[71][11] = 3.550000;
bbcCutZDep[72][11] = 3.150000;
bbcCutZDep[73][11] = 2.683333;
bbcCutZDep[74][11] = 2.250000;
bbcCutZDep[75][11] = 1.816667;
bbcCutZDep[76][11] = 1.283333;
bbcCutZDep[77][11] = 0.883333;
bbcCutZDep[78][11] = -0.500000;
bbcCutZDep[ 0][12] = 999.983337;
bbcCutZDep[ 1][12] = 68.949997;
bbcCutZDep[ 2][12] = 61.083332;
bbcCutZDep[ 3][12] = 56.450001;
bbcCutZDep[ 4][12] = 53.049999;
bbcCutZDep[ 5][12] = 50.116665;
bbcCutZDep[ 6][12] = 47.683334;
bbcCutZDep[ 7][12] = 45.716667;
bbcCutZDep[ 8][12] = 43.916668;
bbcCutZDep[ 9][12] = 42.283333;
bbcCutZDep[10][12] = 40.816666;
bbcCutZDep[11][12] = 39.450001;
bbcCutZDep[12][12] = 38.216667;
bbcCutZDep[13][12] = 37.083332;
bbcCutZDep[14][12] = 36.016666;
bbcCutZDep[15][12] = 34.916668;
bbcCutZDep[16][12] = 33.950001;
bbcCutZDep[17][12] = 32.983334;
bbcCutZDep[18][12] = 32.116665;
bbcCutZDep[19][12] = 31.216667;
bbcCutZDep[20][12] = 30.416666;
bbcCutZDep[21][12] = 29.616667;
bbcCutZDep[22][12] = 28.883333;
bbcCutZDep[23][12] = 28.116667;
bbcCutZDep[24][12] = 27.383333;
bbcCutZDep[25][12] = 26.683332;
bbcCutZDep[26][12] = 25.983334;
bbcCutZDep[27][12] = 25.283333;
bbcCutZDep[28][12] = 24.616667;
bbcCutZDep[29][12] = 24.016666;
bbcCutZDep[30][12] = 23.383333;
bbcCutZDep[31][12] = 22.783333;
bbcCutZDep[32][12] = 22.183332;
bbcCutZDep[33][12] = 21.583334;
bbcCutZDep[34][12] = 21.049999;
bbcCutZDep[35][12] = 20.450001;
bbcCutZDep[36][12] = 19.916666;
bbcCutZDep[37][12] = 19.350000;
bbcCutZDep[38][12] = 18.783333;
bbcCutZDep[39][12] = 18.283333;
bbcCutZDep[40][12] = 17.783333;
bbcCutZDep[41][12] = 17.283333;
bbcCutZDep[42][12] = 16.750000;
bbcCutZDep[43][12] = 16.250000;
bbcCutZDep[44][12] = 15.750000;
bbcCutZDep[45][12] = 15.283334;
bbcCutZDep[46][12] = 14.750000;
bbcCutZDep[47][12] = 14.283334;
bbcCutZDep[48][12] = 13.850000;
bbcCutZDep[49][12] = 13.383333;
bbcCutZDep[50][12] = 12.916667;
bbcCutZDep[51][12] = 12.450000;
bbcCutZDep[52][12] = 11.950000;
bbcCutZDep[53][12] = 11.483334;
bbcCutZDep[54][12] = 11.016666;
bbcCutZDep[55][12] = 10.550000;
bbcCutZDep[56][12] = 10.083333;
bbcCutZDep[57][12] = 9.650000;
bbcCutZDep[58][12] = 9.250000;
bbcCutZDep[59][12] = 8.816667;
bbcCutZDep[60][12] = 8.383333;
bbcCutZDep[61][12] = 7.950000;
bbcCutZDep[62][12] = 7.516667;
bbcCutZDep[63][12] = 7.116667;
bbcCutZDep[64][12] = 6.683333;
bbcCutZDep[65][12] = 6.250000;
bbcCutZDep[66][12] = 5.783333;
bbcCutZDep[67][12] = 5.350000;
bbcCutZDep[68][12] = 4.950000;
bbcCutZDep[69][12] = 4.550000;
bbcCutZDep[70][12] = 4.083333;
bbcCutZDep[71][12] = 3.650000;
bbcCutZDep[72][12] = 3.183333;
bbcCutZDep[73][12] = 2.750000;
bbcCutZDep[74][12] = 2.283333;
bbcCutZDep[75][12] = 1.850000;
bbcCutZDep[76][12] = 1.316667;
bbcCutZDep[77][12] = 0.883333;
bbcCutZDep[78][12] = -0.500000;
bbcCutZDep[ 0][13] = 999.983337;
bbcCutZDep[ 1][13] = 69.283333;
bbcCutZDep[ 2][13] = 61.450001;
bbcCutZDep[ 3][13] = 56.516666;
bbcCutZDep[ 4][13] = 53.049999;
bbcCutZDep[ 5][13] = 50.116665;
bbcCutZDep[ 6][13] = 47.783333;
bbcCutZDep[ 7][13] = 45.750000;
bbcCutZDep[ 8][13] = 43.849998;
bbcCutZDep[ 9][13] = 42.250000;
bbcCutZDep[10][13] = 40.783333;
bbcCutZDep[11][13] = 39.483334;
bbcCutZDep[12][13] = 38.216667;
bbcCutZDep[13][13] = 37.049999;
bbcCutZDep[14][13] = 35.950001;
bbcCutZDep[15][13] = 34.916668;
bbcCutZDep[16][13] = 33.883335;
bbcCutZDep[17][13] = 32.950001;
bbcCutZDep[18][13] = 32.016666;
bbcCutZDep[19][13] = 31.183332;
bbcCutZDep[20][13] = 30.450001;
bbcCutZDep[21][13] = 29.616667;
bbcCutZDep[22][13] = 28.783333;
bbcCutZDep[23][13] = 28.049999;
bbcCutZDep[24][13] = 27.250000;
bbcCutZDep[25][13] = 26.516666;
bbcCutZDep[26][13] = 25.783333;
bbcCutZDep[27][13] = 25.150000;
bbcCutZDep[28][13] = 24.483334;
bbcCutZDep[29][13] = 23.850000;
bbcCutZDep[30][13] = 23.216667;
bbcCutZDep[31][13] = 22.616667;
bbcCutZDep[32][13] = 21.983334;
bbcCutZDep[33][13] = 21.450001;
bbcCutZDep[34][13] = 20.850000;
bbcCutZDep[35][13] = 20.283333;
bbcCutZDep[36][13] = 19.683332;
bbcCutZDep[37][13] = 19.150000;
bbcCutZDep[38][13] = 18.616667;
bbcCutZDep[39][13] = 18.116667;
bbcCutZDep[40][13] = 17.616667;
bbcCutZDep[41][13] = 17.116667;
bbcCutZDep[42][13] = 16.616667;
bbcCutZDep[43][13] = 16.116667;
bbcCutZDep[44][13] = 15.616667;
bbcCutZDep[45][13] = 15.116667;
bbcCutZDep[46][13] = 14.650000;
bbcCutZDep[47][13] = 14.183333;
bbcCutZDep[48][13] = 13.716666;
bbcCutZDep[49][13] = 13.250000;
bbcCutZDep[50][13] = 12.816667;
bbcCutZDep[51][13] = 12.350000;
bbcCutZDep[52][13] = 11.916667;
bbcCutZDep[53][13] = 11.483334;
bbcCutZDep[54][13] = 11.050000;
bbcCutZDep[55][13] = 10.583333;
bbcCutZDep[56][13] = 10.150000;
bbcCutZDep[57][13] = 9.716666;
bbcCutZDep[58][13] = 9.250000;
bbcCutZDep[59][13] = 8.850000;
bbcCutZDep[60][13] = 8.416667;
bbcCutZDep[61][13] = 7.950000;
bbcCutZDep[62][13] = 7.516667;
bbcCutZDep[63][13] = 7.083333;
bbcCutZDep[64][13] = 6.650000;
bbcCutZDep[65][13] = 6.216667;
bbcCutZDep[66][13] = 5.783333;
bbcCutZDep[67][13] = 5.383333;
bbcCutZDep[68][13] = 4.916667;
bbcCutZDep[69][13] = 4.483333;
bbcCutZDep[70][13] = 4.050000;
bbcCutZDep[71][13] = 3.616667;
bbcCutZDep[72][13] = 3.183333;
bbcCutZDep[73][13] = 2.716667;
bbcCutZDep[74][13] = 2.250000;
bbcCutZDep[75][13] = 1.816667;
bbcCutZDep[76][13] = 1.316667;
bbcCutZDep[77][13] = 0.883333;
bbcCutZDep[78][13] = -0.500000;
bbcCutZDep[ 0][14] = 999.983337;
bbcCutZDep[ 1][14] = 69.250000;
bbcCutZDep[ 2][14] = 61.083332;
bbcCutZDep[ 3][14] = 56.316666;
bbcCutZDep[ 4][14] = 52.849998;
bbcCutZDep[ 5][14] = 49.983334;
bbcCutZDep[ 6][14] = 47.783333;
bbcCutZDep[ 7][14] = 45.683334;
bbcCutZDep[ 8][14] = 43.983334;
bbcCutZDep[ 9][14] = 42.383335;
bbcCutZDep[10][14] = 40.883335;
bbcCutZDep[11][14] = 39.616665;
bbcCutZDep[12][14] = 38.250000;
bbcCutZDep[13][14] = 37.049999;
bbcCutZDep[14][14] = 35.950001;
bbcCutZDep[15][14] = 34.950001;
bbcCutZDep[16][14] = 33.883335;
bbcCutZDep[17][14] = 32.916668;
bbcCutZDep[18][14] = 32.016666;
bbcCutZDep[19][14] = 31.183332;
bbcCutZDep[20][14] = 30.383333;
bbcCutZDep[21][14] = 29.616667;
bbcCutZDep[22][14] = 28.850000;
bbcCutZDep[23][14] = 28.049999;
bbcCutZDep[24][14] = 27.250000;
bbcCutZDep[25][14] = 26.549999;
bbcCutZDep[26][14] = 25.816668;
bbcCutZDep[27][14] = 25.150000;
bbcCutZDep[28][14] = 24.483334;
bbcCutZDep[29][14] = 23.816668;
bbcCutZDep[30][14] = 23.216667;
bbcCutZDep[31][14] = 22.616667;
bbcCutZDep[32][14] = 21.983334;
bbcCutZDep[33][14] = 21.416666;
bbcCutZDep[34][14] = 20.816668;
bbcCutZDep[35][14] = 20.216667;
bbcCutZDep[36][14] = 19.650000;
bbcCutZDep[37][14] = 19.083334;
bbcCutZDep[38][14] = 18.583334;
bbcCutZDep[39][14] = 18.049999;
bbcCutZDep[40][14] = 17.516666;
bbcCutZDep[41][14] = 17.016666;
bbcCutZDep[42][14] = 16.516666;
bbcCutZDep[43][14] = 16.016666;
bbcCutZDep[44][14] = 15.516666;
bbcCutZDep[45][14] = 14.983334;
bbcCutZDep[46][14] = 14.483334;
bbcCutZDep[47][14] = 14.016666;
bbcCutZDep[48][14] = 13.550000;
bbcCutZDep[49][14] = 13.083333;
bbcCutZDep[50][14] = 12.616667;
bbcCutZDep[51][14] = 12.150000;
bbcCutZDep[52][14] = 11.683333;
bbcCutZDep[53][14] = 11.250000;
bbcCutZDep[54][14] = 10.816667;
bbcCutZDep[55][14] = 10.416667;
bbcCutZDep[56][14] = 10.016666;
bbcCutZDep[57][14] = 9.583333;
bbcCutZDep[58][14] = 9.183333;
bbcCutZDep[59][14] = 8.750000;
bbcCutZDep[60][14] = 8.316667;
bbcCutZDep[61][14] = 7.850000;
bbcCutZDep[62][14] = 7.450000;
bbcCutZDep[63][14] = 7.016667;
bbcCutZDep[64][14] = 6.616667;
bbcCutZDep[65][14] = 6.183333;
bbcCutZDep[66][14] = 5.750000;
bbcCutZDep[67][14] = 5.316667;
bbcCutZDep[68][14] = 4.883333;
bbcCutZDep[69][14] = 4.450000;
bbcCutZDep[70][14] = 3.983333;
bbcCutZDep[71][14] = 3.583333;
bbcCutZDep[72][14] = 3.150000;
bbcCutZDep[73][14] = 2.683333;
bbcCutZDep[74][14] = 2.250000;
bbcCutZDep[75][14] = 1.816667;
bbcCutZDep[76][14] = 1.316667;
bbcCutZDep[77][14] = 0.883333;
bbcCutZDep[78][14] = -0.500000;
bbcCutZDep[ 0][15] = 999.983337;
bbcCutZDep[ 1][15] = 68.883331;
bbcCutZDep[ 2][15] = 61.150002;
bbcCutZDep[ 3][15] = 56.549999;
bbcCutZDep[ 4][15] = 53.083332;
bbcCutZDep[ 5][15] = 50.250000;
bbcCutZDep[ 6][15] = 47.783333;
bbcCutZDep[ 7][15] = 45.849998;
bbcCutZDep[ 8][15] = 44.016666;
bbcCutZDep[ 9][15] = 42.349998;
bbcCutZDep[10][15] = 40.816666;
bbcCutZDep[11][15] = 39.383335;
bbcCutZDep[12][15] = 38.150002;
bbcCutZDep[13][15] = 37.049999;
bbcCutZDep[14][15] = 35.950001;
bbcCutZDep[15][15] = 34.883335;
bbcCutZDep[16][15] = 33.916668;
bbcCutZDep[17][15] = 32.950001;
bbcCutZDep[18][15] = 32.049999;
bbcCutZDep[19][15] = 31.150000;
bbcCutZDep[20][15] = 30.283333;
bbcCutZDep[21][15] = 29.516666;
bbcCutZDep[22][15] = 28.716667;
bbcCutZDep[23][15] = 27.983334;
bbcCutZDep[24][15] = 27.183332;
bbcCutZDep[25][15] = 26.516666;
bbcCutZDep[26][15] = 25.816668;
bbcCutZDep[27][15] = 25.183332;
bbcCutZDep[28][15] = 24.516666;
bbcCutZDep[29][15] = 23.816668;
bbcCutZDep[30][15] = 23.216667;
bbcCutZDep[31][15] = 22.616667;
bbcCutZDep[32][15] = 22.016666;
bbcCutZDep[33][15] = 21.416666;
bbcCutZDep[34][15] = 20.883333;
bbcCutZDep[35][15] = 20.350000;
bbcCutZDep[36][15] = 19.783333;
bbcCutZDep[37][15] = 19.216667;
bbcCutZDep[38][15] = 18.683332;
bbcCutZDep[39][15] = 18.150000;
bbcCutZDep[40][15] = 17.650000;
bbcCutZDep[41][15] = 17.116667;
bbcCutZDep[42][15] = 16.616667;
bbcCutZDep[43][15] = 16.083334;
bbcCutZDep[44][15] = 15.616667;
bbcCutZDep[45][15] = 15.116667;
bbcCutZDep[46][15] = 14.650000;
bbcCutZDep[47][15] = 14.150000;
bbcCutZDep[48][15] = 13.650000;
bbcCutZDep[49][15] = 13.183333;
bbcCutZDep[50][15] = 12.716666;
bbcCutZDep[51][15] = 12.250000;
bbcCutZDep[52][15] = 11.816667;
bbcCutZDep[53][15] = 11.350000;
bbcCutZDep[54][15] = 10.916667;
bbcCutZDep[55][15] = 10.450000;
bbcCutZDep[56][15] = 10.050000;
bbcCutZDep[57][15] = 9.616667;
bbcCutZDep[58][15] = 9.183333;
bbcCutZDep[59][15] = 8.783334;
bbcCutZDep[60][15] = 8.350000;
bbcCutZDep[61][15] = 7.883333;
bbcCutZDep[62][15] = 7.450000;
bbcCutZDep[63][15] = 7.016667;
bbcCutZDep[64][15] = 6.550000;
bbcCutZDep[65][15] = 6.150000;
bbcCutZDep[66][15] = 5.750000;
bbcCutZDep[67][15] = 5.350000;
bbcCutZDep[68][15] = 4.916667;
bbcCutZDep[69][15] = 4.483333;
bbcCutZDep[70][15] = 4.016667;
bbcCutZDep[71][15] = 3.583333;
bbcCutZDep[72][15] = 3.150000;
bbcCutZDep[73][15] = 2.716667;
bbcCutZDep[74][15] = 2.250000;
bbcCutZDep[75][15] = 1.783333;
bbcCutZDep[76][15] = 1.283333;
bbcCutZDep[77][15] = 0.850000;
bbcCutZDep[78][15] = -0.500000;
bbcCutZDep[ 0][16] = 999.983337;
bbcCutZDep[ 1][16] = 70.016670;
bbcCutZDep[ 2][16] = 62.049999;
bbcCutZDep[ 3][16] = 56.950001;
bbcCutZDep[ 4][16] = 53.583332;
bbcCutZDep[ 5][16] = 50.549999;
bbcCutZDep[ 6][16] = 48.183334;
bbcCutZDep[ 7][16] = 46.016666;
bbcCutZDep[ 8][16] = 44.183334;
bbcCutZDep[ 9][16] = 42.383335;
bbcCutZDep[10][16] = 41.016666;
bbcCutZDep[11][16] = 39.650002;
bbcCutZDep[12][16] = 38.349998;
bbcCutZDep[13][16] = 37.216667;
bbcCutZDep[14][16] = 36.083332;
bbcCutZDep[15][16] = 34.983334;
bbcCutZDep[16][16] = 34.016666;
bbcCutZDep[17][16] = 33.083332;
bbcCutZDep[18][16] = 32.150002;
bbcCutZDep[19][16] = 31.250000;
bbcCutZDep[20][16] = 30.450001;
bbcCutZDep[21][16] = 29.616667;
bbcCutZDep[22][16] = 28.783333;
bbcCutZDep[23][16] = 28.016666;
bbcCutZDep[24][16] = 27.250000;
bbcCutZDep[25][16] = 26.549999;
bbcCutZDep[26][16] = 25.850000;
bbcCutZDep[27][16] = 25.183332;
bbcCutZDep[28][16] = 24.483334;
bbcCutZDep[29][16] = 23.883333;
bbcCutZDep[30][16] = 23.283333;
bbcCutZDep[31][16] = 22.683332;
bbcCutZDep[32][16] = 22.016666;
bbcCutZDep[33][16] = 21.416666;
bbcCutZDep[34][16] = 20.850000;
bbcCutZDep[35][16] = 20.316668;
bbcCutZDep[36][16] = 19.750000;
bbcCutZDep[37][16] = 19.216667;
bbcCutZDep[38][16] = 18.683332;
bbcCutZDep[39][16] = 18.150000;
bbcCutZDep[40][16] = 17.616667;
bbcCutZDep[41][16] = 17.083334;
bbcCutZDep[42][16] = 16.583334;
bbcCutZDep[43][16] = 16.049999;
bbcCutZDep[44][16] = 15.516666;
bbcCutZDep[45][16] = 15.016666;
bbcCutZDep[46][16] = 14.550000;
bbcCutZDep[47][16] = 14.083333;
bbcCutZDep[48][16] = 13.616667;
bbcCutZDep[49][16] = 13.183333;
bbcCutZDep[50][16] = 12.683333;
bbcCutZDep[51][16] = 12.216666;
bbcCutZDep[52][16] = 11.783334;
bbcCutZDep[53][16] = 11.316667;
bbcCutZDep[54][16] = 10.883333;
bbcCutZDep[55][16] = 10.416667;
bbcCutZDep[56][16] = 10.016666;
bbcCutZDep[57][16] = 9.583333;
bbcCutZDep[58][16] = 9.183333;
bbcCutZDep[59][16] = 8.716666;
bbcCutZDep[60][16] = 8.316667;
bbcCutZDep[61][16] = 7.883333;
bbcCutZDep[62][16] = 7.483333;
bbcCutZDep[63][16] = 7.083333;
bbcCutZDep[64][16] = 6.650000;
bbcCutZDep[65][16] = 6.216667;
bbcCutZDep[66][16] = 5.783333;
bbcCutZDep[67][16] = 5.350000;
bbcCutZDep[68][16] = 4.916667;
bbcCutZDep[69][16] = 4.483333;
bbcCutZDep[70][16] = 4.050000;
bbcCutZDep[71][16] = 3.616667;
bbcCutZDep[72][16] = 3.150000;
bbcCutZDep[73][16] = 2.683333;
bbcCutZDep[74][16] = 2.250000;
bbcCutZDep[75][16] = 1.816667;
bbcCutZDep[76][16] = 1.316667;
bbcCutZDep[77][16] = 0.883333;
bbcCutZDep[78][16] = -0.500000;
bbcCutZDep[ 0][17] = 999.983337;
bbcCutZDep[ 1][17] = 69.716667;
bbcCutZDep[ 2][17] = 61.650002;
bbcCutZDep[ 3][17] = 56.416668;
bbcCutZDep[ 4][17] = 52.983334;
bbcCutZDep[ 5][17] = 50.150002;
bbcCutZDep[ 6][17] = 47.783333;
bbcCutZDep[ 7][17] = 45.750000;
bbcCutZDep[ 8][17] = 44.016666;
bbcCutZDep[ 9][17] = 42.383335;
bbcCutZDep[10][17] = 41.016666;
bbcCutZDep[11][17] = 39.650002;
bbcCutZDep[12][17] = 38.416668;
bbcCutZDep[13][17] = 37.216667;
bbcCutZDep[14][17] = 36.116665;
bbcCutZDep[15][17] = 35.083332;
bbcCutZDep[16][17] = 34.049999;
bbcCutZDep[17][17] = 33.083332;
bbcCutZDep[18][17] = 32.150002;
bbcCutZDep[19][17] = 31.250000;
bbcCutZDep[20][17] = 30.416666;
bbcCutZDep[21][17] = 29.583334;
bbcCutZDep[22][17] = 28.816668;
bbcCutZDep[23][17] = 28.049999;
bbcCutZDep[24][17] = 27.316668;
bbcCutZDep[25][17] = 26.616667;
bbcCutZDep[26][17] = 25.883333;
bbcCutZDep[27][17] = 25.183332;
bbcCutZDep[28][17] = 24.516666;
bbcCutZDep[29][17] = 23.916666;
bbcCutZDep[30][17] = 23.283333;
bbcCutZDep[31][17] = 22.683332;
bbcCutZDep[32][17] = 22.116667;
bbcCutZDep[33][17] = 21.549999;
bbcCutZDep[34][17] = 20.950001;
bbcCutZDep[35][17] = 20.383333;
bbcCutZDep[36][17] = 19.816668;
bbcCutZDep[37][17] = 19.316668;
bbcCutZDep[38][17] = 18.750000;
bbcCutZDep[39][17] = 18.216667;
bbcCutZDep[40][17] = 17.683332;
bbcCutZDep[41][17] = 17.116667;
bbcCutZDep[42][17] = 16.616667;
bbcCutZDep[43][17] = 16.116667;
bbcCutZDep[44][17] = 15.683333;
bbcCutZDep[45][17] = 15.183333;
bbcCutZDep[46][17] = 14.683333;
bbcCutZDep[47][17] = 14.216666;
bbcCutZDep[48][17] = 13.750000;
bbcCutZDep[49][17] = 13.283334;
bbcCutZDep[50][17] = 12.850000;
bbcCutZDep[51][17] = 12.383333;
bbcCutZDep[52][17] = 11.883333;
bbcCutZDep[53][17] = 11.483334;
bbcCutZDep[54][17] = 11.016666;
bbcCutZDep[55][17] = 10.583333;
bbcCutZDep[56][17] = 10.116667;
bbcCutZDep[57][17] = 9.683333;
bbcCutZDep[58][17] = 9.250000;
bbcCutZDep[59][17] = 8.816667;
bbcCutZDep[60][17] = 8.350000;
bbcCutZDep[61][17] = 7.916667;
bbcCutZDep[62][17] = 7.516667;
bbcCutZDep[63][17] = 7.083333;
bbcCutZDep[64][17] = 6.650000;
bbcCutZDep[65][17] = 6.216667;
bbcCutZDep[66][17] = 5.783333;
bbcCutZDep[67][17] = 5.383333;
bbcCutZDep[68][17] = 4.950000;
bbcCutZDep[69][17] = 4.516667;
bbcCutZDep[70][17] = 4.050000;
bbcCutZDep[71][17] = 3.650000;
bbcCutZDep[72][17] = 3.183333;
bbcCutZDep[73][17] = 2.716667;
bbcCutZDep[74][17] = 2.283333;
bbcCutZDep[75][17] = 1.850000;
bbcCutZDep[76][17] = 1.350000;
bbcCutZDep[77][17] = 0.883333;
bbcCutZDep[78][17] = -0.500000;
bbcCutZDep[ 0][18] = 999.983337;
bbcCutZDep[ 1][18] = 69.683334;
bbcCutZDep[ 2][18] = 61.716667;
bbcCutZDep[ 3][18] = 57.150002;
bbcCutZDep[ 4][18] = 53.516666;
bbcCutZDep[ 5][18] = 50.683334;
bbcCutZDep[ 6][18] = 48.250000;
bbcCutZDep[ 7][18] = 46.183334;
bbcCutZDep[ 8][18] = 44.383335;
bbcCutZDep[ 9][18] = 42.683334;
bbcCutZDep[10][18] = 41.216667;
bbcCutZDep[11][18] = 39.849998;
bbcCutZDep[12][18] = 38.650002;
bbcCutZDep[13][18] = 37.450001;
bbcCutZDep[14][18] = 36.316666;
bbcCutZDep[15][18] = 35.183334;
bbcCutZDep[16][18] = 34.150002;
bbcCutZDep[17][18] = 33.216667;
bbcCutZDep[18][18] = 32.283333;
bbcCutZDep[19][18] = 31.416666;
bbcCutZDep[20][18] = 30.549999;
bbcCutZDep[21][18] = 29.750000;
bbcCutZDep[22][18] = 28.983334;
bbcCutZDep[23][18] = 28.216667;
bbcCutZDep[24][18] = 27.516666;
bbcCutZDep[25][18] = 26.816668;
bbcCutZDep[26][18] = 26.116667;
bbcCutZDep[27][18] = 25.450001;
bbcCutZDep[28][18] = 24.783333;
bbcCutZDep[29][18] = 24.116667;
bbcCutZDep[30][18] = 23.450001;
bbcCutZDep[31][18] = 22.883333;
bbcCutZDep[32][18] = 22.283333;
bbcCutZDep[33][18] = 21.650000;
bbcCutZDep[34][18] = 21.083334;
bbcCutZDep[35][18] = 20.549999;
bbcCutZDep[36][18] = 20.016666;
bbcCutZDep[37][18] = 19.516666;
bbcCutZDep[38][18] = 18.916666;
bbcCutZDep[39][18] = 18.383333;
bbcCutZDep[40][18] = 17.850000;
bbcCutZDep[41][18] = 17.316668;
bbcCutZDep[42][18] = 16.783333;
bbcCutZDep[43][18] = 16.283333;
bbcCutZDep[44][18] = 15.850000;
bbcCutZDep[45][18] = 15.316667;
bbcCutZDep[46][18] = 14.816667;
bbcCutZDep[47][18] = 14.350000;
bbcCutZDep[48][18] = 13.883333;
bbcCutZDep[49][18] = 13.450000;
bbcCutZDep[50][18] = 13.016666;
bbcCutZDep[51][18] = 12.550000;
bbcCutZDep[52][18] = 12.050000;
bbcCutZDep[53][18] = 11.616667;
bbcCutZDep[54][18] = 11.150000;
bbcCutZDep[55][18] = 10.683333;
bbcCutZDep[56][18] = 10.250000;
bbcCutZDep[57][18] = 9.783334;
bbcCutZDep[58][18] = 9.350000;
bbcCutZDep[59][18] = 8.883333;
bbcCutZDep[60][18] = 8.483334;
bbcCutZDep[61][18] = 8.016666;
bbcCutZDep[62][18] = 7.583333;
bbcCutZDep[63][18] = 7.150000;
bbcCutZDep[64][18] = 6.750000;
bbcCutZDep[65][18] = 6.316667;
bbcCutZDep[66][18] = 5.883333;
bbcCutZDep[67][18] = 5.450000;
bbcCutZDep[68][18] = 5.016667;
bbcCutZDep[69][18] = 4.583333;
bbcCutZDep[70][18] = 4.150000;
bbcCutZDep[71][18] = 3.716667;
bbcCutZDep[72][18] = 3.250000;
bbcCutZDep[73][18] = 2.816667;
bbcCutZDep[74][18] = 2.383333;
bbcCutZDep[75][18] = 1.916667;
bbcCutZDep[76][18] = 1.383333;
bbcCutZDep[77][18] = 0.916667;
bbcCutZDep[78][18] = -0.500000;
bbcCutZDep[ 0][19] = 999.983337;
bbcCutZDep[ 1][19] = 70.683334;
bbcCutZDep[ 2][19] = 62.250000;
bbcCutZDep[ 3][19] = 57.349998;
bbcCutZDep[ 4][19] = 53.750000;
bbcCutZDep[ 5][19] = 50.750000;
bbcCutZDep[ 6][19] = 48.450001;
bbcCutZDep[ 7][19] = 46.349998;
bbcCutZDep[ 8][19] = 44.516666;
bbcCutZDep[ 9][19] = 42.916668;
bbcCutZDep[10][19] = 41.483334;
bbcCutZDep[11][19] = 40.150002;
bbcCutZDep[12][19] = 38.783333;
bbcCutZDep[13][19] = 37.549999;
bbcCutZDep[14][19] = 36.416668;
bbcCutZDep[15][19] = 35.383335;
bbcCutZDep[16][19] = 34.316666;
bbcCutZDep[17][19] = 33.349998;
bbcCutZDep[18][19] = 32.416668;
bbcCutZDep[19][19] = 31.450001;
bbcCutZDep[20][19] = 30.616667;
bbcCutZDep[21][19] = 29.816668;
bbcCutZDep[22][19] = 29.083334;
bbcCutZDep[23][19] = 28.316668;
bbcCutZDep[24][19] = 27.583334;
bbcCutZDep[25][19] = 26.916666;
bbcCutZDep[26][19] = 26.183332;
bbcCutZDep[27][19] = 25.549999;
bbcCutZDep[28][19] = 24.883333;
bbcCutZDep[29][19] = 24.250000;
bbcCutZDep[30][19] = 23.616667;
bbcCutZDep[31][19] = 22.983334;
bbcCutZDep[32][19] = 22.383333;
bbcCutZDep[33][19] = 21.750000;
bbcCutZDep[34][19] = 21.216667;
bbcCutZDep[35][19] = 20.616667;
bbcCutZDep[36][19] = 20.016666;
bbcCutZDep[37][19] = 19.483334;
bbcCutZDep[38][19] = 18.950001;
bbcCutZDep[39][19] = 18.416666;
bbcCutZDep[40][19] = 17.883333;
bbcCutZDep[41][19] = 17.350000;
bbcCutZDep[42][19] = 16.850000;
bbcCutZDep[43][19] = 16.350000;
bbcCutZDep[44][19] = 15.850000;
bbcCutZDep[45][19] = 15.350000;
bbcCutZDep[46][19] = 14.850000;
bbcCutZDep[47][19] = 14.350000;
bbcCutZDep[48][19] = 13.916667;
bbcCutZDep[49][19] = 13.450000;
bbcCutZDep[50][19] = 12.983334;
bbcCutZDep[51][19] = 12.516666;
bbcCutZDep[52][19] = 12.050000;
bbcCutZDep[53][19] = 11.583333;
bbcCutZDep[54][19] = 11.150000;
bbcCutZDep[55][19] = 10.683333;
bbcCutZDep[56][19] = 10.250000;
bbcCutZDep[57][19] = 9.783334;
bbcCutZDep[58][19] = 9.350000;
bbcCutZDep[59][19] = 8.883333;
bbcCutZDep[60][19] = 8.450000;
bbcCutZDep[61][19] = 8.050000;
bbcCutZDep[62][19] = 7.616667;
bbcCutZDep[63][19] = 7.150000;
bbcCutZDep[64][19] = 6.716667;
bbcCutZDep[65][19] = 6.283333;
bbcCutZDep[66][19] = 5.850000;
bbcCutZDep[67][19] = 5.450000;
bbcCutZDep[68][19] = 5.016667;
bbcCutZDep[69][19] = 4.583333;
bbcCutZDep[70][19] = 4.116667;
bbcCutZDep[71][19] = 3.683333;
bbcCutZDep[72][19] = 3.216667;
bbcCutZDep[73][19] = 2.783333;
bbcCutZDep[74][19] = 2.316667;
bbcCutZDep[75][19] = 1.850000;
bbcCutZDep[76][19] = 1.350000;
bbcCutZDep[77][19] = 0.883333;
bbcCutZDep[78][19] = -0.500000;

}

void Run16dAu62GeVCentralityReco::InitScaleFactor ()
{

  // initializig to zero done in ctor

  // scale factors determined from <bbc-south charge> on a run-by-run basis

  // j.nagle - updated from online production with <bbcs>=23.4 on 6/21/2016
  // now complete from full taxi (note this was checked with BBC triggers to catch all runs) - but this is only relative 10/17/2016


BBCScaleFactor[0] = 1.021;
BBCScaleFactor[1] = 1.005;
BBCScaleFactor[2] = 1.002;
BBCScaleFactor[3] = 0.987;
BBCScaleFactor[40] = 1.003;
BBCScaleFactor[44] = 0.990;
BBCScaleFactor[45] = 0.984;
BBCScaleFactor[82] = 1.007;
BBCScaleFactor[83] = 1.001;
BBCScaleFactor[84] = 0.999;
BBCScaleFactor[85] = 0.996;
BBCScaleFactor[104] = 1.011;
BBCScaleFactor[105] = 1.007;
BBCScaleFactor[106] = 1.004;
BBCScaleFactor[117] = 1.019;
BBCScaleFactor[118] = 1.016;
BBCScaleFactor[119] = 1.017;
BBCScaleFactor[120] = 1.017;
BBCScaleFactor[122] = 1.028;
BBCScaleFactor[123] = 1.027;
BBCScaleFactor[124] = 1.027;
BBCScaleFactor[125] = 1.025;
BBCScaleFactor[126] = 1.030;
BBCScaleFactor[127] = 1.029;
BBCScaleFactor[158] = 1.039;
BBCScaleFactor[159] = 1.034;
BBCScaleFactor[160] = 1.034;
BBCScaleFactor[161] = 1.033;
BBCScaleFactor[162] = 1.030;
BBCScaleFactor[187] = 1.043;
BBCScaleFactor[188] = 1.043;
BBCScaleFactor[189] = 1.041;
BBCScaleFactor[190] = 1.043;
BBCScaleFactor[191] = 1.041;
BBCScaleFactor[193] = 1.041;
BBCScaleFactor[194] = 1.040;
BBCScaleFactor[195] = 1.037;
BBCScaleFactor[223] = 1.044;
BBCScaleFactor[224] = 1.048;
BBCScaleFactor[225] = 1.040;
BBCScaleFactor[229] = 1.036;
BBCScaleFactor[230] = 1.037;
BBCScaleFactor[231] = 1.026;
BBCScaleFactor[232] = 1.036;
BBCScaleFactor[233] = 1.037;
BBCScaleFactor[291] = 1.006;
BBCScaleFactor[292] = 1.005;
BBCScaleFactor[293] = 1.014;
BBCScaleFactor[294] = 1.009;
BBCScaleFactor[295] = 1.015;
BBCScaleFactor[296] = 1.013;
BBCScaleFactor[297] = 1.012;
BBCScaleFactor[298] = 1.016;
BBCScaleFactor[314] = 1.030;
BBCScaleFactor[315] = 1.027;
BBCScaleFactor[317] = 1.021;
BBCScaleFactor[318] = 1.030;
BBCScaleFactor[319] = 1.033;
BBCScaleFactor[320] = 1.030;
BBCScaleFactor[321] = 1.031;
BBCScaleFactor[349] = 1.045;
BBCScaleFactor[350] = 1.044;
BBCScaleFactor[352] = 1.046;
BBCScaleFactor[353] = 1.045;
BBCScaleFactor[408] = 1.056;
BBCScaleFactor[409] = 1.056;
BBCScaleFactor[411] = 1.051;
BBCScaleFactor[415] = 1.052;
BBCScaleFactor[416] = 1.048;
BBCScaleFactor[417] = 1.054;
BBCScaleFactor[419] = 1.045;
BBCScaleFactor[420] = 1.051;
BBCScaleFactor[421] = 1.041;
BBCScaleFactor[489] = 1.033;

  // set all missing values to the value of the run before it ....
  // now enter list of runs that were QA checked, but failed

  for (int i = 0; i < MAXRUN-MINRUN+1; i++)
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
Run16dAu62GeVCentralityReco::GetScaleFactor(const int runnumber) const
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
Run16dAu62GeVCentralityReco::GetQAStatus(const int runnumber) const
{

  if (! (runnumber >= MINRUN && runnumber <= MAXRUN))
    {
      cout << "Run16dAu62GeVCentralityReco::GetQAStatus - ERROR in run number range (Not a Run-16 dAu 62 GeV run number) = " << runnumber << endl;
      return 0;
    }

  int qastatus = QAstatus[runnumber - MINRUN];

  cout << "Run16dAu62GeVCentralityReco::GetQAStatus - Run " << runnumber << " Centrality QA = ";
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
