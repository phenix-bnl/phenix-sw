#include "Run16dAu39GeVCentralityReco.h"

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
static const int MINRUN = 457634; // first Run-16 d+Au @ 39 GeV physics run, June 10, 2016
static const int MAXRUN = 458167; // last run on JUne 17, 2016

Run16dAu39GeVCentralityReco::Run16dAu39GeVCentralityReco() : Recalibrator("Run16dAu39GeVCentralityReco")
{
  memset(bbcCutZDep, 0, sizeof(bbcCutZDep));
  memset(BBCScaleFactor, 0, sizeof(BBCScaleFactor));
  fill(QAstatus, QAstatus + sizeof(QAstatus) / sizeof(int), QAstatus_PASS);
  baseclasses.insert("PHGlobal");
  return ;
}

int
Run16dAu39GeVCentralityReco::isValidRun(const int runno) const
{
  if (runno < MINRUN || runno > MAXRUN)
    {
      return 0;
    }

  return 1;
}

int Run16dAu39GeVCentralityReco::Init(PHCompositeNode *topNode)
{

  help(); // just for test !!!
  InitArray1();
  InitScaleFactor();
  return 0;

}

void
Run16dAu39GeVCentralityReco::help() const
{
  cout << "===================================================================" << endl;
  cout << "Run16dAu39GeVCentralityReco::help method output"                    << endl;
  cout << "Author: J.L. Nagle (jamie.nagle@colorado.edu)." << endl;
  cout << "Comment: Run-16 dAu@39 GeV Centrality Using BBC Only uses 74% "             << endl;
  cout << "           of the inelastic cross section."                          << endl;
  cout << "         This method updates the value in PHGlobal for"              << endl;
  cout << "           BbcPercentile to go from 1 to 74."                        << endl;
  cout << "         This is applicable for runs " << MINRUN << " - " << MAXRUN                << endl;
  cout << "         NOTE: This is the final production version with BBC run-to-run rescaling"                << endl;
  cout << "         NOTE: The calibration has been performed in 1 cm wide z-vertex bins" << endl;
  cout << " Run16 dAu 39 GeV with minimum bias defined by FVTX_NS Level-1, an offline FVTX z-vertex and BBC Charge South > 0" << endl;
  cout << " The zvertex range is from -10 cm to 10 cm only, and the number of FVTX north + south clusters < 500 (to reomve background)" << endl;
  cout << "  The calibration is based upon the BBC south charge distribution." << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int
Run16dAu39GeVCentralityReco::process_event(PHCompositeNode *topNode)
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
Run16dAu39GeVCentralityReco::getCentralityByBBCRun16dAu(PHCompositeNode* topNode) const
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

  // look for FVTX z-vertex as the optimal (i.e. pick this if it exists, otherwise BBC)
  PHPoint fvtx_vertex = vertexes->get_Vertex("FVTX");
  float  FVTXz = fvtx_vertex.getZ();
  if(FVTXz!=FVTXz) {
    FVTXz=-9999;
    // if no FVTXz, then use BBC z-vertex
  } else {
    zvertex = FVTXz;  
  }

  int   runno   = run->get_RunNumber();

  // For d+Au @ 39 GeV, calibration is only for +/- 10 cm...
  // require a non-zero BBC charge south and FVTX zvertex within +/-10 cm
  bool isEventOK = fabs(zvertex) < 10; // +- 10 (cm)
  if (! (bbc2>0)) isEventOK = false;  // require non-zero charge in BBC south for min.bias definition!!!

  if(!isEventOK) return -999;

  return getCentralityByBBCRun16dAu(bbc1, bbc2, zdc1, zdc2, zvertex, runno);

}

int
Run16dAu39GeVCentralityReco::getCentralityByBBCRun16dAu(const float bbc1, const float bbc2, const float zdc1, const float zdc2, const float zvertex, const int runno) const
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
      cout << "Run16dAu39GeVCentralityReco::getCentralityByBBCRun16dAu - ERROR with run " <<
	runno << " and retrieved scale factor " << scalefactor << endl;
      return -999;
    }
  bbcsum = bbcsum / scalefactor;

  int centrality = -1;
  // use lookup table to determine centrality of this event using clock method
  for (int jjj = 0; jjj < 74; jjj++)
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
Run16dAu39GeVCentralityReco::InitArray1()
{

  // Lookup Table (BBC South Charge Cut Values) for Run16dAu39-Centrality
  // (-10<bbcz<10 cm, 1 cm step)
  // initial pass (j.nagle) from online production 6/22/2016
  // final taxi pass (j.nagle) from full production 10/5/2016

bbcCutZDep[ 0][ 0] = 999.983337;
bbcCutZDep[ 1][ 0] = 60.849998;
bbcCutZDep[ 2][ 0] = 54.416668;
bbcCutZDep[ 3][ 0] = 50.450001;
bbcCutZDep[ 4][ 0] = 47.516666;
bbcCutZDep[ 5][ 0] = 45.183334;
bbcCutZDep[ 6][ 0] = 43.183334;
bbcCutZDep[ 7][ 0] = 41.450001;
bbcCutZDep[ 7][ 0] = 41.450001;
bbcCutZDep[ 8][ 0] = 39.950001;
bbcCutZDep[ 9][ 0] = 38.549999;
bbcCutZDep[10][ 0] = 37.283333;
bbcCutZDep[11][ 0] = 36.116665;
bbcCutZDep[12][ 0] = 35.049999;
bbcCutZDep[13][ 0] = 34.016666;
bbcCutZDep[14][ 0] = 33.049999;
bbcCutZDep[15][ 0] = 32.150002;
bbcCutZDep[16][ 0] = 31.283333;
bbcCutZDep[17][ 0] = 30.450001;
bbcCutZDep[18][ 0] = 29.650000;
bbcCutZDep[19][ 0] = 28.883333;
bbcCutZDep[20][ 0] = 28.150000;
bbcCutZDep[21][ 0] = 27.416666;
bbcCutZDep[22][ 0] = 26.750000;
bbcCutZDep[23][ 0] = 26.049999;
bbcCutZDep[24][ 0] = 25.416666;
bbcCutZDep[25][ 0] = 24.783333;
bbcCutZDep[26][ 0] = 24.150000;
bbcCutZDep[27][ 0] = 23.549999;
bbcCutZDep[28][ 0] = 22.950001;
bbcCutZDep[29][ 0] = 22.383333;
bbcCutZDep[30][ 0] = 21.816668;
bbcCutZDep[31][ 0] = 21.250000;
bbcCutZDep[32][ 0] = 20.716667;
bbcCutZDep[33][ 0] = 20.183332;
bbcCutZDep[34][ 0] = 19.650000;
bbcCutZDep[35][ 0] = 19.116667;
bbcCutZDep[36][ 0] = 18.616667;
bbcCutZDep[37][ 0] = 18.116667;
bbcCutZDep[38][ 0] = 17.616667;
bbcCutZDep[39][ 0] = 17.116667;
bbcCutZDep[40][ 0] = 16.616667;
bbcCutZDep[41][ 0] = 16.150000;
bbcCutZDep[42][ 0] = 15.650000;
bbcCutZDep[43][ 0] = 15.183333;
bbcCutZDep[44][ 0] = 14.716666;
bbcCutZDep[45][ 0] = 14.250000;
bbcCutZDep[46][ 0] = 13.783334;
bbcCutZDep[47][ 0] = 13.316667;
bbcCutZDep[48][ 0] = 12.850000;
bbcCutZDep[49][ 0] = 12.383333;
bbcCutZDep[50][ 0] = 11.950000;
bbcCutZDep[51][ 0] = 11.483334;
bbcCutZDep[52][ 0] = 11.016666;
bbcCutZDep[53][ 0] = 10.583333;
bbcCutZDep[54][ 0] = 10.116667;
bbcCutZDep[55][ 0] = 9.683333;
bbcCutZDep[56][ 0] = 9.216666;
bbcCutZDep[57][ 0] = 8.750000;
bbcCutZDep[58][ 0] = 8.316667;
bbcCutZDep[59][ 0] = 7.850000;
bbcCutZDep[60][ 0] = 7.416667;
bbcCutZDep[61][ 0] = 6.950000;
bbcCutZDep[62][ 0] = 6.483333;
bbcCutZDep[63][ 0] = 6.016667;
bbcCutZDep[64][ 0] = 5.550000;
bbcCutZDep[65][ 0] = 5.050000;
bbcCutZDep[66][ 0] = 4.583333;
bbcCutZDep[67][ 0] = 4.083333;
bbcCutZDep[68][ 0] = 3.583333;
bbcCutZDep[69][ 0] = 3.083333;
bbcCutZDep[70][ 0] = 2.550000;
bbcCutZDep[71][ 0] = 2.016667;
bbcCutZDep[72][ 0] = 1.450000;
bbcCutZDep[73][ 0] = 0.916667;
bbcCutZDep[74][ 0] = -0.500000;
bbcCutZDep[ 0][ 1] = 999.983337;
bbcCutZDep[ 1][ 1] = 60.616665;
bbcCutZDep[ 2][ 1] = 54.183334;
bbcCutZDep[ 3][ 1] = 50.216667;
bbcCutZDep[ 4][ 1] = 47.283333;
bbcCutZDep[ 5][ 1] = 44.916668;
bbcCutZDep[ 6][ 1] = 42.950001;
bbcCutZDep[ 7][ 1] = 41.216667;
bbcCutZDep[ 8][ 1] = 39.683334;
bbcCutZDep[ 9][ 1] = 38.316666;
bbcCutZDep[10][ 1] = 37.049999;
bbcCutZDep[11][ 1] = 35.883335;
bbcCutZDep[12][ 1] = 34.783333;
bbcCutZDep[13][ 1] = 33.783333;
bbcCutZDep[14][ 1] = 32.816666;
bbcCutZDep[15][ 1] = 31.883333;
bbcCutZDep[16][ 1] = 31.016666;
bbcCutZDep[17][ 1] = 30.183332;
bbcCutZDep[18][ 1] = 29.416666;
bbcCutZDep[19][ 1] = 28.650000;
bbcCutZDep[20][ 1] = 27.916666;
bbcCutZDep[21][ 1] = 27.183332;
bbcCutZDep[22][ 1] = 26.483334;
bbcCutZDep[23][ 1] = 25.816668;
bbcCutZDep[24][ 1] = 25.183332;
bbcCutZDep[25][ 1] = 24.549999;
bbcCutZDep[26][ 1] = 23.916666;
bbcCutZDep[27][ 1] = 23.316668;
bbcCutZDep[28][ 1] = 22.716667;
bbcCutZDep[29][ 1] = 22.150000;
bbcCutZDep[30][ 1] = 21.583334;
bbcCutZDep[31][ 1] = 21.049999;
bbcCutZDep[32][ 1] = 20.483334;
bbcCutZDep[33][ 1] = 19.950001;
bbcCutZDep[34][ 1] = 19.416666;
bbcCutZDep[35][ 1] = 18.916666;
bbcCutZDep[36][ 1] = 18.383333;
bbcCutZDep[37][ 1] = 17.883333;
bbcCutZDep[38][ 1] = 17.383333;
bbcCutZDep[39][ 1] = 16.883333;
bbcCutZDep[40][ 1] = 16.416666;
bbcCutZDep[41][ 1] = 15.916667;
bbcCutZDep[42][ 1] = 15.450000;
bbcCutZDep[43][ 1] = 14.983334;
bbcCutZDep[44][ 1] = 14.516666;
bbcCutZDep[45][ 1] = 14.050000;
bbcCutZDep[46][ 1] = 13.583333;
bbcCutZDep[47][ 1] = 13.116667;
bbcCutZDep[48][ 1] = 12.650000;
bbcCutZDep[49][ 1] = 12.216666;
bbcCutZDep[50][ 1] = 11.750000;
bbcCutZDep[51][ 1] = 11.316667;
bbcCutZDep[52][ 1] = 10.850000;
bbcCutZDep[53][ 1] = 10.416667;
bbcCutZDep[54][ 1] = 9.950000;
bbcCutZDep[55][ 1] = 9.516666;
bbcCutZDep[56][ 1] = 9.083333;
bbcCutZDep[57][ 1] = 8.616667;
bbcCutZDep[58][ 1] = 8.183333;
bbcCutZDep[59][ 1] = 7.716667;
bbcCutZDep[60][ 1] = 7.250000;
bbcCutZDep[61][ 1] = 6.816667;
bbcCutZDep[62][ 1] = 6.350000;
bbcCutZDep[63][ 1] = 5.883333;
bbcCutZDep[64][ 1] = 5.416667;
bbcCutZDep[65][ 1] = 4.950000;
bbcCutZDep[66][ 1] = 4.483333;
bbcCutZDep[67][ 1] = 3.983333;
bbcCutZDep[68][ 1] = 3.483333;
bbcCutZDep[69][ 1] = 2.983333;
bbcCutZDep[70][ 1] = 2.483333;
bbcCutZDep[71][ 1] = 1.950000;
bbcCutZDep[72][ 1] = 1.383333;
bbcCutZDep[73][ 1] = 0.883333;
bbcCutZDep[74][ 1] = -0.500000;
bbcCutZDep[ 0][ 2] = 999.983337;
bbcCutZDep[ 1][ 2] = 60.383335;
bbcCutZDep[ 2][ 2] = 53.916668;
bbcCutZDep[ 3][ 2] = 49.950001;
bbcCutZDep[ 4][ 2] = 47.016666;
bbcCutZDep[ 5][ 2] = 44.650002;
bbcCutZDep[ 6][ 2] = 42.683334;
bbcCutZDep[ 7][ 2] = 40.950001;
bbcCutZDep[ 8][ 2] = 39.416668;
bbcCutZDep[ 9][ 2] = 38.049999;
bbcCutZDep[10][ 2] = 36.783333;
bbcCutZDep[11][ 2] = 35.616665;
bbcCutZDep[12][ 2] = 34.549999;
bbcCutZDep[13][ 2] = 33.516666;
bbcCutZDep[14][ 2] = 32.549999;
bbcCutZDep[15][ 2] = 31.650000;
bbcCutZDep[16][ 2] = 30.783333;
bbcCutZDep[17][ 2] = 29.950001;
bbcCutZDep[18][ 2] = 29.150000;
bbcCutZDep[19][ 2] = 28.416666;
bbcCutZDep[20][ 2] = 27.683332;
bbcCutZDep[21][ 2] = 26.950001;
bbcCutZDep[22][ 2] = 26.283333;
bbcCutZDep[23][ 2] = 25.616667;
bbcCutZDep[24][ 2] = 24.950001;
bbcCutZDep[25][ 2] = 24.316668;
bbcCutZDep[26][ 2] = 23.716667;
bbcCutZDep[27][ 2] = 23.083334;
bbcCutZDep[28][ 2] = 22.516666;
bbcCutZDep[29][ 2] = 21.950001;
bbcCutZDep[30][ 2] = 21.383333;
bbcCutZDep[31][ 2] = 20.816668;
bbcCutZDep[32][ 2] = 20.283333;
bbcCutZDep[33][ 2] = 19.750000;
bbcCutZDep[34][ 2] = 19.216667;
bbcCutZDep[35][ 2] = 18.716667;
bbcCutZDep[36][ 2] = 18.183332;
bbcCutZDep[37][ 2] = 17.683332;
bbcCutZDep[38][ 2] = 17.183332;
bbcCutZDep[39][ 2] = 16.716667;
bbcCutZDep[40][ 2] = 16.216667;
bbcCutZDep[41][ 2] = 15.750000;
bbcCutZDep[42][ 2] = 15.250000;
bbcCutZDep[43][ 2] = 14.783334;
bbcCutZDep[44][ 2] = 14.316667;
bbcCutZDep[45][ 2] = 13.850000;
bbcCutZDep[46][ 2] = 13.416667;
bbcCutZDep[47][ 2] = 12.950000;
bbcCutZDep[48][ 2] = 12.483334;
bbcCutZDep[49][ 2] = 12.050000;
bbcCutZDep[50][ 2] = 11.583333;
bbcCutZDep[51][ 2] = 11.150000;
bbcCutZDep[52][ 2] = 10.683333;
bbcCutZDep[53][ 2] = 10.250000;
bbcCutZDep[54][ 2] = 9.816667;
bbcCutZDep[55][ 2] = 9.350000;
bbcCutZDep[56][ 2] = 8.916667;
bbcCutZDep[57][ 2] = 8.483334;
bbcCutZDep[58][ 2] = 8.016666;
bbcCutZDep[59][ 2] = 7.583333;
bbcCutZDep[60][ 2] = 7.150000;
bbcCutZDep[61][ 2] = 6.683333;
bbcCutZDep[62][ 2] = 6.250000;
bbcCutZDep[63][ 2] = 5.783333;
bbcCutZDep[64][ 2] = 5.316667;
bbcCutZDep[65][ 2] = 4.850000;
bbcCutZDep[66][ 2] = 4.383333;
bbcCutZDep[67][ 2] = 3.916667;
bbcCutZDep[68][ 2] = 3.416667;
bbcCutZDep[69][ 2] = 2.950000;
bbcCutZDep[70][ 2] = 2.416667;
bbcCutZDep[71][ 2] = 1.916667;
bbcCutZDep[72][ 2] = 1.383333;
bbcCutZDep[73][ 2] = 0.883333;
bbcCutZDep[74][ 2] = -0.500000;
bbcCutZDep[ 0][ 3] = 999.983337;
bbcCutZDep[ 1][ 3] = 60.150002;
bbcCutZDep[ 2][ 3] = 53.683334;
bbcCutZDep[ 3][ 3] = 49.716667;
bbcCutZDep[ 4][ 3] = 46.783333;
bbcCutZDep[ 5][ 3] = 44.450001;
bbcCutZDep[ 6][ 3] = 42.483334;
bbcCutZDep[ 7][ 3] = 40.750000;
bbcCutZDep[ 8][ 3] = 39.250000;
bbcCutZDep[ 9][ 3] = 37.849998;
bbcCutZDep[10][ 3] = 36.583332;
bbcCutZDep[11][ 3] = 35.416668;
bbcCutZDep[12][ 3] = 34.349998;
bbcCutZDep[13][ 3] = 33.316666;
bbcCutZDep[14][ 3] = 32.349998;
bbcCutZDep[15][ 3] = 31.450001;
bbcCutZDep[16][ 3] = 30.583334;
bbcCutZDep[17][ 3] = 29.750000;
bbcCutZDep[18][ 3] = 28.983334;
bbcCutZDep[19][ 3] = 28.216667;
bbcCutZDep[20][ 3] = 27.483334;
bbcCutZDep[21][ 3] = 26.783333;
bbcCutZDep[22][ 3] = 26.083334;
bbcCutZDep[23][ 3] = 25.416666;
bbcCutZDep[24][ 3] = 24.783333;
bbcCutZDep[25][ 3] = 24.150000;
bbcCutZDep[26][ 3] = 23.516666;
bbcCutZDep[27][ 3] = 22.916666;
bbcCutZDep[28][ 3] = 22.350000;
bbcCutZDep[29][ 3] = 21.750000;
bbcCutZDep[30][ 3] = 21.216667;
bbcCutZDep[31][ 3] = 20.650000;
bbcCutZDep[32][ 3] = 20.116667;
bbcCutZDep[33][ 3] = 19.583334;
bbcCutZDep[34][ 3] = 19.049999;
bbcCutZDep[35][ 3] = 18.549999;
bbcCutZDep[36][ 3] = 18.016666;
bbcCutZDep[37][ 3] = 17.516666;
bbcCutZDep[38][ 3] = 17.049999;
bbcCutZDep[39][ 3] = 16.549999;
bbcCutZDep[40][ 3] = 16.049999;
bbcCutZDep[41][ 3] = 15.583333;
bbcCutZDep[42][ 3] = 15.116667;
bbcCutZDep[43][ 3] = 14.650000;
bbcCutZDep[44][ 3] = 14.183333;
bbcCutZDep[45][ 3] = 13.716666;
bbcCutZDep[46][ 3] = 13.283334;
bbcCutZDep[47][ 3] = 12.816667;
bbcCutZDep[48][ 3] = 12.350000;
bbcCutZDep[49][ 3] = 11.916667;
bbcCutZDep[50][ 3] = 11.483334;
bbcCutZDep[51][ 3] = 11.016666;
bbcCutZDep[52][ 3] = 10.583333;
bbcCutZDep[53][ 3] = 10.150000;
bbcCutZDep[54][ 3] = 9.683333;
bbcCutZDep[55][ 3] = 9.250000;
bbcCutZDep[56][ 3] = 8.816667;
bbcCutZDep[57][ 3] = 8.383333;
bbcCutZDep[58][ 3] = 7.950000;
bbcCutZDep[59][ 3] = 7.483333;
bbcCutZDep[60][ 3] = 7.050000;
bbcCutZDep[61][ 3] = 6.616667;
bbcCutZDep[62][ 3] = 6.150000;
bbcCutZDep[63][ 3] = 5.716667;
bbcCutZDep[64][ 3] = 5.250000;
bbcCutZDep[65][ 3] = 4.783333;
bbcCutZDep[66][ 3] = 4.316667;
bbcCutZDep[67][ 3] = 3.850000;
bbcCutZDep[68][ 3] = 3.383333;
bbcCutZDep[69][ 3] = 2.883333;
bbcCutZDep[70][ 3] = 2.383333;
bbcCutZDep[71][ 3] = 1.883333;
bbcCutZDep[72][ 3] = 1.350000;
bbcCutZDep[73][ 3] = 0.883333;
bbcCutZDep[74][ 3] = -0.500000;
bbcCutZDep[ 0][ 4] = 999.983337;
bbcCutZDep[ 1][ 4] = 59.849998;
bbcCutZDep[ 2][ 4] = 53.416668;
bbcCutZDep[ 3][ 4] = 49.483334;
bbcCutZDep[ 4][ 4] = 46.549999;
bbcCutZDep[ 5][ 4] = 44.216667;
bbcCutZDep[ 6][ 4] = 42.216667;
bbcCutZDep[ 7][ 4] = 40.516666;
bbcCutZDep[ 8][ 4] = 38.983334;
bbcCutZDep[ 9][ 4] = 37.616665;
bbcCutZDep[10][ 4] = 36.349998;
bbcCutZDep[11][ 4] = 35.183334;
bbcCutZDep[12][ 4] = 34.116665;
bbcCutZDep[13][ 4] = 33.083332;
bbcCutZDep[14][ 4] = 32.150002;
bbcCutZDep[15][ 4] = 31.216667;
bbcCutZDep[16][ 4] = 30.350000;
bbcCutZDep[17][ 4] = 29.549999;
bbcCutZDep[18][ 4] = 28.750000;
bbcCutZDep[19][ 4] = 27.983334;
bbcCutZDep[20][ 4] = 27.250000;
bbcCutZDep[21][ 4] = 26.549999;
bbcCutZDep[22][ 4] = 25.850000;
bbcCutZDep[23][ 4] = 25.216667;
bbcCutZDep[24][ 4] = 24.549999;
bbcCutZDep[25][ 4] = 23.916666;
bbcCutZDep[26][ 4] = 23.316668;
bbcCutZDep[27][ 4] = 22.716667;
bbcCutZDep[28][ 4] = 22.150000;
bbcCutZDep[29][ 4] = 21.583334;
bbcCutZDep[30][ 4] = 21.016666;
bbcCutZDep[31][ 4] = 20.450001;
bbcCutZDep[32][ 4] = 19.916666;
bbcCutZDep[33][ 4] = 19.383333;
bbcCutZDep[34][ 4] = 18.883333;
bbcCutZDep[35][ 4] = 18.350000;
bbcCutZDep[36][ 4] = 17.850000;
bbcCutZDep[37][ 4] = 17.350000;
bbcCutZDep[38][ 4] = 16.883333;
bbcCutZDep[39][ 4] = 16.383333;
bbcCutZDep[40][ 4] = 15.916667;
bbcCutZDep[41][ 4] = 15.416667;
bbcCutZDep[42][ 4] = 14.950000;
bbcCutZDep[43][ 4] = 14.483334;
bbcCutZDep[44][ 4] = 14.016666;
bbcCutZDep[45][ 4] = 13.583333;
bbcCutZDep[46][ 4] = 13.116667;
bbcCutZDep[47][ 4] = 12.683333;
bbcCutZDep[48][ 4] = 12.216666;
bbcCutZDep[49][ 4] = 11.783334;
bbcCutZDep[50][ 4] = 11.350000;
bbcCutZDep[51][ 4] = 10.883333;
bbcCutZDep[52][ 4] = 10.450000;
bbcCutZDep[53][ 4] = 10.016666;
bbcCutZDep[54][ 4] = 9.583333;
bbcCutZDep[55][ 4] = 9.150000;
bbcCutZDep[56][ 4] = 8.716666;
bbcCutZDep[57][ 4] = 8.283334;
bbcCutZDep[58][ 4] = 7.850000;
bbcCutZDep[59][ 4] = 7.383333;
bbcCutZDep[60][ 4] = 6.950000;
bbcCutZDep[61][ 4] = 6.516667;
bbcCutZDep[62][ 4] = 6.083333;
bbcCutZDep[63][ 4] = 5.616667;
bbcCutZDep[64][ 4] = 5.183333;
bbcCutZDep[65][ 4] = 4.716667;
bbcCutZDep[66][ 4] = 4.250000;
bbcCutZDep[67][ 4] = 3.816667;
bbcCutZDep[68][ 4] = 3.316667;
bbcCutZDep[69][ 4] = 2.850000;
bbcCutZDep[70][ 4] = 2.350000;
bbcCutZDep[71][ 4] = 1.883333;
bbcCutZDep[72][ 4] = 1.316667;
bbcCutZDep[73][ 4] = 0.850000;
bbcCutZDep[74][ 4] = -0.500000;
bbcCutZDep[ 0][ 5] = 999.983337;
bbcCutZDep[ 1][ 5] = 59.616665;
bbcCutZDep[ 2][ 5] = 53.216667;
bbcCutZDep[ 3][ 5] = 49.250000;
bbcCutZDep[ 4][ 5] = 46.349998;
bbcCutZDep[ 5][ 5] = 44.016666;
bbcCutZDep[ 6][ 5] = 42.016666;
bbcCutZDep[ 7][ 5] = 40.316666;
bbcCutZDep[ 8][ 5] = 38.783333;
bbcCutZDep[ 9][ 5] = 37.416668;
bbcCutZDep[10][ 5] = 36.150002;
bbcCutZDep[11][ 5] = 34.983334;
bbcCutZDep[12][ 5] = 33.916668;
bbcCutZDep[13][ 5] = 32.916668;
bbcCutZDep[14][ 5] = 31.950001;
bbcCutZDep[15][ 5] = 31.049999;
bbcCutZDep[16][ 5] = 30.183332;
bbcCutZDep[17][ 5] = 29.383333;
bbcCutZDep[18][ 5] = 28.583334;
bbcCutZDep[19][ 5] = 27.816668;
bbcCutZDep[20][ 5] = 27.083334;
bbcCutZDep[21][ 5] = 26.383333;
bbcCutZDep[22][ 5] = 25.716667;
bbcCutZDep[23][ 5] = 25.049999;
bbcCutZDep[24][ 5] = 24.416666;
bbcCutZDep[25][ 5] = 23.783333;
bbcCutZDep[26][ 5] = 23.183332;
bbcCutZDep[27][ 5] = 22.583334;
bbcCutZDep[28][ 5] = 21.983334;
bbcCutZDep[29][ 5] = 21.416666;
bbcCutZDep[30][ 5] = 20.883333;
bbcCutZDep[31][ 5] = 20.316668;
bbcCutZDep[32][ 5] = 19.783333;
bbcCutZDep[33][ 5] = 19.250000;
bbcCutZDep[34][ 5] = 18.750000;
bbcCutZDep[35][ 5] = 18.216667;
bbcCutZDep[36][ 5] = 17.716667;
bbcCutZDep[37][ 5] = 17.216667;
bbcCutZDep[38][ 5] = 16.750000;
bbcCutZDep[39][ 5] = 16.250000;
bbcCutZDep[40][ 5] = 15.783334;
bbcCutZDep[41][ 5] = 15.283334;
bbcCutZDep[42][ 5] = 14.816667;
bbcCutZDep[43][ 5] = 14.383333;
bbcCutZDep[44][ 5] = 13.916667;
bbcCutZDep[45][ 5] = 13.450000;
bbcCutZDep[46][ 5] = 13.016666;
bbcCutZDep[47][ 5] = 12.550000;
bbcCutZDep[48][ 5] = 12.116667;
bbcCutZDep[49][ 5] = 11.683333;
bbcCutZDep[50][ 5] = 11.216666;
bbcCutZDep[51][ 5] = 10.783334;
bbcCutZDep[52][ 5] = 10.350000;
bbcCutZDep[53][ 5] = 9.916667;
bbcCutZDep[54][ 5] = 9.483334;
bbcCutZDep[55][ 5] = 9.050000;
bbcCutZDep[56][ 5] = 8.616667;
bbcCutZDep[57][ 5] = 8.183333;
bbcCutZDep[58][ 5] = 7.750000;
bbcCutZDep[59][ 5] = 7.316667;
bbcCutZDep[60][ 5] = 6.883333;
bbcCutZDep[61][ 5] = 6.450000;
bbcCutZDep[62][ 5] = 6.016667;
bbcCutZDep[63][ 5] = 5.550000;
bbcCutZDep[64][ 5] = 5.116667;
bbcCutZDep[65][ 5] = 4.683333;
bbcCutZDep[66][ 5] = 4.216667;
bbcCutZDep[67][ 5] = 3.750000;
bbcCutZDep[68][ 5] = 3.283333;
bbcCutZDep[69][ 5] = 2.816667;
bbcCutZDep[70][ 5] = 2.316667;
bbcCutZDep[71][ 5] = 1.850000;
bbcCutZDep[72][ 5] = 1.316667;
bbcCutZDep[73][ 5] = 0.850000;
bbcCutZDep[74][ 5] = -0.500000;
bbcCutZDep[ 0][ 6] = 999.983337;
bbcCutZDep[ 1][ 6] = 59.316666;
bbcCutZDep[ 2][ 6] = 52.916668;
bbcCutZDep[ 3][ 6] = 48.983334;
bbcCutZDep[ 4][ 6] = 46.083332;
bbcCutZDep[ 5][ 6] = 43.750000;
bbcCutZDep[ 6][ 6] = 41.783333;
bbcCutZDep[ 7][ 6] = 40.083332;
bbcCutZDep[ 8][ 6] = 38.583332;
bbcCutZDep[ 9][ 6] = 37.216667;
bbcCutZDep[10][ 6] = 35.950001;
bbcCutZDep[11][ 6] = 34.816666;
bbcCutZDep[12][ 6] = 33.716667;
bbcCutZDep[13][ 6] = 32.716667;
bbcCutZDep[14][ 6] = 31.783333;
bbcCutZDep[15][ 6] = 30.850000;
bbcCutZDep[16][ 6] = 30.016666;
bbcCutZDep[17][ 6] = 29.183332;
bbcCutZDep[18][ 6] = 28.416666;
bbcCutZDep[19][ 6] = 27.650000;
bbcCutZDep[20][ 6] = 26.916666;
bbcCutZDep[21][ 6] = 26.216667;
bbcCutZDep[22][ 6] = 25.549999;
bbcCutZDep[23][ 6] = 24.883333;
bbcCutZDep[24][ 6] = 24.250000;
bbcCutZDep[25][ 6] = 23.616667;
bbcCutZDep[26][ 6] = 23.016666;
bbcCutZDep[27][ 6] = 22.416666;
bbcCutZDep[28][ 6] = 21.850000;
bbcCutZDep[29][ 6] = 21.283333;
bbcCutZDep[30][ 6] = 20.716667;
bbcCutZDep[31][ 6] = 20.183332;
bbcCutZDep[32][ 6] = 19.650000;
bbcCutZDep[33][ 6] = 19.116667;
bbcCutZDep[34][ 6] = 18.616667;
bbcCutZDep[35][ 6] = 18.083334;
bbcCutZDep[36][ 6] = 17.616667;
bbcCutZDep[37][ 6] = 17.116667;
bbcCutZDep[38][ 6] = 16.616667;
bbcCutZDep[39][ 6] = 16.150000;
bbcCutZDep[40][ 6] = 15.650000;
bbcCutZDep[41][ 6] = 15.183333;
bbcCutZDep[42][ 6] = 14.716666;
bbcCutZDep[43][ 6] = 14.283334;
bbcCutZDep[44][ 6] = 13.816667;
bbcCutZDep[45][ 6] = 13.350000;
bbcCutZDep[46][ 6] = 12.916667;
bbcCutZDep[47][ 6] = 12.450000;
bbcCutZDep[48][ 6] = 12.016666;
bbcCutZDep[49][ 6] = 11.583333;
bbcCutZDep[50][ 6] = 11.150000;
bbcCutZDep[51][ 6] = 10.716666;
bbcCutZDep[52][ 6] = 10.283334;
bbcCutZDep[53][ 6] = 9.850000;
bbcCutZDep[54][ 6] = 9.416667;
bbcCutZDep[55][ 6] = 8.983334;
bbcCutZDep[56][ 6] = 8.550000;
bbcCutZDep[57][ 6] = 8.116667;
bbcCutZDep[58][ 6] = 7.683333;
bbcCutZDep[59][ 6] = 7.250000;
bbcCutZDep[60][ 6] = 6.816667;
bbcCutZDep[61][ 6] = 6.383333;
bbcCutZDep[62][ 6] = 5.950000;
bbcCutZDep[63][ 6] = 5.516667;
bbcCutZDep[64][ 6] = 5.083333;
bbcCutZDep[65][ 6] = 4.616667;
bbcCutZDep[66][ 6] = 4.183333;
bbcCutZDep[67][ 6] = 3.716667;
bbcCutZDep[68][ 6] = 3.250000;
bbcCutZDep[69][ 6] = 2.783333;
bbcCutZDep[70][ 6] = 2.316667;
bbcCutZDep[71][ 6] = 1.816667;
bbcCutZDep[72][ 6] = 1.316667;
bbcCutZDep[73][ 6] = 0.850000;
bbcCutZDep[74][ 6] = -0.500000;
bbcCutZDep[ 0][ 7] = 999.983337;
bbcCutZDep[ 1][ 7] = 59.016666;
bbcCutZDep[ 2][ 7] = 52.650002;
bbcCutZDep[ 3][ 7] = 48.716667;
bbcCutZDep[ 4][ 7] = 45.849998;
bbcCutZDep[ 5][ 7] = 43.516666;
bbcCutZDep[ 6][ 7] = 41.583332;
bbcCutZDep[ 7][ 7] = 39.883335;
bbcCutZDep[ 8][ 7] = 38.349998;
bbcCutZDep[ 9][ 7] = 37.016666;
bbcCutZDep[10][ 7] = 35.750000;
bbcCutZDep[11][ 7] = 34.616665;
bbcCutZDep[12][ 7] = 33.549999;
bbcCutZDep[13][ 7] = 32.549999;
bbcCutZDep[14][ 7] = 31.583334;
bbcCutZDep[15][ 7] = 30.683332;
bbcCutZDep[16][ 7] = 29.850000;
bbcCutZDep[17][ 7] = 29.016666;
bbcCutZDep[18][ 7] = 28.250000;
bbcCutZDep[19][ 7] = 27.483334;
bbcCutZDep[20][ 7] = 26.783333;
bbcCutZDep[21][ 7] = 26.083334;
bbcCutZDep[22][ 7] = 25.383333;
bbcCutZDep[23][ 7] = 24.750000;
bbcCutZDep[24][ 7] = 24.116667;
bbcCutZDep[25][ 7] = 23.483334;
bbcCutZDep[26][ 7] = 22.883333;
bbcCutZDep[27][ 7] = 22.283333;
bbcCutZDep[28][ 7] = 21.716667;
bbcCutZDep[29][ 7] = 21.150000;
bbcCutZDep[30][ 7] = 20.583334;
bbcCutZDep[31][ 7] = 20.049999;
bbcCutZDep[32][ 7] = 19.516666;
bbcCutZDep[33][ 7] = 18.983334;
bbcCutZDep[34][ 7] = 18.483334;
bbcCutZDep[35][ 7] = 17.983334;
bbcCutZDep[36][ 7] = 17.483334;
bbcCutZDep[37][ 7] = 16.983334;
bbcCutZDep[38][ 7] = 16.516666;
bbcCutZDep[39][ 7] = 16.016666;
bbcCutZDep[40][ 7] = 15.550000;
bbcCutZDep[41][ 7] = 15.083333;
bbcCutZDep[42][ 7] = 14.616667;
bbcCutZDep[43][ 7] = 14.150000;
bbcCutZDep[44][ 7] = 13.716666;
bbcCutZDep[45][ 7] = 13.250000;
bbcCutZDep[46][ 7] = 12.816667;
bbcCutZDep[47][ 7] = 12.383333;
bbcCutZDep[48][ 7] = 11.950000;
bbcCutZDep[49][ 7] = 11.516666;
bbcCutZDep[50][ 7] = 11.050000;
bbcCutZDep[51][ 7] = 10.616667;
bbcCutZDep[52][ 7] = 10.183333;
bbcCutZDep[53][ 7] = 9.750000;
bbcCutZDep[54][ 7] = 9.350000;
bbcCutZDep[55][ 7] = 8.916667;
bbcCutZDep[56][ 7] = 8.483334;
bbcCutZDep[57][ 7] = 8.050000;
bbcCutZDep[58][ 7] = 7.616667;
bbcCutZDep[59][ 7] = 7.216667;
bbcCutZDep[60][ 7] = 6.783333;
bbcCutZDep[61][ 7] = 6.350000;
bbcCutZDep[62][ 7] = 5.916667;
bbcCutZDep[63][ 7] = 5.483333;
bbcCutZDep[64][ 7] = 5.050000;
bbcCutZDep[65][ 7] = 4.583333;
bbcCutZDep[66][ 7] = 4.150000;
bbcCutZDep[67][ 7] = 3.683333;
bbcCutZDep[68][ 7] = 3.250000;
bbcCutZDep[69][ 7] = 2.783333;
bbcCutZDep[70][ 7] = 2.283333;
bbcCutZDep[71][ 7] = 1.816667;
bbcCutZDep[72][ 7] = 1.283333;
bbcCutZDep[73][ 7] = 0.850000;
bbcCutZDep[74][ 7] = -0.500000;
bbcCutZDep[ 0][ 8] = 999.983337;
bbcCutZDep[ 1][ 8] = 58.783333;
bbcCutZDep[ 2][ 8] = 52.416668;
bbcCutZDep[ 3][ 8] = 48.516666;
bbcCutZDep[ 4][ 8] = 45.616665;
bbcCutZDep[ 5][ 8] = 43.316666;
bbcCutZDep[ 6][ 8] = 41.383335;
bbcCutZDep[ 7][ 8] = 39.683334;
bbcCutZDep[ 8][ 8] = 38.150002;
bbcCutZDep[ 9][ 8] = 36.816666;
bbcCutZDep[10][ 8] = 35.549999;
bbcCutZDep[11][ 8] = 34.416668;
bbcCutZDep[12][ 8] = 33.349998;
bbcCutZDep[13][ 8] = 32.349998;
bbcCutZDep[14][ 8] = 31.416666;
bbcCutZDep[15][ 8] = 30.516666;
bbcCutZDep[16][ 8] = 29.683332;
bbcCutZDep[17][ 8] = 28.883333;
bbcCutZDep[18][ 8] = 28.083334;
bbcCutZDep[19][ 8] = 27.350000;
bbcCutZDep[20][ 8] = 26.616667;
bbcCutZDep[21][ 8] = 25.916666;
bbcCutZDep[22][ 8] = 25.250000;
bbcCutZDep[23][ 8] = 24.583334;
bbcCutZDep[24][ 8] = 23.950001;
bbcCutZDep[25][ 8] = 23.350000;
bbcCutZDep[26][ 8] = 22.750000;
bbcCutZDep[27][ 8] = 22.150000;
bbcCutZDep[28][ 8] = 21.583334;
bbcCutZDep[29][ 8] = 21.016666;
bbcCutZDep[30][ 8] = 20.483334;
bbcCutZDep[31][ 8] = 19.950001;
bbcCutZDep[32][ 8] = 19.416666;
bbcCutZDep[33][ 8] = 18.883333;
bbcCutZDep[34][ 8] = 18.383333;
bbcCutZDep[35][ 8] = 17.883333;
bbcCutZDep[36][ 8] = 17.383333;
bbcCutZDep[37][ 8] = 16.883333;
bbcCutZDep[38][ 8] = 16.416666;
bbcCutZDep[39][ 8] = 15.916667;
bbcCutZDep[40][ 8] = 15.450000;
bbcCutZDep[41][ 8] = 14.983334;
bbcCutZDep[42][ 8] = 14.550000;
bbcCutZDep[43][ 8] = 14.083333;
bbcCutZDep[44][ 8] = 13.616667;
bbcCutZDep[45][ 8] = 13.183333;
bbcCutZDep[46][ 8] = 12.750000;
bbcCutZDep[47][ 8] = 12.283334;
bbcCutZDep[48][ 8] = 11.850000;
bbcCutZDep[49][ 8] = 11.416667;
bbcCutZDep[50][ 8] = 10.983334;
bbcCutZDep[51][ 8] = 10.550000;
bbcCutZDep[52][ 8] = 10.116667;
bbcCutZDep[53][ 8] = 9.716666;
bbcCutZDep[54][ 8] = 9.283334;
bbcCutZDep[55][ 8] = 8.850000;
bbcCutZDep[56][ 8] = 8.416667;
bbcCutZDep[57][ 8] = 8.016666;
bbcCutZDep[58][ 8] = 7.583333;
bbcCutZDep[59][ 8] = 7.150000;
bbcCutZDep[60][ 8] = 6.716667;
bbcCutZDep[61][ 8] = 6.316667;
bbcCutZDep[62][ 8] = 5.883333;
bbcCutZDep[63][ 8] = 5.450000;
bbcCutZDep[64][ 8] = 5.016667;
bbcCutZDep[65][ 8] = 4.550000;
bbcCutZDep[66][ 8] = 4.116667;
bbcCutZDep[67][ 8] = 3.683333;
bbcCutZDep[68][ 8] = 3.216667;
bbcCutZDep[69][ 8] = 2.750000;
bbcCutZDep[70][ 8] = 2.283333;
bbcCutZDep[71][ 8] = 1.816667;
bbcCutZDep[72][ 8] = 1.283333;
bbcCutZDep[73][ 8] = 0.850000;
bbcCutZDep[74][ 8] = -0.500000;
bbcCutZDep[ 0][ 9] = 999.983337;
bbcCutZDep[ 1][ 9] = 58.483334;
bbcCutZDep[ 2][ 9] = 52.183334;
bbcCutZDep[ 3][ 9] = 48.316666;
bbcCutZDep[ 4][ 9] = 45.450001;
bbcCutZDep[ 5][ 9] = 43.150002;
bbcCutZDep[ 6][ 9] = 41.183334;
bbcCutZDep[ 7][ 9] = 39.516666;
bbcCutZDep[ 8][ 9] = 38.016666;
bbcCutZDep[ 9][ 9] = 36.650002;
bbcCutZDep[10][ 9] = 35.416668;
bbcCutZDep[11][ 9] = 34.283333;
bbcCutZDep[12][ 9] = 33.216667;
bbcCutZDep[13][ 9] = 32.250000;
bbcCutZDep[14][ 9] = 31.283333;
bbcCutZDep[15][ 9] = 30.416666;
bbcCutZDep[16][ 9] = 29.549999;
bbcCutZDep[17][ 9] = 28.750000;
bbcCutZDep[18][ 9] = 27.983334;
bbcCutZDep[19][ 9] = 27.216667;
bbcCutZDep[20][ 9] = 26.516666;
bbcCutZDep[21][ 9] = 25.816668;
bbcCutZDep[22][ 9] = 25.150000;
bbcCutZDep[23][ 9] = 24.483334;
bbcCutZDep[24][ 9] = 23.850000;
bbcCutZDep[25][ 9] = 23.250000;
bbcCutZDep[26][ 9] = 22.650000;
bbcCutZDep[27][ 9] = 22.049999;
bbcCutZDep[28][ 9] = 21.483334;
bbcCutZDep[29][ 9] = 20.916666;
bbcCutZDep[30][ 9] = 20.383333;
bbcCutZDep[31][ 9] = 19.850000;
bbcCutZDep[32][ 9] = 19.316668;
bbcCutZDep[33][ 9] = 18.783333;
bbcCutZDep[34][ 9] = 18.283333;
bbcCutZDep[35][ 9] = 17.783333;
bbcCutZDep[36][ 9] = 17.283333;
bbcCutZDep[37][ 9] = 16.816668;
bbcCutZDep[38][ 9] = 16.316668;
bbcCutZDep[39][ 9] = 15.850000;
bbcCutZDep[40][ 9] = 15.383333;
bbcCutZDep[41][ 9] = 14.916667;
bbcCutZDep[42][ 9] = 14.450000;
bbcCutZDep[43][ 9] = 14.016666;
bbcCutZDep[44][ 9] = 13.550000;
bbcCutZDep[45][ 9] = 13.116667;
bbcCutZDep[46][ 9] = 12.683333;
bbcCutZDep[47][ 9] = 12.250000;
bbcCutZDep[48][ 9] = 11.783334;
bbcCutZDep[49][ 9] = 11.350000;
bbcCutZDep[50][ 9] = 10.950000;
bbcCutZDep[51][ 9] = 10.516666;
bbcCutZDep[52][ 9] = 10.083333;
bbcCutZDep[53][ 9] = 9.650000;
bbcCutZDep[54][ 9] = 9.216666;
bbcCutZDep[55][ 9] = 8.816667;
bbcCutZDep[56][ 9] = 8.383333;
bbcCutZDep[57][ 9] = 7.950000;
bbcCutZDep[58][ 9] = 7.550000;
bbcCutZDep[59][ 9] = 7.116667;
bbcCutZDep[60][ 9] = 6.716667;
bbcCutZDep[61][ 9] = 6.283333;
bbcCutZDep[62][ 9] = 5.850000;
bbcCutZDep[63][ 9] = 5.416667;
bbcCutZDep[64][ 9] = 4.983333;
bbcCutZDep[65][ 9] = 4.550000;
bbcCutZDep[66][ 9] = 4.116667;
bbcCutZDep[67][ 9] = 3.683333;
bbcCutZDep[68][ 9] = 3.216667;
bbcCutZDep[69][ 9] = 2.750000;
bbcCutZDep[70][ 9] = 2.283333;
bbcCutZDep[71][ 9] = 1.816667;
bbcCutZDep[72][ 9] = 1.283333;
bbcCutZDep[73][ 9] = 0.850000;
bbcCutZDep[74][ 9] = -0.500000;
bbcCutZDep[ 0][10] = 999.983337;
bbcCutZDep[ 1][10] = 58.349998;
bbcCutZDep[ 2][10] = 52.016666;
bbcCutZDep[ 3][10] = 48.150002;
bbcCutZDep[ 4][10] = 45.283333;
bbcCutZDep[ 5][10] = 42.983334;
bbcCutZDep[ 6][10] = 41.049999;
bbcCutZDep[ 7][10] = 39.383335;
bbcCutZDep[ 8][10] = 37.883335;
bbcCutZDep[ 9][10] = 36.549999;
bbcCutZDep[10][10] = 35.316666;
bbcCutZDep[11][10] = 34.183334;
bbcCutZDep[12][10] = 33.116665;
bbcCutZDep[13][10] = 32.116665;
bbcCutZDep[14][10] = 31.183332;
bbcCutZDep[15][10] = 30.283333;
bbcCutZDep[16][10] = 29.450001;
bbcCutZDep[17][10] = 28.650000;
bbcCutZDep[18][10] = 27.883333;
bbcCutZDep[19][10] = 27.116667;
bbcCutZDep[20][10] = 26.416666;
bbcCutZDep[21][10] = 25.716667;
bbcCutZDep[22][10] = 25.049999;
bbcCutZDep[23][10] = 24.383333;
bbcCutZDep[24][10] = 23.750000;
bbcCutZDep[25][10] = 23.150000;
bbcCutZDep[26][10] = 22.549999;
bbcCutZDep[27][10] = 21.950001;
bbcCutZDep[28][10] = 21.383333;
bbcCutZDep[29][10] = 20.850000;
bbcCutZDep[30][10] = 20.283333;
bbcCutZDep[31][10] = 19.750000;
bbcCutZDep[32][10] = 19.250000;
bbcCutZDep[33][10] = 18.716667;
bbcCutZDep[34][10] = 18.216667;
bbcCutZDep[35][10] = 17.716667;
bbcCutZDep[36][10] = 17.216667;
bbcCutZDep[37][10] = 16.716667;
bbcCutZDep[38][10] = 16.250000;
bbcCutZDep[39][10] = 15.783334;
bbcCutZDep[40][10] = 15.316667;
bbcCutZDep[41][10] = 14.850000;
bbcCutZDep[42][10] = 14.383333;
bbcCutZDep[43][10] = 13.950000;
bbcCutZDep[44][10] = 13.516666;
bbcCutZDep[45][10] = 13.050000;
bbcCutZDep[46][10] = 12.616667;
bbcCutZDep[47][10] = 12.183333;
bbcCutZDep[48][10] = 11.750000;
bbcCutZDep[49][10] = 11.316667;
bbcCutZDep[50][10] = 10.883333;
bbcCutZDep[51][10] = 10.450000;
bbcCutZDep[52][10] = 10.050000;
bbcCutZDep[53][10] = 9.616667;
bbcCutZDep[54][10] = 9.183333;
bbcCutZDep[55][10] = 8.783334;
bbcCutZDep[56][10] = 8.350000;
bbcCutZDep[57][10] = 7.950000;
bbcCutZDep[58][10] = 7.516667;
bbcCutZDep[59][10] = 7.083333;
bbcCutZDep[60][10] = 6.683333;
bbcCutZDep[61][10] = 6.250000;
bbcCutZDep[62][10] = 5.816667;
bbcCutZDep[63][10] = 5.383333;
bbcCutZDep[64][10] = 4.983333;
bbcCutZDep[65][10] = 4.550000;
bbcCutZDep[66][10] = 4.083333;
bbcCutZDep[67][10] = 3.650000;
bbcCutZDep[68][10] = 3.216667;
bbcCutZDep[69][10] = 2.750000;
bbcCutZDep[70][10] = 2.283333;
bbcCutZDep[71][10] = 1.816667;
bbcCutZDep[72][10] = 1.283333;
bbcCutZDep[73][10] = 0.850000;
bbcCutZDep[74][10] = -0.500000;
bbcCutZDep[ 0][11] = 999.983337;
bbcCutZDep[ 1][11] = 58.116665;
bbcCutZDep[ 2][11] = 51.849998;
bbcCutZDep[ 3][11] = 47.983334;
bbcCutZDep[ 4][11] = 45.150002;
bbcCutZDep[ 5][11] = 42.849998;
bbcCutZDep[ 6][11] = 40.916668;
bbcCutZDep[ 7][11] = 39.250000;
bbcCutZDep[ 8][11] = 37.750000;
bbcCutZDep[ 9][11] = 36.416668;
bbcCutZDep[10][11] = 35.183334;
bbcCutZDep[11][11] = 34.049999;
bbcCutZDep[12][11] = 32.983334;
bbcCutZDep[13][11] = 32.016666;
bbcCutZDep[14][11] = 31.049999;
bbcCutZDep[15][11] = 30.183332;
bbcCutZDep[16][11] = 29.350000;
bbcCutZDep[17][11] = 28.549999;
bbcCutZDep[18][11] = 27.783333;
bbcCutZDep[19][11] = 27.016666;
bbcCutZDep[20][11] = 26.316668;
bbcCutZDep[21][11] = 25.616667;
bbcCutZDep[22][11] = 24.950001;
bbcCutZDep[23][11] = 24.316668;
bbcCutZDep[24][11] = 23.683332;
bbcCutZDep[25][11] = 23.083334;
bbcCutZDep[26][11] = 22.483334;
bbcCutZDep[27][11] = 21.883333;
bbcCutZDep[28][11] = 21.316668;
bbcCutZDep[29][11] = 20.750000;
bbcCutZDep[30][11] = 20.216667;
bbcCutZDep[31][11] = 19.683332;
bbcCutZDep[32][11] = 19.150000;
bbcCutZDep[33][11] = 18.650000;
bbcCutZDep[34][11] = 18.150000;
bbcCutZDep[35][11] = 17.650000;
bbcCutZDep[36][11] = 17.150000;
bbcCutZDep[37][11] = 16.683332;
bbcCutZDep[38][11] = 16.183332;
bbcCutZDep[39][11] = 15.716666;
bbcCutZDep[40][11] = 15.250000;
bbcCutZDep[41][11] = 14.783334;
bbcCutZDep[42][11] = 14.350000;
bbcCutZDep[43][11] = 13.883333;
bbcCutZDep[44][11] = 13.450000;
bbcCutZDep[45][11] = 13.016666;
bbcCutZDep[46][11] = 12.550000;
bbcCutZDep[47][11] = 12.116667;
bbcCutZDep[48][11] = 11.716666;
bbcCutZDep[49][11] = 11.283334;
bbcCutZDep[50][11] = 10.850000;
bbcCutZDep[51][11] = 10.416667;
bbcCutZDep[52][11] = 10.016666;
bbcCutZDep[53][11] = 9.583333;
bbcCutZDep[54][11] = 9.150000;
bbcCutZDep[55][11] = 8.750000;
bbcCutZDep[56][11] = 8.316667;
bbcCutZDep[57][11] = 7.916667;
bbcCutZDep[58][11] = 7.483333;
bbcCutZDep[59][11] = 7.083333;
bbcCutZDep[60][11] = 6.650000;
bbcCutZDep[61][11] = 6.216667;
bbcCutZDep[62][11] = 5.816667;
bbcCutZDep[63][11] = 5.383333;
bbcCutZDep[64][11] = 4.950000;
bbcCutZDep[65][11] = 4.516667;
bbcCutZDep[66][11] = 4.083333;
bbcCutZDep[67][11] = 3.650000;
bbcCutZDep[68][11] = 3.183333;
bbcCutZDep[69][11] = 2.750000;
bbcCutZDep[70][11] = 2.283333;
bbcCutZDep[71][11] = 1.816667;
bbcCutZDep[72][11] = 1.283333;
bbcCutZDep[73][11] = 0.850000;
bbcCutZDep[74][11] = -0.500000;
bbcCutZDep[ 0][12] = 999.983337;
bbcCutZDep[ 1][12] = 58.016666;
bbcCutZDep[ 2][12] = 51.750000;
bbcCutZDep[ 3][12] = 47.916668;
bbcCutZDep[ 4][12] = 45.049999;
bbcCutZDep[ 5][12] = 42.783333;
bbcCutZDep[ 6][12] = 40.849998;
bbcCutZDep[ 7][12] = 39.150002;
bbcCutZDep[ 8][12] = 37.683334;
bbcCutZDep[ 9][12] = 36.349998;
bbcCutZDep[10][12] = 35.116665;
bbcCutZDep[11][12] = 33.983334;
bbcCutZDep[12][12] = 32.916668;
bbcCutZDep[13][12] = 31.950001;
bbcCutZDep[14][12] = 31.016666;
bbcCutZDep[15][12] = 30.116667;
bbcCutZDep[16][12] = 29.250000;
bbcCutZDep[17][12] = 28.450001;
bbcCutZDep[18][12] = 27.683332;
bbcCutZDep[19][12] = 26.950001;
bbcCutZDep[20][12] = 26.250000;
bbcCutZDep[21][12] = 25.549999;
bbcCutZDep[22][12] = 24.883333;
bbcCutZDep[23][12] = 24.250000;
bbcCutZDep[24][12] = 23.616667;
bbcCutZDep[25][12] = 23.016666;
bbcCutZDep[26][12] = 22.416666;
bbcCutZDep[27][12] = 21.816668;
bbcCutZDep[28][12] = 21.250000;
bbcCutZDep[29][12] = 20.716667;
bbcCutZDep[30][12] = 20.150000;
bbcCutZDep[31][12] = 19.616667;
bbcCutZDep[32][12] = 19.116667;
bbcCutZDep[33][12] = 18.583334;
bbcCutZDep[34][12] = 18.083334;
bbcCutZDep[35][12] = 17.583334;
bbcCutZDep[36][12] = 17.116667;
bbcCutZDep[37][12] = 16.616667;
bbcCutZDep[38][12] = 16.150000;
bbcCutZDep[39][12] = 15.683333;
bbcCutZDep[40][12] = 15.216666;
bbcCutZDep[41][12] = 14.750000;
bbcCutZDep[42][12] = 14.283334;
bbcCutZDep[43][12] = 13.850000;
bbcCutZDep[44][12] = 13.416667;
bbcCutZDep[45][12] = 12.950000;
bbcCutZDep[46][12] = 12.516666;
bbcCutZDep[47][12] = 12.083333;
bbcCutZDep[48][12] = 11.650000;
bbcCutZDep[49][12] = 11.216666;
bbcCutZDep[50][12] = 10.816667;
bbcCutZDep[51][12] = 10.383333;
bbcCutZDep[52][12] = 9.950000;
bbcCutZDep[53][12] = 9.550000;
bbcCutZDep[54][12] = 9.116667;
bbcCutZDep[55][12] = 8.716666;
bbcCutZDep[56][12] = 8.283334;
bbcCutZDep[57][12] = 7.883333;
bbcCutZDep[58][12] = 7.450000;
bbcCutZDep[59][12] = 7.050000;
bbcCutZDep[60][12] = 6.616667;
bbcCutZDep[61][12] = 6.216667;
bbcCutZDep[62][12] = 5.783333;
bbcCutZDep[63][12] = 5.350000;
bbcCutZDep[64][12] = 4.950000;
bbcCutZDep[65][12] = 4.516667;
bbcCutZDep[66][12] = 4.083333;
bbcCutZDep[67][12] = 3.650000;
bbcCutZDep[68][12] = 3.183333;
bbcCutZDep[69][12] = 2.716667;
bbcCutZDep[70][12] = 2.283333;
bbcCutZDep[71][12] = 1.816667;
bbcCutZDep[72][12] = 1.283333;
bbcCutZDep[73][12] = 0.850000;
bbcCutZDep[74][12] = -0.500000;
bbcCutZDep[ 0][13] = 999.983337;
bbcCutZDep[ 1][13] = 57.883335;
bbcCutZDep[ 2][13] = 51.683334;
bbcCutZDep[ 3][13] = 47.849998;
bbcCutZDep[ 4][13] = 44.983334;
bbcCutZDep[ 5][13] = 42.716667;
bbcCutZDep[ 6][13] = 40.783333;
bbcCutZDep[ 7][13] = 39.116665;
bbcCutZDep[ 8][13] = 37.616665;
bbcCutZDep[ 9][13] = 36.283333;
bbcCutZDep[10][13] = 35.049999;
bbcCutZDep[11][13] = 33.916668;
bbcCutZDep[12][13] = 32.883335;
bbcCutZDep[13][13] = 31.883333;
bbcCutZDep[14][13] = 30.950001;
bbcCutZDep[15][13] = 30.049999;
bbcCutZDep[16][13] = 29.216667;
bbcCutZDep[17][13] = 28.416666;
bbcCutZDep[18][13] = 27.650000;
bbcCutZDep[19][13] = 26.916666;
bbcCutZDep[20][13] = 26.183332;
bbcCutZDep[21][13] = 25.516666;
bbcCutZDep[22][13] = 24.850000;
bbcCutZDep[23][13] = 24.216667;
bbcCutZDep[24][13] = 23.583334;
bbcCutZDep[25][13] = 22.983334;
bbcCutZDep[26][13] = 22.383333;
bbcCutZDep[27][13] = 21.783333;
bbcCutZDep[28][13] = 21.216667;
bbcCutZDep[29][13] = 20.683332;
bbcCutZDep[30][13] = 20.116667;
bbcCutZDep[31][13] = 19.616667;
bbcCutZDep[32][13] = 19.083334;
bbcCutZDep[33][13] = 18.549999;
bbcCutZDep[34][13] = 18.049999;
bbcCutZDep[35][13] = 17.583334;
bbcCutZDep[36][13] = 17.083334;
bbcCutZDep[37][13] = 16.583334;
bbcCutZDep[38][13] = 16.116667;
bbcCutZDep[39][13] = 15.650000;
bbcCutZDep[40][13] = 15.183333;
bbcCutZDep[41][13] = 14.716666;
bbcCutZDep[42][13] = 14.283334;
bbcCutZDep[43][13] = 13.816667;
bbcCutZDep[44][13] = 13.383333;
bbcCutZDep[45][13] = 12.950000;
bbcCutZDep[46][13] = 12.516666;
bbcCutZDep[47][13] = 12.083333;
bbcCutZDep[48][13] = 11.650000;
bbcCutZDep[49][13] = 11.216666;
bbcCutZDep[50][13] = 10.783334;
bbcCutZDep[51][13] = 10.350000;
bbcCutZDep[52][13] = 9.950000;
bbcCutZDep[53][13] = 9.516666;
bbcCutZDep[54][13] = 9.116667;
bbcCutZDep[55][13] = 8.683333;
bbcCutZDep[56][13] = 8.283334;
bbcCutZDep[57][13] = 7.850000;
bbcCutZDep[58][13] = 7.450000;
bbcCutZDep[59][13] = 7.016667;
bbcCutZDep[60][13] = 6.616667;
bbcCutZDep[61][13] = 6.183333;
bbcCutZDep[62][13] = 5.783333;
bbcCutZDep[63][13] = 5.350000;
bbcCutZDep[64][13] = 4.916667;
bbcCutZDep[65][13] = 4.516667;
bbcCutZDep[66][13] = 4.083333;
bbcCutZDep[67][13] = 3.616667;
bbcCutZDep[68][13] = 3.183333;
bbcCutZDep[69][13] = 2.716667;
bbcCutZDep[70][13] = 2.283333;
bbcCutZDep[71][13] = 1.816667;
bbcCutZDep[72][13] = 1.283333;
bbcCutZDep[73][13] = 0.850000;
bbcCutZDep[74][13] = -0.500000;
bbcCutZDep[ 0][14] = 999.983337;
bbcCutZDep[ 1][14] = 57.883335;
bbcCutZDep[ 2][14] = 51.650002;
bbcCutZDep[ 3][14] = 47.783333;
bbcCutZDep[ 4][14] = 44.950001;
bbcCutZDep[ 5][14] = 42.650002;
bbcCutZDep[ 6][14] = 40.716667;
bbcCutZDep[ 7][14] = 39.049999;
bbcCutZDep[ 8][14] = 37.583332;
bbcCutZDep[ 9][14] = 36.250000;
bbcCutZDep[10][14] = 35.016666;
bbcCutZDep[11][14] = 33.883335;
bbcCutZDep[12][14] = 32.849998;
bbcCutZDep[13][14] = 31.850000;
bbcCutZDep[14][14] = 30.916666;
bbcCutZDep[15][14] = 30.049999;
bbcCutZDep[16][14] = 29.216667;
bbcCutZDep[17][14] = 28.416666;
bbcCutZDep[18][14] = 27.616667;
bbcCutZDep[19][14] = 26.883333;
bbcCutZDep[20][14] = 26.183332;
bbcCutZDep[21][14] = 25.483334;
bbcCutZDep[22][14] = 24.816668;
bbcCutZDep[23][14] = 24.183332;
bbcCutZDep[24][14] = 23.549999;
bbcCutZDep[25][14] = 22.950001;
bbcCutZDep[26][14] = 22.350000;
bbcCutZDep[27][14] = 21.783333;
bbcCutZDep[28][14] = 21.183332;
bbcCutZDep[29][14] = 20.650000;
bbcCutZDep[30][14] = 20.116667;
bbcCutZDep[31][14] = 19.583334;
bbcCutZDep[32][14] = 19.049999;
bbcCutZDep[33][14] = 18.549999;
bbcCutZDep[34][14] = 18.049999;
bbcCutZDep[35][14] = 17.549999;
bbcCutZDep[36][14] = 17.049999;
bbcCutZDep[37][14] = 16.583334;
bbcCutZDep[38][14] = 16.083334;
bbcCutZDep[39][14] = 15.616667;
bbcCutZDep[40][14] = 15.150000;
bbcCutZDep[41][14] = 14.716666;
bbcCutZDep[42][14] = 14.250000;
bbcCutZDep[43][14] = 13.816667;
bbcCutZDep[44][14] = 13.383333;
bbcCutZDep[45][14] = 12.916667;
bbcCutZDep[46][14] = 12.483334;
bbcCutZDep[47][14] = 12.050000;
bbcCutZDep[48][14] = 11.616667;
bbcCutZDep[49][14] = 11.216666;
bbcCutZDep[50][14] = 10.783334;
bbcCutZDep[51][14] = 10.350000;
bbcCutZDep[52][14] = 9.950000;
bbcCutZDep[53][14] = 9.516666;
bbcCutZDep[54][14] = 9.116667;
bbcCutZDep[55][14] = 8.683333;
bbcCutZDep[56][14] = 8.283334;
bbcCutZDep[57][14] = 7.850000;
bbcCutZDep[58][14] = 7.450000;
bbcCutZDep[59][14] = 7.016667;
bbcCutZDep[60][14] = 6.616667;
bbcCutZDep[61][14] = 6.183333;
bbcCutZDep[62][14] = 5.783333;
bbcCutZDep[63][14] = 5.350000;
bbcCutZDep[64][14] = 4.916667;
bbcCutZDep[65][14] = 4.516667;
bbcCutZDep[66][14] = 4.083333;
bbcCutZDep[67][14] = 3.616667;
bbcCutZDep[68][14] = 3.183333;
bbcCutZDep[69][14] = 2.750000;
bbcCutZDep[70][14] = 2.283333;
bbcCutZDep[71][14] = 1.816667;
bbcCutZDep[72][14] = 1.283333;
bbcCutZDep[73][14] = 0.850000;
bbcCutZDep[74][14] = -0.500000;
bbcCutZDep[ 0][15] = 999.983337;
bbcCutZDep[ 1][15] = 57.849998;
bbcCutZDep[ 2][15] = 51.616665;
bbcCutZDep[ 3][15] = 47.816666;
bbcCutZDep[ 4][15] = 44.983334;
bbcCutZDep[ 5][15] = 42.683334;
bbcCutZDep[ 6][15] = 40.750000;
bbcCutZDep[ 7][15] = 39.083332;
bbcCutZDep[ 8][15] = 37.583332;
bbcCutZDep[ 9][15] = 36.250000;
bbcCutZDep[10][15] = 35.049999;
bbcCutZDep[11][15] = 33.916668;
bbcCutZDep[12][15] = 32.849998;
bbcCutZDep[13][15] = 31.850000;
bbcCutZDep[14][15] = 30.916666;
bbcCutZDep[15][15] = 30.049999;
bbcCutZDep[16][15] = 29.216667;
bbcCutZDep[17][15] = 28.416666;
bbcCutZDep[18][15] = 27.616667;
bbcCutZDep[19][15] = 26.883333;
bbcCutZDep[20][15] = 26.183332;
bbcCutZDep[21][15] = 25.483334;
bbcCutZDep[22][15] = 24.816668;
bbcCutZDep[23][15] = 24.183332;
bbcCutZDep[24][15] = 23.549999;
bbcCutZDep[25][15] = 22.950001;
bbcCutZDep[26][15] = 22.350000;
bbcCutZDep[27][15] = 21.783333;
bbcCutZDep[28][15] = 21.216667;
bbcCutZDep[29][15] = 20.650000;
bbcCutZDep[30][15] = 20.116667;
bbcCutZDep[31][15] = 19.583334;
bbcCutZDep[32][15] = 19.049999;
bbcCutZDep[33][15] = 18.549999;
bbcCutZDep[34][15] = 18.049999;
bbcCutZDep[35][15] = 17.549999;
bbcCutZDep[36][15] = 17.049999;
bbcCutZDep[37][15] = 16.583334;
bbcCutZDep[38][15] = 16.116667;
bbcCutZDep[39][15] = 15.650000;
bbcCutZDep[40][15] = 15.183333;
bbcCutZDep[41][15] = 14.716666;
bbcCutZDep[42][15] = 14.250000;
bbcCutZDep[43][15] = 13.816667;
bbcCutZDep[44][15] = 13.383333;
bbcCutZDep[45][15] = 12.916667;
bbcCutZDep[46][15] = 12.483334;
bbcCutZDep[47][15] = 12.050000;
bbcCutZDep[48][15] = 11.650000;
bbcCutZDep[49][15] = 11.216666;
bbcCutZDep[50][15] = 10.783334;
bbcCutZDep[51][15] = 10.350000;
bbcCutZDep[52][15] = 9.950000;
bbcCutZDep[53][15] = 9.516666;
bbcCutZDep[54][15] = 9.116667;
bbcCutZDep[55][15] = 8.683333;
bbcCutZDep[56][15] = 8.283334;
bbcCutZDep[57][15] = 7.850000;
bbcCutZDep[58][15] = 7.450000;
bbcCutZDep[59][15] = 7.050000;
bbcCutZDep[60][15] = 6.616667;
bbcCutZDep[61][15] = 6.183333;
bbcCutZDep[62][15] = 5.783333;
bbcCutZDep[63][15] = 5.350000;
bbcCutZDep[64][15] = 4.950000;
bbcCutZDep[65][15] = 4.516667;
bbcCutZDep[66][15] = 4.083333;
bbcCutZDep[67][15] = 3.616667;
bbcCutZDep[68][15] = 3.183333;
bbcCutZDep[69][15] = 2.750000;
bbcCutZDep[70][15] = 2.283333;
bbcCutZDep[71][15] = 1.816667;
bbcCutZDep[72][15] = 1.283333;
bbcCutZDep[73][15] = 0.850000;
bbcCutZDep[74][15] = -0.500000;
bbcCutZDep[ 0][16] = 999.983337;
bbcCutZDep[ 1][16] = 57.916668;
bbcCutZDep[ 2][16] = 51.683334;
bbcCutZDep[ 3][16] = 47.849998;
bbcCutZDep[ 4][16] = 45.016666;
bbcCutZDep[ 5][16] = 42.750000;
bbcCutZDep[ 6][16] = 40.816666;
bbcCutZDep[ 7][16] = 39.150002;
bbcCutZDep[ 8][16] = 37.650002;
bbcCutZDep[ 9][16] = 36.316666;
bbcCutZDep[10][16] = 35.116665;
bbcCutZDep[11][16] = 33.983334;
bbcCutZDep[12][16] = 32.916668;
bbcCutZDep[13][16] = 31.916666;
bbcCutZDep[14][16] = 30.983334;
bbcCutZDep[15][16] = 30.083334;
bbcCutZDep[16][16] = 29.250000;
bbcCutZDep[17][16] = 28.450001;
bbcCutZDep[18][16] = 27.683332;
bbcCutZDep[19][16] = 26.950001;
bbcCutZDep[20][16] = 26.216667;
bbcCutZDep[21][16] = 25.549999;
bbcCutZDep[22][16] = 24.883333;
bbcCutZDep[23][16] = 24.250000;
bbcCutZDep[24][16] = 23.616667;
bbcCutZDep[25][16] = 22.983334;
bbcCutZDep[26][16] = 22.416666;
bbcCutZDep[27][16] = 21.816668;
bbcCutZDep[28][16] = 21.250000;
bbcCutZDep[29][16] = 20.716667;
bbcCutZDep[30][16] = 20.150000;
bbcCutZDep[31][16] = 19.616667;
bbcCutZDep[32][16] = 19.116667;
bbcCutZDep[33][16] = 18.583334;
bbcCutZDep[34][16] = 18.083334;
bbcCutZDep[35][16] = 17.583334;
bbcCutZDep[36][16] = 17.116667;
bbcCutZDep[37][16] = 16.616667;
bbcCutZDep[38][16] = 16.150000;
bbcCutZDep[39][16] = 15.683333;
bbcCutZDep[40][16] = 15.216666;
bbcCutZDep[41][16] = 14.750000;
bbcCutZDep[42][16] = 14.316667;
bbcCutZDep[43][16] = 13.850000;
bbcCutZDep[44][16] = 13.416667;
bbcCutZDep[45][16] = 12.983334;
bbcCutZDep[46][16] = 12.516666;
bbcCutZDep[47][16] = 12.116667;
bbcCutZDep[48][16] = 11.683333;
bbcCutZDep[49][16] = 11.250000;
bbcCutZDep[50][16] = 10.816667;
bbcCutZDep[51][16] = 10.383333;
bbcCutZDep[52][16] = 9.983334;
bbcCutZDep[53][16] = 9.550000;
bbcCutZDep[54][16] = 9.150000;
bbcCutZDep[55][16] = 8.716666;
bbcCutZDep[56][16] = 8.316667;
bbcCutZDep[57][16] = 7.883333;
bbcCutZDep[58][16] = 7.483333;
bbcCutZDep[59][16] = 7.050000;
bbcCutZDep[60][16] = 6.650000;
bbcCutZDep[61][16] = 6.216667;
bbcCutZDep[62][16] = 5.816667;
bbcCutZDep[63][16] = 5.383333;
bbcCutZDep[64][16] = 4.950000;
bbcCutZDep[65][16] = 4.516667;
bbcCutZDep[66][16] = 4.083333;
bbcCutZDep[67][16] = 3.650000;
bbcCutZDep[68][16] = 3.216667;
bbcCutZDep[69][16] = 2.750000;
bbcCutZDep[70][16] = 2.283333;
bbcCutZDep[71][16] = 1.816667;
bbcCutZDep[72][16] = 1.316667;
bbcCutZDep[73][16] = 0.850000;
bbcCutZDep[74][16] = -0.500000;
bbcCutZDep[ 0][17] = 999.983337;
bbcCutZDep[ 1][17] = 58.083332;
bbcCutZDep[ 2][17] = 51.849998;
bbcCutZDep[ 3][17] = 48.016666;
bbcCutZDep[ 4][17] = 45.150002;
bbcCutZDep[ 5][17] = 42.883335;
bbcCutZDep[ 6][17] = 40.950001;
bbcCutZDep[ 7][17] = 39.250000;
bbcCutZDep[ 8][17] = 37.783333;
bbcCutZDep[ 9][17] = 36.416668;
bbcCutZDep[10][17] = 35.183334;
bbcCutZDep[11][17] = 34.049999;
bbcCutZDep[12][17] = 33.016666;
bbcCutZDep[13][17] = 32.016666;
bbcCutZDep[14][17] = 31.083334;
bbcCutZDep[15][17] = 30.183332;
bbcCutZDep[16][17] = 29.350000;
bbcCutZDep[17][17] = 28.549999;
bbcCutZDep[18][17] = 27.750000;
bbcCutZDep[19][17] = 27.016666;
bbcCutZDep[20][17] = 26.316668;
bbcCutZDep[21][17] = 25.616667;
bbcCutZDep[22][17] = 24.950001;
bbcCutZDep[23][17] = 24.316668;
bbcCutZDep[24][17] = 23.683332;
bbcCutZDep[25][17] = 23.083334;
bbcCutZDep[26][17] = 22.483334;
bbcCutZDep[27][17] = 21.883333;
bbcCutZDep[28][17] = 21.316668;
bbcCutZDep[29][17] = 20.783333;
bbcCutZDep[30][17] = 20.216667;
bbcCutZDep[31][17] = 19.683332;
bbcCutZDep[32][17] = 19.183332;
bbcCutZDep[33][17] = 18.650000;
bbcCutZDep[34][17] = 18.150000;
bbcCutZDep[35][17] = 17.650000;
bbcCutZDep[36][17] = 17.150000;
bbcCutZDep[37][17] = 16.683332;
bbcCutZDep[38][17] = 16.216667;
bbcCutZDep[39][17] = 15.716666;
bbcCutZDep[40][17] = 15.283334;
bbcCutZDep[41][17] = 14.816667;
bbcCutZDep[42][17] = 14.350000;
bbcCutZDep[43][17] = 13.916667;
bbcCutZDep[44][17] = 13.450000;
bbcCutZDep[45][17] = 13.016666;
bbcCutZDep[46][17] = 12.583333;
bbcCutZDep[47][17] = 12.150000;
bbcCutZDep[48][17] = 11.716666;
bbcCutZDep[49][17] = 11.283334;
bbcCutZDep[50][17] = 10.850000;
bbcCutZDep[51][17] = 10.450000;
bbcCutZDep[52][17] = 10.016666;
bbcCutZDep[53][17] = 9.616667;
bbcCutZDep[54][17] = 9.183333;
bbcCutZDep[55][17] = 8.750000;
bbcCutZDep[56][17] = 8.350000;
bbcCutZDep[57][17] = 7.916667;
bbcCutZDep[58][17] = 7.516667;
bbcCutZDep[59][17] = 7.083333;
bbcCutZDep[60][17] = 6.683333;
bbcCutZDep[61][17] = 6.250000;
bbcCutZDep[62][17] = 5.850000;
bbcCutZDep[63][17] = 5.416667;
bbcCutZDep[64][17] = 4.983333;
bbcCutZDep[65][17] = 4.550000;
bbcCutZDep[66][17] = 4.116667;
bbcCutZDep[67][17] = 3.683333;
bbcCutZDep[68][17] = 3.216667;
bbcCutZDep[69][17] = 2.783333;
bbcCutZDep[70][17] = 2.316667;
bbcCutZDep[71][17] = 1.850000;
bbcCutZDep[72][17] = 1.316667;
bbcCutZDep[73][17] = 0.850000;
bbcCutZDep[74][17] = -0.500000;
bbcCutZDep[ 0][18] = 999.983337;
bbcCutZDep[ 1][18] = 58.283333;
bbcCutZDep[ 2][18] = 52.016666;
bbcCutZDep[ 3][18] = 48.150002;
bbcCutZDep[ 4][18] = 45.316666;
bbcCutZDep[ 5][18] = 43.016666;
bbcCutZDep[ 6][18] = 41.049999;
bbcCutZDep[ 7][18] = 39.383335;
bbcCutZDep[ 8][18] = 37.883335;
bbcCutZDep[ 9][18] = 36.549999;
bbcCutZDep[10][18] = 35.316666;
bbcCutZDep[11][18] = 34.183334;
bbcCutZDep[12][18] = 33.116665;
bbcCutZDep[13][18] = 32.116665;
bbcCutZDep[14][18] = 31.183332;
bbcCutZDep[15][18] = 30.283333;
bbcCutZDep[16][18] = 29.450001;
bbcCutZDep[17][18] = 28.616667;
bbcCutZDep[18][18] = 27.850000;
bbcCutZDep[19][18] = 27.116667;
bbcCutZDep[20][18] = 26.383333;
bbcCutZDep[21][18] = 25.716667;
bbcCutZDep[22][18] = 25.049999;
bbcCutZDep[23][18] = 24.383333;
bbcCutZDep[24][18] = 23.750000;
bbcCutZDep[25][18] = 23.150000;
bbcCutZDep[26][18] = 22.549999;
bbcCutZDep[27][18] = 21.983334;
bbcCutZDep[28][18] = 21.416666;
bbcCutZDep[29][18] = 20.850000;
bbcCutZDep[30][18] = 20.283333;
bbcCutZDep[31][18] = 19.750000;
bbcCutZDep[32][18] = 19.250000;
bbcCutZDep[33][18] = 18.716667;
bbcCutZDep[34][18] = 18.216667;
bbcCutZDep[35][18] = 17.716667;
bbcCutZDep[36][18] = 17.216667;
bbcCutZDep[37][18] = 16.750000;
bbcCutZDep[38][18] = 16.250000;
bbcCutZDep[39][18] = 15.783334;
bbcCutZDep[40][18] = 15.316667;
bbcCutZDep[41][18] = 14.850000;
bbcCutZDep[42][18] = 14.416667;
bbcCutZDep[43][18] = 13.950000;
bbcCutZDep[44][18] = 13.516666;
bbcCutZDep[45][18] = 13.083333;
bbcCutZDep[46][18] = 12.616667;
bbcCutZDep[47][18] = 12.183333;
bbcCutZDep[48][18] = 11.750000;
bbcCutZDep[49][18] = 11.350000;
bbcCutZDep[50][18] = 10.916667;
bbcCutZDep[51][18] = 10.483334;
bbcCutZDep[52][18] = 10.050000;
bbcCutZDep[53][18] = 9.650000;
bbcCutZDep[54][18] = 9.216666;
bbcCutZDep[55][18] = 8.816667;
bbcCutZDep[56][18] = 8.383333;
bbcCutZDep[57][18] = 7.983333;
bbcCutZDep[58][18] = 7.550000;
bbcCutZDep[59][18] = 7.150000;
bbcCutZDep[60][18] = 6.716667;
bbcCutZDep[61][18] = 6.316667;
bbcCutZDep[62][18] = 5.883333;
bbcCutZDep[63][18] = 5.450000;
bbcCutZDep[64][18] = 5.016667;
bbcCutZDep[65][18] = 4.583333;
bbcCutZDep[66][18] = 4.150000;
bbcCutZDep[67][18] = 3.716667;
bbcCutZDep[68][18] = 3.250000;
bbcCutZDep[69][18] = 2.783333;
bbcCutZDep[70][18] = 2.316667;
bbcCutZDep[71][18] = 1.850000;
bbcCutZDep[72][18] = 1.316667;
bbcCutZDep[73][18] = 0.850000;
bbcCutZDep[74][18] = -0.500000;
bbcCutZDep[ 0][19] = 999.983337;
bbcCutZDep[ 1][19] = 58.349998;
bbcCutZDep[ 2][19] = 52.116665;
bbcCutZDep[ 3][19] = 48.250000;
bbcCutZDep[ 4][19] = 45.383335;
bbcCutZDep[ 5][19] = 43.083332;
bbcCutZDep[ 6][19] = 41.150002;
bbcCutZDep[ 7][19] = 39.483334;
bbcCutZDep[ 8][19] = 37.983334;
bbcCutZDep[ 9][19] = 36.616665;
bbcCutZDep[10][19] = 35.383335;
bbcCutZDep[11][19] = 34.250000;
bbcCutZDep[12][19] = 33.183334;
bbcCutZDep[13][19] = 32.183334;
bbcCutZDep[14][19] = 31.250000;
bbcCutZDep[15][19] = 30.383333;
bbcCutZDep[16][19] = 29.516666;
bbcCutZDep[17][19] = 28.716667;
bbcCutZDep[18][19] = 27.916666;
bbcCutZDep[19][19] = 27.183332;
bbcCutZDep[20][19] = 26.483334;
bbcCutZDep[21][19] = 25.783333;
bbcCutZDep[22][19] = 25.116667;
bbcCutZDep[23][19] = 24.483334;
bbcCutZDep[24][19] = 23.850000;
bbcCutZDep[25][19] = 23.216667;
bbcCutZDep[26][19] = 22.616667;
bbcCutZDep[27][19] = 22.049999;
bbcCutZDep[28][19] = 21.483334;
bbcCutZDep[29][19] = 20.916666;
bbcCutZDep[30][19] = 20.383333;
bbcCutZDep[31][19] = 19.850000;
bbcCutZDep[32][19] = 19.316668;
bbcCutZDep[33][19] = 18.783333;
bbcCutZDep[34][19] = 18.283333;
bbcCutZDep[35][19] = 17.783333;
bbcCutZDep[36][19] = 17.283333;
bbcCutZDep[37][19] = 16.816668;
bbcCutZDep[38][19] = 16.316668;
bbcCutZDep[39][19] = 15.850000;
bbcCutZDep[40][19] = 15.383333;
bbcCutZDep[41][19] = 14.916667;
bbcCutZDep[42][19] = 14.483334;
bbcCutZDep[43][19] = 14.016666;
bbcCutZDep[44][19] = 13.583333;
bbcCutZDep[45][19] = 13.150000;
bbcCutZDep[46][19] = 12.683333;
bbcCutZDep[47][19] = 12.250000;
bbcCutZDep[48][19] = 11.816667;
bbcCutZDep[49][19] = 11.383333;
bbcCutZDep[50][19] = 10.983334;
bbcCutZDep[51][19] = 10.550000;
bbcCutZDep[52][19] = 10.116667;
bbcCutZDep[53][19] = 9.716666;
bbcCutZDep[54][19] = 9.283334;
bbcCutZDep[55][19] = 8.850000;
bbcCutZDep[56][19] = 8.450000;
bbcCutZDep[57][19] = 8.016666;
bbcCutZDep[58][19] = 7.616667;
bbcCutZDep[59][19] = 7.183333;
bbcCutZDep[60][19] = 6.783333;
bbcCutZDep[61][19] = 6.350000;
bbcCutZDep[62][19] = 5.916667;
bbcCutZDep[63][19] = 5.483333;
bbcCutZDep[64][19] = 5.050000;
bbcCutZDep[65][19] = 4.616667;
bbcCutZDep[66][19] = 4.183333;
bbcCutZDep[67][19] = 3.750000;
bbcCutZDep[68][19] = 3.283333;
bbcCutZDep[69][19] = 2.816667;
bbcCutZDep[70][19] = 2.350000;
bbcCutZDep[71][19] = 1.883333;
bbcCutZDep[72][19] = 1.350000;
bbcCutZDep[73][19] = 0.883333;
bbcCutZDep[74][19] = -0.500000;


}

void
Run16dAu39GeVCentralityReco::InitScaleFactor ()
{

  // scale factors determined from <bbc-south charge> on a run-by-run basis

  for (int i=0;i<5000;i++) BBCScaleFactor[i] = 0.00000;

  // j.nagle - updated from online production on 06/21/2016
  // j.nagle -updated from full taxi production on 10/05/2016

BBCScaleFactor[0] = 0.994;
BBCScaleFactor[1] = 0.994;
BBCScaleFactor[3] = 0.989;
BBCScaleFactor[84] = 0.997;
BBCScaleFactor[85] = 1.000;
BBCScaleFactor[88] = 1.005;
BBCScaleFactor[89] = 1.008;
BBCScaleFactor[115] = 1.006;
BBCScaleFactor[119] = 1.008;
BBCScaleFactor[120] = 1.007;
BBCScaleFactor[121] = 1.010;
BBCScaleFactor[123] = 1.011;
BBCScaleFactor[124] = 1.005;
BBCScaleFactor[125] = 1.009;
BBCScaleFactor[126] = 1.005;
BBCScaleFactor[157] = 1.022;
BBCScaleFactor[158] = 1.024;
BBCScaleFactor[159] = 1.023;
BBCScaleFactor[160] = 1.022;
BBCScaleFactor[163] = 1.020;
BBCScaleFactor[164] = 1.024;
BBCScaleFactor[165] = 1.015;
BBCScaleFactor[198] = 1.023;
BBCScaleFactor[199] = 1.021;
BBCScaleFactor[200] = 1.020;
BBCScaleFactor[201] = 1.023;
BBCScaleFactor[202] = 1.024;
BBCScaleFactor[203] = 1.024;
BBCScaleFactor[204] = 1.022;
BBCScaleFactor[205] = 1.020;
BBCScaleFactor[206] = 1.023;
BBCScaleFactor[207] = 1.030;
BBCScaleFactor[208] = 1.026;
BBCScaleFactor[209] = 1.030;
BBCScaleFactor[213] = 1.027;
BBCScaleFactor[215] = 1.029;
BBCScaleFactor[216] = 1.029;
BBCScaleFactor[217] = 1.025;
BBCScaleFactor[263] = 1.033;
BBCScaleFactor[264] = 1.033;
BBCScaleFactor[265] = 1.034;
BBCScaleFactor[267] = 1.032;
BBCScaleFactor[270] = 1.037;
BBCScaleFactor[271] = 1.037;
BBCScaleFactor[272] = 1.034;
BBCScaleFactor[275] = 1.036;
BBCScaleFactor[276] = 1.034;
BBCScaleFactor[278] = 1.034;
BBCScaleFactor[280] = 1.033;
BBCScaleFactor[281] = 1.039;
BBCScaleFactor[282] = 1.041;
BBCScaleFactor[283] = 1.041;
BBCScaleFactor[285] = 1.038;
BBCScaleFactor[286] = 1.038;
BBCScaleFactor[287] = 1.037;
BBCScaleFactor[288] = 1.038;
BBCScaleFactor[289] = 1.038;
BBCScaleFactor[291] = 1.036;
BBCScaleFactor[295] = 1.041;
BBCScaleFactor[296] = 1.040;
BBCScaleFactor[297] = 1.043;
BBCScaleFactor[298] = 1.043;
BBCScaleFactor[299] = 1.042;
BBCScaleFactor[300] = 1.048;
BBCScaleFactor[301] = 1.046;
BBCScaleFactor[302] = 1.041;
BBCScaleFactor[304] = 1.046;
BBCScaleFactor[305] = 1.045;
BBCScaleFactor[306] = 1.044;
BBCScaleFactor[308] = 1.049;
BBCScaleFactor[309] = 1.051;
BBCScaleFactor[310] = 1.051;
BBCScaleFactor[311] = 1.055;
BBCScaleFactor[312] = 1.049;
BBCScaleFactor[313] = 1.048;
BBCScaleFactor[314] = 1.050;
BBCScaleFactor[315] = 1.055;
BBCScaleFactor[316] = 1.049;
BBCScaleFactor[321] = 1.046;
BBCScaleFactor[322] = 1.045;
BBCScaleFactor[323] = 1.047;
BBCScaleFactor[325] = 1.047;
BBCScaleFactor[326] = 1.049;
BBCScaleFactor[327] = 1.045;
BBCScaleFactor[362] = 1.040;
BBCScaleFactor[373] = 1.059;
BBCScaleFactor[374] = 1.055;
BBCScaleFactor[375] = 1.054;
BBCScaleFactor[377] = 1.052;
BBCScaleFactor[378] = 1.056;
BBCScaleFactor[379] = 1.058;
BBCScaleFactor[380] = 1.059;
BBCScaleFactor[480] = 1.074;
BBCScaleFactor[481] = 1.075;
BBCScaleFactor[484] = 1.072;
BBCScaleFactor[518] = 1.081;
BBCScaleFactor[519] = 1.085;
BBCScaleFactor[520] = 1.084;
BBCScaleFactor[521] = 1.085;
BBCScaleFactor[523] = 1.082;
BBCScaleFactor[524] = 1.080;
BBCScaleFactor[525] = 1.080;
BBCScaleFactor[526] = 1.078;
BBCScaleFactor[527] = 1.076;
BBCScaleFactor[528] = 1.079;
BBCScaleFactor[531] = 1.087;
BBCScaleFactor[532] = 1.086;

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
Run16dAu39GeVCentralityReco::GetScaleFactor(const int runnumber) const
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
Run16dAu39GeVCentralityReco::GetQAStatus(const int runnumber) const
{

  if (! (runnumber >= MINRUN && runnumber <= MAXRUN))
    {
      cout << "Run16dAu39GeVCentralityReco::GetQAStatus - ERROR in run number range (Not a Run-16 dAu 39 GeV run number) = " << runnumber << endl;
      return 0;
    }

  int qastatus = QAstatus[runnumber - MINRUN];

  cout << "Run16dAu39GeVCentralityReco::GetQAStatus - Run " << runnumber << " Centrality QA = ";
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
