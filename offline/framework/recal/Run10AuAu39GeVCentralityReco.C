#include "Run10AuAu39GeVCentralityReco.h"

#include <PHGlobal.h>
#include <RunHeader.h>
#include <TrigLvl1.h>

#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <PHCompositeNode.h>

#include <TriggerHelper.h>

#include <iostream>

using namespace std;

Run10AuAu39GeVCentralityReco::Run10AuAu39GeVCentralityReco() : Recalibrator("Run10AuAu39GeVCentralityReco")
{
  // add these initialization routines to the constructor so that they are immediately available
  InitArray1();
  InitScaleFactor();

  baseclasses.insert("PHGlobal");
  return ;
}

int Run10AuAu39GeVCentralityReco::isValidRun(const int runno) const
{
  if (runno < 313500 || runno > 315000)
    // end of 62GeV:	313322
    // begin of 39GeV:	313591
    // end of 39GeV:	314994
    // begin of 7GeV:	315450
    {
      return 0;
    }

  return 1;
}

void Run10AuAu39GeVCentralityReco::help()
{
  cout << "===================================================================" << endl;
  cout << "Run10AuAu39GeVCentralityReco::help method output"                    << endl;
  cout << "===================================================================" << endl;
}

int Run10AuAu39GeVCentralityReco::process_event(PHCompositeNode *topNode)
{

  int intcentral = getCentralityByBBCRun10AuAu39(topNode);
  if (intcentral < 0)
    {
      if ( Verbosity() > 0 )
	cout << PHWHERE << " centrality < 0" << endl;
      return ABORTEVENT;
    }
  
  PHGlobal* global = findNode::getClass<PHGlobal>(topNode, inputnodename.c_str());
  global->setCentrality((float)intcentral);

  return EVENT_OK;

}

int Run10AuAu39GeVCentralityReco::getCentralityByBBCRun10AuAu39(PHCompositeNode* topNode)
{
  PHGlobal* d_global = findNode::getClass<PHGlobal>(topNode, inputnodename.c_str());
  RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  TrigLvl1* d_trg = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");

  float bbc1 = d_global->getBbcChargeN();
  float bbc2 = d_global->getBbcChargeS();
  float zvertex = d_global->getBbcZVertex();
  int   runno   = run->get_RunNumber();

  int bbcM1 = d_global->getBbcMultS();
  int bbcM2 = d_global->getBbcMultN();

  int bbc0Scale = 0;
  int Scaled = d_trg->get_lvl1_trigscaled();
  if ((Scaled & 0x00000002) > 0) bbc0Scale = 1;

  int bbc1Scale = 0;
  int Live = d_trg->get_lvl1_triglive();
  if (((Live & 0x00000004) > 0) && (bbc0Scale > 0)) bbc1Scale = 1;

  bool isMinBias = ((bbcM1>=2) && (bbcM2>=2) && (bbc1Scale==1)) ? true : false;
  if(!isMinBias) return -999;

  return getCentralityByBBCRun10AuAu39(bbc1, bbc2, zvertex, runno);
}

int Run10AuAu39GeVCentralityReco::getCentralityByBBCRun10AuAu39(float bbc1, float bbc2, float zvertex, int runno)
{
  // only include the range -30 : +30 cm
  int zbin = (int) ((zvertex + 30.0) / 5.0);
  if (zvertex < -30.0 || zvertex > +30.0) return -999;
  if (zbin < 0 || zbin > 11) return -999;

  float bbcsum = bbc1 + bbc2;

  // apply a run-to-run scale factor correction to the bbcsum (south)  
  Float_t scalefactor = GetScaleFactor(runno);
  if (scalefactor <= 0.0) {
    cout << "Run10AuAu39GeVCentralityReco::getCentralityByBBCRun10AuAu39 - ERROR with run " << 
      runno << " and retrieved scale factor " << scalefactor << endl;
    return -999;
  }
  bbcsum = bbcsum / scalefactor;

  float centrality = -1.0;
  // use lookup table to determine centrality of this event using clock method
  for (int jjj = 0; jjj<86; jjj++) 
    {
      if (bbcsum < bbcCutZDep[jjj][zbin] && bbcsum >= bbcCutZDep[jjj+1][zbin]) 
	{
	  centrality = (float) (jjj + 1);
	  break;
	}
    }
    
  if (centrality == -1.0)
    printf("Run10AuAu39GeVCentralityReco::getCentralityByBBCRun10AuAu39 ERROR centrality -1 ==> bbcsum = %f %f %f\n", bbcsum, bbc1, bbc2);
  
  return ((int) centrality);
  
}

void Run10AuAu39GeVCentralityReco::InitArray1()
{
 
  // Lookup Table (BBC Charge Cut Values) for Run10AuAu39-Centrality
  // (-30<bbcz<30 cm, 5 cm step)

  bbcCutZDep[ 0][ 0] = 4999.995117;
  bbcCutZDep[ 1][ 0] = 429.524994;
  bbcCutZDep[ 2][ 0] = 411.535004;
  bbcCutZDep[ 3][ 0] = 398.424988;
  bbcCutZDep[ 4][ 0] = 387.285004;
  bbcCutZDep[ 5][ 0] = 377.204987;
  bbcCutZDep[ 6][ 0] = 367.665009;
  bbcCutZDep[ 7][ 0] = 358.404999;
  bbcCutZDep[ 8][ 0] = 349.355011;
  bbcCutZDep[ 9][ 0] = 340.415009;
  bbcCutZDep[10][ 0] = 331.644989;
  bbcCutZDep[11][ 0] = 322.964996;
  bbcCutZDep[12][ 0] = 314.404999;
  bbcCutZDep[13][ 0] = 306.024994;
  bbcCutZDep[14][ 0] = 297.755005;
  bbcCutZDep[15][ 0] = 289.654999;
  bbcCutZDep[16][ 0] = 281.674988;
  bbcCutZDep[17][ 0] = 273.855011;
  bbcCutZDep[18][ 0] = 266.174988;
  bbcCutZDep[19][ 0] = 258.644989;
  bbcCutZDep[20][ 0] = 251.294998;
  bbcCutZDep[21][ 0] = 244.095001;
  bbcCutZDep[22][ 0] = 237.005005;
  bbcCutZDep[23][ 0] = 230.085007;
  bbcCutZDep[24][ 0] = 223.315002;
  bbcCutZDep[25][ 0] = 216.615005;
  bbcCutZDep[26][ 0] = 210.054993;
  bbcCutZDep[27][ 0] = 203.604996;
  bbcCutZDep[28][ 0] = 197.345001;
  bbcCutZDep[29][ 0] = 191.184998;
  bbcCutZDep[30][ 0] = 185.154999;
  bbcCutZDep[31][ 0] = 179.205002;
  bbcCutZDep[32][ 0] = 173.425003;
  bbcCutZDep[33][ 0] = 167.755005;
  bbcCutZDep[34][ 0] = 162.184998;
  bbcCutZDep[35][ 0] = 156.725006;
  bbcCutZDep[36][ 0] = 151.395004;
  bbcCutZDep[37][ 0] = 146.175003;
  bbcCutZDep[38][ 0] = 141.054993;
  bbcCutZDep[39][ 0] = 136.024994;
  bbcCutZDep[40][ 0] = 131.104996;
  bbcCutZDep[41][ 0] = 126.294998;
  bbcCutZDep[42][ 0] = 121.574997;
  bbcCutZDep[43][ 0] = 116.964996;
  bbcCutZDep[44][ 0] = 112.474998;
  bbcCutZDep[45][ 0] = 108.074997;
  bbcCutZDep[46][ 0] = 103.785004;
  bbcCutZDep[47][ 0] = 99.574997;
  bbcCutZDep[48][ 0] = 95.464996;
  bbcCutZDep[49][ 0] = 91.455002;
  bbcCutZDep[50][ 0] = 87.544998;
  bbcCutZDep[51][ 0] = 83.745003;
  bbcCutZDep[52][ 0] = 80.044998;
  bbcCutZDep[53][ 0] = 76.425003;
  bbcCutZDep[54][ 0] = 72.904999;
  bbcCutZDep[55][ 0] = 69.464996;
  bbcCutZDep[56][ 0] = 66.125000;
  bbcCutZDep[57][ 0] = 62.884998;
  bbcCutZDep[58][ 0] = 59.744999;
  bbcCutZDep[59][ 0] = 56.695000;
  bbcCutZDep[60][ 0] = 53.744999;
  bbcCutZDep[61][ 0] = 50.865002;
  bbcCutZDep[62][ 0] = 48.095001;
  bbcCutZDep[63][ 0] = 45.415001;
  bbcCutZDep[64][ 0] = 42.825001;
  bbcCutZDep[65][ 0] = 40.314999;
  bbcCutZDep[66][ 0] = 37.915001;
  bbcCutZDep[67][ 0] = 35.584999;
  bbcCutZDep[68][ 0] = 33.345001;
  bbcCutZDep[69][ 0] = 31.184999;
  bbcCutZDep[70][ 0] = 29.105000;
  bbcCutZDep[71][ 0] = 27.105000;
  bbcCutZDep[72][ 0] = 25.174999;
  bbcCutZDep[73][ 0] = 23.315001;
  bbcCutZDep[74][ 0] = 21.535000;
  bbcCutZDep[75][ 0] = 19.815001;
  bbcCutZDep[76][ 0] = 18.155001;
  bbcCutZDep[77][ 0] = 16.555000;
  bbcCutZDep[78][ 0] = 15.025000;
  bbcCutZDep[79][ 0] = 13.525000;
  bbcCutZDep[80][ 0] = 12.075000;
  bbcCutZDep[81][ 0] = 10.665000;
  bbcCutZDep[82][ 0] = 9.275000;
  bbcCutZDep[83][ 0] = 7.885000;
  bbcCutZDep[84][ 0] = 6.445000;
  bbcCutZDep[85][ 0] = 4.875000;
  bbcCutZDep[86][ 0] = -.005000;
  bbcCutZDep[ 0][ 1] = 4999.995117;
  bbcCutZDep[ 1][ 1] = 430.595001;
  bbcCutZDep[ 2][ 1] = 412.834991;
  bbcCutZDep[ 3][ 1] = 400.015015;
  bbcCutZDep[ 4][ 1] = 389.174988;
  bbcCutZDep[ 5][ 1] = 379.295013;
  bbcCutZDep[ 6][ 1] = 369.964996;
  bbcCutZDep[ 7][ 1] = 360.954987;
  bbcCutZDep[ 8][ 1] = 352.105011;
  bbcCutZDep[ 9][ 1] = 343.364990;
  bbcCutZDep[10][ 1] = 334.704987;
  bbcCutZDep[11][ 1] = 326.114990;
  bbcCutZDep[12][ 1] = 317.625000;
  bbcCutZDep[13][ 1] = 309.225006;
  bbcCutZDep[14][ 1] = 300.945007;
  bbcCutZDep[15][ 1] = 292.774994;
  bbcCutZDep[16][ 1] = 284.744995;
  bbcCutZDep[17][ 1] = 276.904999;
  bbcCutZDep[18][ 1] = 269.114990;
  bbcCutZDep[19][ 1] = 261.505005;
  bbcCutZDep[20][ 1] = 253.985001;
  bbcCutZDep[21][ 1] = 246.654999;
  bbcCutZDep[22][ 1] = 239.395004;
  bbcCutZDep[23][ 1] = 232.294998;
  bbcCutZDep[24][ 1] = 225.345001;
  bbcCutZDep[25][ 1] = 218.485001;
  bbcCutZDep[26][ 1] = 211.785004;
  bbcCutZDep[27][ 1] = 205.214996;
  bbcCutZDep[28][ 1] = 198.785004;
  bbcCutZDep[29][ 1] = 192.475006;
  bbcCutZDep[30][ 1] = 186.294998;
  bbcCutZDep[31][ 1] = 180.225006;
  bbcCutZDep[32][ 1] = 174.285004;
  bbcCutZDep[33][ 1] = 168.455002;
  bbcCutZDep[34][ 1] = 162.714996;
  bbcCutZDep[35][ 1] = 157.145004;
  bbcCutZDep[36][ 1] = 151.654999;
  bbcCutZDep[37][ 1] = 146.255005;
  bbcCutZDep[38][ 1] = 140.994995;
  bbcCutZDep[39][ 1] = 135.865005;
  bbcCutZDep[40][ 1] = 130.815002;
  bbcCutZDep[41][ 1] = 125.875000;
  bbcCutZDep[42][ 1] = 121.055000;
  bbcCutZDep[43][ 1] = 116.355003;
  bbcCutZDep[44][ 1] = 111.764999;
  bbcCutZDep[45][ 1] = 107.264999;
  bbcCutZDep[46][ 1] = 102.885002;
  bbcCutZDep[47][ 1] = 98.605003;
  bbcCutZDep[48][ 1] = 94.415001;
  bbcCutZDep[49][ 1] = 90.364998;
  bbcCutZDep[50][ 1] = 86.394997;
  bbcCutZDep[51][ 1] = 82.525002;
  bbcCutZDep[52][ 1] = 78.754997;
  bbcCutZDep[53][ 1] = 75.095001;
  bbcCutZDep[54][ 1] = 71.535004;
  bbcCutZDep[55][ 1] = 68.065002;
  bbcCutZDep[56][ 1] = 64.705002;
  bbcCutZDep[57][ 1] = 61.445000;
  bbcCutZDep[58][ 1] = 58.285000;
  bbcCutZDep[59][ 1] = 55.224998;
  bbcCutZDep[60][ 1] = 52.264999;
  bbcCutZDep[61][ 1] = 49.395000;
  bbcCutZDep[62][ 1] = 46.634998;
  bbcCutZDep[63][ 1] = 43.965000;
  bbcCutZDep[64][ 1] = 41.395000;
  bbcCutZDep[65][ 1] = 38.924999;
  bbcCutZDep[66][ 1] = 36.544998;
  bbcCutZDep[67][ 1] = 34.255001;
  bbcCutZDep[68][ 1] = 32.064999;
  bbcCutZDep[69][ 1] = 29.955000;
  bbcCutZDep[70][ 1] = 27.934999;
  bbcCutZDep[71][ 1] = 25.985001;
  bbcCutZDep[72][ 1] = 24.115000;
  bbcCutZDep[73][ 1] = 22.325001;
  bbcCutZDep[74][ 1] = 20.615000;
  bbcCutZDep[75][ 1] = 18.965000;
  bbcCutZDep[76][ 1] = 17.375000;
  bbcCutZDep[77][ 1] = 15.865000;
  bbcCutZDep[78][ 1] = 14.405000;
  bbcCutZDep[79][ 1] = 12.975000;
  bbcCutZDep[80][ 1] = 11.605000;
  bbcCutZDep[81][ 1] = 10.255000;
  bbcCutZDep[82][ 1] = 8.935000;
  bbcCutZDep[83][ 1] = 7.615000;
  bbcCutZDep[84][ 1] = 6.245000;
  bbcCutZDep[85][ 1] = 4.745000;
  bbcCutZDep[86][ 1] = -.005000;
  bbcCutZDep[ 0][ 2] = 4999.995117;
  bbcCutZDep[ 1][ 2] = 429.494995;
  bbcCutZDep[ 2][ 2] = 411.845001;
  bbcCutZDep[ 3][ 2] = 399.035004;
  bbcCutZDep[ 4][ 2] = 388.165009;
  bbcCutZDep[ 5][ 2] = 378.355011;
  bbcCutZDep[ 6][ 2] = 369.054993;
  bbcCutZDep[ 7][ 2] = 360.084991;
  bbcCutZDep[ 8][ 2] = 351.225006;
  bbcCutZDep[ 9][ 2] = 342.494995;
  bbcCutZDep[10][ 2] = 333.864990;
  bbcCutZDep[11][ 2] = 325.265015;
  bbcCutZDep[12][ 2] = 316.785004;
  bbcCutZDep[13][ 2] = 308.404999;
  bbcCutZDep[14][ 2] = 300.144989;
  bbcCutZDep[15][ 2] = 291.994995;
  bbcCutZDep[16][ 2] = 283.964996;
  bbcCutZDep[17][ 2] = 276.045013;
  bbcCutZDep[18][ 2] = 268.295013;
  bbcCutZDep[19][ 2] = 260.654999;
  bbcCutZDep[20][ 2] = 253.145004;
  bbcCutZDep[21][ 2] = 245.744995;
  bbcCutZDep[22][ 2] = 238.544998;
  bbcCutZDep[23][ 2] = 231.464996;
  bbcCutZDep[24][ 2] = 224.535004;
  bbcCutZDep[25][ 2] = 217.705002;
  bbcCutZDep[26][ 2] = 211.024994;
  bbcCutZDep[27][ 2] = 204.464996;
  bbcCutZDep[28][ 2] = 198.005005;
  bbcCutZDep[29][ 2] = 191.695007;
  bbcCutZDep[30][ 2] = 185.494995;
  bbcCutZDep[31][ 2] = 179.455002;
  bbcCutZDep[32][ 2] = 173.505005;
  bbcCutZDep[33][ 2] = 167.684998;
  bbcCutZDep[34][ 2] = 161.975006;
  bbcCutZDep[35][ 2] = 156.375000;
  bbcCutZDep[36][ 2] = 150.895004;
  bbcCutZDep[37][ 2] = 145.554993;
  bbcCutZDep[38][ 2] = 140.285004;
  bbcCutZDep[39][ 2] = 135.145004;
  bbcCutZDep[40][ 2] = 130.125000;
  bbcCutZDep[41][ 2] = 125.195000;
  bbcCutZDep[42][ 2] = 120.385002;
  bbcCutZDep[43][ 2] = 115.675003;
  bbcCutZDep[44][ 2] = 111.074997;
  bbcCutZDep[45][ 2] = 106.595001;
  bbcCutZDep[46][ 2] = 102.214996;
  bbcCutZDep[47][ 2] = 97.925003;
  bbcCutZDep[48][ 2] = 93.764999;
  bbcCutZDep[49][ 2] = 89.705002;
  bbcCutZDep[50][ 2] = 85.735001;
  bbcCutZDep[51][ 2] = 81.875000;
  bbcCutZDep[52][ 2] = 78.114998;
  bbcCutZDep[53][ 2] = 74.455002;
  bbcCutZDep[54][ 2] = 70.894997;
  bbcCutZDep[55][ 2] = 67.445000;
  bbcCutZDep[56][ 2] = 64.095001;
  bbcCutZDep[57][ 2] = 60.855000;
  bbcCutZDep[58][ 2] = 57.705002;
  bbcCutZDep[59][ 2] = 54.654999;
  bbcCutZDep[60][ 2] = 51.705002;
  bbcCutZDep[61][ 2] = 48.855000;
  bbcCutZDep[62][ 2] = 46.105000;
  bbcCutZDep[63][ 2] = 43.455002;
  bbcCutZDep[64][ 2] = 40.895000;
  bbcCutZDep[65][ 2] = 38.435001;
  bbcCutZDep[66][ 2] = 36.064999;
  bbcCutZDep[67][ 2] = 33.794998;
  bbcCutZDep[68][ 2] = 31.615000;
  bbcCutZDep[69][ 2] = 29.514999;
  bbcCutZDep[70][ 2] = 27.504999;
  bbcCutZDep[71][ 2] = 25.575001;
  bbcCutZDep[72][ 2] = 23.735001;
  bbcCutZDep[73][ 2] = 21.965000;
  bbcCutZDep[74][ 2] = 20.264999;
  bbcCutZDep[75][ 2] = 18.645000;
  bbcCutZDep[76][ 2] = 17.084999;
  bbcCutZDep[77][ 2] = 15.585000;
  bbcCutZDep[78][ 2] = 14.145000;
  bbcCutZDep[79][ 2] = 12.755000;
  bbcCutZDep[80][ 2] = 11.405000;
  bbcCutZDep[81][ 2] = 10.085000;
  bbcCutZDep[82][ 2] = 8.785000;
  bbcCutZDep[83][ 2] = 7.495000;
  bbcCutZDep[84][ 2] = 6.165000;
  bbcCutZDep[85][ 2] = 4.695000;
  bbcCutZDep[86][ 2] = -.005000;
  bbcCutZDep[ 0][ 3] = 4999.995117;
  bbcCutZDep[ 1][ 3] = 426.045013;
  bbcCutZDep[ 2][ 3] = 408.434998;
  bbcCutZDep[ 3][ 3] = 395.704987;
  bbcCutZDep[ 4][ 3] = 384.964996;
  bbcCutZDep[ 5][ 3] = 375.244995;
  bbcCutZDep[ 6][ 3] = 366.024994;
  bbcCutZDep[ 7][ 3] = 357.184998;
  bbcCutZDep[ 8][ 3] = 348.464996;
  bbcCutZDep[ 9][ 3] = 339.834991;
  bbcCutZDep[10][ 3] = 331.274994;
  bbcCutZDep[11][ 3] = 322.825012;
  bbcCutZDep[12][ 3] = 314.424988;
  bbcCutZDep[13][ 3] = 306.174988;
  bbcCutZDep[14][ 3] = 297.984985;
  bbcCutZDep[15][ 3] = 289.924988;
  bbcCutZDep[16][ 3] = 281.994995;
  bbcCutZDep[17][ 3] = 274.174988;
  bbcCutZDep[18][ 3] = 266.494995;
  bbcCutZDep[19][ 3] = 258.894989;
  bbcCutZDep[20][ 3] = 251.475006;
  bbcCutZDep[21][ 3] = 244.195007;
  bbcCutZDep[22][ 3] = 237.044998;
  bbcCutZDep[23][ 3] = 230.005005;
  bbcCutZDep[24][ 3] = 223.115005;
  bbcCutZDep[25][ 3] = 216.375000;
  bbcCutZDep[26][ 3] = 209.735001;
  bbcCutZDep[27][ 3] = 203.244995;
  bbcCutZDep[28][ 3] = 196.865005;
  bbcCutZDep[29][ 3] = 190.604996;
  bbcCutZDep[30][ 3] = 184.455002;
  bbcCutZDep[31][ 3] = 178.445007;
  bbcCutZDep[32][ 3] = 172.535004;
  bbcCutZDep[33][ 3] = 166.755005;
  bbcCutZDep[34][ 3] = 161.104996;
  bbcCutZDep[35][ 3] = 155.554993;
  bbcCutZDep[36][ 3] = 150.115005;
  bbcCutZDep[37][ 3] = 144.785004;
  bbcCutZDep[38][ 3] = 139.554993;
  bbcCutZDep[39][ 3] = 134.445007;
  bbcCutZDep[40][ 3] = 129.464996;
  bbcCutZDep[41][ 3] = 124.584999;
  bbcCutZDep[42][ 3] = 119.794998;
  bbcCutZDep[43][ 3] = 115.125000;
  bbcCutZDep[44][ 3] = 110.544998;
  bbcCutZDep[45][ 3] = 106.074997;
  bbcCutZDep[46][ 3] = 101.714996;
  bbcCutZDep[47][ 3] = 97.445000;
  bbcCutZDep[48][ 3] = 93.305000;
  bbcCutZDep[49][ 3] = 89.264999;
  bbcCutZDep[50][ 3] = 85.305000;
  bbcCutZDep[51][ 3] = 81.464996;
  bbcCutZDep[52][ 3] = 77.714996;
  bbcCutZDep[53][ 3] = 74.084999;
  bbcCutZDep[54][ 3] = 70.535004;
  bbcCutZDep[55][ 3] = 67.095001;
  bbcCutZDep[56][ 3] = 63.755001;
  bbcCutZDep[57][ 3] = 60.505001;
  bbcCutZDep[58][ 3] = 57.375000;
  bbcCutZDep[59][ 3] = 54.325001;
  bbcCutZDep[60][ 3] = 51.395000;
  bbcCutZDep[61][ 3] = 48.555000;
  bbcCutZDep[62][ 3] = 45.814999;
  bbcCutZDep[63][ 3] = 43.174999;
  bbcCutZDep[64][ 3] = 40.634998;
  bbcCutZDep[65][ 3] = 38.195000;
  bbcCutZDep[66][ 3] = 35.845001;
  bbcCutZDep[67][ 3] = 33.584999;
  bbcCutZDep[68][ 3] = 31.415001;
  bbcCutZDep[69][ 3] = 29.325001;
  bbcCutZDep[70][ 3] = 27.325001;
  bbcCutZDep[71][ 3] = 25.415001;
  bbcCutZDep[72][ 3] = 23.575001;
  bbcCutZDep[73][ 3] = 21.815001;
  bbcCutZDep[74][ 3] = 20.135000;
  bbcCutZDep[75][ 3] = 18.525000;
  bbcCutZDep[76][ 3] = 16.975000;
  bbcCutZDep[77][ 3] = 15.485000;
  bbcCutZDep[78][ 3] = 14.045000;
  bbcCutZDep[79][ 3] = 12.665000;
  bbcCutZDep[80][ 3] = 11.335000;
  bbcCutZDep[81][ 3] = 10.025000;
  bbcCutZDep[82][ 3] = 8.745000;
  bbcCutZDep[83][ 3] = 7.455000;
  bbcCutZDep[84][ 3] = 6.135000;
  bbcCutZDep[85][ 3] = 4.675000;
  bbcCutZDep[86][ 3] = -.005000;
  bbcCutZDep[ 0][ 4] = 4999.995117;
  bbcCutZDep[ 1][ 4] = 418.255005;
  bbcCutZDep[ 2][ 4] = 400.894989;
  bbcCutZDep[ 3][ 4] = 388.445007;
  bbcCutZDep[ 4][ 4] = 377.954987;
  bbcCutZDep[ 5][ 4] = 368.464996;
  bbcCutZDep[ 6][ 4] = 359.535004;
  bbcCutZDep[ 7][ 4] = 350.864990;
  bbcCutZDep[ 8][ 4] = 342.345001;
  bbcCutZDep[ 9][ 4] = 333.984985;
  bbcCutZDep[10][ 4] = 325.714996;
  bbcCutZDep[11][ 4] = 317.484985;
  bbcCutZDep[12][ 4] = 309.304993;
  bbcCutZDep[13][ 4] = 301.234985;
  bbcCutZDep[14][ 4] = 293.244995;
  bbcCutZDep[15][ 4] = 285.364990;
  bbcCutZDep[16][ 4] = 277.595001;
  bbcCutZDep[17][ 4] = 269.954987;
  bbcCutZDep[18][ 4] = 262.434998;
  bbcCutZDep[19][ 4] = 255.065002;
  bbcCutZDep[20][ 4] = 247.804993;
  bbcCutZDep[21][ 4] = 240.684998;
  bbcCutZDep[22][ 4] = 233.705002;
  bbcCutZDep[23][ 4] = 226.835007;
  bbcCutZDep[24][ 4] = 220.104996;
  bbcCutZDep[25][ 4] = 213.475006;
  bbcCutZDep[26][ 4] = 206.975006;
  bbcCutZDep[27][ 4] = 200.595001;
  bbcCutZDep[28][ 4] = 194.335007;
  bbcCutZDep[29][ 4] = 188.184998;
  bbcCutZDep[30][ 4] = 182.145004;
  bbcCutZDep[31][ 4] = 176.225006;
  bbcCutZDep[32][ 4] = 170.455002;
  bbcCutZDep[33][ 4] = 164.764999;
  bbcCutZDep[34][ 4] = 159.175003;
  bbcCutZDep[35][ 4] = 153.705002;
  bbcCutZDep[36][ 4] = 148.345001;
  bbcCutZDep[37][ 4] = 143.115005;
  bbcCutZDep[38][ 4] = 137.964996;
  bbcCutZDep[39][ 4] = 132.945007;
  bbcCutZDep[40][ 4] = 128.014999;
  bbcCutZDep[41][ 4] = 123.195000;
  bbcCutZDep[42][ 4] = 118.495003;
  bbcCutZDep[43][ 4] = 113.885002;
  bbcCutZDep[44][ 4] = 109.375000;
  bbcCutZDep[45][ 4] = 104.995003;
  bbcCutZDep[46][ 4] = 100.695000;
  bbcCutZDep[47][ 4] = 96.514999;
  bbcCutZDep[48][ 4] = 92.415001;
  bbcCutZDep[49][ 4] = 88.394997;
  bbcCutZDep[50][ 4] = 84.504997;
  bbcCutZDep[51][ 4] = 80.695000;
  bbcCutZDep[52][ 4] = 77.004997;
  bbcCutZDep[53][ 4] = 73.404999;
  bbcCutZDep[54][ 4] = 69.904999;
  bbcCutZDep[55][ 4] = 66.495003;
  bbcCutZDep[56][ 4] = 63.195000;
  bbcCutZDep[57][ 4] = 59.994999;
  bbcCutZDep[58][ 4] = 56.884998;
  bbcCutZDep[59][ 4] = 53.884998;
  bbcCutZDep[60][ 4] = 50.985001;
  bbcCutZDep[61][ 4] = 48.165001;
  bbcCutZDep[62][ 4] = 45.455002;
  bbcCutZDep[63][ 4] = 42.845001;
  bbcCutZDep[64][ 4] = 40.334999;
  bbcCutZDep[65][ 4] = 37.915001;
  bbcCutZDep[66][ 4] = 35.584999;
  bbcCutZDep[67][ 4] = 33.345001;
  bbcCutZDep[68][ 4] = 31.195000;
  bbcCutZDep[69][ 4] = 29.125000;
  bbcCutZDep[70][ 4] = 27.135000;
  bbcCutZDep[71][ 4] = 25.245001;
  bbcCutZDep[72][ 4] = 23.415001;
  bbcCutZDep[73][ 4] = 21.684999;
  bbcCutZDep[74][ 4] = 20.004999;
  bbcCutZDep[75][ 4] = 18.405001;
  bbcCutZDep[76][ 4] = 16.865000;
  bbcCutZDep[77][ 4] = 15.385000;
  bbcCutZDep[78][ 4] = 13.965000;
  bbcCutZDep[79][ 4] = 12.585000;
  bbcCutZDep[80][ 4] = 11.265000;
  bbcCutZDep[81][ 4] = 9.965000;
  bbcCutZDep[82][ 4] = 8.685000;
  bbcCutZDep[83][ 4] = 7.415000;
  bbcCutZDep[84][ 4] = 6.105000;
  bbcCutZDep[85][ 4] = 4.645000;
  bbcCutZDep[86][ 4] = -.005000;
  bbcCutZDep[ 0][ 5] = 4999.995117;
  bbcCutZDep[ 1][ 5] = 410.725006;
  bbcCutZDep[ 2][ 5] = 393.875000;
  bbcCutZDep[ 3][ 5] = 381.755005;
  bbcCutZDep[ 4][ 5] = 371.515015;
  bbcCutZDep[ 5][ 5] = 362.274994;
  bbcCutZDep[ 6][ 5] = 353.565002;
  bbcCutZDep[ 7][ 5] = 345.114990;
  bbcCutZDep[ 8][ 5] = 336.875000;
  bbcCutZDep[ 9][ 5] = 328.684998;
  bbcCutZDep[10][ 5] = 320.614990;
  bbcCutZDep[11][ 5] = 312.575012;
  bbcCutZDep[12][ 5] = 304.625000;
  bbcCutZDep[13][ 5] = 296.704987;
  bbcCutZDep[14][ 5] = 288.904999;
  bbcCutZDep[15][ 5] = 281.214996;
  bbcCutZDep[16][ 5] = 273.644989;
  bbcCutZDep[17][ 5] = 266.154999;
  bbcCutZDep[18][ 5] = 258.815002;
  bbcCutZDep[19][ 5] = 251.544998;
  bbcCutZDep[20][ 5] = 244.425003;
  bbcCutZDep[21][ 5] = 237.434998;
  bbcCutZDep[22][ 5] = 230.565002;
  bbcCutZDep[23][ 5] = 223.794998;
  bbcCutZDep[24][ 5] = 217.154999;
  bbcCutZDep[25][ 5] = 210.654999;
  bbcCutZDep[26][ 5] = 204.255005;
  bbcCutZDep[27][ 5] = 197.994995;
  bbcCutZDep[28][ 5] = 191.824997;
  bbcCutZDep[29][ 5] = 185.785004;
  bbcCutZDep[30][ 5] = 179.884995;
  bbcCutZDep[31][ 5] = 174.074997;
  bbcCutZDep[32][ 5] = 168.365005;
  bbcCutZDep[33][ 5] = 162.774994;
  bbcCutZDep[34][ 5] = 157.274994;
  bbcCutZDep[35][ 5] = 151.904999;
  bbcCutZDep[36][ 5] = 146.664993;
  bbcCutZDep[37][ 5] = 141.514999;
  bbcCutZDep[38][ 5] = 136.445007;
  bbcCutZDep[39][ 5] = 131.485001;
  bbcCutZDep[40][ 5] = 126.635002;
  bbcCutZDep[41][ 5] = 121.904999;
  bbcCutZDep[42][ 5] = 117.264999;
  bbcCutZDep[43][ 5] = 112.724998;
  bbcCutZDep[44][ 5] = 108.294998;
  bbcCutZDep[45][ 5] = 103.955002;
  bbcCutZDep[46][ 5] = 99.695000;
  bbcCutZDep[47][ 5] = 95.565002;
  bbcCutZDep[48][ 5] = 91.525002;
  bbcCutZDep[49][ 5] = 87.584999;
  bbcCutZDep[50][ 5] = 83.745003;
  bbcCutZDep[51][ 5] = 79.985001;
  bbcCutZDep[52][ 5] = 76.334999;
  bbcCutZDep[53][ 5] = 72.764999;
  bbcCutZDep[54][ 5] = 69.305000;
  bbcCutZDep[55][ 5] = 65.945000;
  bbcCutZDep[56][ 5] = 62.685001;
  bbcCutZDep[57][ 5] = 59.514999;
  bbcCutZDep[58][ 5] = 56.445000;
  bbcCutZDep[59][ 5] = 53.474998;
  bbcCutZDep[60][ 5] = 50.595001;
  bbcCutZDep[61][ 5] = 47.814999;
  bbcCutZDep[62][ 5] = 45.125000;
  bbcCutZDep[63][ 5] = 42.535000;
  bbcCutZDep[64][ 5] = 40.044998;
  bbcCutZDep[65][ 5] = 37.645000;
  bbcCutZDep[66][ 5] = 35.334999;
  bbcCutZDep[67][ 5] = 33.105000;
  bbcCutZDep[68][ 5] = 30.965000;
  bbcCutZDep[69][ 5] = 28.915001;
  bbcCutZDep[70][ 5] = 26.955000;
  bbcCutZDep[71][ 5] = 25.075001;
  bbcCutZDep[72][ 5] = 23.275000;
  bbcCutZDep[73][ 5] = 21.535000;
  bbcCutZDep[74][ 5] = 19.885000;
  bbcCutZDep[75][ 5] = 18.295000;
  bbcCutZDep[76][ 5] = 16.764999;
  bbcCutZDep[77][ 5] = 15.305000;
  bbcCutZDep[78][ 5] = 13.895000;
  bbcCutZDep[79][ 5] = 12.525000;
  bbcCutZDep[80][ 5] = 11.205000;
  bbcCutZDep[81][ 5] = 9.925000;
  bbcCutZDep[82][ 5] = 8.655000;
  bbcCutZDep[83][ 5] = 7.395000;
  bbcCutZDep[84][ 5] = 6.085000;
  bbcCutZDep[85][ 5] = 4.645000;
  bbcCutZDep[86][ 5] = -.005000;
  bbcCutZDep[ 0][ 6] = 4999.995117;
  bbcCutZDep[ 1][ 6] = 416.614990;
  bbcCutZDep[ 2][ 6] = 399.385010;
  bbcCutZDep[ 3][ 6] = 387.024994;
  bbcCutZDep[ 4][ 6] = 376.554993;
  bbcCutZDep[ 5][ 6] = 367.095001;
  bbcCutZDep[ 6][ 6] = 358.195007;
  bbcCutZDep[ 7][ 6] = 349.584991;
  bbcCutZDep[ 8][ 6] = 341.154999;
  bbcCutZDep[ 9][ 6] = 332.845001;
  bbcCutZDep[10][ 6] = 324.575012;
  bbcCutZDep[11][ 6] = 316.364990;
  bbcCutZDep[12][ 6] = 308.255005;
  bbcCutZDep[13][ 6] = 300.214996;
  bbcCutZDep[14][ 6] = 292.285004;
  bbcCutZDep[15][ 6] = 284.494995;
  bbcCutZDep[16][ 6] = 276.785004;
  bbcCutZDep[17][ 6] = 269.225006;
  bbcCutZDep[18][ 6] = 261.744995;
  bbcCutZDep[19][ 6] = 254.384995;
  bbcCutZDep[20][ 6] = 247.154999;
  bbcCutZDep[21][ 6] = 240.054993;
  bbcCutZDep[22][ 6] = 233.104996;
  bbcCutZDep[23][ 6] = 226.264999;
  bbcCutZDep[24][ 6] = 219.565002;
  bbcCutZDep[25][ 6] = 212.964996;
  bbcCutZDep[26][ 6] = 206.494995;
  bbcCutZDep[27][ 6] = 200.125000;
  bbcCutZDep[28][ 6] = 193.925003;
  bbcCutZDep[29][ 6] = 187.824997;
  bbcCutZDep[30][ 6] = 181.824997;
  bbcCutZDep[31][ 6] = 175.934998;
  bbcCutZDep[32][ 6] = 170.154999;
  bbcCutZDep[33][ 6] = 164.475006;
  bbcCutZDep[34][ 6] = 158.925003;
  bbcCutZDep[35][ 6] = 153.485001;
  bbcCutZDep[36][ 6] = 148.134995;
  bbcCutZDep[37][ 6] = 142.895004;
  bbcCutZDep[38][ 6] = 137.755005;
  bbcCutZDep[39][ 6] = 132.755005;
  bbcCutZDep[40][ 6] = 127.845001;
  bbcCutZDep[41][ 6] = 123.035004;
  bbcCutZDep[42][ 6] = 118.324997;
  bbcCutZDep[43][ 6] = 113.745003;
  bbcCutZDep[44][ 6] = 109.235001;
  bbcCutZDep[45][ 6] = 104.845001;
  bbcCutZDep[46][ 6] = 100.555000;
  bbcCutZDep[47][ 6] = 96.355003;
  bbcCutZDep[48][ 6] = 92.285004;
  bbcCutZDep[49][ 6] = 88.285004;
  bbcCutZDep[50][ 6] = 84.404999;
  bbcCutZDep[51][ 6] = 80.595001;
  bbcCutZDep[52][ 6] = 76.904999;
  bbcCutZDep[53][ 6] = 73.305000;
  bbcCutZDep[54][ 6] = 69.815002;
  bbcCutZDep[55][ 6] = 66.415001;
  bbcCutZDep[56][ 6] = 63.105000;
  bbcCutZDep[57][ 6] = 59.904999;
  bbcCutZDep[58][ 6] = 56.814999;
  bbcCutZDep[59][ 6] = 53.814999;
  bbcCutZDep[60][ 6] = 50.915001;
  bbcCutZDep[61][ 6] = 48.115002;
  bbcCutZDep[62][ 6] = 45.415001;
  bbcCutZDep[63][ 6] = 42.805000;
  bbcCutZDep[64][ 6] = 40.294998;
  bbcCutZDep[65][ 6] = 37.884998;
  bbcCutZDep[66][ 6] = 35.555000;
  bbcCutZDep[67][ 6] = 33.314999;
  bbcCutZDep[68][ 6] = 31.165001;
  bbcCutZDep[69][ 6] = 29.105000;
  bbcCutZDep[70][ 6] = 27.115000;
  bbcCutZDep[71][ 6] = 25.225000;
  bbcCutZDep[72][ 6] = 23.405001;
  bbcCutZDep[73][ 6] = 21.665001;
  bbcCutZDep[74][ 6] = 19.995001;
  bbcCutZDep[75][ 6] = 18.395000;
  bbcCutZDep[76][ 6] = 16.855000;
  bbcCutZDep[77][ 6] = 15.375000;
  bbcCutZDep[78][ 6] = 13.955000;
  bbcCutZDep[79][ 6] = 12.595000;
  bbcCutZDep[80][ 6] = 11.265000;
  bbcCutZDep[81][ 6] = 9.965000;
  bbcCutZDep[82][ 6] = 8.685000;
  bbcCutZDep[83][ 6] = 7.415000;
  bbcCutZDep[84][ 6] = 6.105000;
  bbcCutZDep[85][ 6] = 4.655000;
  bbcCutZDep[86][ 6] = -.005000;
  bbcCutZDep[ 0][ 7] = 4999.995117;
  bbcCutZDep[ 1][ 7] = 427.654999;
  bbcCutZDep[ 2][ 7] = 409.875000;
  bbcCutZDep[ 3][ 7] = 397.144989;
  bbcCutZDep[ 4][ 7] = 386.385010;
  bbcCutZDep[ 5][ 7] = 376.605011;
  bbcCutZDep[ 6][ 7] = 367.364990;
  bbcCutZDep[ 7][ 7] = 358.484985;
  bbcCutZDep[ 8][ 7] = 349.774994;
  bbcCutZDep[ 9][ 7] = 341.114990;
  bbcCutZDep[10][ 7] = 332.505005;
  bbcCutZDep[11][ 7] = 324.015015;
  bbcCutZDep[12][ 7] = 315.614990;
  bbcCutZDep[13][ 7] = 307.315002;
  bbcCutZDep[14][ 7] = 299.095001;
  bbcCutZDep[15][ 7] = 290.975006;
  bbcCutZDep[16][ 7] = 283.015015;
  bbcCutZDep[17][ 7] = 275.174988;
  bbcCutZDep[18][ 7] = 267.445007;
  bbcCutZDep[19][ 7] = 259.885010;
  bbcCutZDep[20][ 7] = 252.455002;
  bbcCutZDep[21][ 7] = 245.154999;
  bbcCutZDep[22][ 7] = 237.975006;
  bbcCutZDep[23][ 7] = 230.914993;
  bbcCutZDep[24][ 7] = 224.005005;
  bbcCutZDep[25][ 7] = 217.235001;
  bbcCutZDep[26][ 7] = 210.595001;
  bbcCutZDep[27][ 7] = 204.065002;
  bbcCutZDep[28][ 7] = 197.664993;
  bbcCutZDep[29][ 7] = 191.365005;
  bbcCutZDep[30][ 7] = 185.195007;
  bbcCutZDep[31][ 7] = 179.125000;
  bbcCutZDep[32][ 7] = 173.205002;
  bbcCutZDep[33][ 7] = 167.404999;
  bbcCutZDep[34][ 7] = 161.725006;
  bbcCutZDep[35][ 7] = 156.154999;
  bbcCutZDep[36][ 7] = 150.684998;
  bbcCutZDep[37][ 7] = 145.324997;
  bbcCutZDep[38][ 7] = 140.104996;
  bbcCutZDep[39][ 7] = 134.985001;
  bbcCutZDep[40][ 7] = 129.955002;
  bbcCutZDep[41][ 7] = 125.044998;
  bbcCutZDep[42][ 7] = 120.214996;
  bbcCutZDep[43][ 7] = 115.514999;
  bbcCutZDep[44][ 7] = 110.925003;
  bbcCutZDep[45][ 7] = 106.425003;
  bbcCutZDep[46][ 7] = 102.035004;
  bbcCutZDep[47][ 7] = 97.764999;
  bbcCutZDep[48][ 7] = 93.584999;
  bbcCutZDep[49][ 7] = 89.504997;
  bbcCutZDep[50][ 7] = 85.535004;
  bbcCutZDep[51][ 7] = 81.675003;
  bbcCutZDep[52][ 7] = 77.915001;
  bbcCutZDep[53][ 7] = 74.254997;
  bbcCutZDep[54][ 7] = 70.695000;
  bbcCutZDep[55][ 7] = 67.235001;
  bbcCutZDep[56][ 7] = 63.865002;
  bbcCutZDep[57][ 7] = 60.625000;
  bbcCutZDep[58][ 7] = 57.465000;
  bbcCutZDep[59][ 7] = 54.424999;
  bbcCutZDep[60][ 7] = 51.485001;
  bbcCutZDep[61][ 7] = 48.645000;
  bbcCutZDep[62][ 7] = 45.904999;
  bbcCutZDep[63][ 7] = 43.244999;
  bbcCutZDep[64][ 7] = 40.685001;
  bbcCutZDep[65][ 7] = 38.224998;
  bbcCutZDep[66][ 7] = 35.875000;
  bbcCutZDep[67][ 7] = 33.595001;
  bbcCutZDep[68][ 7] = 31.415001;
  bbcCutZDep[69][ 7] = 29.325001;
  bbcCutZDep[70][ 7] = 27.325001;
  bbcCutZDep[71][ 7] = 25.405001;
  bbcCutZDep[72][ 7] = 23.565001;
  bbcCutZDep[73][ 7] = 21.805000;
  bbcCutZDep[74][ 7] = 20.115000;
  bbcCutZDep[75][ 7] = 18.495001;
  bbcCutZDep[76][ 7] = 16.945000;
  bbcCutZDep[77][ 7] = 15.465000;
  bbcCutZDep[78][ 7] = 14.035000;
  bbcCutZDep[79][ 7] = 12.645000;
  bbcCutZDep[80][ 7] = 11.315000;
  bbcCutZDep[81][ 7] = 10.005000;
  bbcCutZDep[82][ 7] = 8.725000;
  bbcCutZDep[83][ 7] = 7.445000;
  bbcCutZDep[84][ 7] = 6.125000;
  bbcCutZDep[85][ 7] = 4.655000;
  bbcCutZDep[86][ 7] = -.005000;
  bbcCutZDep[ 0][ 8] = 4999.995117;
  bbcCutZDep[ 1][ 8] = 433.524994;
  bbcCutZDep[ 2][ 8] = 415.614990;
  bbcCutZDep[ 3][ 8] = 402.714996;
  bbcCutZDep[ 4][ 8] = 391.755005;
  bbcCutZDep[ 5][ 8] = 381.815002;
  bbcCutZDep[ 6][ 8] = 372.424988;
  bbcCutZDep[ 7][ 8] = 363.295013;
  bbcCutZDep[ 8][ 8] = 354.334991;
  bbcCutZDep[ 9][ 8] = 345.515015;
  bbcCutZDep[10][ 8] = 336.765015;
  bbcCutZDep[11][ 8] = 328.095001;
  bbcCutZDep[12][ 8] = 319.524994;
  bbcCutZDep[13][ 8] = 311.035004;
  bbcCutZDep[14][ 8] = 302.674988;
  bbcCutZDep[15][ 8] = 294.415009;
  bbcCutZDep[16][ 8] = 286.325012;
  bbcCutZDep[17][ 8] = 278.375000;
  bbcCutZDep[18][ 8] = 270.575012;
  bbcCutZDep[19][ 8] = 262.864990;
  bbcCutZDep[20][ 8] = 255.285004;
  bbcCutZDep[21][ 8] = 247.865005;
  bbcCutZDep[22][ 8] = 240.574997;
  bbcCutZDep[23][ 8] = 233.425003;
  bbcCutZDep[24][ 8] = 226.384995;
  bbcCutZDep[25][ 8] = 219.464996;
  bbcCutZDep[26][ 8] = 212.695007;
  bbcCutZDep[27][ 8] = 206.085007;
  bbcCutZDep[28][ 8] = 199.585007;
  bbcCutZDep[29][ 8] = 193.205002;
  bbcCutZDep[30][ 8] = 186.945007;
  bbcCutZDep[31][ 8] = 180.824997;
  bbcCutZDep[32][ 8] = 174.804993;
  bbcCutZDep[33][ 8] = 168.904999;
  bbcCutZDep[34][ 8] = 163.154999;
  bbcCutZDep[35][ 8] = 157.505005;
  bbcCutZDep[36][ 8] = 151.964996;
  bbcCutZDep[37][ 8] = 146.544998;
  bbcCutZDep[38][ 8] = 141.244995;
  bbcCutZDep[39][ 8] = 136.044998;
  bbcCutZDep[40][ 8] = 130.964996;
  bbcCutZDep[41][ 8] = 125.995003;
  bbcCutZDep[42][ 8] = 121.135002;
  bbcCutZDep[43][ 8] = 116.385002;
  bbcCutZDep[44][ 8] = 111.735001;
  bbcCutZDep[45][ 8] = 107.195000;
  bbcCutZDep[46][ 8] = 102.785004;
  bbcCutZDep[47][ 8] = 98.485001;
  bbcCutZDep[48][ 8] = 94.264999;
  bbcCutZDep[49][ 8] = 90.154999;
  bbcCutZDep[50][ 8] = 86.144997;
  bbcCutZDep[51][ 8] = 82.245003;
  bbcCutZDep[52][ 8] = 78.455002;
  bbcCutZDep[53][ 8] = 74.754997;
  bbcCutZDep[54][ 8] = 71.144997;
  bbcCutZDep[55][ 8] = 67.644997;
  bbcCutZDep[56][ 8] = 64.264999;
  bbcCutZDep[57][ 8] = 60.985001;
  bbcCutZDep[58][ 8] = 57.805000;
  bbcCutZDep[59][ 8] = 54.724998;
  bbcCutZDep[60][ 8] = 51.744999;
  bbcCutZDep[61][ 8] = 48.884998;
  bbcCutZDep[62][ 8] = 46.125000;
  bbcCutZDep[63][ 8] = 43.455002;
  bbcCutZDep[64][ 8] = 40.884998;
  bbcCutZDep[65][ 8] = 38.415001;
  bbcCutZDep[66][ 8] = 36.025002;
  bbcCutZDep[67][ 8] = 33.744999;
  bbcCutZDep[68][ 8] = 31.545000;
  bbcCutZDep[69][ 8] = 29.455000;
  bbcCutZDep[70][ 8] = 27.445000;
  bbcCutZDep[71][ 8] = 25.514999;
  bbcCutZDep[72][ 8] = 23.674999;
  bbcCutZDep[73][ 8] = 21.905001;
  bbcCutZDep[74][ 8] = 20.205000;
  bbcCutZDep[75][ 8] = 18.575001;
  bbcCutZDep[76][ 8] = 17.035000;
  bbcCutZDep[77][ 8] = 15.535000;
  bbcCutZDep[78][ 8] = 14.095000;
  bbcCutZDep[79][ 8] = 12.705000;
  bbcCutZDep[80][ 8] = 11.355000;
  bbcCutZDep[81][ 8] = 10.045000;
  bbcCutZDep[82][ 8] = 8.755000;
  bbcCutZDep[83][ 8] = 7.465000;
  bbcCutZDep[84][ 8] = 6.135000;
  bbcCutZDep[85][ 8] = 4.675000;
  bbcCutZDep[86][ 8] = -.005000;
  bbcCutZDep[ 0][ 9] = 4999.995117;
  bbcCutZDep[ 1][ 9] = 435.625000;
  bbcCutZDep[ 2][ 9] = 417.625000;
  bbcCutZDep[ 3][ 9] = 404.595001;
  bbcCutZDep[ 4][ 9] = 393.584991;
  bbcCutZDep[ 5][ 9] = 383.554993;
  bbcCutZDep[ 6][ 9] = 374.035004;
  bbcCutZDep[ 7][ 9] = 364.845001;
  bbcCutZDep[ 8][ 9] = 355.845001;
  bbcCutZDep[ 9][ 9] = 346.924988;
  bbcCutZDep[10][ 9] = 338.105011;
  bbcCutZDep[11][ 9] = 329.364990;
  bbcCutZDep[12][ 9] = 320.714996;
  bbcCutZDep[13][ 9] = 312.174988;
  bbcCutZDep[14][ 9] = 303.725006;
  bbcCutZDep[15][ 9] = 295.445007;
  bbcCutZDep[16][ 9] = 287.274994;
  bbcCutZDep[17][ 9] = 279.285004;
  bbcCutZDep[18][ 9] = 271.355011;
  bbcCutZDep[19][ 9] = 263.595001;
  bbcCutZDep[20][ 9] = 255.975006;
  bbcCutZDep[21][ 9] = 248.505005;
  bbcCutZDep[22][ 9] = 241.154999;
  bbcCutZDep[23][ 9] = 233.975006;
  bbcCutZDep[24][ 9] = 226.925003;
  bbcCutZDep[25][ 9] = 220.005005;
  bbcCutZDep[26][ 9] = 213.205002;
  bbcCutZDep[27][ 9] = 206.565002;
  bbcCutZDep[28][ 9] = 200.024994;
  bbcCutZDep[29][ 9] = 193.615005;
  bbcCutZDep[30][ 9] = 187.324997;
  bbcCutZDep[31][ 9] = 181.164993;
  bbcCutZDep[32][ 9] = 175.154999;
  bbcCutZDep[33][ 9] = 169.264999;
  bbcCutZDep[34][ 9] = 163.475006;
  bbcCutZDep[35][ 9] = 157.824997;
  bbcCutZDep[36][ 9] = 152.274994;
  bbcCutZDep[37][ 9] = 146.854996;
  bbcCutZDep[38][ 9] = 141.544998;
  bbcCutZDep[39][ 9] = 136.335007;
  bbcCutZDep[40][ 9] = 131.244995;
  bbcCutZDep[41][ 9] = 126.254997;
  bbcCutZDep[42][ 9] = 121.394997;
  bbcCutZDep[43][ 9] = 116.635002;
  bbcCutZDep[44][ 9] = 111.995003;
  bbcCutZDep[45][ 9] = 107.445000;
  bbcCutZDep[46][ 9] = 103.035004;
  bbcCutZDep[47][ 9] = 98.705002;
  bbcCutZDep[48][ 9] = 94.495003;
  bbcCutZDep[49][ 9] = 90.385002;
  bbcCutZDep[50][ 9] = 86.375000;
  bbcCutZDep[51][ 9] = 82.474998;
  bbcCutZDep[52][ 9] = 78.675003;
  bbcCutZDep[53][ 9] = 74.974998;
  bbcCutZDep[54][ 9] = 71.375000;
  bbcCutZDep[55][ 9] = 67.894997;
  bbcCutZDep[56][ 9] = 64.514999;
  bbcCutZDep[57][ 9] = 61.235001;
  bbcCutZDep[58][ 9] = 58.055000;
  bbcCutZDep[59][ 9] = 54.985001;
  bbcCutZDep[60][ 9] = 52.025002;
  bbcCutZDep[61][ 9] = 49.174999;
  bbcCutZDep[62][ 9] = 46.395000;
  bbcCutZDep[63][ 9] = 43.715000;
  bbcCutZDep[64][ 9] = 41.145000;
  bbcCutZDep[65][ 9] = 38.665001;
  bbcCutZDep[66][ 9] = 36.285000;
  bbcCutZDep[67][ 9] = 33.994999;
  bbcCutZDep[68][ 9] = 31.795000;
  bbcCutZDep[69][ 9] = 29.684999;
  bbcCutZDep[70][ 9] = 27.665001;
  bbcCutZDep[71][ 9] = 25.715000;
  bbcCutZDep[72][ 9] = 23.855000;
  bbcCutZDep[73][ 9] = 22.084999;
  bbcCutZDep[74][ 9] = 20.365000;
  bbcCutZDep[75][ 9] = 18.745001;
  bbcCutZDep[76][ 9] = 17.174999;
  bbcCutZDep[77][ 9] = 15.655000;
  bbcCutZDep[78][ 9] = 14.205000;
  bbcCutZDep[79][ 9] = 12.815000;
  bbcCutZDep[80][ 9] = 11.455000;
  bbcCutZDep[81][ 9] = 10.135000;
  bbcCutZDep[82][ 9] = 8.825000;
  bbcCutZDep[83][ 9] = 7.525000;
  bbcCutZDep[84][ 9] = 6.185000;
  bbcCutZDep[85][ 9] = 4.695000;
  bbcCutZDep[86][ 9] = -.005000;
  bbcCutZDep[ 0][10] = 4999.995117;
  bbcCutZDep[ 1][10] = 436.135010;
  bbcCutZDep[ 2][10] = 418.174988;
  bbcCutZDep[ 3][10] = 405.084991;
  bbcCutZDep[ 4][10] = 394.054993;
  bbcCutZDep[ 5][10] = 383.984985;
  bbcCutZDep[ 6][10] = 374.505005;
  bbcCutZDep[ 7][10] = 365.285004;
  bbcCutZDep[ 8][10] = 356.285004;
  bbcCutZDep[ 9][10] = 347.375000;
  bbcCutZDep[10][10] = 338.595001;
  bbcCutZDep[11][10] = 329.845001;
  bbcCutZDep[12][10] = 321.204987;
  bbcCutZDep[13][10] = 312.734985;
  bbcCutZDep[14][10] = 304.355011;
  bbcCutZDep[15][10] = 296.075012;
  bbcCutZDep[16][10] = 287.904999;
  bbcCutZDep[17][10] = 279.934998;
  bbcCutZDep[18][10] = 272.095001;
  bbcCutZDep[19][10] = 264.364990;
  bbcCutZDep[20][10] = 256.774994;
  bbcCutZDep[21][10] = 249.304993;
  bbcCutZDep[22][10] = 242.024994;
  bbcCutZDep[23][10] = 234.865005;
  bbcCutZDep[24][10] = 227.845001;
  bbcCutZDep[25][10] = 220.964996;
  bbcCutZDep[26][10] = 214.184998;
  bbcCutZDep[27][10] = 207.535004;
  bbcCutZDep[28][10] = 201.035004;
  bbcCutZDep[29][10] = 194.695007;
  bbcCutZDep[30][10] = 188.455002;
  bbcCutZDep[31][10] = 182.324997;
  bbcCutZDep[32][10] = 176.324997;
  bbcCutZDep[33][10] = 170.434998;
  bbcCutZDep[34][10] = 164.664993;
  bbcCutZDep[35][10] = 159.014999;
  bbcCutZDep[36][10] = 153.464996;
  bbcCutZDep[37][10] = 148.074997;
  bbcCutZDep[38][10] = 142.764999;
  bbcCutZDep[39][10] = 137.574997;
  bbcCutZDep[40][10] = 132.475006;
  bbcCutZDep[41][10] = 127.495003;
  bbcCutZDep[42][10] = 122.635002;
  bbcCutZDep[43][10] = 117.904999;
  bbcCutZDep[44][10] = 113.254997;
  bbcCutZDep[45][10] = 108.714996;
  bbcCutZDep[46][10] = 104.285004;
  bbcCutZDep[47][10] = 99.964996;
  bbcCutZDep[48][10] = 95.745003;
  bbcCutZDep[49][10] = 91.635002;
  bbcCutZDep[50][10] = 87.614998;
  bbcCutZDep[51][10] = 83.705002;
  bbcCutZDep[52][10] = 79.904999;
  bbcCutZDep[53][10] = 76.214996;
  bbcCutZDep[54][10] = 72.605003;
  bbcCutZDep[55][10] = 69.105003;
  bbcCutZDep[56][10] = 65.705002;
  bbcCutZDep[57][10] = 62.404999;
  bbcCutZDep[58][10] = 59.215000;
  bbcCutZDep[59][10] = 56.115002;
  bbcCutZDep[60][10] = 53.125000;
  bbcCutZDep[61][10] = 50.224998;
  bbcCutZDep[62][10] = 47.424999;
  bbcCutZDep[63][10] = 44.724998;
  bbcCutZDep[64][10] = 42.115002;
  bbcCutZDep[65][10] = 39.605000;
  bbcCutZDep[66][10] = 37.185001;
  bbcCutZDep[67][10] = 34.875000;
  bbcCutZDep[68][10] = 32.645000;
  bbcCutZDep[69][10] = 30.485001;
  bbcCutZDep[70][10] = 28.424999;
  bbcCutZDep[71][10] = 26.455000;
  bbcCutZDep[72][10] = 24.555000;
  bbcCutZDep[73][10] = 22.725000;
  bbcCutZDep[74][10] = 20.975000;
  bbcCutZDep[75][10] = 19.285000;
  bbcCutZDep[76][10] = 17.684999;
  bbcCutZDep[77][10] = 16.135000;
  bbcCutZDep[78][10] = 14.645000;
  bbcCutZDep[79][10] = 13.195000;
  bbcCutZDep[80][10] = 11.795000;
  bbcCutZDep[81][10] = 10.425000;
  bbcCutZDep[82][10] = 9.075000;
  bbcCutZDep[83][10] = 7.715000;
  bbcCutZDep[84][10] = 6.335000;
  bbcCutZDep[85][10] = 4.795000;
  bbcCutZDep[86][10] = -.005000;
  bbcCutZDep[ 0][11] = 4999.995117;
  bbcCutZDep[ 1][11] = 424.105011;
  bbcCutZDep[ 2][11] = 403.105011;
  bbcCutZDep[ 3][11] = 387.364990;
  bbcCutZDep[ 4][11] = 373.665009;
  bbcCutZDep[ 5][11] = 361.084991;
  bbcCutZDep[ 6][11] = 349.214996;
  bbcCutZDep[ 7][11] = 337.975006;
  bbcCutZDep[ 8][11] = 327.195007;
  bbcCutZDep[ 9][11] = 316.894989;
  bbcCutZDep[10][11] = 306.994995;
  bbcCutZDep[11][11] = 297.535004;
  bbcCutZDep[12][11] = 288.385010;
  bbcCutZDep[13][11] = 279.614990;
  bbcCutZDep[14][11] = 271.125000;
  bbcCutZDep[15][11] = 262.894989;
  bbcCutZDep[16][11] = 254.994995;
  bbcCutZDep[17][11] = 247.324997;
  bbcCutZDep[18][11] = 239.865005;
  bbcCutZDep[19][11] = 232.654999;
  bbcCutZDep[20][11] = 225.615005;
  bbcCutZDep[21][11] = 218.794998;
  bbcCutZDep[22][11] = 212.154999;
  bbcCutZDep[23][11] = 205.684998;
  bbcCutZDep[24][11] = 199.395004;
  bbcCutZDep[25][11] = 193.274994;
  bbcCutZDep[26][11] = 187.285004;
  bbcCutZDep[27][11] = 181.485001;
  bbcCutZDep[28][11] = 175.815002;
  bbcCutZDep[29][11] = 170.274994;
  bbcCutZDep[30][11] = 164.854996;
  bbcCutZDep[31][11] = 159.565002;
  bbcCutZDep[32][11] = 154.395004;
  bbcCutZDep[33][11] = 149.354996;
  bbcCutZDep[34][11] = 144.425003;
  bbcCutZDep[35][11] = 139.585007;
  bbcCutZDep[36][11] = 134.875000;
  bbcCutZDep[37][11] = 130.264999;
  bbcCutZDep[38][11] = 125.764999;
  bbcCutZDep[39][11] = 121.345001;
  bbcCutZDep[40][11] = 117.025002;
  bbcCutZDep[41][11] = 112.805000;
  bbcCutZDep[42][11] = 108.675003;
  bbcCutZDep[43][11] = 104.614998;
  bbcCutZDep[44][11] = 100.665001;
  bbcCutZDep[45][11] = 96.845001;
  bbcCutZDep[46][11] = 93.065002;
  bbcCutZDep[47][11] = 89.394997;
  bbcCutZDep[48][11] = 85.815002;
  bbcCutZDep[49][11] = 82.315002;
  bbcCutZDep[50][11] = 78.894997;
  bbcCutZDep[51][11] = 75.565002;
  bbcCutZDep[52][11] = 72.315002;
  bbcCutZDep[53][11] = 69.144997;
  bbcCutZDep[54][11] = 66.055000;
  bbcCutZDep[55][11] = 63.064999;
  bbcCutZDep[56][11] = 60.154999;
  bbcCutZDep[57][11] = 57.325001;
  bbcCutZDep[58][11] = 54.555000;
  bbcCutZDep[59][11] = 51.875000;
  bbcCutZDep[60][11] = 49.275002;
  bbcCutZDep[61][11] = 46.744999;
  bbcCutZDep[62][11] = 44.275002;
  bbcCutZDep[63][11] = 41.904999;
  bbcCutZDep[64][11] = 39.595001;
  bbcCutZDep[65][11] = 37.365002;
  bbcCutZDep[66][11] = 35.215000;
  bbcCutZDep[67][11] = 33.134998;
  bbcCutZDep[68][11] = 31.115000;
  bbcCutZDep[69][11] = 29.165001;
  bbcCutZDep[70][11] = 27.285000;
  bbcCutZDep[71][11] = 25.465000;
  bbcCutZDep[72][11] = 23.715000;
  bbcCutZDep[73][11] = 22.025000;
  bbcCutZDep[74][11] = 20.395000;
  bbcCutZDep[75][11] = 18.815001;
  bbcCutZDep[76][11] = 17.285000;
  bbcCutZDep[77][11] = 15.815000;
  bbcCutZDep[78][11] = 14.375000;
  bbcCutZDep[79][11] = 12.985000;
  bbcCutZDep[80][11] = 11.635000;
  bbcCutZDep[81][11] = 10.295000;
  bbcCutZDep[82][11] = 8.975000;
  bbcCutZDep[83][11] = 7.655000;
  bbcCutZDep[84][11] = 6.275000;
  bbcCutZDep[85][11] = 4.765000;
  bbcCutZDep[86][11] = -.005000;

}

void Run10AuAu39GeVCentralityReco::InitScaleFactor () 
{

  for (int i=0;i<1500;i++) BBCScaleFactor[i] = 0.0;

  BBCScaleFactor[  91] = 0.989;
  BBCScaleFactor[  92] = 0.991;
  BBCScaleFactor[  95] = 0.991;
  BBCScaleFactor[  96] = 0.993;
  BBCScaleFactor[ 106] = 0.992;
  BBCScaleFactor[ 109] = 0.990;
  BBCScaleFactor[ 118] = 0.993;
  BBCScaleFactor[ 119] = 0.995;
  BBCScaleFactor[ 120] = 0.994;
  BBCScaleFactor[ 125] = 0.994;
  BBCScaleFactor[ 128] = 0.995;
  BBCScaleFactor[ 129] = 0.994;
  BBCScaleFactor[ 141] = 0.995;
  BBCScaleFactor[ 225] = 0.993;
  BBCScaleFactor[ 226] = 0.996;
  BBCScaleFactor[ 227] = 0.994;
  BBCScaleFactor[ 228] = 0.994;
  BBCScaleFactor[ 229] = 0.997;
  BBCScaleFactor[ 231] = 0.991;
  BBCScaleFactor[ 236] = 0.995;
  BBCScaleFactor[ 237] = 0.997;
  BBCScaleFactor[ 239] = 0.995;
  BBCScaleFactor[ 240] = 0.999;
  BBCScaleFactor[ 243] = 0.996;
  BBCScaleFactor[ 244] = 0.997;
  BBCScaleFactor[ 245] = 0.998;
  BBCScaleFactor[ 246] = 0.997;
  BBCScaleFactor[ 248] = 0.998;
  BBCScaleFactor[ 249] = 1.000;
  BBCScaleFactor[ 251] = 0.997;
  BBCScaleFactor[ 252] = 0.998;
  BBCScaleFactor[ 254] = 0.998;
  BBCScaleFactor[ 255] = 0.998;
  BBCScaleFactor[ 257] = 0.999;
  BBCScaleFactor[ 258] = 0.999;
  BBCScaleFactor[ 290] = 0.997;
  BBCScaleFactor[ 291] = 0.998;
  BBCScaleFactor[ 292] = 0.997;
  BBCScaleFactor[ 293] = 0.997;
  BBCScaleFactor[ 349] = 0.995;
  BBCScaleFactor[ 350] = 0.996;
  BBCScaleFactor[ 351] = 0.998;
  BBCScaleFactor[ 352] = 0.995;
  BBCScaleFactor[ 383] = 0.997;
  BBCScaleFactor[ 384] = 0.997;
  BBCScaleFactor[ 385] = 0.998;
  BBCScaleFactor[ 387] = 0.997;
  BBCScaleFactor[ 396] = 1.000;
  BBCScaleFactor[ 401] = 1.000;
  BBCScaleFactor[ 404] = 0.999;
  BBCScaleFactor[ 407] = 1.000;
  BBCScaleFactor[ 408] = 0.999;
  BBCScaleFactor[ 409] = 1.000;
  BBCScaleFactor[ 410] = 0.999;
  BBCScaleFactor[ 411] = 0.999;
  BBCScaleFactor[ 467] = 0.999;
  BBCScaleFactor[ 468] = 1.001;
  BBCScaleFactor[ 471] = 1.003;
  BBCScaleFactor[ 472] = 0.999;
  BBCScaleFactor[ 478] = 1.001;
  BBCScaleFactor[ 481] = 1.005;
  BBCScaleFactor[ 511] = 1.001;
  BBCScaleFactor[ 512] = 1.003;
  BBCScaleFactor[ 513] = 1.002;
  BBCScaleFactor[ 516] = 1.005;
  BBCScaleFactor[ 518] = 1.006;
  BBCScaleFactor[ 519] = 1.006;
  BBCScaleFactor[ 525] = 1.003;
  BBCScaleFactor[ 526] = 1.005;
  BBCScaleFactor[ 527] = 1.003;
  BBCScaleFactor[ 528] = 1.005;
  BBCScaleFactor[ 529] = 1.003;
  BBCScaleFactor[ 531] = 1.004;
  BBCScaleFactor[ 533] = 1.008;
  BBCScaleFactor[ 534] = 1.005;
  BBCScaleFactor[ 539] = 1.004;
  BBCScaleFactor[ 541] = 1.004;
  BBCScaleFactor[ 542] = 1.004;
  BBCScaleFactor[ 558] = 1.003;
  BBCScaleFactor[ 559] = 1.003;
  BBCScaleFactor[ 561] = 1.006;
  BBCScaleFactor[ 571] = 1.007;
  BBCScaleFactor[ 572] = 1.006;
  BBCScaleFactor[ 575] = 1.007;
  BBCScaleFactor[ 576] = 1.006;
  BBCScaleFactor[ 577] = 1.006;
  BBCScaleFactor[ 578] = 1.008;
  BBCScaleFactor[ 580] = 1.009;
  BBCScaleFactor[ 761] = 1.000;
  BBCScaleFactor[ 764] = 1.002;
  BBCScaleFactor[ 765] = 1.002;
  BBCScaleFactor[ 768] = 1.003;
  BBCScaleFactor[ 770] = 1.003;
  BBCScaleFactor[ 771] = 1.004;
  BBCScaleFactor[ 773] = 1.003;
  BBCScaleFactor[ 781] = 1.002;
  BBCScaleFactor[ 783] = 1.006;
  BBCScaleFactor[ 784] = 1.005;
  BBCScaleFactor[ 785] = 1.007;
  BBCScaleFactor[ 799] = 1.006;
  BBCScaleFactor[ 800] = 1.007;
  BBCScaleFactor[ 801] = 1.006;
  BBCScaleFactor[ 803] = 1.010;
  BBCScaleFactor[ 809] = 1.006;
  BBCScaleFactor[ 813] = 1.008;
  BBCScaleFactor[ 815] = 1.008;
  BBCScaleFactor[ 816] = 1.006;
  BBCScaleFactor[ 818] = 1.008;
  BBCScaleFactor[ 947] = 1.004;
  BBCScaleFactor[ 948] = 1.006;
  BBCScaleFactor[ 953] = 1.008;
  BBCScaleFactor[ 955] = 1.007;
  BBCScaleFactor[ 958] = 1.005;
  BBCScaleFactor[ 959] = 1.007;
  BBCScaleFactor[ 963] = 1.009;
  BBCScaleFactor[ 965] = 1.007;
  BBCScaleFactor[ 969] = 0.992;
  BBCScaleFactor[ 970] = 0.994;
  BBCScaleFactor[1043] = 1.005;
  BBCScaleFactor[1080] = 1.003;
  BBCScaleFactor[1081] = 1.003;
  BBCScaleFactor[1082] = 1.005;
  BBCScaleFactor[1083] = 1.003;
  BBCScaleFactor[1087] = 0.999;
  BBCScaleFactor[1089] = 1.000;
  BBCScaleFactor[1090] = 1.000;
  BBCScaleFactor[1091] = 1.005;
  BBCScaleFactor[1094] = 1.002;
  BBCScaleFactor[1097] = 1.001;
  BBCScaleFactor[1098] = 1.000;
  BBCScaleFactor[1099] = 0.998;
  BBCScaleFactor[1100] = 0.998;
  BBCScaleFactor[1101] = 0.997;
  BBCScaleFactor[1103] = 0.996;
  BBCScaleFactor[1104] = 0.998;
  BBCScaleFactor[1106] = 0.997;
  BBCScaleFactor[1107] = 1.000;
  BBCScaleFactor[1108] = 0.999;
  BBCScaleFactor[1109] = 0.999;
  BBCScaleFactor[1124] = 0.998;
  BBCScaleFactor[1125] = 0.998;
  BBCScaleFactor[1126] = 0.999;
  BBCScaleFactor[1127] = 0.998;
  BBCScaleFactor[1128] = 0.998;
  BBCScaleFactor[1193] = 1.000;
  BBCScaleFactor[1195] = 0.999;
  BBCScaleFactor[1196] = 1.001;
  BBCScaleFactor[1197] = 0.997;
  BBCScaleFactor[1198] = 0.998;
  BBCScaleFactor[1199] = 1.000;
  BBCScaleFactor[1200] = 1.000;
  BBCScaleFactor[1202] = 0.998;
  BBCScaleFactor[1203] = 1.000;
  BBCScaleFactor[1204] = 0.997;
  BBCScaleFactor[1207] = 0.998;
  BBCScaleFactor[1208] = 0.997;
  BBCScaleFactor[1209] = 1.001;
  BBCScaleFactor[1235] = 0.997;
  BBCScaleFactor[1236] = 0.998;
  BBCScaleFactor[1253] = 0.997;
  BBCScaleFactor[1254] = 0.999;
  BBCScaleFactor[1256] = 1.000;
  BBCScaleFactor[1257] = 0.999;
  BBCScaleFactor[1258] = 0.998;
  BBCScaleFactor[1295] = 1.000;
  BBCScaleFactor[1296] = 1.000;
  BBCScaleFactor[1297] = 1.001;
  BBCScaleFactor[1299] = 1.003;
  BBCScaleFactor[1329] = 1.000;
  BBCScaleFactor[1331] = 1.002;
  BBCScaleFactor[1332] = 1.001;
  BBCScaleFactor[1333] = 0.999;
  BBCScaleFactor[1334] = 0.998;
  BBCScaleFactor[1335] = 1.000;
  BBCScaleFactor[1336] = 1.000;
  BBCScaleFactor[1337] = 0.997;
  BBCScaleFactor[1340] = 0.998;
  BBCScaleFactor[1341] = 0.999;
  BBCScaleFactor[1342] = 0.999;
  BBCScaleFactor[1343] = 0.999;
  BBCScaleFactor[1358] = 0.998;
  BBCScaleFactor[1359] = 0.999;
  BBCScaleFactor[1361] = 1.001;
  BBCScaleFactor[1362] = 1.000;
  BBCScaleFactor[1363] = 1.001;
  BBCScaleFactor[1365] = 1.000;
  BBCScaleFactor[1366] = 1.000;
  BBCScaleFactor[1367] = 0.999;
  BBCScaleFactor[1368] = 1.001;
  BBCScaleFactor[1435] = 1.001;
  BBCScaleFactor[1436] = 1.001;
  BBCScaleFactor[1437] = 1.003;
  BBCScaleFactor[1442] = 1.002;
  BBCScaleFactor[1444] = 1.005;
  BBCScaleFactor[1445] = 1.002;
  BBCScaleFactor[1447] = 0.996;
  BBCScaleFactor[1448] = 1.001;
  BBCScaleFactor[1449] = 1.000;
  BBCScaleFactor[1450] = 1.001;
  BBCScaleFactor[1451] = 1.000;
  BBCScaleFactor[1455] = 1.000;
  BBCScaleFactor[1456] = 1.003;
  BBCScaleFactor[1457] = 1.003;
  BBCScaleFactor[1473] = 1.001;
  BBCScaleFactor[1474] = 1.002;
  BBCScaleFactor[1475] = 1.001;
  BBCScaleFactor[1476] = 1.003;
  BBCScaleFactor[1491] = 1.001;
  BBCScaleFactor[1492] = 1.001;
  BBCScaleFactor[1493] = 1.001;
  BBCScaleFactor[1494] = 1.001;

  // set all missing values to the value of the run before it ....

 for (int i=0;i<1500;i++) QAstatus[i] = QAstatus_PASS; // set all runs to passing QA first
 // QAstatus[?????? - 313500] = QAstatus_FAIL; // failed 
 // then below (for any runs without scalefactor) change to no check
  
  for (int i=0;i<1500;i++) {
    
    if (BBCScaleFactor[i] == 0.0) {

     // this implies that the run was not QA checked (because no scale factor exists above)
     QAstatus[i] = QAstatus_NOCHECK; // 
      
      if (i==0) {
	BBCScaleFactor[i] = 1.00;
      } else {
	// search for last run number with non-zero value
	for (int j=i-1;j>=0;j--) {
	  if (BBCScaleFactor[j] != 0.0) {
	    BBCScaleFactor[i] = BBCScaleFactor[j];
	    break;
	  }
	}
      }
      
    } 
    
  } // end loop over checking all run-numbers 
  
  //  for (int i=0;i<1500;i++) cout << "Scale Factor = " << BBCScaleFactor[i] << " for Run " << i+313500 << endl;
  
  return;
  
}

float Run10AuAu39GeVCentralityReco::GetScaleFactor(int runnumber) {

  if (! (runnumber >= 313500 && runnumber <= 315000)) {
    cout << "MyGetScaleFactor error in run number range " << runnumber << endl;
    return 0.0;
  }

  float scalefactor = BBCScaleFactor[runnumber - 313500]; // subtracting reference offset index

  return scalefactor;

}

int Run10AuAu39GeVCentralityReco::GetQAStatus(int runnumber) {

  if (! (runnumber >= 313500 && runnumber <= 315000)) {
    cout << "Run10AuAu39GeVCentralityReco::GetQAStatus - ERROR in run number range (Not a Run-10 AuAu 39GeV run number) = " 
      << runnumber << endl;
    return 0;
  }

  int qastatus = QAstatus[runnumber - 313500];

  cout << "Run10AuAu39GeVCentralityReco::GetQAStatus - Run " << runnumber << " Centrality QA = ";
  if (qastatus == QAstatus_PASS) {
    cout << "Passed" << endl;
  } else if (qastatus == QAstatus_FAIL) {
    cout << "Failed" << endl;
  } else {
    cout << "Never Checked (Not available at time of QA)" << endl;
  }

  return qastatus;

}
