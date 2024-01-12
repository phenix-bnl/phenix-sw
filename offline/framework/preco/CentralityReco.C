#include <CentralityReco.h> 
// DB handlers
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>

#include <TH1.h>
#include <phool.h>

#include <iostream>
using namespace std;

CentralityReco * CentralityReco::__instance = NULL;

CentralityReco *CentralityReco::instance(int runno)
{
  if (__instance)
    {
      if (runno == (__instance->currentrun))
        {
          return __instance;
        }
      else
        {
          __instance->Reset();
          __instance->currentrun = runno;
          __instance->Init();
          return __instance;
        }
    }
  else
    {
      __instance = new CentralityReco(runno);
      return __instance;
    }

}

CentralityReco::CentralityReco(int runno)
{
  currentrun = runno;
  for (int i = 0; i < 25; i++)
    {
      BBC_h[i] = 0;
    }

  for (int i = 0; i < 26; i++)
    {
      for (int j = 0; j < 3; j++)
        {
          BBC_hcal[j][i] = 0;
        }
    }
  verbosity = 0;
  Init();
  return ;
}

CentralityReco::~CentralityReco()
{
  Reset();
  for (int i = 0; i < 25; i++)
    {
      if (BBC_h[i])
        BBC_h[i]->Delete();
    }
  return ;
}

int CentralityReco::Reset()
{
  currentrun = 0;
  for (int i = 0; i < 25; i++)
    {
      if (BBC_h[i])
	{
          BBC_h[i]->Delete();
	}
      BBC_h[i] = 0;
    }
  return 0;
}


int CentralityReco::Init()
{
  fetchBBChistos_DB(currentrun);



  for (int i = 1; i < 26; i++)
    {
      BBC_h[i - 1] = static_cast<TH1F*>((BBC_hcal[0][i])->Clone());
      string name = BBC_h[i - 1]->GetName();
      name += "CR";
      BBC_h[i - 1]->SetName(name.c_str());
      BBC_h[i - 1]->SetBit(kCanDelete);
      norms[i - 1] = BBC_h[i - 1]->Integral();
    }
  for (int i = 0; i < 25; i++)
    {
      if (!(norms[i] > 0))
        {
          cout << "Centrality Reco INIT FAILED " << endl;
          exit(1);
        }
    }
  return 0;
}

float
CentralityReco::getBBCPercentile(float vertex, float bbcqsum)
{
  if (bbcqsum < 0)
    {
      return -1;
    }

  float percentile = -1;
  int zbin = -50;
  int i = -1;
  if (vertex < -50)
    { // bounds check
      i = 0;
    }
  else
    {
      if (vertex > 50)
        { // bounds check
          i = 24;
        }
      else
        {
          while (vertex >= zbin)
            {
              zbin += 4;
              i++;
            }
        }
    }
  if ((i > -1) && (i < 25))
    {
      if (BBC_h[i])
        {
          int bin = BBC_h[i]->FindBin(bbcqsum);
          float bincenter = BBC_h[i]->GetBinCenter(bin);

          float percentile1 = 100. * (BBC_h[i]->Integral(1, bin)) / (norms[i]);

          int bin2 = bin;
          if (bincenter < bbcqsum)
            {
              bin2++;
            }
          else
            {
              bin2--;
            }
          if ((bin2 > (BBC_h[i]->GetNbinsX())) || (bin2 < 1) || (bin == bin2))
            {
              return (100. - percentile1);
            }
          else
            {
              float bincenter2 = BBC_h[i]->GetBinCenter(bin2);
              float percentile2 = 100. * (BBC_h[i]->Integral(1, bin2)) / (norms[i]);
              percentile = percentile1 - (percentile1 - percentile2) * (bincenter - bbcqsum) / (bincenter - bincenter2);
              return (100. - percentile);
            }
        }
      else
        {
          cout << PHWHERE << " INIT FAILED " << endl;
          exit(1);
        }
    }
  else
    {
      cout << PHWHERE << " OUT OF BOUNDS " << endl;
      exit(1);
    }

  return -1;
}


int
CentralityReco::fetchBBChistos_DB(const int run)
{
  //  OK posse...now it gets intense.
  //  In this routine we will be _retreiving_ the entire bank
  //  of ZDC Centrality calibration parameters...YIP-E-KI-YAY!
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == number of entries that *SHOULD* follow
  //
  //   Based on scheme
  //   we know binning in z, bins of histogram, lower and upper limits
  //   if you want to change binning in z or histogram, change scheme
  //    if (z<0&&z+4<0) { sprintf(name,"BBC_m%d_m%d",abs(z),abs(z+4));    server->registerHisto(name, new TH1F(name,name,2500,-.5,2499.5));}
  //  b   /   number --> xy, north or south
  //  d   \   position
  //
  //  i
  //  0   x_south
  //  1   y_south
  //  2   x_north
  //  3   y_north

  //  The number of times the body repeats will be NCentrality
  //  in the first scheme known as scheme 1.
  //
  //                          STJL 2-4-2004
  //
  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("CentralityCal::", PHError, "Aborting ... Database not readable");
      application->abort();
    }


  //  Make a bank ID...
  PdbBankID bankID;
  bankID.set("*.Percentiles.BBC.AUTO");
  bankID.setInternalValue(1);
  char *nameDB = "calib.centrality.BBCPercentiles";

  //  Grap a pointer to the bank...
  PdbParameter *parameter;
  int index = 0;
  cout << " fetch bank " << endl;
  PdbCalBank *deltaBank = bankManager->fetchBank("PdbParameterBank", bankID, nameDB, run);

  cout << "successfully got bank " << deltaBank << endl;
  //----------------------------------------------------
  //  OK...now is the time to actually unpack the data...
  //  three checks...length of record, scheme and no. entries...
  int length = 2 * (3 * 26) /*histos*/*2501 /*bins*/ + 2 /* header*/;
  int truelength = deltaBank->getLength();
  if (length != truelength)
    {
      cout << "BBCPercentalie:: FATAL...wrong length DB read for BBC Percentile" << endl;
      return -1;
    }


  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  int scheme = (int)parameter->getParameter();
  if (scheme != 1)
    {
      cout << "BBCPercentalie:: FATAL...wrong scheme DB read for BBC Percentile" << endl;
      return -1;
    }


  parameter = (PdbParameter *) & deltaBank->getEntry(index++);
  int entries = (int)parameter->getParameter();
  if (entries != length - 2)
    {
      cout << "BBCPercentalie:: FATAL...wrong entries DB read for BBC Percentile" << endl;
      return -1;
    }


  if (BBC_hcal[0][0] == 0)
    {
      BBC_hcal[0][0] = new TH1F("BBC_T" , "BBC Centrality Histogram" , 2500, -.5, 2499.5);
      BBC_hcal[1][0] = new TH1F("BBC_N", "BBC North Centrality Histogram", 2500, -.5, 2499.5);
      BBC_hcal[2][0] = new TH1F("BBC_S", "BBC South Centrality Histogram", 2500, -.5, 2499.5);

      int g = 0;
      for (int z = -50; z < 47; z = z + 4)
        {
          char name[400];
          g++;
          if (z < 0 && z + 4 < 0)
            {
              sprintf(name, "BBC_m%d_m%d", abs(z), abs(z + 4));
              BBC_hcal[0][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
          if (z < 0 && z + 4 > 0)
            {
              sprintf(name, "BBC_m%d_p%d", abs(z), abs(z + 4));
              BBC_hcal[0][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
          if (z > 0 && z + 4 > 0)
            {
              sprintf(name, "BBC_p%d_p%d", abs(z), abs(z + 4));
              BBC_hcal[0][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }


          if (z < 0 && z + 4 < 0)
            {
              sprintf(name, "BBC_N_m%d_m%d", abs(z), abs(z + 4));
              BBC_hcal[1][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
          if (z < 0 && z + 4 > 0)
            {
              sprintf(name, "BBC_N_m%d_p%d", abs(z), abs(z + 4));
              BBC_hcal[1][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
          if (z > 0 && z + 4 > 0)
            {
              sprintf(name, "BBC_N_p%d_p%d", abs(z), abs(z + 4));
              BBC_hcal[1][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }

          if (z < 0 && z + 4 < 0)
            {
              sprintf(name, "BBC_S_m%d_m%d", abs(z), abs(z + 4));
              BBC_hcal[2][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
          if (z < 0 && z + 4 > 0)
            {
              sprintf(name, "BBC_S_m%d_p%d", abs(z), abs(z + 4));
              BBC_hcal[2][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
          if (z > 0 && z + 4 > 0)
            {
              sprintf(name, "BBC_S_p%d_p%d", abs(z), abs(z + 4));
              BBC_hcal[2][g] = new TH1F(name, name, 2500, -.5, 2499.5);
            }
        }
    }
  else
    {

      cout << "BBCPercentalie:: FATAL...histograms already in memory for BBC Percentile, will not overwrite" << endl;
      return -1;

    }

  //----------------------------------------------------
  //  Checks passed, histograms ready...get the parameters...
  if (verbosity == 0)
    cout << "READ from Database: BBC Percentile" << endl;

  for (unsigned int j = 0; j < 3; j++)
    {
      for (unsigned int k = 0; k < 26; k++)
        {
          TH1 * histo = BBC_hcal[j][k];
          for (unsigned int i = 0; i < 2501; i++)
            {
              parameter = (PdbParameter *) & deltaBank->getEntry(index++);
              int bin = (int)parameter->getParameter();
              parameter = (PdbParameter *) & deltaBank->getEntry(index++);
              float val = (float) parameter->getParameter();
              histo->SetBinContent(bin, val);
            }
        }
    }

  return 0;
}

