#include "TCentralityRecalibratorBBC.h"

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbBankID.hh>
#include <RunToTime.hh>

#include <PdbFloatVector.hh>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>

#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

TCentralityRecalibratorBBC::TCentralityRecalibratorBBC():
  pn(0.),
  ps(0.),
  key(0),
  key2(0),
  CENT(NULL)
{
  memset(PARTN, 0, sizeof(PARTN));
  memset(PARTS, 0, sizeof(PARTS));
  memset(NCOLL, 0, sizeof(NCOLL));
  memset(BIMP, 0, sizeof(BIMP));
  memset(AREA, 0, sizeof(AREA));
  memset(ECCE, 0, sizeof(ECCE));
  memset(EFFI, 0, sizeof(EFFI));
  return ;
}

void
TCentralityRecalibratorBBC::help() const
{
  cout << "=======================================================" << endl;
  cout << "TCentralityRecalibratorBBC::help method output"          << endl;
  cout << "Author:  K.Miki (kentaro@rcf2.rhic.bnl.gov)"             << endl;
  cout << "Updated: Oct. 11, 2007"                                  << endl;
  cout << "Comment: Run-7 AuAu Centrality Using Npart."             << endl;
  cout << "         This is applicable for runs 227016 - 240121   " << endl;
  cout << "         Run7 AuAu 200 GeV with BBCLL1(>0tubes)        " << endl;
  cout << "         The correction parameter from online production" << endl;
  cout << "         The Look Up Table is calibrated by Sasha Milov" << endl;
  cout << "                               (sasha.milov@gmail.com)." << endl;
  cout << endl;
  cout << "Updated: Jun. 24, 2008, K.Miki"                          << endl;
  cout << endl;
  cout << "=======================================================" << endl;
}


int
TCentralityRecalibratorBBC::isValidRun(const int run_number) const
{

  if (run_number >= 227016 && run_number <= 240121) // run7 AuAu 200GeV
    {
      return 1;
    }

  return 0;

}

void
TCentralityRecalibratorBBC::SetEventParameters(const int run_number, const float zvertex, const float bbc_charge_north_in, const float bbc_charge_south_in)
{
  if(!isValidRun(run_number))
    {
      cout << "TCentralityRecalibratorBBC can not working for this run number" << endl;
      return;
    }


  int vbin = (int) ((zvertex - (-40.0) ) / 2.0);
  if (vbin < 0 || vbin >= 40) vbin = -1;
  if (vbin < 5 || vbin >= 35)
    {
      cout << "Over the z-vertex range" << endl;
      cout << "Please use +- 30 cm cut" << endl;
      return;
    }

  int tbin = vbin - 5;
  int bound[5] = {229793, 231156, 233475, 236008, 240121};
  int bbin = 0;
  float cx[5][3]   = {{254.656956,   0.190241,  -0.008404},
		      {265.509557,  -0.015964,  -0.000126},
		      {266.920787,  -0.025423,   0.000004},
		      {243.856827,   0.128006,  -0.000227},
		      {231.329948,   0.114402,  -0.000102}
  };
  float cy[5][3]   = {{272.088676,   0.142233,  -0.008014},
		      {288.751005,  -0.054540,   0.000033},
		      {294.957046,  -0.074865,   0.000082},
		      {238.187319,   0.300186,  -0.000498},
		      {236.951630,   0.181120,  -0.000161}
  };
  float normx = 261;
  float normy = 282;
  while (run_number > bound[bbin]) bbin++;
  float nfile = runn_to_nfile(run_number);
  float kx = (cx[bbin][0] + cx[bbin][1] * nfile + cx[bbin][2] * nfile * nfile) / normx;
  float ky = (cy[bbin][0] + cy[bbin][1] * nfile + cy[bbin][2] * nfile * nfile) / normy;

  float bbc_charge_north = bbc_charge_north_in / kx;
  float bbc_charge_south = bbc_charge_south_in / ky;

  if(bbc_charge_north < 1.5) bbc_charge_north = 2;
  if(bbc_charge_south < 1.5) bbc_charge_south = 2;


  // -- return the key of map --
  float limit;
  int ibin;
  if(-0.5 < bbc_charge_north && bbc_charge_north < 40.5)
    {
      limit =   0.5;
      ibin =    1;
      while(bbc_charge_north > limit)
	{
	  limit += 1;
	  ibin  ++;
	}
    }
  else if(  40.5 <= bbc_charge_north && bbc_charge_north < 100.5)
    {
      limit =  42.5;
      ibin =   42;
      while(bbc_charge_north > limit)
	{
	  limit += 2;
	  ibin  ++;
	}
    }
  else if( 100.5 <= bbc_charge_north && bbc_charge_north < 202.5)
    {
      limit = 103.5;
      ibin =   72;
      while(bbc_charge_north > limit)
	{
	  limit += 3;
	  ibin  ++;
	}
    }
  else if( 202.5 <= bbc_charge_north && bbc_charge_north < 402.5)
    {
      limit = 206.5;
      ibin =  106;
      while(bbc_charge_north > limit)
	{
	  limit += 4;
	  ibin  ++;
	}
    }
  else if( 402.5 <= bbc_charge_north && bbc_charge_north < 804.5)
    {
      limit = 408.5;
      ibin =  156;
      while(bbc_charge_north > limit)
	{
	  limit += 6;
	  ibin  ++;
	}
    }
  else if( 804.5 <= bbc_charge_north && bbc_charge_north < 1204.5)
    {
      limit = 812.5;
      ibin =  223;
      while(bbc_charge_north > limit)
	{
	  limit += 8;
	  ibin  ++;
	}
    }
  else if(1204.5 <= bbc_charge_north && bbc_charge_north < 1784.5)
    {
      limit = 1214.5;
      ibin =  273;
      while(bbc_charge_north > limit)
	{
	  limit += 10;
	  ibin  ++;
	}
    }
  else if(1784.5 <= bbc_charge_north && bbc_charge_north < 1799.5)
    {
      ibin =  331;
    }
  else
    {
      return;
    }
  int xbin = ibin;

  if(-0.5 < bbc_charge_south && bbc_charge_south < 40.5)
    {
      limit =   0.5;
      ibin =    1;
      while(bbc_charge_south > limit)
	{
	  limit += 1;
	  ibin  ++;
	}
    }
  else if(  40.5 <= bbc_charge_south && bbc_charge_south < 100.5)
    {
      limit =  42.5;
      ibin =   42;
      while(bbc_charge_south > limit)
	{
	  limit += 2;
	  ibin  ++;
	}
    }
  else if( 100.5 <= bbc_charge_south && bbc_charge_south < 202.5)
    {
      limit = 103.5;
      ibin =   72;
      while(bbc_charge_south > limit)
	{
	  limit += 3;
	  ibin  ++;
	}
    }
  else if( 202.5 <= bbc_charge_south && bbc_charge_south < 402.5)
    {
      limit = 206.5;
      ibin =  106;
      while(bbc_charge_south > limit)
	{
	  limit += 4;
	  ibin  ++;
	}
    }
  else if( 402.5 <= bbc_charge_south && bbc_charge_south < 804.5)
    {
      limit = 408.5;
      ibin =  156;
      while(bbc_charge_south > limit)
	{
	  limit += 6;
	  ibin  ++;
	}
    }
  else if( 804.5 <= bbc_charge_south && bbc_charge_south < 1204.5)
    {
      limit = 812.5;
      ibin =  223;
      while(bbc_charge_south > limit)
	{
	  limit += 8;
	  ibin  ++;
	}
    }
  else if(1204.5 <= bbc_charge_south && bbc_charge_south < 1784.5)
    {
      limit = 1214.5;
      ibin =  273;
      while(bbc_charge_south > limit)
	{
	  limit += 10;
	  ibin  ++;
	}
    }
  else if(1784.5 <= bbc_charge_south && bbc_charge_south < 1799.5)
    {
      ibin =  331;
    }
  else
    {
      return;
    }
  int ybin = ibin;


  if(xbin < 1 || ybin < 1)  return;

  key = (xbin - 1) + 331 * (ybin - 1) + 331 * 331 * tbin;

  pn = GetNpartNorth();
  ps = GetNpartSouth();

  int index = (int)(pn + ps);
  if((pn + ps) - index >= 0.2 && (pn + ps) - index < 0.4)
    {
      key2 = (int)(pn + ps) * 5 + 1 + 80 * tbin;
    }
  else if((pn + ps) - index >= 0.4 && (pn + ps) - index < 0.6)
    {
      key2 = (int)(pn + ps) * 5 + 2 + 80 * tbin;
    }
  else if((pn + ps) - index >= 0.6 && (pn + ps) - index < 0.8)
    {
      key2 = (int)(pn + ps) * 5 + 3 + 80 * tbin;
    }
  else if((pn + ps) - index >= 0.8 && (pn + ps) - index < 1.0)
    {
      key2 = (int)(pn + ps) * 5 + 4 + 80 * tbin;
    }
  else
    {
      key2 = (int)(pn + ps) * 5 + 0 + 80 * tbin;
    }

  return;

}


float
TCentralityRecalibratorBBC::GetNpartNorth() const
{
  return vNpartn[key];
}

float
TCentralityRecalibratorBBC::GetNpartSouth() const
{
  return vNparts[key];
}

float
TCentralityRecalibratorBBC::GetNcollisions() const
{
  if(pn < 1 || ps < 1) return -999;
  return vNcoll[key];
}

float
TCentralityRecalibratorBBC::GetBimpact() const
{
  if(pn < 1 || ps < 1) return -999;
  return vBimp[key];
}

float
TCentralityRecalibratorBBC::GetOverlapArea() const
{
  if(pn < 1 || ps < 1) return -999;
  return vArea[key];
}

float
TCentralityRecalibratorBBC::GetEccentricity() const
{
  if(pn < 1 || ps < 1) return -999;
  return vEcce[key];
}

float
TCentralityRecalibratorBBC::GetMBtriggerEfficiency() const
{
  if(pn < 1 || ps < 1) return -999;
  if(pn + ps >= 15)
    {
      return 1.0;
    }
  else
    {
      return vEffi[key2];
    }
}

float
TCentralityRecalibratorBBC::GetCentrality() const
{
  if(pn < 1 || ps < 1) return -999;
  int index = (int) (pn + ps);
  float exc = pn + ps - index;
  float top = vCent[index + 1];
  float low = vCent[index + 0];
  return low + (top - low) * exc;
}



// ======================================
// Data Base update/fetch member function
// 1. fetch functions
//  1-0. fetch_DataBase
//  1-1. fetch_RecalCentBBC
// 2. update functions
//  2-0. update_DataBase
//  2-1. update_RecalCentBBC
//       flagDB 0 - 8
//        (0... not working)
//        (1... update npart north)
//        (2... update npart south)
//        ...
// ======================================


void
TCentralityRecalibratorBBC::fetch_RecalCentBBC(const int runn)
{
  cout << "TCentralityRecalibratorBBC:: READING from Data Base..." << flush;

  const char* nameDB_npartn = "recal.centBBC.Npartn";
  const char* nameDB_nparts = "recal.centBBC.Nparts";
  const char* nameDB_ncoll  = "recal.centBBC.Ncoll";
  const char* nameDB_bimp   = "recal.centBBC.Bimpact";
  const char* nameDB_area   = "recal.centBBC.area";
  const char* nameDB_ecce   = "recal.centBBC.eccentricity";
  const char* nameDB_effi   = "recal.centBBC.efficiency";
  const char* nameDB_cent   = "recal.centBBC.centrality";
  fetch_DataBase(nameDB_npartn, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_nparts, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_ncoll, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_bimp, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_area, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_ecce, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_effi, runn);
  cout << "." << flush;
  fetch_DataBase(nameDB_cent, runn);
  cout << "...DONE !!" << endl;


  return;

}


void
TCentralityRecalibratorBBC::fetch_DataBase(const char* nameDB,  const int runn)
{

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();
  if(!application->startRead())
    {
      PHMessage("TCentralityRecalibratorBBC::", PHError, "Aborting ... Database not readable");
      application -> abort();
    }

  PdbBankID bankID(1);
  PdbCalBank *recalBank = bankManager->fetchBank("PdbFloatVectorBank", bankID, nameDB, runn);
  if(recalBank)
    {

      PdbFloatVector* pfv = (PdbFloatVector *) & recalBank->getEntry(0);

      if(strcmp(nameDB, "recal.centBBC.Npartn") == 0)
	{
	  vNpartn = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.Nparts") == 0)
	{
	  vNparts = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.Ncoll") == 0)
	{
	  vNcoll = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.Bimpact") == 0)
	{
	  vBimp  = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.area") == 0)
	{
	  vArea  = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.eccentricity") == 0)
	{
	  vEcce  = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.efficiency") == 0)
	{
	  vEffi  = pfv -> getVector();
	}
      else if(strcmp(nameDB, "recal.centBBC.centrality") == 0)
	{
	  vCent  = pfv -> getVector();
	}
      else
	{
	  cout << "Data Base name error" << endl;
	  cout << "Check the nameDB"     << endl;
	  return;
	}

      delete recalBank;

    }
  else
    {
      cout << PHWHERE << "Failed to get cent.recalBBC info from DB" << endl;
      exit(1);
    }
  return;
}


void
TCentralityRecalibratorBBC::update_RecalCentBBC(const int beginrun, const int endrun, const int flagDB = 0)
{

  if(flagDB == 1)
    {
      const char* nameDB_npartn = "recal.centBBC.Npartn";
      update_DataBase(nameDB_npartn, beginrun, endrun);
    }
  if(flagDB == 2)
    {
      const char* nameDB_nparts = "recal.centBBC.Nparts";
      update_DataBase(nameDB_nparts, beginrun, endrun);
    }
  if(flagDB == 3)
    {
      const char* nameDB_ncoll  = "recal.centBBC.Ncoll";
      update_DataBase(nameDB_ncoll, beginrun, endrun);
    }
  if(flagDB == 4)
    {
      const char* nameDB_bimp   = "recal.centBBC.Bimpact";
      update_DataBase(nameDB_bimp, beginrun, endrun);
    }
  if(flagDB == 5)
    {
      const char* nameDB_area   = "recal.centBBC.area";
      update_DataBase(nameDB_area, beginrun, endrun);
    }
  if(flagDB == 6)
    {
      const char* nameDB_ecce   = "recal.centBBC.eccentricity";
      update_DataBase(nameDB_ecce, beginrun, endrun);
    }
  if(flagDB == 7)
    {
      const char* nameDB_effi   = "recal.centBBC.efficiency";
      update_DataBase(nameDB_effi, beginrun, endrun);
    }
  if(flagDB == 8)
    {
      const char* nameDB_cent   = "recal.centBBC.centrality";
      update_DataBase(nameDB_cent, beginrun, endrun);
    }

  return;

}


void
TCentralityRecalibratorBBC::update_DataBase(const char* nameDB, const int beginrun, const int endrun)
{

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  if(endrun > 0)
    {
      ts = runTime -> getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();
  if(!application->startUpdate())
    {
      PHMessage("TCentralityRecalibratorBBC::", PHError, "Aborting ... Database not writable");
      application -> abort();
    }

  PdbBankID bankID(1);
  const char* descrip = "Parameters submitted by recal object";

  PdbCalBank *recalBank = bankManager->createBank("PdbFloatVectorBank", bankID, descrip, Tstart, Tstop, nameDB);
  if(recalBank)
    {
      recalBank -> setLength(1);
      PdbFloatVector *pfv = new PdbFloatVector();
      if(strcmp(nameDB, "recal.centBBC.Npartn") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int b = 1; b < nbiny + 1; b++)
		{
		  for(int a = 1; a < nbinx + 1; a++)
		    {
		      pfv->add_float((float)(PARTN[c]->GetBinContent(a, b)));
		    }
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.Nparts") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int b = 1; b < nbiny + 1; b++)
		{
		  for(int a = 1; a < nbinx + 1; a++)
		    {
		      pfv->add_float((float)(PARTS[c]->GetBinContent(a, b)));
		    }
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.Ncoll") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int b = 1; b < nbiny + 1; b++)
		{
		  for(int a = 1; a < nbinx + 1; a++)
		    {
		      pfv->add_float((float)(NCOLL[c]->GetBinContent(a, b)));
		    }
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.Bimpact") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int b = 1; b < nbiny + 1; b++)
		{
		  for(int a = 1; a < nbinx + 1; a++)
		    {
		      pfv->add_float((float)(BIMP[c]->GetBinContent(a, b)));
		    }
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.area") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int b = 1; b < nbiny + 1; b++)
		{
		  for(int a = 1; a < nbinx + 1; a++)
		    {
		      pfv->add_float((float)(AREA[c]->GetBinContent(a, b)));
		    }
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.eccentricity") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int b = 1; b < nbiny + 1; b++)
		{
		  for(int a = 1; a < nbinx + 1; a++)
		    {
		      pfv->add_float((float)(ECCE[c]->GetBinContent(a, b)));
		    }
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.efficiency") == 0)
	{
	  for(int c = 0; c < nzver; c++)
	    {
	      for(int a = 1; a < nbine + 1; a++)
		{
		  pfv->add_float((float)(EFFI[c]->GetBinContent(a)));
		}
	    }
	}
      else if(strcmp(nameDB, "recal.centBBC.centrality") == 0)
	{
	  for(int a = 1; a < nbinc + 1; a++)
	    {
	      pfv->add_float((float)(CENT->GetBinContent(a)));
	    }
	}
      else
	{
	  cout << "Data Base name error" << endl;
	  cout << "Check the nameDB"     << endl;
	  return;
	}

      PdbFloatVector* entry = (PdbFloatVector *) & recalBank -> getEntry(0);
      *entry = *pfv;
      application->commit(recalBank);
      delete recalBank;
      cout << "Update " << nameDB << " DONE !!" << endl;

    }
  else
    {
      cout << PHWHERE << "Failed to set the TCentralityRecalibratorBBC info to DB" << endl;
      return;
    }

  return;

}


void
TCentralityRecalibratorBBC::read_file(const char* input_file_name)
{

  TFile* table_file = new TFile(input_file_name, "r");
  char hist_name[100];

  for(int ibin = 0; ibin < nzver; ibin++)
    {
      sprintf(hist_name, "PARTN_%d", ibin);
      PARTN[ibin] = (TH2F*) table_file->Get(hist_name);
      sprintf(hist_name, "PARTS_%d", ibin);
      PARTS[ibin] = (TH2F*) table_file->Get(hist_name);
      sprintf(hist_name, "NCOLL_%d", ibin);
      NCOLL[ibin] = (TH2F*) table_file->Get(hist_name);
      sprintf(hist_name, "BIMP_%d", ibin);
      BIMP [ibin] = (TH2F*) table_file->Get(hist_name);
      sprintf(hist_name, "AREA_%d", ibin);
      AREA [ibin] = (TH2F*) table_file->Get(hist_name);
      sprintf(hist_name, "ECCE_%d", ibin);
      ECCE [ibin] = (TH2F*) table_file->Get(hist_name);
      sprintf(hist_name, "EFFI_%d", ibin);
      EFFI [ibin]  = (TH1F*) table_file->Get(hist_name);
    }
  CENT = (TH1F*) table_file -> Get("CENT");

  return;

}


float
TCentralityRecalibratorBBC::runn_to_nfile(const int run_number) const
{

  int runlist[807] =
    {
      229680, 229681, 229686, 229687, 229688, 229689, 229690,
      229691, 229693, 229694, 229696, 229697, 229698, 229699,
      229700, 229701, 229790, 229793, 229794, 229796, 229799,
      229800, 229801, 229806, 229808, 229809, 229810, 229811,
      229812, 229813, 229815, 229817, 229818, 229819, 229820,
      229821, 229902, 230013, 230014, 230079, 230080, 230083,
      230084, 230085, 230086, 230109, 230110, 230111, 230112,
      230113, 230120, 230121, 230122, 230123, 230126, 230127,
      230128, 230129, 230152, 230155, 230159, 230161, 230162,
      230163, 230168, 230170, 230171, 230173, 230174, 230175,
      230176, 230246, 230249, 230251, 230253, 230254, 230255,
      230256, 230258, 230299, 230304, 230305, 230306, 230307,
      230308, 230309, 230310, 230311, 230312, 230313, 230362,
      230363, 230365, 230366, 230367, 230370, 230372, 230374,
      230456, 230539, 230540, 230542, 230543, 230544, 230550,
      230551, 230554, 230555, 230556, 230557, 230659, 230660,
      230661, 230662, 230663, 230664, 230668, 230669, 230670,
      230671, 230672, 230673, 230676, 230679, 230680, 230682,
      230683, 230684, 230686, 230800, 230844, 230845, 230846,
      230847, 230848, 230935, 230937, 230945, 230947, 230956,
      230973, 230995, 230996, 231154, 231155, 231156, 231158,
      231211, 231216, 231217, 231218, 231219, 231221, 231427,
      231428, 231429, 231434, 231438, 231639, 231640, 231641,
      231695, 231696, 231697, 231699, 231706, 231716, 231717,
      231718, 231719, 231720, 231721, 231722, 231723, 231765,
      231766, 231767, 231768, 231769, 231770, 231773, 231775,
      231776, 231777, 231778, 231780, 231784, 231786, 231796,
      231800, 231811, 231821, 231875, 231876, 231877, 231878,
      231879, 231880, 231881, 231920, 231921, 231922, 231931,
      231932, 231933, 231992, 231993, 231994, 231996, 231997,
      232000, 232003, 232004, 232005, 232006, 232011, 232012,
      232014, 232020, 232021, 232377, 232378, 232379, 232380,
      232381, 232382, 232383, 232384, 232457, 232458, 232459,
      232460, 232461, 232462, 232463, 232464, 232465, 232599,
      232600, 232601, 232602, 232603, 232610, 232611, 232612,
      232613, 232615, 232616, 232617, 232618, 232619, 232620,
      232805, 232806, 232807, 232810, 232822, 232823, 232824,
      232825, 232828, 232829, 232830, 232831, 232837, 232839,
      232871, 232872, 232992, 232996, 233000, 233001, 233002,
      233004, 233005, 233006, 233007, 233010, 233013, 233015,
      233016, 233017, 233018, 233019, 233020, 233022, 233025,
      233027, 233141, 233164, 233165, 233170, 233171, 233172,
      233278, 233279, 233280, 233281, 233282, 233283, 233284,
      233286, 233287, 233296, 233297, 233306, 233307, 233308,
      233379, 233381, 233382, 233385, 233388, 233394, 233395,
      233396, 233398, 233415, 233474, 233475, 234136, 234137,
      234139, 234142, 234143, 234175, 234202, 234416, 234440,
      234441, 234449, 234457, 234522, 234523, 234524, 234525,
      234526, 234571, 234572, 234573, 234574, 234667, 234673,
      234674, 234680, 234682, 234685, 234686, 234687, 234688,
      234690, 234746, 234757, 234869, 234871, 234872, 234873,
      234876, 234878, 234879, 234880, 234881, 234887, 234889,
      234891, 234896, 234898, 235032, 235033, 235050, 235051,
      235223, 235226, 235227, 235228, 235229, 235231, 235232,
      235234, 235235, 235238, 235241, 235242, 235244, 235363,
      235364, 235365, 235366, 235390, 235391, 235397, 235524,
      235526, 235529, 235531, 235532, 235549, 235551, 235553,
      235554, 235555, 235559, 235561, 235563, 235565, 235567,
      235568, 235569, 235570, 235571, 235572, 235573, 235612,
      235613, 235614, 235616, 235656, 235657, 235658, 235659,
      235660, 235683, 235684, 235685, 235686, 235690, 235691,
      235695, 235696, 235698, 235699, 235700, 235794, 235795,
      235796, 235797, 235798, 235800, 235801, 235802, 235803,
      235804, 235805, 235807, 235809, 235810, 235889, 235890,
      235891, 235892, 235893, 235894, 235895, 235900, 235901,
      235902, 236004, 236005, 236007, 236008, 236009, 236133,
      236134, 236135, 236136, 236137, 236245, 236246, 236247,
      236248, 236258, 236259, 236260, 236261, 236262, 236263,
      236374, 236379, 236380, 236381, 236382, 236385, 236390,
      236394, 236395, 236398, 236399, 236400, 236401, 236402,
      236403, 236404, 236406, 236407, 236409, 236413, 236416,
      236418, 236419, 236420, 236421, 236463, 236464, 236465,
      236466, 236467, 236505, 236506, 236521, 236522, 236523,
      236524, 236536, 236538, 236539, 236540, 236605, 236666,
      236778, 236779, 236780, 236782, 236784, 236786, 236794,
      236803, 236804, 236805, 236903, 236949, 236950, 236952,
      236954, 236955, 237089, 237090, 237091, 237092, 237093,
      237094, 237095, 237098, 237099, 237100, 237101, 237102,
      237104, 237230, 237232, 237233, 237234, 237235, 237247,
      237248, 237269, 237270, 237271, 237272, 237355, 237356,
      237357, 237360, 237379, 237474, 237475, 237476, 237477,
      237592, 237593, 237594, 237595, 237596, 237608, 237621,
      237622, 237624, 237731, 237733, 237745, 237746, 237748,
      237749, 237751, 237814, 237815, 237817, 237818, 237819,
      237820, 237822, 237826, 237829, 237830, 237831, 238026,
      238027, 238030, 238031, 238135, 238136, 238137, 238143,
      238146, 238148, 238149, 238150, 238151, 238152, 238153,
      238154, 238165, 238167, 238170, 238171, 238327, 238328,
      238330, 238331, 238332, 238334, 238336, 238337, 238338,
      238339, 238383, 238384, 238385, 238387, 238515, 238516,
      238517, 238518, 238519, 238524, 238525, 238526, 238528,
      238530, 238531, 238536, 238637, 238648, 238649, 238650,
      238651, 238652, 238653, 238654, 238655, 238658, 238662,
      238663, 238682, 238683, 238684, 238698, 238843, 238845,
      238846, 238847, 238848, 238966, 238968, 238969, 238970,
      238971, 238975, 239418, 239464, 239465, 239466, 239467,
      239471, 239472, 239473, 239474, 239475, 239476, 239478,
      239479, 239481, 239483, 239540, 239542, 239543, 239599,
      239600, 239601, 239602, 239603, 239606, 239608, 239609,
      239610, 239611, 239612, 239657, 239658, 239659, 239660,
      239666, 239667, 239668, 239669, 239670, 239739, 239740,
      239741, 239755, 239756, 239758, 239759, 239760, 239772,
      239773, 239774, 239775, 239776, 239777, 239778, 239779,
      239816, 239817, 239818, 239820, 239821, 239823, 239824,
      239827, 239828, 239833, 239834, 239837, 239838, 239839,
      239840, 239841, 239842, 239843, 239891, 239909, 239911,
      239912, 239913, 239914, 239916, 239917, 239919, 239922,
      239923, 239924, 239925, 239926, 239927, 239933, 239935,
      239936, 239937, 239949, 239950, 239951, 240041, 240042,
      240043, 240044, 240048, 240049, 240053, 240054, 240055,
      240058, 240059, 240060, 240061, 240062, 240064, 240065,
      240066, 240072, 240073, 240075, 240076, 240077, 240078,
      240087, 240088, 240090, 240091, 240092, 240096, 240097,
      240098, 240099, 240100, 240102, 240106, 240107, 240108,
      240110, 240111, 240112, 240114, 240115, 240116, 240118,
      240120, 240121
    };

  int nfile = 0;
  if(run_number < 229680) return   1;
  if(run_number > 240121) return 807;
  while (run_number >= runlist[nfile]) nfile++;
  if(run_number == runlist[nfile])
    {
      return (float)nfile;
    }
  else
    {
      return (float)(nfile - 1) + (run_number - runlist[nfile - 1]) / (runlist[nfile] - runlist[nfile - 1]);
    }
}
