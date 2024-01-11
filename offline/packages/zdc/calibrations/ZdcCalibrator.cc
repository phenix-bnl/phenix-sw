#include "ZdcCalibrator.h"

ClassImp(ZdcCalibrator)

ZdcCalibrator::ZdcCalibrator()
{
/*
  TString name;
  for (int ich=0; ich<40; ich++)
    {
      for (int itdc=0; itdc<2; itdc++)
        {
	  name = "slewprofch"; name += ich;
	  name += "tdc"; name += itdc;
	  slewprof[ich][itdc]->SetName( name );
	  slewprof[ich][itdc]->SetTitle( name );
	  slewprof[ich][itdc]->SetBins(4096,-0.5,4095.5);
        }
    }
*/
}

int ZdcCalibrator::ReadZdcTree(const char *fname)
{
  zdctreefile = new TFile(fname,"READ");
  zdctree = (TTree*)zdctreefile->Get("T");
  zdcraw = 0;
  bbcout = 0;
  TBranch *b1 = zdctree->GetBranch("ZdcRaw");
  TBranch *b2 = zdctree->GetBranch("BbcOut");
  b1->SetAddress(&zdcraw);
  b2->SetAddress(&bbcout);

  Int_t nb = 0;
  for (Int_t i=0;i<nevent;i++)
    {
      if (i%50 == 0) printf("Event:%d\n",i);
      nb += zdctree->GetEvent(i);
    }
  return 1;
}

int ZdcCalibrator::ReadSlewProfiles(const char *fname)
{
  zdchistfile = new TFile(fname,"READ");

  TString name;
  for (int ich=0; ich<40; ich++)
    {
      for (int itdc=0; itdc<2; itdc++)
        {
	  name = "zdcslewprofch"; name += ich;
	  name += "tdc"; name += itdc;
	  slewprof[ich][itdc] = (TProfile*)zdchistfile->Get( name );
	}
    }
}

int ZdcCalibrator::CalculateSlew()
{
  for (int ich=0; ich<8; ich++)
    {
      for (int ich=0; ich<8; ich++)
        {
	}
    }
  return 1;
}
