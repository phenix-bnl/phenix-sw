//
// compare the ped values from two different sets of MpcCal.*ped files
//
void timedelayscan(const char *fnamelist = "scan.list")
{
  gSystem->Load("libmpc.so");

  MpcMap *mpcmap = MpcMap::instance();

  int nfiles = 0;
  string temp_fname;
  float  temp_delay;
  vector<string> flist;
  ifstream inflist;
  inflist.open( fnamelist );
  while ( inflist >> temp_fname >> temp_delay )
    {
      flist.push_back( temp_fname );
      cout << flist[nfiles] << endl;
      nfiles++;
    }
  inflist.close();

  TFile *tfile[20];
  TH1F *htdc[20][576];
  
  TCanvas *ac[2] = {0};
  ac[0] = new TCanvas("ac0","ac0",2000,2000);
  ac[1] = new TCanvas("ac1","ac1",2000,2000);

  //ac[0]->Divide(18,18,-1,-1);
  //ac[1]->Divide(18,18,-1,-1);

  TString name;
  //for (int ifile=0; ifile<3; ifile++)
  for (int ifile=0; ifile<nfiles; ifile++)
    {
       tfile[ifile] = new TFile( flist[ifile].c_str(),"READ");

       //for (int ich=0; ich<576; ich++)
       for (int ich=288; ich<289; ich++)
         {
           name = "htdc_"; name += ich;
           htdc[ifile][ich] = (TH1F*)tfile[ifile]->Get( name );
           htdc[ifile][ich]->SetLineColor(ifile+1);

           if ( htdc[ifile][ich]==0 ) continue;
           if ( mpcmap->isCrystal(ich) == 0 ) continue;
           int ix = mpcmap->getGridX( ich );
           int iy = mpcmap->getGridY( ich );
           int arm = mpcmap->getArm( ich );

           ac[arm]->cd( (17-iy)*18 + ix + 1 );
           htdc[ifile][ich]->Rebin(10);
           htdc[ifile][ich]->Draw("same");
         }
    }

/*
  ac[0]->SaveAs("timedelay_arm0.png");
  ac[1]->SaveAs("timedelay_arm1.png");
  ac[0]->SaveAs("timedelay_arm0.eps");
  ac[1]->SaveAs("timedelay_arm1.eps");
*/
}

