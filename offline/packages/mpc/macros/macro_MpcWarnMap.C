//This is an example use of the MpcWarnMap class.  I use one run and
//multiple fit options to test how sigma (=
//sqrt(ChiSqPerDof)/correction) depends on the fit parameter used
//(ROB=0.x). Also, I test the effect of the background correction and
//the results are output to stdout



{
int thresh[4] = {25,35,60,100};
int NPTS=7;
int runNumber = 260000;
float fitargs[NPTS] = {0.51,0.55,0.6,0.65,0.7,0.75,0.8};
float sigma[5][2][4][NPTS]={0.0};
float chiperdof[5][2][4][NPTS] = {0.0};
const char* fn="-graphs.png";
  
gSystem->Load("libmpc.so");
MpcWarnMap* warn = new MpcWarnMap(runNumber);
warn->SetSigmaCut(4.0);

for(int ipar=0;ipar<NPTS;ipar++)
{
  warn->CreateGraphs();
  warn->SetFitArg( fitargs[ipar] );
  warn->FitGraphs();
  for(int impc=0;impc<2;impc++)
    {
      TString ts_mpc = impc?"_S_":"_N_";
      TString FileName = warn->GetFitOpt(); FileName+=ts_mpc; FileName+= fn; 
      warn->DrawGraphsBounds(FileName,impc);
    }
  for(int itr=0;itr<5;itr++)
    {
      for(int impc=0;impc<2;impc++)
	{
	  for(int ithr=0;ithr<4;ithr++)
	    {
	      sigma[itr][impc][ithr][ipar] = warn->GetSigma(impc,ithr);
	      chiperdof[itr][impc][ithr][ipar] = sqrt(warn->GetChiSqPerDOF(impc,ithr));
	      cout << warn->GetPercentNoisy(impc, ithr) <<", "; endl;
	    }
	}
      warn->Summarize();
      warn->CalcAllPercentNoisy();
      
    }
  warn->Summarize();
  warn->Reset();
  warn->Initialize(runNumber);
}

for(int itr=0;itr<5;itr++){
  cout << "\n\n iteration: " << itr << endl;
  cout << "Stdev----: " << "\t"; 
  for(int impc=0;impc<2;impc++){
    for(int ithr=0;ithr<4;ithr++){
      cout << TMath::RMS(NPTS,sigma[itr][impc][ithr]) << "\t";
    }
  }
  cout << endl;
  
  
  cout << "Mean-----: " << "\t"; 
  for(int impc=0;impc<2;impc++){
    for(int ithr=0;ithr<4;ithr++){
      cout << TMath::Mean(NPTS,sigma[itr][impc][ithr]) << "\t";
    }
  }
  cout << endl;
  
  
  
  cout << "stdev/mean--: " << "\t"; 
  for(int impc=0;impc<2;impc++){
    for(int ithr=0;ithr<4;ithr++){
      cout << (TMath::RMS(NPTS,sigma[itr][impc][ithr])/TMath::Mean(NPTS,sigma[itr][impc][ithr])) << "\t";
    }
  }
  cout << endl;
  
  
  
  for(int ipar=0;ipar<NPTS;ipar++){
    cout << "FitArg: " << fitargs[ipar] << "\t"; 
    for(int impc=0;impc<2;impc++){
      for(int ithr=0;ithr<4;ithr++){
	cout << sigma[itr][impc][ithr][ipar] << "\t";
      }
    }
    cout << endl;
  }
}
  
    
} // end block
