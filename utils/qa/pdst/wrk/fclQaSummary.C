{
  gSystem->Load("libfcl.so");
  FclIndexer* indexer = FclIndexer::Instance();

  
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- FCL QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  
  Int_t fclstatus = 0;

int numSouthDead = 0;
int numNorthDead = 0;

  TH2F *fclADCNorth = (TH2F *) qafile->Get ("fclADCNorth");  
  TH2F *fclADCSouth = (TH2F *) qafile->Get ("fclADCSouth");
  TProfile *fclADCNorthP = (TProfile *) qafile->Get ("fclADCNorthP");  
  TProfile *fclADCSouthP = (TProfile *) qafile->Get ("fclADCSouthP");


  // check that the histograms exist
  if (fclADCNorth == NULL || fclADCSouth == NULL ||
      fclADCNorthP == NULL || fclADCSouthP == NULL)
  {
    textFile << " FCL ERROR: could not extract histograms" << endl;
    fclstatus +=1;
  }
  else
  {

    for (int iChan = 0; iChan<144; iChan++){
      if (indexer->getColumnSouth(iChan)>0 &&
	  indexer->getColumnSouth(iChan)<8){ // skip the last couple columns on the south
	if (fclADCSouthP->GetBinContent(iChan)-
	    fclADCSouthP->GetBinError(iChan) < 0.0) numSouthDead++;
      }
	
      if (indexer->getColumnNorth(iChan)>0 &&
	  indexer->getColumnNorth(iChan)<7){ // skip the last few columns on the north
	if (fclADCNorthP->GetBinContent(iChan)-
	    fclADCNorthP->GetBinError(iChan) < 0.0) numNorthDead++;
      }
    }
    
    
    textFile << "FCAL Low ADC North Channels: " << numNorthDead << endl;
    textFile << "FCAL Low ADC South Channels: " << numSouthDead << endl;
    textFile << "More FCAL Summary stuff here:" << endl;

    if (numNorthDead>15) fclstats +=2;
    if (numNorthDead>15) fclstats +=4;

  }
textFile << "FCAL status:" << fclstatus << endl;
statusFile << fclstatus << " " ;
}
