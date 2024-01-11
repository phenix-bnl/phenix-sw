//............................................
// convert simulated LUT to two 128x128 LUT
// The mapping on two board are exactly the same 
// therefore split the original LUT into two LUT, 
// one for each 

#include <iostream>

bool simLUT[172][256];
bool realLUT_1stBoard[128][128];
bool realLUT_2ndBoard[128][128];
int emcChipMap[172];
int crkChipMap[256];

void GetTrgBoardMapping()
{
  FILE* fp;
  if((fp = fopen("emcRichLL1BoardMap.dat", "r"))==NULL) {
    cout<<" emcRichLL1BoardMap.dat does not exist, exit"<<endl;
    exit(1);
  }  

  //.. read EMCal mapping first ...
  for(int i = 0; i<18; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 18; i<36; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 36; i<54; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 54; i<72; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 72; i<90; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 90; i<108; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 108; i<126; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 126; i<144; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 144; i<162; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  for(int i = 162; i<172; i++) 
    fscanf(fp, "%d", &emcChipMap[i]);

  //.. now RICH mapping   
  for(int i = 0; i<18; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 18; i<36; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 36; i<54; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 54; i<72; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 72; i<90; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 90; i<108; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 108; i<126; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 126; i<144; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 144; i<162; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 162; i<180; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 180; i<198; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 198; i<216; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 216; i<234; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 234; i<252; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  for(int i = 252; i<256; i++) 
    fscanf(fp, "%d", &crkChipMap[i]);

  fclose(fp);
}

void ConvertLUT()
{
  gSystem->Load("libert.so");

  GetTrgBoardMapping();

  for(int iemc = 0; iemc<172; iemc++) 
    for(int icrk = 0; icrk<256; icrk++) 
	simLUT[iemc][icrk] = false;
  
  FILE *fp;
  if((fp = fopen("LUT.dat", "r"))==NULL) {
    cout<<"Error:  no LUT.dat avaliable, exiting now "<<endl;
    exit(9);
  }

  int emctt, ncrk, crktt; 
  for(int itbl = 0; itbl<172; itbl++) {
    fscanf(fp, "%d%d", &emctt, &ncrk); 
    for(int i = 0; i<ncrk; i++) {
      fscanf(fp, "%d", &crktt);
      simLUT[emctt][crktt] = true;
    }
  }
  fclose(fp);

  //..........................
  for(int i = 0; i<128; i++) 
    for(int j = 0; j<128; j++) {
      realLUT_1stBoard[i][j] = 0;
      realLUT_2ndBoard[i][j] = 0;
    }

  //   for the 1st board 
  //............................
  for(int iemc = 0; iemc<72; iemc++) {
    for(int icrk = 0; icrk<128; icrk++) {
      int emcmap = emcChipMap[iemc];
      int crkmap = crkChipMap[icrk];
      if(simLUT[iemc][icrk]) {
          realLUT_1stBoard[emcmap][crkmap] = 1;
          //cout<<" emctt = "<<iemc<<" crktt = "<<icrk<<" simLUT[iemc][icrk] = "<<simLUT[iemc][icrk]<<endl;
          //cout<<" emcmap = "<<emcmap<<" crkmap = "<<crkmap<<endl;
      }
    }
  }
  for(int iemc = 72; iemc<172; iemc++) {
    for(int icrk = 128; icrk<256; icrk++) {
      int emcmap = emcChipMap[iemc];
      int crkmap = crkChipMap[icrk];
      if(simLUT[iemc][icrk]) 
          realLUT_2ndBoard[emcmap][crkmap] = 1;
    }
  }
  //............................
  FILE *result = fopen("LUT_1stBoard.dat", "w");
  for(int iemc = 0; iemc<128; iemc++) {
    for(int icrk = 0; icrk<128; icrk++) {
      fprintf(result, "%d ", realLUT_1stBoard[iemc][icrk]);
    }
    fprintf(result, "\n");
  }

  FILE *result = fopen("LUT_2ndBoard.dat", "w");
  for(int iemc = 0; iemc<128; iemc++) {
    for(int icrk = 0; icrk<128; icrk++) {
      fprintf(result, "%d ", realLUT_2ndBoard[iemc][icrk]);
    }
    fprintf(result, "\n");
  }
}
