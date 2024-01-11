#include "CentralityParameters.h"
#include <fstream>
//INCLUDECHECKER: Removed this line: #include <iostream>

CentralityParameters CentralityParameters::para;

CentralityParameters::CentralityParameters()
{
  for(int i=0; i<700; i++){
    run_no[i] = 0;
    for(int j=0; j<3; j++){
      bbc_para[i][j] = 0.0;
      zdc_para[i][j] = 0.0;
    }
  }
  
  std::ifstream input;
  input.open("./centralityByPerpCorr.dat");
 
  int   tmp_run;
  float tmp[6];
  int   loop = 0;

  do{
    input >> tmp_run >> tmp[0] >> tmp[1] >> tmp[2]
	  >> tmp[3] >> tmp[4] >> tmp[5];
    if( input.fail() ) break;
    run_no[loop] = tmp_run;
    bbc_para[loop][0] = 221.0/tmp[3];
    bbc_para[loop][1] = 233.0/tmp[4];
    bbc_para[loop][2] = 454.0/tmp[5];
    zdc_para[loop][0] = 2350.0/tmp[0];
    zdc_para[loop][1] = 2350.0/tmp[1];
    zdc_para[loop][2] = 4700.0/tmp[2];
    loop++;
  }while( !input.eof() );
  input.close();
}

CentralityParameters::~CentralityParameters()
{

}



CentralityParameters *CentralityParameters::getPointer()
{
  return &para;
}

int CentralityParameters::getCalibForPerpRun4(int runno, float *zdc_constS, float *zdc_constN, float *bbc_constS, float *bbc_constN)
{
  int loop = 0 ;
  bool flag = false;
  for(loop=0; loop<700; loop++){
    if(runno <= run_no[loop]){
      flag = true;
      break;
    }
  }

  if(flag){
    *zdc_constS = zdc_para[loop][0];
    *zdc_constN = zdc_para[loop][1];
    *bbc_constS = bbc_para[loop][0];
    *bbc_constN = bbc_para[loop][1];
  }else{
    *zdc_constS = 1.0;
    *zdc_constN = 1.0;
    *bbc_constS = 1.0;
    *bbc_constN = 1.0;
  }

  return 0;
}
