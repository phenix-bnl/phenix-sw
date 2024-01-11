#ifndef __CENTRALITYPARAMETERS_H__
#define __CENTRALITYPARAMETERS_H__ 1
 
class CentralityParameters{

 public:

  CentralityParameters();
  ~CentralityParameters();
  static CentralityParameters *getPointer();
  int getCalibForPerpRun4(int runno, float *zdc_constS, float *zdc_constN, float *bbc_constS, float *bbc_constN);

 private:

  static CentralityParameters para;
  int   run_no[700];
  float bbc_para[700][3]; // [0]->south, [1]->north, [2]->sum
  float zdc_para[700][3]; // [0]->south, [1]->north, [2]->sum
};

#endif
