
#ifndef __RPSUMXY_H__
#define __RPSUMXY_H__

#include <RpConst.h>

class PHCompositeNode;

class RpSumXY {

 public:
   enum {
     RP_BBCS = 0,  RP_BBCN = 1,  RP_BBCSN = 2,
     RP_SMDS = 3,  RP_SMDN = 4,  RP_SMDSN = 5,
     RP_MVDS = 6,  RP_MVDN = 7,  RP_MVDSN = 8,
     RP_FCLS = 9,  RP_FCLN = 10, RP_FCLSN = 11,
     RP_CNT0 = 12, RP_CNT1 = 13, RP_CNT2 = 14, RP_CNT3 = 15, RP_CNT4 = 16,
     RP_RXNSin = 17, RP_RXNSout = 18, RP_RXNS  = 19,
     RP_RXNNin = 20, RP_RXNNout = 21, RP_RXNN  = 22,
     RP_RXNin  = 23, RP_RXNout  = 24, RP_RXNSN = 25,
     RP_MPCS   = 26, RP_MPCN    = 27, RP_MPCSN = 28
   };

  RpSumXY();
  virtual ~RpSumXY();

  int EndRun();
  int InitRun(const int runNumber);
  int ResetEvent();

  void fillFlowVectorAll(PHCompositeNode* topNode);
  void fillFlowVectorBbc(PHCompositeNode* topNode);
  void fillFlowVectorSmd(PHCompositeNode* topNode);
  void fillFlowVectorMvd(PHCompositeNode* topNode);
  void fillFlowVectorFcl(PHCompositeNode* topNode);
  void fillFlowVectorCnt(PHCompositeNode* topNode);

//#####################RXN & MPC########################//

  void fillFlowVectorRxn(PHCompositeNode* topNode);
  void fillFlowVectorMpc(PHCompositeNode* topNode);  

//#######################################################//

  void fillFlowVector(const int idet, const float phi, 
                      const float weight, const float sign);
  void fillFlowVector(const int idet, const int ihar,
                      const float x, const float y, 
                      const float weight, const float sign);

  void Normalization();

  void set_Qx(const int idet, const int ihar, const float val);
  void set_Qy(const int idet, const int ihar, const float val);
  void set_Qw(const int idet, const int ihar, const float val);

  float get_Qx(const int idet, const int ihar) const;
  float get_Qy(const int idet, const int ihar) const;
  float get_Qw(const int idet, const int ihar) const;

  void Verbosity(const int v) {verbosity = v;}

 private:
  float Qx[RP::NDET][RP::NHAR];
  float Qy[RP::NDET][RP::NHAR];
  float Qw[RP::NDET][RP::NHAR];

//#############RXN & MPC##########//

  float Qx2[RP::NDET2-RP::NDET][RP::NHAR2];
  float Qy2[RP::NDET2-RP::NDET][RP::NHAR2];
  float Qw2[RP::NDET2-RP::NDET][RP::NHAR2];

//################################//

  int verbosity;
  int _runNumber;
  int _anaEvent;
};

#endif
