/////////////////////////////////////////////////////////////////////
// Systematic error evaluation of the relative luminosity measurement
// with the bunch shuffling method ...
// 
// Original author: Takahiro Kawabata
// C++ version    : Yuji Goto + Masashi Kaneta
// Created        : Apr.24, 2003
/////////////////////////////////////////////////////////////////////

class TH1D;

class utiBunchShuffleLumi {
private:
  int polb[60];
  int poly[60];
  int polby[60];
  int nloop;
  char chopt[10];
  TH1D *h1;
  double staterr;
  double fiterr;
public:
  utiBunchShuffleLumi();
  utiBunchShuffleLumi(const int* pol_b, const int* pol_y, const int n_loop, const char *ch_opt);
  virtual ~utiBunchShuffleLumi();

  void BunchShuffle(int* ch1, int* ch2);

  void SetPolPattern(const int* pol_b, const int* pol_y){
    for(int i=0; i<60; i++){
      polb[i]=pol_b[i];
      poly[i]=pol_y[i];
    }
  }
  void SetNLoop(const int n_loop){ nloop=n_loop; }
  void SetChOpt(const char *ch_opt);

  TH1D* GetHistogram(){return h1;}
  double GetStatErr(){return staterr;}
  double GetFitErr(){return fiterr;}
};
