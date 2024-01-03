#ifndef __KEventCent__
#define __KEventCent__

#include <exception>

#define MAX_NCLUSTER 1024

class KEventCent {

  // 20 is the maximum number of mult or cent bins supported (private)
  enum { maxbins = 20 };

 public:

  // Exception(s)
  class OutOfRange : public std::exception {
  public:
    const char* what() const throw() { return "KEventCent:: Index out of range"; }
  };

  KEventCent();
  virtual ~KEventCent();

  void setMultBins( unsigned int i, int lower, int upper );
  void setCentBins( unsigned int i, float lower, float upper );

  int getCentBin( float percent , int nbins);
  int getMultBin( int nemc, int nbins );

  int          mult_lower[maxbins];
  int          mult_upper[maxbins];  
  float        cent_lower[maxbins];
  float        cent_upper[maxbins];

 private:

};

#endif


