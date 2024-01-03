#ifndef __KTofCutter__
#define __KTofCutter__

//#include "EmcAnaCommon.h"
// using namespace EmcAnaCommon;

#define N_ARMSECT 8

class TGraph;

class KTofCutter { //: public TNamed {
//  friend class KTofCutter;
 
 protected:
  KTofCutter ();
  static KTofCutter * instance;
  static bool instanciated;

 public:
  ~KTofCutter();
  
  static KTofCutter * getInstance() {
    if(!instanciated) {
      instance     = new KTofCutter();
      instanciated = true;
    }
    return instance ;
  }

  int init(int prod = 0);
  
  
  double evalMean(int sec = 0, double pt = 1); // returns tofMean for a sector sec at a pt  
  double evalWidth(int sec = 0, double pt = 1); // returns tofWidth for a sector sec at a pt  
  double tofAbsOffset(int sec = 0, double pt = 1, double tof = 0); // returns absolute offset of clusters's tof to paramerization [ns]
  double tofRelOffset(int sec = 0, double pt = 1, double tof = 0); // relative offset in units of sigma
  
//  private:
  TGraph *gtof[N_ARMSECT][2]; // TOF mean and width parametrization is stored here
};

#endif // __KTofCutter__
