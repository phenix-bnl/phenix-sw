#ifndef __TOFWCALIB_H_
#define __TOFWCALIB_H_

#include "TofwPar.h"
#include "PHString.h"
#include <string>

class PHTimeStamp;
class TofwAddress;

class TofwCalib
{
 public:
  TofwCalib();
  virtual ~TofwCalib() {}
  
  void set_debug(const int ival) {debug = ival;}
  
  int get_end(int ich);
  int get_strip(int ich);
  int get_chamber(int iboard, int ich);
  float getTvcConv(int ibox, int ichamber, int istrip, int iend);
  float getTvcConv(int ibox, int ich);

  bool setTvcConv(int ibox, int ich, float value);

  float get_toffset(int istrip);
  bool set_toffset(int istrip, float value);

  float get_runoffset() {return DeltaTRun;}
  void set_runoffset(float value){ DeltaTRun=value;}

  float get_sigma_dphi(int ipar) {return sigma_dphi_par[ipar];}
  void set_sigma_dphi(int ipar, float value) {sigma_dphi_par[ipar] = value;}

  float get_sigma_dz(int ipar) {return sigma_dz_par[ipar];}
  void set_sigma_dz(int ipar, float value) {sigma_dz_par[ipar] = value;}

  float get_DeltaT(int istrip) {return DeltaT[istrip];}
  void set_DeltaT(int istrip, float value) {DeltaT[istrip] = value;}
  
  float get_Slewing_A(int istrip) {return Slewing_A[istrip];}
  void set_Slewing_A(int istrip, float value) {Slewing_A[istrip] = value;}
  
  float get_Slewing_B(int istrip) {return Slewing_B[istrip];}
  void set_Slewing_B(int istrip, float value) {Slewing_B[istrip] = value;}
  
  float get_Mean_Dz_Plus(int istrip) {return Mean_Dz_Plus[istrip];}
  void set_Mean_Dz_Plus(int istrip, float value) {Mean_Dz_Plus[istrip] = value;}
  
  float get_Mean_Dz_Minus(int istrip) {return Mean_Dz_Minus[istrip];}
  void set_Mean_Dz_Minus(int istrip, float value) {Mean_Dz_Minus[istrip] = value;}
  
  float get_Mean_Dphi_Plus(int istrip) {return Mean_Dphi_Plus[istrip];}
  void set_Mean_Dphi_Plus(int istrip, float value) {Mean_Dphi_Plus[istrip] = value;}
  
  float get_Mean_Dphi_Minus(int istrip) {return Mean_Dphi_Minus[istrip];}
  void set_Mean_Dphi_Minus(int istrip, float value) {Mean_Dphi_Minus[istrip] = value;}

  //calib.tofw.tdc
  PHBoolean fetchTvcConv(const int run);
  PHBoolean fetchTvcConv(const int ibox, const char *filename);
  PHBoolean updateTvcConv(const int beginrun, const int endrun=-1);
  
  //calib.tofw.Toffset
  PHBoolean fetchToffset(const int run);
  PHBoolean fetchToffset(const char *filename);
  PHBoolean updateToffset(const int beginrun, const int endrun=-1);

  //calib.tofw.stripoff
  PHBoolean fetchstripoff(const int run);
  PHBoolean fetchstripoff(const char *filename);
  PHBoolean updatestripoff(const int beginrun, const int endrun=-1);

  //calib.tofw.runoff
  PHBoolean fetchrunoff(const int run);
  PHBoolean fetchrunoff(const char *filename);
  PHBoolean updaterunoff(const int beginrun, const int endrun=-1);
  PHBoolean updaterunoff(const char *filename);

  int Channel2Strip(int ch);
  int Channel2End(int ch);

  bool getGoodStrip(int istrip);

  int debug;

  int nruncalib;

 protected:
   float TVC_ConvSouthBottom[TOFW_NBOARD * TOFW_NCH];
   float TVC_ConvNorthBottom[TOFW_NBOARD * TOFW_NCH];
   float TVC_ConvSouthTop[TOFW_NBOARD * TOFW_NCH];
   float TVC_ConvNorthTop[TOFW_NBOARD * TOFW_NCH];

   float toffset[TOFW_NSTRIP_TOTAL];

   float sigma_dphi_par[4];
   float sigma_dz_par[4];

   float DeltaT[TOFW_NSTRIP_TOTAL];
   float Slewing_A[TOFW_NSTRIP_TOTAL];
   float Slewing_B[TOFW_NSTRIP_TOTAL];
   float Mean_Dz_Plus[TOFW_NSTRIP_TOTAL];
   float Mean_Dz_Minus[TOFW_NSTRIP_TOTAL];
   float Mean_Dphi_Plus[TOFW_NSTRIP_TOTAL];
   float Mean_Dphi_Minus[TOFW_NSTRIP_TOTAL];

   float DeltaTRun;

   
};

#endif
