#ifndef __THIJING_H__
#define __THIJING_H__

#include <math.h>
#include <PHGenerator.h>

struct HIMAIN1 {
  int natt;
  int eatt;
  int jatt;
  int nt;
  int np;
  int n0;
  int n01;
  int n10;
  int n11;
};

struct HIMAIN2 {
  int katt[4][130000];
  float patt[4][130000];
  float vatt[4][130000];
};

class HIJINGPAR;
class HIPARNT;
class TDatabasePDG;

class THijing : public PHGenerator {

public:

  THijing(const char* name = "THijing",
	  const char* title = "THijing");

  ~THijing();

  void Init(); 

  void GenerateEvent();

  double GetParameter(const char* name) const;
  void SetParameter(const char* name, double val);
  void SetParameter(const char* name, const char* val) {};
  void SetDecay(int pid, bool flag);

  TObjArray* ImportParticles(Option_t* o = "") { return fParticles; }

  void setSeed(int seed) {}

private:
  void GenerateVertex();
  void GenerateMomentum();
  
  TDatabasePDG* _PDGdb;

  // Current values
  float _bimpact;
  int _nt;
  int _np;

  // Parameters
  float _efrm; // Energy in frame of reference
  char _frame[5]; // Frame of reference
  char _proj[5];  // Type of projectile 
  char _targ[5];  // Type of target
  int _iap;
  int _izp;
  int _iat;
  int _izt;
  float _bmin;
  float _bmax;
  int _iseed;
  int _iseed_skip;
  int _jet_trigger;
  float _pzero;
  float _pthard;
  float _pthard_max;
  int _jet_quench;
  int _str_rad_flag;
  int _ini_rad_flag;
  float _scaleup_factor;
  float _loss_factor;
  float _pthetamin;
  float _pthetamax;
  int _history;// History string
  HIJINGPAR& _pars;
  HIPARNT& _hiparnt;

  // Output common(s)
  HIMAIN1& _himain1;
  HIMAIN2& _himain2;
  
  ClassDef(THijing,0);              
};

#endif // __TSINGLEPARTICLEGENERATOR_H__
