#ifndef ERTOUTV1_h
#define ERTOUTV1_h

#include <iostream>
#include "ErtOut.h"

class ErtOutv1: public ErtOut 
{
 public:

  ErtOutv1();
  ErtOutv1(const ErtOutv1&);
  ErtOutv1& operator=(const ErtOutv1&);
  virtual ~ErtOutv1();

  ErtOutv1* clone() const { return new ErtOutv1(*this); }

  void Reset();
  void identify(std::ostream& os = std::cout) const;
  int isValid() const;

  int get_ERThit_N() const {return ERThit_N;}
  int get_ERTtrigmode(int ii) const
    {
      if(ii>=ERThit_N) return ERT_INVALID_INT;
      return ERTtrigmode[ii];
    }
  int get_ERTarm(int ii) const
    {
      if(ii>=ERThit_N) return ERT_INVALID_INT;
      return ERTarm[ii];
    }
  int get_ERTsector(int ii) const
    {
      if(ii>=ERThit_N) return ERT_INVALID_INT;
      return ERTsector[ii];
    }
  int get_ERTsm(int ii) const
    {
      if(ii>=ERThit_N) return ERT_INVALID_INT;
      return ERTsm[ii];
    }
  int get_ERTbit(int trigmode, int arm, int sector, int sm) const;


  int set_ERTbit(int trigmode, int arm, int sector, int sm);


private:
  void copyTo(ErtOutv1&) const;

  int ERThit_N;
  int* ERTtrigmode;	//[ERThit_N]		// 0:4x4a, 1:4x4b, 2:4x4c, 3:2x2, 4:RICH
  int* ERTarm;		//[ERThit_N]
  int* ERTsector;	//[ERThit_N]
  int* ERTsm;		//[ERThit_N]

  ClassDef(ErtOutv1,1)
};

#endif
