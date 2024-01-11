#ifndef __TFVTXQUADRATICTRKPAR_V1_H__
#define __TFVTXQUADRATICTRKPAR_V1_H__

#include <TFvtxQuadraticTrkPar.h>

class TFvtxQuadraticTrkPar_v1 : public TFvtxQuadraticTrkPar
{
public:

  TFvtxQuadraticTrkPar_v1() {}

  //! default constructor
  TFvtxQuadraticTrkPar_v1(const int arm, 
			  const double ax=0, 
			  const double ay=0, 
			  const double bx=0, 
			  const double by=0,
			  const double cx=0,
			  const double cy=0,
			  const double chi_square=0,
			  const int ndf = 0) : 
    TFvtxQuadraticTrkPar(arm,ax,ay,bx,by,cx,cy,chi_square,ndf)
  {
  }
  
  //! constructor
  TFvtxQuadraticTrkPar_v1(const Key key, const unsigned short arm, const unsigned short index) :
    TFvtxQuadraticTrkPar(key,arm,index)
  {
  }

  //! constructor
  TFvtxQuadraticTrkPar_v1(const TFvtxQuadraticTrkPar* ptr) :
    TFvtxQuadraticTrkPar(*ptr)
  {
  }

  //! constructor
  TFvtxQuadraticTrkPar_v1(const TFvtxQuadraticTrkPar& ref) :
    TFvtxQuadraticTrkPar(ref)
  {}

private:
  ClassDef(TFvtxQuadraticTrkPar_v1,1)
};

#endif
