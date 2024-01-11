#ifndef Tpoint_HH
#define Tpoint_HH

#include <Rtypes.h>
#include <TObject.h>
#include <cstdlib>
#include <TGraphErrors.h>
#include <TSeqCollection.h>
#include <iostream>

void sort_TGraphErrors(TGraphErrors* gra);

class Tpoint : public TObject {
public:
  float x,y,ex,ey;

  Tpoint(){x=0;y=0;ex=0;ey=0;};
  Tpoint(float fx,float fy,float fex,float fey){
    x = fx; y = fy;
    ex = fex; ey = fey;
  };
  void Set(float fx,float fy,float fex,float fey){
    x = fx; y = fy;
    ex = fex; ey = fey;
  };
  virtual int Compare(const TObject* a) const{
    if( x  > ((Tpoint*)a)->x )
      return 1;
    else if( x  == ((Tpoint*)a)->x )
      return 0;
    else
      return -1;
  };

  ClassDef(Tpoint,1) //point class for TGraphErrors.

};

#endif
