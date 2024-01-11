#ifndef hoge_hh
#define hoge_hh

#include <stream.h>
#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
//#include "TNamedDir.hh"
#include <TClonesArray.h>
#include <TGraphErrors.h>
#include <TH1.h>

class hoge : public TNamed {
public:
  float run;                         // run number
  int evn;                           // event number
  //  TClonesArray* _twr_array;         //->

public:
  hoge();
  hoge(char* name,char* title);
  ~hoge();
  //  hoge(const hoge& t){
  //    cout<<" copy constructor "<<endl;
  //  };
  //  hoge& operator=(hoge& t){
  //    cout<<" operator= "<<endl;
  //    return *this;
  //  };
  //  hoge operator+(hoge& t){
  //    cout<<" operator+ "<<endl;
  //    hoge tmp;
  //    return tmp;
  //  };

  ClassDef(hoge,2)  //hoge class
};

#endif
//
