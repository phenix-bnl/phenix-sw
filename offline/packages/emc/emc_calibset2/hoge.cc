
#include "hoge.hh"

ClassImp(hoge)
//
hoge::hoge() : TNamed() {
  cout<<" constructor "<<endl; 
  char hname[128];
  //  _twr_array = new TClonesArray("TH1F");
  //  TClonesArray& array = *_twr_array;
  //  TH1F* h1;
  //  for( int ch = 0; ch < nch; ch++ ){
  //    sprintf(hname,"%s_h1_%d",GetName(),ch);
  //    h1 = new(array[ch]) TH1F(hname,"test",10,0,1);
  //  }
};
//
hoge::hoge(char* name,char* title) : TNamed(name,title){
  cout<<" constructor ("<<name<<","<<title<<") "<<endl; 
  char hname[128];
  int nch = 10;
  //  _twr_array = new TClonesArray("TH1F",nch);
  //  TClonesArray& array = *_twr_array;
  //  TH1F* h1;
  //  for( int ch = 0; ch < nch; ch++ ){
  //    sprintf(hname,"%s_h1_%d",name,ch);
  //    h1 = new(array[ch]) TH1F(hname,"test",10,0,1);
  //  }
};
//
hoge::~hoge(){
  cout<<" destructor "<<endl; 
  //  _twr_array->Delete();
}
//
