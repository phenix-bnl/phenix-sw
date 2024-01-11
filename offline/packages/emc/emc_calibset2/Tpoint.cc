
#include "Tpoint.hh"

ClassImp(Tpoint)
//=====================================================================
void sort_TGraphErrors(TGraphErrors* gra){
  double* x = gra->GetX();
  double* y = gra->GetY();
  double* ex = gra->GetEX();
  double* ey = gra->GetEY();
  int n;
  n = gra->GetN();
  Tpoint p[n];
  TObject* pt[n];
  while(n--){
    p[n].Set(x[n],y[n],ex[n],ey[n]);
    pt[n] = (TObject*) &p[n];
  }
  //cout<<((Tpoint*)pt[0])->x<<" , "<<((Tpoint*)pt[1])->x<<endl;
  n = gra->GetN();
  TSeqCollection::QSort(pt,0,n);
  //cout<<((Tpoint*)pt[0])->x<<" , "<<((Tpoint*)pt[1])->x<<endl;
  n = gra->GetN();
  while( n-- ){
    x[n] = ((Tpoint*)pt[n])->x;
    y[n] = ((Tpoint*)pt[n])->y;
    ex[n] = ((Tpoint*)pt[n])->ex;
    ey[n] = ((Tpoint*)pt[n])->ey;
  }
  return;
};
//=====================================================================
