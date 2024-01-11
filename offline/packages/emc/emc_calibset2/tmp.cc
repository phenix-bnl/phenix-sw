

Double_t func(Double_t* x,Double_t* p){
  int bin;
  double ret = 0;
  //
  bin = (int)x[0];
  ret = p[bin];
  return ret;
}

TF3* f3x;
TF1* f1x;

void tmp(){

  f1x = new TF1("f1x",func,0,2592,2592);
  double par[2592];
  int n =100;
  while(n--) par[n] = 100;
  par[0] = 10;
  f1x->SetParameters(par);


}
