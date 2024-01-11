Mean()
{
  char title[400];

  for(int i = 0;i<96;i++){
    sprintf(title,"ADCch%d",i); 
    TH2F *tmp = (TH2F*)gDirectory->Get(title);
    TH1D *tmp1d = tmp->ProjectionY();
    cout <<  "ch " << i << ", mean:" <<  tmp1d->GetMean() << endl;
  }
}
