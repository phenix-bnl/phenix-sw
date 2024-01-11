#ifndef __CINT__
#include "utiBunchShuffleLumi.hh"
#include <iostream>
#endif // __CINT__

//
// An example to demonstrate utiBunchShuffleLumi class
//
//  Author : Yuji Goto
//  Date   : 2003-04-24
//
// How to run in root
//  root [] gSystem->Load("../.libs/libuspin.so");
//  root [] .x BunchShuffleLumi.C
//

int mainBunchShuffleLumi()
{
  TCanvas *c1 = new TCanvas("c1","c1",600,800);
  c1->Divide(1,3);

  int ch1[60],ch2[60];
  TH1D *h1;

  utiBunchShuffleLumi *bsl = new utiBunchShuffleLumi();
  bsl->SetNLoop(1000);

  // 1
  for(int i=0;i<60;i++){
    ch1[i]=(int)gRandom->Gaus(10000,100);
    ch2[i]=(int)gRandom->Gaus(40000,200);
  }
  bsl->BunchShuffle(ch1,ch2);
  h1 = bsl->GetHistogram();
  c1->cd(1);
  h1->DrawCopy();
  cout << "staterr = " << bsl->GetStatErr() << " fiterr = " << bsl->GetFitErr() << endl;
  // 2
  for(int i=0;i<60;i++){
    ch1[i]=(int)gRandom->Gaus(1000,100);
    ch2[i]=(int)gRandom->Gaus(4000,200);
  }
  bsl->BunchShuffle(ch1,ch2);
  h1 = bsl->GetHistogram();
  c1->cd(2);
  h1->DrawCopy();
  cout << "staterr = " << bsl->GetStatErr() << " fiterr = " << bsl->GetFitErr() << endl;
  // 3
  for(int i=0;i<60;i++){
    ch1[i]=(int)gRandom->Gaus(10000,1000);
    ch2[i]=(int)gRandom->Gaus(40000,2000);
  }
  bsl->BunchShuffle(ch1,ch2);
  h1 = bsl->GetHistogram();
  c1->cd(3);
  h1->DrawCopy();
  cout << "staterr = " << bsl->GetStatErr() << " fiterr = " << bsl->GetFitErr() << endl;

  delete bsl;
}
