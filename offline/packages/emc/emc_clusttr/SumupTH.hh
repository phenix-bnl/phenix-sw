#ifndef SumupTH_HH
#define SumupTH_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TF1.h>
#include <TF2.h>
#include <TF3.h>
#include <TGraph.h>

int SumupTH1(TH1* hout, TH1* hin, Double_t shift = 0, char *opt = "");

int SumupTH2(TH2* hout, TH2* hin, Double_t shift = 0, char *opt = "");
int SumupTH2(TH2* hout, TH2* hin, TH1* hshift, char *opt = "");
int SumupTH2(TH2* hout, TH2* hin, TGraph* gshift, char *opt = "");

int SumupY_TH3(TH3* hout, TH3* hin, TH1* hshift, char *opt = "");
int SumupY_TH3(TH3* hout, TH3* hin, TGraph* gshift, char *opt = "");

#define NSLICETH1 10
#define NSLICETH2 10
#define NSLICETH3 4

int SumupTH1(TH1* hout, TH1* hin, TF1* fx, char *opt = "");
int SumupTH2(TH2* hout, TH2* hin, TF1* fx, TF1* fy, char *opt = "");
int SumupTH3(TH3* hout, TH3* hin, TF1* fx, TF1* fy, TF1* fz, char *opt = "");

#endif
//
