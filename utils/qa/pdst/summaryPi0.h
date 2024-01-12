#ifndef _SUMMARYEMC_H
#define _SUMMARYEMC_H

class TFile;

void qasummaryPi0(TFile* qafile, char* textFile, char* statusFile, int run);

class Pi0Util
{
public:
Double_t Pi0FitAndCount(TH1D *hist, double *parameter,double *errors, double *npi0);
//void CommitPi0QAToQADatabase(char *subsystem, char *parname, float parvalue, float parerror);
};

#endif 

