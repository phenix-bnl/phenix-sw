#ifndef _SUMMARYEMC_H
#define _SUMMARYEMC_H

class TFile;

void qasummaryEmc(TFile* qafile, char* textFile, char* statusFile, int run);
void CommitEMCToDatabase(TFile* qafile, int runnumber);

#endif 

