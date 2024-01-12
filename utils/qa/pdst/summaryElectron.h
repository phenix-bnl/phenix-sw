#ifndef _SUMMARYELECTRON_H
#define _SUMMARYELECTRON_H

class TFile;

void qasummaryElectron(TFile* qafile, char* textFile, char* statusFile, int run);
void CommitEWGToDatabase(TFile* qafile, int runnumer);

#endif 

