#ifndef _SUMMARYBBC_H
#define _SUMMARYBBC_H

class TFile;

void qasummaryBbc(TFile* qafile, char* textFile, char* statusFile, int runNumber);
void CommitToDatabase(TFile* qafile, int runnumber, float EndPoint);

#endif 

