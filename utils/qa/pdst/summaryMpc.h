#ifndef _SUMMARYMPC_H
#define _SUMMARYMPC_H

class TFile;

void qasummaryMpc(TFile* qafile, char* textFile, char* statusFile, int runNumber);
void CommitToDatabase(TFile* qafile, int runnumber, float EndPoint);

#endif 

