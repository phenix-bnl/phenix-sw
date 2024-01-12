#ifndef _SUMMARYTEC_H
#define _SUMMARYTEC_H

class TFile;

void qasummaryTec(TFile* qafile, char* textFile, char* statusFile, int run);
void CommitTECToDatabase(TFile* qafile, int runnumber);

#endif 

