#ifndef _SUMMARYDCH_H
#define _SUMMARYDCH_H

class TFile;

void qasummaryDch(TFile* qafile, char* textFile, char* statusFile, int run);
void CommitDCHToDatabase(TFile* qafile, int runnumber);

#endif 

