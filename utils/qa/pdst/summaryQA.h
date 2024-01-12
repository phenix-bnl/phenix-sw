#ifndef _SUMMARYQA_H
#define _SUMMARYQA_H

class TFile;

class QASummary {

public:

  QASummary();  
  virtual ~QASummary() {}

  int Init();
  int processTec();
  int processTof();
  int processZdc();
  int processPad();
  int processDch();
  int processBbc();
  int processEmc();
  int processMui();
  int processMut();
  int processFcl();
  int processCrk();
  int processPi0();
  int processElectron(const float min_mom=0.2, const float max_mom=4.0);
  int processErt();
  int End();

  void setRunNumber(int run) {runNumber=run;}
  void setSegmentNumber(int segment=-1) {segmentNumber=segment;}
  void setTagEntry(const char* str) {tagEntry=str;}
  void setInputFileName(const char* fname) {inputName=fname;}
  void setOutputTextFileName(const char* fname) {outputName1=fname;}
  void setOutputStatusFileName(const char* fname) {outputName2=fname;}
  void setCommitToDatabase(bool val=0) {CommitToDatabase = val;}
  int CommitToQADatabase(const char* subsystem, const char* parname, float parvalue, float parerror);

protected:

  const char* inputName;
  const char* outputName1;
  const char* outputName2;
  int runNumber;
  int segmentNumber;
  const char* tagEntry;
  bool CommitToDatabase;

  TFile* qafile;
	
};

#endif


