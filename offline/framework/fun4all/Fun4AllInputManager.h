#ifndef FUN4ALLINPUTMANAGER_H__
#define FUN4ALLINPUTMANAGER_H__

#include "Fun4AllBase.h"
#include "Fun4AllReturnCodes.h"
#include <list>
#include <string>
#include <vector>

class PHCompositeNode;
class SubsysReco;
class SyncObject;
class Fun4AllSyncManager;

class Fun4AllInputManager: public Fun4AllBase
{
 public:

  virtual ~Fun4AllInputManager();
  virtual int fileopen(const std::string& /*filename*/) {return -1;}
  virtual int fileclose() {return -1;}
  virtual int isOpen() {return 0;}
  virtual int run(const int /*nevents=0*/) {return -1;}
  virtual int ReadInRunNode(PHCompositeNode* /*RunNode*/) {return -1;}
  virtual std::string Filename() {return filename;}
  virtual int GetSyncObject(SyncObject** /*mastersync*/) {return 0;}
  virtual int SyncIt(const SyncObject* /*mastersync*/) {return SYNC_FAIL;}
  virtual int BranchSelect(const std::string& /*branch*/, const int /*iflag*/) {return -1;}
  virtual int setBranches() {return -1;}
  virtual void Print(const std::string &what = "ALL") const;
  virtual int PushBackEvents(const int /*nevt*/) {return -1;}
  // so people can use the skip they are used to instead of PushBackEvents
  // with negative arg
  virtual int skip(const int nevt) {return PushBackEvents(-nevt);}
  virtual int NoSyncPushBackEvents(const int /*nevt*/) {return -1;}
  int AddFile(const std::string &filename);
  int AddListFile(const std::string &filename);
  int registerSubsystem(SubsysReco *subsystem);
  virtual int RejectEvent();
  void Repeat(const int i=-1) {repeat = i;}
  virtual void setSyncManager(Fun4AllSyncManager *master) {mySyncManager = master;}
  int ResetFileList();
  virtual int ResetEvent() {return 0;}
  virtual void SetRunNumber(const int runno) {myrunnumber = runno;}
  virtual int RunNumber() const {return myrunnumber;}
  void AddToFileOpened(const std::string &filename) {filelist_opened.push_back(filename);}
  const std::list<std::string> GetFileList() const {return filelist_copy;}
  const std::list<std::string> GetFileOpenedList() const {return filelist_opened;}
  virtual int ReadEvent(const int evtcnt) {return -1;} 

 protected:
  Fun4AllInputManager(const std::string &name = "DUMMY", const std::string &nodename = "DST", const std::string &topnodename = "TOP");
  std::vector<SubsysReco *> Subsystems;
  std::string InputNode;
  std::string filename;
  std::string topNodeName;
  std::list<std::string>filelist;
  std::list<std::string>filelist_copy;
  std::list<std::string>filelist_opened;  // all files which were opened during running
  Fun4AllSyncManager *mySyncManager;
  int repeat;
  int myrunnumber;
  int initrun;
};

#endif /* __FUN4ALLINPUTMANAGER_H__ */
