#ifndef __FUN4ALLNOSYNCDSTINPUTMANAGER_H__
#define __FUN4ALLNOSYNCDSTINPUTMANAGER_H__

#include "Fun4AllDstInputManager.h"
#include "Fun4AllReturnCodes.h"

class PHCompositeNode;
class PHNodeIOManager;
class SyncObject;

class Fun4AllNoSyncDstInputManager : public Fun4AllDstInputManager
{
 public:

  Fun4AllNoSyncDstInputManager(const std::string &name = "DUMMY", const std::string &nodename = "DST", const std::string &topnodename = "TOP");
  
  virtual ~Fun4AllNoSyncDstInputManager() {}
  
  // Effectivly turn off the synchronization checking
  //
  int SyncIt(const SyncObject* /*mastersync*/) {return SYNC_OK;}
  int GetSyncObject(SyncObject** /*mastersync*/) {return SYNC_NOOBJECT;}
  int NoSyncPushBackEvents(const int nevt) {return PushBackEvents(nevt);}
  // no sync object we don't need to enable the sync variables
  int setSyncBranches(PHNodeIOManager* /*IManager*/) {return 0;}

  // turn off reading of the runwise TTree to make run mixing for embedding possible
  int NoRunTTree();

};

#endif /* __FUN4ALLNOSYNCDSTINPUTMANAGER_H__ */
