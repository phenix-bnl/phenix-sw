#include "Fun4AllNoSyncDstInputManager.h"

#include <TSystem.h>

#include <cstdlib>
#include <iostream>

using namespace std;

Fun4AllNoSyncDstInputManager::Fun4AllNoSyncDstInputManager(const string &name,
							   const string &nodename,
							   const string &topnodename) :
  Fun4AllDstInputManager(name, nodename, topnodename)
{
  return ;
}

int
Fun4AllNoSyncDstInputManager::NoRunTTree()
{
  if (!isopen)
    {
      readrunttree = 0;
    }
  else
    {
      cout << ThisName 
           << "NoRunTTree() has to be done before opening a file" << endl;
      gSystem->Exit(1);
      exit(1);
    }
  return 0;
}

