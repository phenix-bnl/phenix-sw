#ifndef __FUN4ALLPISAINPUTMANAGER_H__
#define __FUN4ALLPISAINPUTMANAGER_H__

// $Id: Fun4AllPisaInputManager.h,v 1.10 2012/01/10 05:19:14 phnxbld Exp $
/*!
  \file Fun4AllPisaInputManager.h
  \brief Fun4All compliant input manager for PISAEvent.root files
  \author  Hugo Pereira
  \version $Revision: 1.10 $
  \date $Date: 2012/01/10 05:19:14 $
*/

#include <Fun4AllReturnCodes.h>
#include <Fun4AllInputManager.h>
#include <string>

#ifndef __CINT__

// need to hide from root because of shared_pointers
#include <PISARun.h>

#endif

// forward declaration
class PHCompositeNode;

//! Fun4All compliant input manager for PISAEvent.root files
class Fun4AllPisaInputManager : public Fun4AllInputManager
{
  public:
  
  //! constructor
  Fun4AllPisaInputManager(const std::string &name = "DUMMY", const std::string &nodename = "DST",  const int verbose = 0);
  
  //! destructor
  virtual ~Fun4AllPisaInputManager();
  
  //! open file
  int fileopen(const std::string &filein);
  
  //! close file
  int fileclose();
  
  //! open additional file, merged with the one opened
  int pisaMergeFileOpen(const std::string &filein, const int kFile)
  {
    std::cout << "Fun4AllPisaInputManager::pisaMergeFileOpen - this method is obsolete." << std::endl;
    std::cout << "Fun4AllPisaInputManager::pisaMergeFileOpen - use pisaMergeFileOpen( \"filename\" ). " << std::endl;
    return pisaMergeFileOpen( filein );
  }
  
  //! open additional file, merged with the one opened
  int pisaMergeFileOpen( const std::string& filein );
  
  //! run
  int run(const int nevents = 0);
  
  //! returns true if main file is opened
  int isOpen() 
  {return _is_open;}
  
  //! define number of files to merge
  void setMergeFiles(const int i)
  { 
    std::cout << "Fun4AllPisaInputManager::setMergeFiles - this method is obsolete. " << std::endl;
    std::cout << "Fun4AllPisaInputManager::setMergeFiles - The number of merged files is computed from pisaMergeFileOpen internally" << std::endl;
  }

  //! decrement local event counter by a given amount
  int PushBackEvents(const int nevt);

  protected:
  
  //! reset tables and files
  void Reset( void );
  
  //! make sure needed nodes are present
  int CreateNodeTree(PHCompositeNode *topNode);
  
  //! open next file from input list
  int OpenNextFile( void );
   
  //! load one entry
  /*! it reads entries for all TTrees and all branches */
  int LoadEntry( void );
  
  //! number of merged files
  int _n_files;

  //! true if main file is open
  bool _is_open;
  
  //! total number of events
  int _events_total;
  
  //! current event index in current file
  /*! 
    it is checked against the max number of events in the file 
    so that one makes sure it does not exceed the root tree size
  */
  int _events_thisfile;
    
  //! array of TTrees from which events are read
  #ifndef __CINT__
    
  //! pisa Run object
  /*! interface to pisa event processing */
  PISARun _pisa_run;
  
  #endif

};


#endif /* __FUN4ALLINPUTMANAGER_H__ */
