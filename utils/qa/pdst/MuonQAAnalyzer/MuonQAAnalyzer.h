#ifndef MUONQAANALYZER_H
#define MUONQAANALYZER_H

// STL Includes
#include<map>

// BOOST Include
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#endif

// Forward Declarations
class SubsystemQA;

// Package Includes
#include<SubsystemQA.h> // CINT forces me to include this here...

class MuonQAAnalyzer
{
public:
  static MuonQAAnalyzer* getInstance();
  ~MuonQAAnalyzer();

#ifndef __CINT__
  typedef boost::shared_ptr<SubsystemQA> SubsysPtr;
  typedef std::map<std::string, SubsysPtr> SubsysMap;
  typedef SubsysMap::iterator::value_type MVT;
#else
  class SubsysPtr;
  class SubsysMap;  
#endif
  
  /** Meat of the QA analysis that opens each file for processing */
  void processQA();
  /** Steer the output of a good run list */
  void outputGoodRunList();
  /** Add a new subsystem by pointer for QA analysis */
  void addSubsystem(SubsystemQA* subsys);
  /** Add a new subsystem by shared_ptr for QA analysis */
#ifndef __CINT__  
  void addSubsystem(SubsysPtr subsys);
#endif  
  /** Retrieve the current run from a given subsystem */
  int getRun(std::string subsysName) const;
  /** Retrieve the current from active subsystem */
  int getRunActive() const;
  /** Static accessor for current active run */
  static int getRunActiveExt();
  /** Static accessor for the current run */
  static int getRunExt(std::string subsysName); 
  /** Retrieve the current segment from a given subsystem */
  int getSegment(std::string subsysName) const;
  /** Static accessor for the current segment */
  static int getSegmentExt(std::string subsysName);

#ifndef __CINT__  
  static void setActive(SubsysPtr ptr);
#endif  

protected:
  /* Disallow direct instantiation, copy and assignment */
  MuonQAAnalyzer();
  MuonQAAnalyzer(const MuonQAAnalyzer& copy);
  MuonQAAnalyzer& operator=(const MuonQAAnalyzer& copy);    

#ifndef __CINT__
  void _setActive(SubsysPtr ptr);
#endif 

  SubsysMap _subsystems; /**< Internal subsystem storage */
  SubsysPtr _activeSubsys; /** Keep a pointer to active subsys */
};

#endif // __MUONQAANALYZER_H__
