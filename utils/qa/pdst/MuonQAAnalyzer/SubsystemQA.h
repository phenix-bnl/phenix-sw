#ifndef MUONQAANALYZER_SUBSYSTEM_QA_H
#define MUONQAANALYZER_SUBSYSTEM_QA_H

// STL Includes
#include<vector>
#include<set>
#include<string>
#include<map>

// BOOST Includes
#ifndef __CINT__
#include<boost/filesystem.hpp>
#include<boost/shared_ptr.hpp>
#include<boost/make_shared.hpp>
#include<boost/enable_shared_from_this.hpp>

// Package Includes
#include<MuonQAFlags.h>

namespace fs = boost::filesystem;
#endif /* __CINT__ */

// Forward declarations
class HistoQABase;
class ObservableQABase;
class TFile;

namespace odbc {
  class Connection;
};   

#ifndef __CINT__
class SubsystemQA : 
  public boost::enable_shared_from_this<SubsystemQA>
#else
class SubsystemQA
#endif  /* __CINT__ */
{
public:
#ifndef __CINT__
  typedef boost::shared_ptr<SubsystemQA> ptr;
  typedef boost::shared_ptr<HistoQABase> HistoQAPtr;
  typedef boost::shared_ptr<ObservableQABase> ObservablePtr;  
#else
  class ptr;
  class HistoQAPtr;
  class ObservablePtr;
  #endif  /* __CINT__ */
  
  typedef std::map<std::string,ptr> map;
  typedef std::map<std::string,HistoQAPtr> histoMap;
  typedef std::map<std::string,std::set<int> > SetMap;
#ifndef __CINT__
  typedef histoMap::iterator::value_type MVT;
#endif /* __CINT__ */
  typedef std::vector<ptr> vector;  
  typedef std::set<std::string> StringSet;

  static const char* runSegmentFilter; /**< Hard coded PHENIX standard run and segment regex */
  
  SubsystemQA();
  SubsystemQA(const std::string& name);
  SubsystemQA(const std::string& name,
              const std::string& path);
  SubsystemQA(const std::string& name,
              const std::string& path,
              const std::string& regex);
  ~SubsystemQA();
  
  /** Add a constructed HistoQA histogram to be analyzed */
  void addHistogram(HistoQABase* histo);
  /** Add a HistoQA by shared_ptr */
  void addHistogram(HistoQAPtr histo);
  
  /** Add a new group of histograms by name and regex */
  void addHistoGroup(const std::string& name,
                     const std::string& regex);
  /** Add an observable to a previously added Histo/HistoGroup */
  void addObservable(const std::string& histName,
                     const std::string& obsName,
                     const int obs,  // Sigh, this should be an enum, but CINT...
                     const int sum, // This too...
                     const double lb = 0.0,
                     const double ub = 0.0);

  /** Add a previously constructed observable to a given histo/group */
  void addObservable(const std::string& histName,
                     const ObservablePtr& obs);

  /** Add a previously constructed observable by raw pointer */
  void addObservable(const std::string& histName,
                     ObservableQABase* obs);

  /** Run over all histograms added for all files */
  void processHistograms();

  /** Build and output the good run list for all histos/observables */
  void outputGoodRunList();

  /** Get a pointer to the outpoot ROOT file for this subsys */
  TFile* getOutfile() { return _outFile; }
  /** Open the current file as a ROOT TFile */
  bool openFile(const std::string& fileName);

  /** Set the RUN and SEGMENT from a file name */
  bool setRunInfo(const std::string& fileName);

  /** Get the current run number */
  int getRun() const { return _curRun; }
  /** Get the current segment number */
  int getSegment() const { return _curSegment; }

  /** Add a single path to the directories to be considered */
  void addPath(const std::string& pathIn);
  /** Recursively add all paths found in basePath matching the
      regular expression regexIn*/
  void addPathsByRegex(const std::string& basePath, 
                       const std::string& regexIn);
  /** Manually remove a path from the directories in use */
  void removePath(const std::string&);
  /** Print out the current path list */
  void printPaths() const;
  /** Retrieve the current path list */
  std::vector<std::string> getPathList() const;
  
  /** Build the list of files to be run over from the current list
      of paths */
  void buildFileList();
  /** Print the list of files that are being run over */
  void printFileList();
  /** Retrieve the current list of files that will be run on */
  std::vector<std::string> getFileList() const;

  /** Set the name of the histogram */
  void setName(const std::string& name) { _name = name; }
  /** Retrieve the name of the histogram */
  std::string getName() const { return _name; }

  /** Retrieve a std::vector<int> of run numbers analyzed */
  std::vector<int> getRunNumbers() const;
  /** Get the segments analyzed in a particular run */
  std::vector<int> getSegmentsInRun(int run) const;
  /** Number of segments in a run for normalizing */
  int getNumSegmentsInRun(int run) const;
  /** Get number of events in a particular analyzed run */
  int getEventsInRun(int run) const;  
  
protected:
  std::string _name; /**< Name of the subsystem to run QA on */
  histoMap _histograms; /**< Storage for the histograms to extract */

#ifndef __CINT__
  /** Nested functor to filter out ROOT files and add to internal storage */
  class appendFiles : 
    public std::unary_function<void,fs::path> {
  private:
    SubsystemQA &f;
  public:
    appendFiles(SubsystemQA &g) : f(g) {}    
    void operator()(const fs::path& p);
  };
#endif  /* __CINT__ */

  /** Get number of events from database */
  int getEventsFromDB(int runIn);
  /** Establish connection to a database */
  odbc::Connection* connectDB(const std::string& dbName, 
                              const std::string& userName);
  
  /** Prepare output file */
  void prepareOutputFile();
  /** Save summary histograms to subsystem file */
  void saveSummaryHistos();

  /** Helper function to build the run list from observables/histos */
  void appendRunList(const SetMap& inputMap);

#ifndef __CINT__
  std::set<fs::path> _pathList; /**< List of all paths added */
#endif
  StringSet _fileList; /**< List of all ROOT files found */

  std::multimap<int,int> _runToSegments; /**< Store the segment numbers in each run */
  std::map<int,int> _runToEvents; /**< Store the number of events per run for all runs */
  SetMap _runMap; /**< Store a map of Observable+Histo to good run lists */

  TFile *_currentFile; /**< Pointer to the currently open ROOT file */
  TFile *_outFile; /**< Pointer to output ROOT file */
  int _curRun; /**< Current run being processed */
  int _curSegment; /**< Current segment being processed */
  int _nEvents; /**< Number of events in the current run */
};

#endif // __MUONQAANALYZER_HISTO_QA_H__












