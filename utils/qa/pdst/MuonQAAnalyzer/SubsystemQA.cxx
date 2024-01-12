
// STL Includes
#include<iostream>
#include<fstream>
#include<iterator>
#include<utility>
#include<sstream>
#include<stdexcept>

// BOOST Includes
#include<boost/regex.hpp>
#include<boost/bind.hpp>
#include<boost/tuple/tuple.hpp>
#include<boost/date_time/posix_time/posix_time.hpp>

// ROOT Includes
#include<TFile.h>

#include<Fun4AllServer.h>

// DB Includes
#include <odbc++/connection.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>

#include<HistoQABase.h>
#include<HistoGroupQA.h>
#include<SubsystemQA.h>
#include<MuonQAAnalyzer.h>
#include<ObservableQABase.h>
#ifndef __CINT__
#include<ObservableFactory.h>
#endif /* __CINT__ */

using boost::bind;

const char* SubsystemQA::runSegmentFilter = ".*-(\\d+)-(\\d+)\\..*";

SubsystemQA::SubsystemQA() :
  _name(),
  _pathList(),
  _fileList(),
  _runToSegments(),
  _runToEvents(),
  _runMap(),
  _currentFile(NULL),
  _curRun(0),
  _curSegment(0)
{
}

SubsystemQA::SubsystemQA(const std::string& name) :
  _name(name),
  _pathList(),
  _fileList(),
  _runToSegments(),
  _runToEvents(),
  _runMap(),
  _currentFile(NULL),
  _curRun(0),
  _curSegment(0)
{
}

SubsystemQA::SubsystemQA(const std::string& name, 
                         const std::string& path) :
  _name(name),
  _pathList(),
  _fileList(),
  _runToSegments(),
  _runToEvents(),
  _runMap(),
  _currentFile(NULL),
  _curRun(0),
  _curSegment(0)
{
  addPath(path); 
}

SubsystemQA::SubsystemQA(const std::string& name, 
                         const std::string& path,
                         const std::string& regex) :
  _name(name),
  _pathList(),
  _fileList(),
  _runToSegments(),
  _runToEvents(),
  _runMap(),
  _currentFile(NULL),
  _curRun(0),
  _curSegment(0)
{
  addPathsByRegex(path, regex);
}

SubsystemQA::~SubsystemQA()
{
  if ( _outFile )
    _outFile->Close();
}
  
void
SubsystemQA::addPath(const std::string& pathIn)
{
  _pathList.insert(fs::path(pathIn)); // std::set automatically excludes duplicates
}

void 
SubsystemQA::addPathsByRegex(const std::string& basePath, const std::string& regexIn)
{
  boost::regex pathFilter(regexIn);
  fs::recursive_directory_iterator end_itr; // Default ctor yields past-the-end   

  for( fs::recursive_directory_iterator it(basePath); it != end_itr; ++it )
    {
      boost::smatch what;
      if (!fs::is_directory(it->status()))
        {
          it.pop(); // Careful, this actually increments the iterator...
          if ( it == end_itr ) break;
        }

      if (!boost::regex_match(it->path().string(), what, pathFilter)) continue;     
      _pathList.insert(it->path());  
    }
} 

void
SubsystemQA::removePath(const std::string& pathIn)
{
  _pathList.erase(_pathList.find(fs::path(pathIn))); // Silently does nothing if not in list
}

void
SubsystemQA::printPaths() const
{
  std::transform(_pathList.begin(), _pathList.end(), 
                 std::ostream_iterator<std::string>(std::cout, "\n"),
                 bind(&fs::path::native,_1));
}

std::vector<std::string>
SubsystemQA::getPathList() const
{
  std::vector<std::string> tmpVec;

  std::transform(_pathList.begin(), _pathList.end(), 
                 std::back_inserter(tmpVec),
                 bind(&fs::path::native,_1));
  
  return tmpVec;
}

std::vector<std::string>
SubsystemQA::getFileList() const
{
  return std::vector<std::string>(_fileList.begin(),_fileList.end());
}

std::vector<int>
SubsystemQA::getRunNumbers() const
{
  std::vector<int> runNums(_runToEvents.size());
  std::transform(_runToEvents.begin(),
                 _runToEvents.end(),
                 runNums.begin(),
                 bind(&std::map<int,int>
                      ::iterator::value_type::first, _1));

  return runNums;
}

std::vector<int>
SubsystemQA::getSegmentsInRun(int run) const
{
  std::vector<int> segNums(_runToSegments.count(run));

  std::multimap<int,int>::const_iterator b,e;
  boost::tie(b,e) = _runToSegments.equal_range(run);

  std::transform(b,
                 e,
                 segNums.begin(),
                 bind(&std::multimap<int,int>
                      ::iterator::value_type::second,_1));  
  std::sort(segNums.begin(),segNums.end());

  return segNums;
}

int
SubsystemQA::getNumSegmentsInRun(int run) const
{
  return _runToSegments.count(run);
}

int 
SubsystemQA::getEventsInRun(int run) const
{
  std::map<int,int>::const_iterator it = _runToEvents.find(run);
  return ( it != _runToEvents.end() ) ? it->second : -1;
}

odbc::Connection* 
SubsystemQA::connectDB(const std::string& dbName, const std::string& userName)
{
  odbc::Connection *con=NULL;
  try
    {
      con = odbc::DriverManager::getConnection(dbName,userName,"");
    }
  catch (odbc::SQLException& e)
    {
      std::cout << "Error connecting to database: " 
                << e.getMessage() << std::endl;
      return(NULL);
    }
  
  return(con);
}

int
SubsystemQA::getEventsFromDB(int runIn)
{  
  odbc::Connection* con = connectDB("daq","");
  if ( !con ) return 0;
  
  std::stringstream cmd;  
  cmd << "select eventsinrun from run where runnumber = " << runIn << ";";
  odbc::Statement *stmt = con->createStatement();
  odbc::ResultSet *rs = NULL;

  /// TODO: Switch this to RAII after I get it working.

  try
    {
      rs=stmt->executeQuery(cmd.str());
    }
  catch(odbc::SQLException &e)
    {
      std::cout << "Error executing query: " << cmd << std::endl
                << e.getMessage() << std::endl;

      delete rs;
      delete stmt;
      delete con;
      return(0);
    }
  
  if(rs->next()==0)
    {
      std::cout << "Error: no entries for query" << std::endl;
      delete rs;
      delete stmt;
      delete con;
      return(0);
    }

  int nEvents = rs->getInt(1);
    
  delete rs;
  delete stmt;
  delete con;
  
  return nEvents;
}

void
SubsystemQA::buildFileList()
{
  std::cout << _name << ": Building file list..." << std::flush;
  std::for_each(_pathList.begin(), _pathList.end(), appendFiles(*this));  
  std::cout << " found " << _fileList.size() << " files" << std::endl;
}

void 
SubsystemQA::printFileList()
{
  std::copy(_fileList.begin(), _fileList.end(), 
                 std::ostream_iterator<std::string>(std::cout, "\n")); 
}

void
SubsystemQA::appendFiles::operator()(const fs::path& p)
{
  // Yes, all these parentheses are necessary.  (See C++'s Most 
  // Vexing Parse)
  std::vector<fs::path> filesInPath((fs::directory_iterator(p)), 
                                    (fs::directory_iterator()));

  // Strip out anything that isn't a ROOT file
  filesInPath.erase(std::remove_if(filesInPath.begin(),
                                   filesInPath.end(),
                                   bind(&fs::path::extension,_1) != ".root"),
                    filesInPath.end());


  // Fill the file list that we will use to load in the ROOT files
  std::transform(filesInPath.begin(), filesInPath.end(), 
                 std::inserter(f._fileList, f._fileList.end()),
                 bind(&fs::path::native,_1));
}

void 
SubsystemQA::addHistogram(HistoQABase* histo)
{
  addHistogram(HistoQAPtr(histo));
}

void 
SubsystemQA::addHistogram(HistoQAPtr histo)
{
  _histograms.insert(std::make_pair(histo->getName(),histo));
  histo->setParent(shared_from_this());
}

void
SubsystemQA::addHistoGroup(const std::string& name,
                           const std::string& regex)
{
  HistoQAPtr hQA = boost::make_shared<HistoGroupQA>(name,regex);
  _histograms.insert(std::make_pair(name, hQA));
  hQA->setParent(shared_from_this());
}

void
SubsystemQA::addObservable(const std::string& histName,
                           const std::string& obsName,
                           const int obs,
                           const int sum,
                           const double lb,
                           const double ub)
{  
  // Unfortunately CINT is not very happy with enums from other namespaces...
  // We will just trust the user knows what they are doing and hope for the best
  ObservablePtr newObs = ObservableFactory::makeObservable(obsName,
                                                           static_cast<MuonQA::ObsType>(obs),
                                                           static_cast<MuonQA::SummaryFlag>(sum)
                                                           );

  if ( lb != 0.0 || ub != 0.0 )
    newObs->setBounds(lb, ub); 

  addObservable(histName,newObs);  
}

void
SubsystemQA::addObservable(const std::string& histName,
                           const ObservablePtr& obs)
{
  histoMap::iterator histoItr  = _histograms.find(histName);
  if ( histoItr == _histograms.end() )
  {
    throw std::runtime_error(std::string("Could not find registered histogram with name: ") + histName);
  }

  histoItr->second->addObservable(obs);

  std::cout << "Added " << obs->getName()
            << " observable to histo/histo group "
            << histName << std::endl;  
}

void
SubsystemQA::addObservable(const std::string& histName,
                           ObservableQABase* obs)
{
  addObservable(histName, ObservablePtr(obs));
}

void 
SubsystemQA::processHistograms()
{
  MuonQAAnalyzer::setActive(shared_from_this());
  std::cout << _name << ": Processing histograms..." << std::endl;
  int fileNum = 1;
  int remainingMins = -1;
  boost::posix_time::ptime start = boost::posix_time::second_clock::local_time();  
  for ( StringSet::iterator it = _fileList.begin(); it != _fileList.end(); ++it )
    {
      boost::posix_time::ptime now = boost::posix_time::second_clock::local_time();
      boost::posix_time::time_duration diff = now - start;
      remainingMins = diff.total_milliseconds()/fileNum*(_fileList.size() - fileNum)/(60.*1000);
      
      std::cout << "\r" << "Processed " << fileNum << " of " << _fileList.size();
      if ( fileNum > 10 )
        std::cout <<  " - approx. " << remainingMins << " minutes remaining";
      std::cout << std::flush;
      
      fileNum++;

      if ( !openFile(*it) ) continue;

      std::for_each(_histograms.begin(),_histograms.end(),
                    bind(&HistoQABase::processObservables,
                         bind(&MVT::second,_1)));
    }
  std::cout << std::endl;

  prepareOutputFile();
  saveSummaryHistos();
}

void
SubsystemQA::outputGoodRunList()
{    
  std::for_each(_histograms.begin(),_histograms.end(),
                bind(&SubsystemQA::appendRunList,this,
                     bind(&HistoQABase::outputGoodRunList,
                          bind(&MVT::second,_1)
                          )
                     )
                );

  std::vector<int> allRuns = getRunNumbers();

  std::ofstream ofs;
  std::string fname = _name + "_goodRuns.list";
  ofs.open( fname.c_str(), std::ofstream::out | std::ofstream::trunc );

  ofs << "Run Number";
  for ( SetMap::iterator MapIt = _runMap.begin();
        MapIt != _runMap.end();
        ++MapIt )
    if ( ! (*MapIt).second.empty() )
    {
      std::cout << "No good runs reported for " << (*MapIt).first
                << " skipping good run list output for this observable" << std::endl;
      ofs << "\t" << (*MapIt).first;
    }
  ofs << std::endl;

  // Now I have the runs analyzed in allRuns and the Histo+Observable lists in _runMap
  for( std::vector<int>::iterator it = allRuns.begin();
       it != allRuns.end();
       ++it )
  {
    ofs << (*it);
    for ( SetMap::iterator MapIt = _runMap.begin();
          MapIt != _runMap.end();
          ++MapIt )
    {
      if ( ! (*MapIt).second.empty() )
      {
        if ( (*MapIt).second.find(*it) != (*MapIt).second.end() )
          ofs << "\t" << 1;
        else
          ofs << "\t" << 0;        
      }
    }
    ofs << std::endl;
  }  
}

void
SubsystemQA::appendRunList(const SetMap& inputMap)
{
  _runMap.insert(inputMap.begin(), inputMap.end());
}

void
SubsystemQA::prepareOutputFile()
{
  std::string fname = _name + ".root";
  _outFile = TFile::Open(fname.c_str(),"RECREATE");
  _outFile->cd();
}

void
SubsystemQA::saveSummaryHistos()
{
  std::for_each(_histograms.begin(),_histograms.end(),
                bind(&HistoQABase::saveSummaries,
                     bind(&MVT::second,_1)));
}

bool
SubsystemQA::openFile(const std::string& fileName)
{
  if ( _currentFile )
  {
    _currentFile->Close("R");
    delete _currentFile;
  }
  _currentFile = TFile::Open(fileName.c_str());
  if ( !_currentFile ) return false;

  std::for_each(_histograms.begin(),_histograms.end(),
                bind(&HistoQABase::setFile,
                     bind(&MVT::second,_1),
                     _currentFile
                     )
                );

  return setRunInfo(fileName);
}

bool
SubsystemQA::setRunInfo(const std::string& fileName)
{
  static const boost::regex filter(runSegmentFilter);
  boost::smatch what;

  if (!boost::regex_match(fileName, what, filter))
    {
      std::cout << " - Could not extract run and segment, aborting "
                << fileName << std::endl;
      return false;
    }
  else
    {
      // This is way too convoluted...
      _curRun = atoi(what[1].str().c_str()); 
      _curSegment = atoi(what[2].str().c_str());
      //      std::cout << " - Run: " << _curRun << " Segment: " << _curSegment;

      _runToSegments.insert(std::make_pair<int,int>(_curRun,_curSegment));
      _runToEvents.insert(std::make_pair<int,int>(_curRun,getEventsFromDB(_curRun)));
      return true;
    }
}
