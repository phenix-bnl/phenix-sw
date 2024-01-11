#ifndef __EMCGAINEVOLUTION_H__
#define __EMCGAINEVOLUTION_H__

#include <string>
#include <vector>
#include <map>
#include <ctime>
#include <iostream>
#include <set>

class TGraphErrors;

/** Helper class to draw gain evolution over time, and play with gain 
base line.
@ingroup calibration
*/

class emcGainEvolution
{
 public:

  enum EHistogrammingType
    {
      kNone=-1,
      kOneFilePerRun,
      kOneFilePer1KRange,
      kOneFileForAll
    };

  emcGainEvolution(const char* outputfile);

  void setTimeOffset(time_t offset) { fTimeOffset = offset; }

  void setHistogrammingType(emcGainEvolution::EHistogrammingType htype);

  ~emcGainEvolution();

  void addRun(int runnumber)
  {
    fRuns.push_back(runnumber); 
  }

  void clearRun()
  {
    fRuns.clear();
  }

  void addSector(const char* sector)
  {
    fSectors.push_back(sector);
  }

  void clearSector()
  {
    fSectors.clear();
  }

  void addTrimmingPercentage(float percent)
  { 
    fPercent.push_back(percent);
  }

  void clearDetails()
  {
    fDetails.clear();
  }

  void clearTrimmingPercentage()
  {
    fPercent.clear();
  }

  void print(std::ostream& out = std::cout) const;

  void run();

  void addDetails(const char* femdetails)
  {
    fDetails.push_back(femdetails);
  }

  void reset()
  {
    clearRun();
    clearSector();
    clearTrimmingPercentage();
    clearDetails();
    fRunChecks.clear();
    fProcesses.clear();
  }

  void verbose(int verboselevel) { fVerbose=verboselevel; }

  int verbose() const { return fVerbose; }

 private:

  void writeGraphs();

 private:

  void createGraph(const std::string& graphname,
		   const std::vector<double>& x,
		   const std::vector<double>& y,
		   const std::vector<double>& xerr,
		   const std::vector<double>& yerr);

  std::string graphName(const std::string& process,
			const std::string& suffix);

  void makeGraphs(const std::string& process);
  void makeGraph_absolute(const std::string& process);
  void makeGraph_wrt_production(const std::string& process);
  void makeGraph_distriShape(const std::string& process);

  void save(int run);

 private:
  
  std::string fOutputFile;

  class BaseLine;
  class RunCheck;

  class BaseLine
  {
  public:
    BaseLine(float value=0.0, float error=0.0,
	     float diff=0.0, float diff_error=0.0,
	     float skew=0.0, float kurt=0.0)
      : fValue(value), fError(error),
	fDiff(diff), fDiffError(error),
	fSkewness(skew), fKurtosis(kurt) {}
    
    float value() const { return fValue; }
    float error() const { return fError; }
    
    float diff() const { return fDiff; }
    float diffError() const { return fDiffError; }
    float skewness() const { return fSkewness; }
    float kurtosis() const { return fKurtosis; }

    void print(std::ostream& os = std::cout) const
    {
      os << "Value=" << value()
	 << " Error=" << error()
	 << " Diff to Ref=" << diff() << " % "
	 << " +- " << diffError() << " %"
	 << std::endl;
    }
    
  private:
    float fValue;
    float fError;
    float fDiff;
    float fDiffError;
    float fSkewness;
    float fKurtosis;
  };
  
  class RunCheck
  {
  public:
    RunCheck(int runnumber=-1)
      : fRunNumber(runnumber) {}
	
    int runnumber() const { return fRunNumber; }
    
    void set(const std::string& details, const BaseLine& value)
    { fValues[details] = value; }
    
    BaseLine get(const std::string& details) const;
    
  private:
    int fRunNumber;
    std::map<std::string,BaseLine> fValues;
  };

 private:
  
  time_t fTimeOffset;
  std::vector<int> fRuns;
  std::map<int,RunCheck> fRunChecks;
  std::vector<float> fPercent;
  std::vector<std::string> fSectors;
  std::set<std::string> fProcesses;
  std::vector<std::string> fDetails;
  std::map<std::string,TGraphErrors*> fGraphs;
  int fVerbose;
  EHistogrammingType fHistogrammingType;
};

#endif
