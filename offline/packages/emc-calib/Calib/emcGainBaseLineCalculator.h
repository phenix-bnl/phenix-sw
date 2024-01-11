#ifndef __EMCGAINBASELINECALCULATOR_H__
#define __EMCGAINBASELINECALCULATOR_H__

#include <map>
#include <set>
#include <string>
#include <vector>

class TH1;
class TH2;
class emcCalibrationDataHelper;
class emcFEMList;

class emcGainBaseLineCalculator
{
 public:

  static void deleteHistos();

  static void histogramming(bool onoff);

  static bool getBaseLine(emcCalibrationDataHelper* cdh,
			  int isector, const char* details,
			  float& value, 
			  float& error_on_value,
			  float& skewness,
			  float& kurtosis);
  

  static void write(bool make_subdirectories);

  class Tuple
  {
  public:
    Tuple() : index_(0), towerid_(-1), value_(0) 
    {}
    
    Tuple(int index, int towerid, float value) : index_(index), towerid_(towerid), value_(value)
    {}
    
    int index() const
    {
      return index_;
    }
    float value() const
    {
      return value_;
    }
    int towerid() const
    {
      return towerid_;
    }
  private:
    int index_;
    int towerid_;
    float value_;
  };

 private:
  
  static void createOneHistoPair(const std::string& basename,
				 const std::string& suffix,
				 int nx,
				 double xmin, double xmax);

  static void createHistos(int isector, const char* details);

  static bool get(const std::string& sname,
		  std::vector<Tuple>& values,
		  emcCalibrationDataHelper& cdh,
		  const emcFEMList& femlist,
		  const std::set<int>& femsToDiscard,
		  const int xminxmax);

  static TH1* getHisto(int isector, const char* details,
		       const std::string& suffix);

  static void fillHistograms(int isector, const char* details,
			     const std::vector<Tuple>& gt,
			     const std::vector<Tuple>& gt0,
			     const std::vector<int>& reject);

 private:

  static std::map<std::string,TH1*> fHistos;

  static int fRefRunNumber;
  static int fCurrentRunNumber;

  static emcCalibrationDataHelper* fCDH0;

  static bool fHistogramming;
};

inline bool operator<(const emcGainBaseLineCalculator::Tuple& fp1, 
		      const emcGainBaseLineCalculator::Tuple& fp2)
{
  return fp1.value() < fp2.value();
}

#endif
