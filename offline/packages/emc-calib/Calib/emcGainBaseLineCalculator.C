#include "emcGainBaseLineCalculator.h"

#include "emcCalFEM.h"
#include "emcCalibrationDataHelper.h"
#include "emcFEMList.h"
#include "EmcIndexer.h"
#include "PHTimeStamp.h"
#include "TH1.h"
#include "TH2.h"
#include "TDirectoryHelper.h"
#include "TDirectory.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <iterator>
#include <list>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

int emcGainBaseLineCalculator::fRefRunNumber = -1;
int emcGainBaseLineCalculator::fCurrentRunNumber = -1;
emcCalibrationDataHelper* emcGainBaseLineCalculator::fCDH0 = 0;
std::map<std::string, TH1*> emcGainBaseLineCalculator::fHistos;
bool emcGainBaseLineCalculator::fHistogramming = false;


namespace
{ 
  void 
  distributionShape(const std::vector<emcGainBaseLineCalculator::Tuple>& vx, 
		    double mean, 
		    unsigned int k,
		    float& variance,
		    float& skewness,
		    float& kurtosis)
  {
    double m2 = 0.0;
    double m3 = 0.0;
    double m4 = 0.0;
    
    unsigned int n = vx.size()-1;

    for ( size_t i = k; i <= n - k ; ++i )
      {
	double x = vx[i].value() - mean;
	m2 += x*x;
	m3 += x*x*x;
	m4 += x*x*x*x;
      }
    
    m2 /= n;
    m3 /= n;
    m4 /= n;
    double s = sqrt(m2);
    
    skewness = m3/(s*s*s);
    kurtosis = m4/(s*s*s*s);
    variance = m2;

  }

  int getIz(const emcGainBaseLineCalculator::Tuple& x)
  {
    int towerid = x.towerid();
    int is,iy,iz;
    EmcIndexer::decodeTowerId(towerid,is,iz,iy);
    return iz;
  }

  int getIy(const emcGainBaseLineCalculator::Tuple& x)
  {
    int towerid = x.towerid();
    int is,iy,iz;
    EmcIndexer::decodeTowerId(towerid,is,iz,iy);
    return iy;
  }

  class BLMethod
  {
  public:
    
    BLMethod() : fZeroSuppressed(false), fPercentage(0) {}

    void zeroSuppressed(bool yesorno) { fZeroSuppressed=yesorno; }
    bool isZeroSuppressed(void) const { return fZeroSuppressed; }

    float percentage() const { return fPercentage; }
    void percentage(float v) { fPercentage = v; }

    virtual 
    void compute(const std::vector<emcGainBaseLineCalculator::Tuple>& gt,
		 std::vector<emcGainBaseLineCalculator::Tuple>& gt0,
		 std::vector<int>& reject,
		 float& value,
		 float& error_on_value,
		 float& skewness,
		 float& kurtosis) = 0;
  private:
    bool fZeroSuppressed;
    float fPercentage;
  };
  
  float ratio(float a, float b, float epsilon)
  {
    if ( fabs(b) > epsilon )
      {
        return a / b;
      }
    else
      {
        return 0.0;
      }
  }

  void split(const std::string& s,
             const char sep,
             std::vector<std::string>& parts)
  {
    std::string str = s;

    std::vector<size_t> sep_pos;

    if ( str[0] != sep )
      {
        str.insert(str.begin(), sep);
      }

    if ( str[str.size() - 1] != sep )
      {
        str.push_back(sep);
      }

    for (size_t i = 0 ; i < str.size() ; i++)
      {
        if ( str[i] == sep )
          {
            sep_pos.push_back(i);
          }
      }

    parts.clear();

    if ( sep_pos.size() > 0 )
      {
        for (size_t i = 0 ; i < sep_pos.size() - 1 ; i++)
          {
            parts.push_back(str.substr(sep_pos[i] + 1,
                                       sep_pos[i + 1] - sep_pos[i] - 1));
          }
      }
  }


  std::string patch(const std::string& src)
  {
    std::string dest = src;
    for ( size_t i = 0; i < dest.size(); ++i )
      {
        if ( dest[i] == ':' )
          {
            dest[i] = '_';
          }
        if ( dest[i] == '-' )
          {
            dest.erase(i, 1);
          }
      }
    return dest;
  }

  std::string buildBasename(int run, int isector, const char* details)
  {
    std::ostringstream str;

    str << run << "/" << EmcIndexer::EmcSectorId(isector)
	<< "_" << patch(details);

    return str.str();
  }

  std::ostream& operator<<(std::ostream& os, 
			   const emcGainBaseLineCalculator::Tuple& fp)
  {
    os << "[" << fp.index() << "," << fp.value() << "] ";
    return os;
  }

  int trim(const std::vector<emcGainBaseLineCalculator::Tuple>& x,
           const float& alpha,
           float& tmean, float& tvar,
           unsigned int& k,
           std::vector<emcGainBaseLineCalculator::Tuple>& sx,
	   std::vector<int>& reject)
  {
    //
    // Calculates the trimmed (tmean) mean
    // of a sample (x) and estimates the variance (tvar)
    // of that mean.
    //

    // First check input parameters

    unsigned int n = x.size(); // number of observations
    if ( n < 2 )
      {
        return -1;
      }

    if ( alpha < 0 || alpha >= 0.5 )
      // proportion of observations
      // to be trimmed at each end of the sorted sample
      {
        return -2;
      }

    // Input parameters are good. Let's move on.

    // Insure we use a sample sorted into ascending order.

    sx = x;
    std::sort(sx.begin(), sx.end());

    // Number of observations trimmed at each end.

    k = static_cast<unsigned int>(floorf(alpha * n));

    float sum = 0.0;

    for ( size_t i = k; i < n - k ; ++i )
      {
        sum += sx[i].value();
      }

    tmean = sum / ( n - 2 * k );

    float t2 = 0.0;

    for ( size_t i = k; i < n - k; ++i )
      {
        t2 += (sx[i].value() - tmean) * (sx[i].value() - tmean);
      }

    tvar = (
	    t2 +
	    k * (sx[k].value() - tmean) * (sx[k].value() - tmean) +
	    k * (sx[n - k - 1].value() - tmean) * (sx[n - k - 1].value() - tmean)
	    ) / (n * n);

    // mark the rejected entries as such
    for ( size_t i = 0; i < k; ++i ) 
      {
	reject[sx[i].index()]=1;
      }

    for ( size_t i = n - k; i < n; ++i ) 
      {
	reject[sx[i].index()]=1;
      }

    return 0;
  }

  void zeroSuppress(const std::vector<emcGainBaseLineCalculator::Tuple>& in,
		    std::vector<emcGainBaseLineCalculator::Tuple>& out,
		    float epsilon,
		    std::vector<int>& reject)
  {
    out.clear();
    for ( size_t i = 0; i < in.size(); ++i )
      {
	if ( std::fabs(in[i].value()) < epsilon )
	  {
	    reject[in[i].index()] = 1;
	  }
	else
	  {
	    out.push_back(in[i]);
	  }
      }
  }

  class BLM_RatioOfAverages : public BLMethod
  {
  public:
    void compute(const std::vector<emcGainBaseLineCalculator::Tuple>& gt,
		 std::vector<emcGainBaseLineCalculator::Tuple>& gt0,
		 std::vector<int>& reject,
		 float& value,
		 float& error_on_value,
		 float& skewness,
		 float& kurtosis)
    {
      if ( gt0.empty() )
	{
	  for ( size_t i = 0; i < gt.size(); ++i )
	    {
	      gt0.push_back(emcGainBaseLineCalculator::Tuple(gt[i].index(),gt[i].towerid(),1.0));
	    }
	}

      value = 0;
      error_on_value = 0;
    
      float mgt, egt;
      float mgt0, egt0;

      std::vector<emcGainBaseLineCalculator::Tuple> cgt;
      std::vector<emcGainBaseLineCalculator::Tuple> cgt0;

      reject.resize(gt.size());

      for ( size_t i = 0; i < gt.size(); ++i )
        {
	  assert(gt[i].index()==gt0[i].index());
          bool both_non_zero = ( gt[i].value() != 0 && gt0[i].value() != 0 );

          if ( !isZeroSuppressed() || 
	       (isZeroSuppressed() && both_non_zero) 
	       )
            {
              cgt.push_back(gt[i]);
              cgt0.push_back(gt0[i]);
	      reject[gt[i].index()] = 0;
            }
          else
            {
	      reject[gt[i].index()] = 1;
             }
        }

      std::vector<emcGainBaseLineCalculator::Tuple> sx;
      std::vector<emcGainBaseLineCalculator::Tuple> sx0;
      unsigned int k;
      unsigned int k0;

      int error = trim(cgt, percentage(), mgt, egt, k, sx, reject);      
      int error0 = trim(cgt0, percentage(), mgt0, egt0, k0, sx0, reject);
      assert(k==k0);

      if ( error || error0 )
	{
	  std::cerr << __FILE__ << ":" << __LINE__ << " trim error."
		    << std::endl;
	  return;
	}

      const float epsilon = 1E-3;

      value = ratio(mgt, mgt0, epsilon);

      egt = ratio(sqrt(egt), mgt, epsilon);
      egt0 = ratio(sqrt(egt), mgt0, epsilon);

      error_on_value = sqrt(egt * egt + egt0 * egt0);

      float variance;

      distributionShape(cgt,mgt,k,variance,skewness,kurtosis);
    }
  };


  /**
     Implementation of BLMethod based on the computation of the average of
     the gain ratios.
     The gains (either at time t or time 0) are first zero suppressed if so 
     required. Then both g(t) and g(t0) samples are trimmed if so required.
     The remaining samples are used to define a set of gain ratios. Finally
     the average is the trimmed-mean of that gain ratios sample. 
   */
  class BLM_AverageOfRatios : public BLMethod
  {
  public:
    void compute(const std::vector<emcGainBaseLineCalculator::Tuple>& gt,
		 std::vector<emcGainBaseLineCalculator::Tuple>& gt0,
		 std::vector<int>& reject,
		 float& value,
		 float& error_on_value,
		 float& skewness,
		 float& kurtosis)
    {
      if ( gt0.empty() )
	{
	  gt0 = gt;
	}

      value = 0;
      error_on_value = 0;
      skewness = 0;
      kurtosis = 0;

      reject.resize(gt.size());

      std::vector<emcGainBaseLineCalculator::Tuple> values;
      std::vector<emcGainBaseLineCalculator::Tuple> sgt;
      std::vector<emcGainBaseLineCalculator::Tuple> sgt0;

      const float epsilon = 1E-3;

      if ( isZeroSuppressed() )
	{
	  // Zero suppress both g(t) and g(t0) if so required.
	  zeroSuppress(gt,sgt,epsilon,reject);
	  zeroSuppress(gt0,sgt0,epsilon,reject);
	}

      std::vector<emcGainBaseLineCalculator::Tuple> sx;
      std::vector<emcGainBaseLineCalculator::Tuple> sx0;
      unsigned int k;
      unsigned int k0;
      float dummy;

      // Trim the zero-suppressed g(t) and gt(0).
      // What is really important after the 2 trim calls is the content
      // of the reject[] array.
      int error = trim(sgt, percentage(), dummy, dummy, k, sx, reject);      
      int error0 = trim(sgt0, percentage(), dummy, dummy, k0, sx0, reject);

      if ( error || error0 )
	{
	  std::cerr << __FILE__ << ":" << __LINE__ << " trim error."
		    << std::endl;
	  return;
	}

      // Compute the ratios only for the gains which survived both
      // the zero suppression and the trimming.
      for ( size_t i = 0; i < gt.size(); ++i )
        {
	  assert(gt[i].index()==gt0[i].index());
	  assert(gt[i].towerid()==gt0[i].towerid());

	  if ( reject[gt[i].index()] ) continue;

          float value = ratio(gt[i].value(), gt0[i].value(), epsilon);

          if ( !isZeroSuppressed() || 
	       (isZeroSuppressed() && value != 0 ) )
            {
              values.push_back(emcGainBaseLineCalculator::Tuple(i,gt[i].towerid(),value));
            }
	  else
	    {
	      reject[gt[i].index()]=1;
	    }
        }

      if ( values.empty() )
        {
          std::cerr << __FILE__ << ":" << __LINE__ << " No values."
		    << std::endl;
          return;
        }

      // Finally we trim those ratios too to get the final answer.
      error = trim(values, percentage(), value, error_on_value, 
		   k, sx, reject);

      if ( error )
	{
	  std::cerr << __FILE__ << ":" << __LINE__ << " trim error."
		    << std::endl;
	  value = 0;
	  error_on_value = 0;
	  return;
	}
      
      float variance;

      distributionShape(values,value,k,variance,skewness,kurtosis);

      error_on_value = sqrt(error_on_value);
    }
  };

  class BLM_AverageOfAsymetries : public BLMethod
  {
  public:
    void compute(const std::vector<emcGainBaseLineCalculator::Tuple>& gt,
		 std::vector<emcGainBaseLineCalculator::Tuple>& gt0,
		 std::vector<int>& reject,
		 float& value,
		 float& error_on_value,
		 float& skewness,
		 float& kurtosis)
    {
      if ( gt0.empty() )
	{
	  // that a non-sense request. Let the value be nonsense too.
	  const float some_crazy_high_number = 1E9;
	  for ( size_t i = 0; i < gt.size(); ++i )
	    {
	      emcGainBaseLineCalculator::Tuple t = 
		emcGainBaseLineCalculator::Tuple(gt[i].index(),
						 gt[i].towerid(),
						 some_crazy_high_number);
	      gt0.push_back(t);
	    }
	}

      value = 0;
      error_on_value = 0;
      skewness = 0;
      kurtosis = 0;

      std::vector<emcGainBaseLineCalculator::Tuple> values;

      const float epsilon = 1E-3;

      reject.resize(gt.size());
   
      for ( size_t i = 0; i < gt.size(); ++i )
	{
	  assert(gt[i].index()==gt0[i].index());
	  assert(gt[i].towerid()==gt0[i].towerid());
	  float value = ratio(gt[i].value()-gt0[i].value(), 
			      gt[i].value()+gt0[i].value(), epsilon);
	  
	  if ( !isZeroSuppressed() || 
	       (isZeroSuppressed() && value != 0 ) )
	    {
	      reject[gt[i].index()] = 0;
	      values.push_back(emcGainBaseLineCalculator::Tuple(i,gt[i].towerid(),value));
	    }
	  else
	    {
	      reject[gt[i].index()] = 1;
	    }
	}

      if ( values.empty() )
        {
          return;
        }

      std::vector<emcGainBaseLineCalculator::Tuple> sx;
      unsigned int k;

      int error = trim(values, percentage(), value, error_on_value, 
		       k, sx, reject);

      if ( error )
	{
	  std::cerr << __FILE__ << ":" << __LINE__ << " trim error."
		    << std::endl;
	  value = 0;
	  error_on_value = 0;
	  return;
	}

      float variance;

      distributionShape(values,value,k,variance,skewness,kurtosis);

      error_on_value = sqrt(error_on_value);
      
    }
  };

  int decodeRunNumber(const std::string str)
  {
    bool ok = true;

    for ( size_t i = 0; i < str.size() && ok; ++i ) 
      {
	if (!isdigit(str[i])) ok=false;
      }
    if (!ok)
      {
	return -1;
      }
    else
      {
	return atoi(str.c_str());
      }
  }

  BLMethod* decodeDetails(const std::string& details,
			  std::set<int>& femsToDiscard,
			  float& percent,
			  int& xminxxmax,
			  bool& zerosuppressed,
			  int& runnumber)
  {
    femsToDiscard.clear();
    percent = 0;
    xminxxmax = -1;
    zerosuppressed = false;
    BLMethod* method = 0;
    bool methodSpecified = false;
    bool runnumberSpecified = false;

    std::vector<std::string> parts;

    split(details, ':', parts);

    std::list<int> residualParts; 
    // list to keep track of indices
    // of parts. By the time the decoding below is finished,
    // this list should be empty. If not, it means something 
    // in the decoding went wrong (most probably a syntax error
    // in the details string, e.g. method's name).

    for ( size_t i = 0; i < parts.size(); ++i )
      {
	residualParts.push_back(i);
      }

    if ( parts.size() > 0)
      {
	percent = atoi(parts[0].c_str());
	if ( percent<0 || percent >= 50 )
	  {
	    std::cerr << __FILE__ << ":" << __LINE__ 
		      << "percent=" << percent << "!!"
		      << std::endl;
	    return 0;
	  }
	percent /= 100;
	residualParts.remove(0);
      }

    for ( size_t i = 1; i < parts.size(); ++i )
      {
	if ( parts[i] == "xmin" )
	  {
	    xminxxmax = -1;
	    residualParts.remove(i);
	  }
	if ( parts[i] == "xmax" )
	  {
	    xminxxmax = + 1;
	    residualParts.remove(i);
	  }
	if ( parts[i] == "x" )
	  {
	    xminxxmax = 0;
	    residualParts.remove(i);
	  }

	if ( parts[i][0] == '-' )
	  {
	    femsToDiscard.insert(atoi(parts[i].c_str() + 1));
	    residualParts.remove(i);
	  }

	if ( parts[i] == "ZS" )
	  {
	    zerosuppressed = true;
	    residualParts.remove(i);
	  }

	if ( parts[i] == "RATIOOFAV" )
	  {
	    delete method;
	    method = new BLM_RatioOfAverages();
	    methodSpecified = true;
	    residualParts.remove(i);
	  }

	if ( parts[i] == "AVOFRATIO" )
	  {
	    delete method;
	    method = new BLM_AverageOfRatios();
	    methodSpecified = true;
	    residualParts.remove(i);
	  }

	if ( parts[i] == "AVOFASYM" )
	  {
	    delete method;
	    method = new BLM_AverageOfAsymetries();
	    methodSpecified = true;
	    residualParts.remove(i);
	  }

// 	if ( parts[i] == "GAUSRATIO" )
// 	  {
// 	    return BLM_GaussianFitOfRatios();
//	    methodSpecified = true;
// 	  }
      }

    //    runnumber = atoi(parts[parts.size() - 1].c_str());
    runnumber = decodeRunNumber(parts[parts.size()-1]);
    if (runnumber>=0 )
      {
	residualParts.remove(parts.size()-1);
	runnumberSpecified=true;
      }

    if ( !runnumberSpecified )
      {
	// let it be 0 by default to be backward compatible.
	runnumber = 0;
      }

    if ( !residualParts.empty() )
      {
	std::cerr << __FILE__ << ":" << __LINE__ << " details' string="
		  << details << " is not valid. Here are the residues "
		  << "(separated by |) : "
		  << std::endl;
	while ( !residualParts.empty() )
	  {
	    int ix = residualParts.front();
	    residualParts.pop_front();
	    std::cerr << parts[ix] << " | " << std::endl;
	  }
	return 0;
      }

    if (!(xminxxmax == -1 || xminxxmax == 0 || xminxxmax == 1))
      {
	std::cerr << __FILE__ << ":" << __LINE__ << " details' string="
		  << details << " is not valid. xminxxmax decoded to "
		  << xminxxmax
		  << std::endl;
	return 0;
      }

    if (!methodSpecified)
      {
	// for backward compatibility.
	// Note that this test is made only AFTER we check that 
	// the FULL details string is correct (e.g. we correctly
	// handle the case where the method name is mispelled)
	method = new BLM_RatioOfAverages();
      }

    method->zeroSuppressed(zerosuppressed);
    method->percentage(percent);
    return method;
  }

}

//_____________________________________________________________________________
void
emcGainBaseLineCalculator::histogramming(bool onoff)
{
  fHistogramming = onoff;
}

//_____________________________________________________________________________
TH1*
emcGainBaseLineCalculator::getHisto(int isector, const char* details,
                                    const std::string& suffix)
{
  std::string name = buildBasename(fCurrentRunNumber, isector, details);

  name += "_";
  name += suffix;

  std::map<std::string, TH1*>::const_iterator it = fHistos.find(name);

  if ( it != fHistos.end() )
    {
      return it->second;
    }
  else
    {
      //      std::cout << "Creating histogram " << name << std::endl;
      createHistos(isector, details);
      it = fHistos.find(name);
      if ( it == fHistos.end() )
        {
          std::cerr << "<E> emcGainBaseLineCalculator::getHisto : Creation "
		    << "failed for " << name << std::endl;
          return 0;
        }
      else
        {
          return it->second;
        }
    }
}

//_____________________________________________________________________________
void
emcGainBaseLineCalculator::createOneHistoPair(const std::string& basename,
					      const std::string& suffix,
					      int nx,
					      double xmin, double xmax)
{
  std::string key = basename;
  key += "_";
  key += suffix;
  std::string::size_type pos = key.find_first_of('/');
  std::string hname = key.substr(pos + 1);

  TH1* h = new TH1F(hname.c_str(), hname.c_str(), nx, xmin, xmax);
     
  h->SetDirectory(0);
  fHistos[key] = h;

  hname += "_yz";
  key += "_yz";

  h = new TH2F(hname.c_str(), hname.c_str(),
	       72,-0.5,71.5,
	       36,-0.5,35.5);

  h->SetDirectory(0);
  fHistos[key] = h;
}

//_____________________________________________________________________________
void
emcGainBaseLineCalculator::createHistos(int isector, const char* details)
{
  std::string basename = buildBasename(fCurrentRunNumber, isector, details);

  createOneHistoPair(basename, "gt", 100, 0, 400);
  createOneHistoPair(basename, "ratio", 100, 0.5, 1.5);
  createOneHistoPair(basename, "asym", 100, -0.4, 0.4);
}

//_____________________________________________________________________________
void
emcGainBaseLineCalculator::deleteHistos()
{
  std::map<std::string, TH1*>::iterator it;
  for ( it = fHistos.begin(); it != fHistos.end(); ++it )
    {
      delete it->second;
    }
  fHistos.clear();
}

//_____________________________________________________________________________
void
emcGainBaseLineCalculator::fillHistograms(int isector, const char* details,
					  const std::vector<emcGainBaseLineCalculator::Tuple>& gt,
					  const std::vector<emcGainBaseLineCalculator::Tuple>& gt0,
					  const std::vector<int>& reject)
{
  assert(gt.size() == gt0.size());
  assert(gt.size() == reject.size());

  TH1* hgt = getHisto(isector, details, "gt");
  TH2* hgt_yz = static_cast<TH2*>(getHisto(isector, details, "gt_yz"));

  for ( size_t i = 0; i < gt.size(); ++i )
    {
      if (!reject[i])
        {
          hgt->Fill(gt[i].value());
	  hgt_yz->Fill(getIz(gt[i]),getIy(gt[i]),gt[i].value());
        }
    }

 //  TH1* hgt0 = getHisto(isector, details, "gt0");
//   TH2* hgt0_yz = static_cast<TH2*>(getHisto(isector, details, "gt0_yz"));

//   for ( size_t i = 0; i < gt0.size(); ++i )
//     {
//       if (!reject[i] )
//         {
//           hgt0->Fill(gt0[i].value());
// 	  hgt0_yz->Fill(getIz(gt0[i]),getIy(gt0[i]),gt0[i].value());
//         }
//     }

  TH1* hratio = getHisto(isector, details, "ratio");
  TH2* hratio_yz = static_cast<TH2*>(getHisto(isector, details, "ratio_yz"));

  for ( size_t i = 0; i < gt0.size(); ++i )
    {
      if (!reject[i] )
        {
          float value = 0;
          if ( gt0[i].value() )
            {
	      assert(gt[i].towerid()==gt0[i].towerid());
              value = gt[i].value() / gt0[i].value();
            }
          hratio->Fill(value);
	  hratio_yz->Fill(getIz(gt[i]),getIy(gt[i]),value);
        }
    }

  TH1* hasym = getHisto(isector, details, "asym");
  TH2* hasym_yz = static_cast<TH2*>(getHisto(isector, details, "asym_yz"));

  for ( size_t i = 0; i < gt0.size(); ++i )
    {
      if (!reject[i] )
        {
          float value = 0;
          if ( gt[i].value() + gt0[i].value() )
            {
	      assert(gt[i].towerid()==gt0[i].towerid());
              value = (gt[i].value()-gt0[i].value())/(gt[i].value()+gt0[i].value());
            }
          hasym->Fill(value);
	  hasym_yz->Fill(getIz(gt[i]),getIy(gt[i]),value);
        }
    }
}


//_____________________________________________________________________________
bool
emcGainBaseLineCalculator::getBaseLine(emcCalibrationDataHelper* cdh,
                                       int isector,
                                       const char* details,
                                       float& value,
                                       float& error_on_value,
				       float& skewness,
				       float& kurtosis)
{
  value = 0;
  error_on_value = 0;
  skewness = 0;
  kurtosis = 0;

  std::string sname = EmcIndexer::EmcSectorId(isector);
  std::set<int> femsToDiscard;
  float percent;
  int xminxxmax;
  bool zerosuppressed;
  int runnumber;

  fCurrentRunNumber = cdh->runNumber();

  emcFEMList* femlist = cdh->femlist();

  // Decode the details to be used for average gain computation
  std::auto_ptr<BLMethod> method(decodeDetails(details, femsToDiscard, percent,
					       xminxxmax, zerosuppressed, 
					       runnumber));

  if (!method.get())
    {
      std::cerr << __FILE__ << ":" << __LINE__ << " Unknown method of "
		<< " base line computation" << std::endl;
      return false;
    }

  std::vector<emcGainBaseLineCalculator::Tuple> gt; // gains at time t
  std::vector<emcGainBaseLineCalculator::Tuple> gt0; // gains at time0

  bool ok = get(sname, gt, *cdh, *femlist, femsToDiscard, xminxxmax);
  if (!ok) 
    {
      return false;
    }

  if ( runnumber > 0 )
    {
      if ( fRefRunNumber != runnumber )
        {
          delete fCDH0;
          fCDH0 = new emcCalibrationDataHelper(runnumber, false);
          fRefRunNumber = runnumber;
        }
      ok = get(sname, gt0, *fCDH0, *femlist, femsToDiscard, xminxxmax);
      if (!ok)
	{
	  return false;
	}
    }

  std::vector<int> reject;

  // Now that we both have gains at time t and at reference time 0,
  // let's compute the averages and takes the ratio.
  // The whole point here is that you can either take ratio of average or
  // average of ratio...

  method->compute(gt,gt0,reject,value,error_on_value,skewness,kurtosis);

  if ( gt.size() != gt0.size() )
    {
      std::cerr << "emcGainBaseLineCalculator::getBaseLine : Houston, "
		<< "we have a serious problem here : gt.size()="
		<< gt.size() << " whereas gt0.size()="
		<< gt0.size() << " for isector=" << isector
		<< " and details=" << details
		<< std::endl;
      exit(1);
    }

  if (fHistogramming)
    {
      fillHistograms(isector, details, gt, gt0, reject);
    }

  return true;
}

//_____________________________________________________________________________
bool
emcGainBaseLineCalculator::get(const std::string& sname,
			       std::vector<emcGainBaseLineCalculator::Tuple>& values,
			       emcCalibrationDataHelper& cdh,
			       const emcFEMList& femlist,
			       const std::set<int>& femsToDiscard,
			       const int xminxxmax)
{
  values.clear();

  // Only consider sectors for which we got configured.
  if ( femlist.hasSector(sname.c_str()) )
    {
      std::set<int> fems = femlist.fems(sname.c_str());
      std::set<int>::const_iterator it;

      time_t tics = cdh.timeStamp().getTics();

      for ( it = fems.begin(); it != fems.end(); ++it )
	{
	  std::set<int>::const_iterator d = femsToDiscard.find((*it));
	  if ( d != femsToDiscard.end() )
	    {
	      // FEM is one to discard. Skip it.
	      continue;
	    }
	  const emcCalFEM* g = cdh.getCalibration((*it), "Gains");
	  if (!g)
	    {
	      std::cerr << __FILE__ << ":" << __LINE__ << "g=0"
			<< std::endl;
	      return false;
	    }


	  for ( size_t i = 0; i < g->GetNumberOfChannels(); ++i )
	    {
	      float value = 0.0;
	      
	      if ( xminxxmax == -1 )
		{
		  value = g->getValue(i, g->GetXmin());
		}
	      else if ( xminxxmax == 0 )
		{
		  value = g->getValue(i, tics);
		}
	      else if ( xminxxmax == 1)
		{
		  value = g->getValue(i, g->GetXmax());
		}
	      
	      int towerid = EmcIndexer::PXSM144iCH_iPX((*it),i);
	      values.push_back(emcGainBaseLineCalculator::Tuple(values.size(),towerid,value));
	    }
	}
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
void
emcGainBaseLineCalculator::write(bool make_subdirectories)
{
  std::map<std::string, TH1*>::const_iterator it;

  TDirectory* save = gDirectory;

  for ( it = fHistos.begin(); it != fHistos.end(); ++it )
    {
      TH1* h = it->second;
      std::string::size_type pos = it->first.find_first_of('/');
      if ( make_subdirectories )
	{
	  TDirectory* dir =
	    TDirectoryHelper::mkdir(save, it->first.substr(0, pos).c_str());
	  dir->cd();
	}
      h->Write();
    }

  save->cd();
}
