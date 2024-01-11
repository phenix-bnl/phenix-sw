#include <vector>
#include <iostream>
#include <iomanip>
#include "emcDataManager.h"
#include "emcCalFEMFactory.h"
#include "emcTracedFEM.h"
#include "PHTimeStamp.h"

/** Helper macro to ease spotting problems in 
 *  long term evolution of PbSc gains.
 *
 *  Before using this one, you must use emcChannelEvolution class to produce
 *   a directory with ASCII files containing the gains for the (long) period
 *   you want to consider.
 *
 * Then you do compile it under ROOT 
 * 
 * a) check that you have a rootlogon.C containing :
 * {
 *     gSystem->SetIncludePath("-Iyour_install_zone_if_needed/include -I${OFFLINE_MAIN}/include");
 * }
 * b) root[0] gSystem->Load("libemcCalibGFX.so");
 *    root[1] gSystem->Load("libemcOMascii.so");
 *    root[2] .L LongTermGains.C++
 *    root[3] LongTermGain(ifem,varthreshold,inputdir)
 *  where ifem is the absolute FEM number you're interested in (from 0 to 107)
 *  and inputdir the directory where the ASCII Gain files are located (w/o the
 *  Gains suffix).
 *  
 *  This will show you the trimmed mean number of items (aka "piece of line")
 *  per fem channel, together with the variance of that. 
 *  For good looking channel, the variance should be close to zero.
 *  If the variance is above a given threshold, a dump of the actual number
 *  of items per channel will be output so you can spot the guiltly guys,
 *  and then you might use emcChannelEvolution again to plot those.
 */

//_____________________________________________________________________________
int trim(const std::vector<float>& x, const float& alpha,
         float& tmean, float& wmean,
         float& tvar, float& wvar,
         int& k,
         std::vector<float>& sx)
{
  //
  // Calculates the trimmed (tmean) and Winsorized (wmean) means
  // of a sample (x) and estimates the variances of the two means.
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
  k = static_cast<int>(floorf(alpha * n));

  assert(n - k - 1 >= 0);

  float sum = 0.0;

  for ( size_t i = k; i < n - k; ++i )
    {
      sum += sx[i];
    }

  tmean = sum / (n - 2 * k);
  wmean = (sum + k * sx[k] + k * sx[n - k - 1]) / n;

  float t2 = 0.0;
  float w2 = 0.0;

  for ( size_t i = k; i < n - k; ++i )
    {
      t2 += (sx[i] - tmean) * (sx[i] - tmean);
      w2 += (sx[i] - wmean) * (sx[i] - wmean);
    }

  tvar = (
	  t2 +
	  k * (sx[k] - tmean) * (sx[k] - tmean) +
	  k * (sx[n - k - 1] - tmean) * (sx[n - k - 1] - tmean)
	  ) / (n * n);

  wvar = (
	  w2 +
	  k * (sx[k] - wmean) * (sx[k] - wmean) +
	  k * (sx[n - k - 1] - wmean) * (sx[n - k - 1] - wmean)
	  ) / (n * n);

  return 0;
}

//_____________________________________________________________________________
void inspect(const emcTracedFEM& g, float tmean, float tvar)
{
  //
  // Output the number of items per channel
  // Channel with a number of items > tmean + 6*sigma
  // are indicated with "**" preceding the number of items.
  //
  std::string flag;

  float sigma = sqrt(tvar);

  for ( size_t i = 0; i < g.GetNumberOfChannels(); ++i ) 
    {
      if ( g.GetNumberOfItems(i)*1.0 > tmean+6*sigma )
	{
	  flag ="**";
	}
      else
	{
	  flag ="  ";
	}

      std::cout << "CH" << std::setw(3) << i << " "
		<< flag << std::setw(5)
		<< g.GetNumberOfItems(i)
		<< " ";
      if ( (i+1) % 5 == 0 )
	{
	  std::cout << std::endl;
	}
    } 
  std::cout << std::endl;
}

//_____________________________________________________________________________
void LongTermGains(int ifem=65, 
		   const float varthreshold=3,
		   const char* inputdir="/phenix/u/aphecetc/Run4Gains")
{
  emcDataManager* dm = emcDataManager::GetInstance();
  dm->SetSourceDir(inputdir);

  emcTracedFEM* g = (emcTracedFEM*)emcCalFEMFactory::Create("Gains", ifem);
  g->SetSource(emcManageable::kFile_ASCII);

  PHTimeStamp tdummy;

  bool ok = dm->Read(*g, tdummy);
  if (!ok)
    {
      std::cerr << "Could not read gains for fem=" << ifem
		<< std::endl;
      return;
    }

  std::vector<float> nitems;

  for ( size_t i = 0; i < g->GetNumberOfChannels(); ++i )
    {
      float f = g->GetNumberOfItems(i)*1.0;
      nitems.push_back(f);
    }

  std::vector<float> sx;
  float tmean, wmean;
  float tvar, wvar;
  int k;
  bool dump = false;
  const float alphamax=0.3;

  for ( float alpha = 0.0; alpha < alphamax; alpha+=0.1 )
    {
      trim(nitems,alpha,tmean,wmean,tvar,wvar,k,sx);

      printf("alpha=%7.2f tmean=%7.2f wmean=%7.2f tvar=%7.2f wvar=%7.2f k=%d\n",
	     alpha,tmean,wmean,tvar,wvar,k);

      if ( !alpha && tvar > varthreshold )
	{
	  dump = true;
	}
    }
	
  if ( dump )
    {
      inspect(*g,tmean,tvar);
    }
  else
    {
      std::cout << "FEM looks OK at this varthreshold value" 
		<< std::endl;
    }
}
