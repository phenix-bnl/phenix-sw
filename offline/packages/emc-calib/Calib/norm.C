#include "emcCalibrationDataHelper.h"
#include "emcTimeStamp.h"
#include "emcDataManager.h"
#include <vector>
#include "EmcIndexer.h"
#include "emcCalFEM.h"
#include <cassert>
#include <cstdlib>
#include <sstream>
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"

// Get the normalization (gains) values distribution and rms
// for PbSc.

//_____________________________________________________________________________
int trim(const std::vector<float>& xx, const float& alpha,
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

  std::vector<float> x;

  for ( size_t i = 0; i < xx.size(); ++i )
    {
      if ( xx[i] > 0 ) 
	{
	  x.push_back(xx[i]);
	}
    }

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
void outliers(const std::vector<float>& v, 
	      float mean, float sigma,
	      size_t& under, size_t& over)
{
  under = over = 0;
  
  for ( size_t i = 0; i < v.size(); ++i )
    {
      if ( v[i] < mean-sigma )
	{
	  ++under;
	}
      if (  v[i] > mean+sigma )
	{
	  ++over;
	}
    }
}

//_____________________________________________________________________________
class Sector 
{
 public:

  Sector(int number)
  {    
    static const size_t ntowers = 2592;
    fNumber = number;
    fGt.resize(ntowers);
    fGt0.resize(ntowers);
    fAsym_gt_gt0.resize(ntowers);
    fRatio_gt_gt0.resize(ntowers);
 
    fHgt = 0;
    fHgt0 = 0;
    fHratio = 0;
    fHasym = 0;

    fHgt_yz = 0;
    fHgt0_yz = 0;
    fHratio_yz = 0;
    fHasym_yz = 0;
  }

  void set(int ist, float gt, float gt0)
  {
    fGt[ist] = gt;
    fGt0[ist] = gt0;
    fRatio_gt_gt0[ist] = 0;
    if ( gt0 > 0 ) 
      {
	fRatio_gt_gt0[ist] = gt/gt0;
      }
    fAsym_gt_gt0[ist] = (gt-gt0)/(gt+gt0);
  }

  float gt(int ist) { return fGt[ist]; }
  float gt0(int ist) { return fGt0[ist] ; }
  float ratio(int ist) { return fRatio_gt_gt0[ist]; }
  float asym(int ist) { return fAsym_gt_gt0[ist]; }

  TH1* hgt() { histos(); return fHgt; }
  TH1* hgt0() { histos(); return fHgt0; }
  TH1* hratio() { histos(); return fHratio; }
  TH1* hasym() { histos(); return fHasym; }

  TH2* hgt_yz() { histos(); return fHgt_yz; }
  TH2* hgt0_yz() { histos(); return fHgt0_yz; }
  TH2* hratio_yz() { histos(); return fHratio_yz; }
  TH2* hasym_yz() { histos(); return fHasym_yz; }

  void print(std::ostream& out = std::cout) const 
  {
    float tmean,wmean;
    float tvar,wvar;
    int k;
    std::vector<float> sorted;
    float percent = 0.0;
    trim(fGt,percent,tmean,wmean,tvar,wvar,k,sorted);
    out << "<gain(t)> = " << tmean << " +- " << sqrt(tvar) << std::endl;
    float tmean0,tvar0;
    trim(fGt0,percent,tmean0,wmean,tvar0,wvar,k,sorted);
    out << "<gain(t0)> = " << tmean0 << " +- " << sqrt(tvar0) << std::endl;
    out << "<gain(t)>/<gain(t0)> = " << tmean/tmean0 << " +- " << sqrt(tvar0*tvar0+tvar*tvar) << std::endl;
    trim(fRatio_gt_gt0,percent,tmean,wmean,tvar,wvar,k,sorted);
    out << "<gain(t)/gain(t0)> = " << tmean << " +- " << sqrt(tvar)
	<< std::endl;
  }

  void write(TFile& f)
  {
    histos();
    TDirectory* dir = gDirectory;
    f.cd();
    hgt()->Write(); 
    hgt0()->Write();
    hratio()->Write();
    hasym()->Write();
    hgt_yz()->Write(); 
    hgt0_yz()->Write();
    hratio_yz()->Write();
    hasym_yz()->Write();
    dir->cd();
  }

 private:

  void histos() 
  {
    if ( !fHgt ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_gt";
	fHgt = new TH1F(name.str().c_str(),name.str().c_str(),
			1000,0,500);
      }
    else
      {
	fHgt->Reset();
      }

    for ( size_t i = 0; i < fGt.size(); ++i )
      {
	fHgt->Fill(fGt[i]);
      }

    if ( !fHgt0 ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_gt0";
	fHgt0 = new TH1F(name.str().c_str(),name.str().c_str(),
			1000,0,500);
      }
    else
      {
	fHgt0->Reset();
      }

    for ( size_t i = 0; i < fGt0.size(); ++i )
      {
	fHgt0->Fill(fGt0[i]);
      }

    if ( !fHratio ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_ratio";
	fHratio = new TH1F(name.str().c_str(),name.str().c_str(),
			   1000,0,3);
      }
    else
      {
	fHratio->Reset();
      }

    for ( size_t i = 0; i < fRatio_gt_gt0.size(); ++i )
      {
	fHratio->Fill(fRatio_gt_gt0[i]);
      }

    if ( !fHasym ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_asym";
	fHasym = new TH1F(name.str().c_str(),name.str().c_str(),
			   1000,-3,3);
      }
    else
      {
	fHasym->Reset();
      }

    for ( size_t i = 0; i < fAsym_gt_gt0.size(); ++i )
      {
	fHasym->Fill(fAsym_gt_gt0[i]);
      }

    // Now for the 2Ds

    assert(fGt.size()==2592);
    assert(fGt0.size()==2592);
    assert(fRatio_gt_gt0.size()==2592);
    assert(fAsym_gt_gt0.size()==2592);

    if ( !fHgt_yz ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_gt_yz";
	fHgt_yz = new TH2F(name.str().c_str(),name.str().c_str(),
			   72,-0.5,71.5,36,-0.5,35.5);
      }
    else
      {
	fHgt_yz->Reset();
      }

    for ( size_t i = 0; i < fGt.size(); ++i ) 
      {
	int is,iz,iy;
	EmcIndexer::decodeTowerId(i,is,iz,iy);
	fHgt_yz->Fill(iz,iy,fGt[i]);
      }

    if ( !fHgt0_yz ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_gt0_yz";
	fHgt0_yz = new TH2F(name.str().c_str(),name.str().c_str(),
			   72,-0.5,71.5,36,-0.5,35.5);
      }
    else
      {
	fHgt0_yz->Reset();
      }

    for ( size_t i = 0; i < fGt0.size(); ++i ) 
      {
	int is,iz,iy;
	EmcIndexer::decodeTowerId(i,is,iz,iy);
	fHgt0_yz->Fill(iz,iy,fGt0[i]);
      }   

    if ( !fHratio_yz ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_ratio_yz";
	fHratio_yz = new TH2F(name.str().c_str(),name.str().c_str(),
			   72,-0.5,71.5,36,-0.5,35.5);
      }
    else
      {
	fHratio_yz->Reset();
      }

    for ( size_t i = 0; i < fRatio_gt_gt0.size(); ++i ) 
      {
	int is,iz,iy;
	EmcIndexer::decodeTowerId(i,is,iz,iy);
	fHratio_yz->Fill(iz,iy,fRatio_gt_gt0[i]);
      }

   if ( !fHasym_yz ) 
      {
	std::ostringstream name;
	name << EmcIndexer::EmcSectorId(fNumber) << "_asym_yz";
	fHasym_yz = new TH2F(name.str().c_str(),name.str().c_str(),
			   72,-0.5,71.5,36,-0.5,35.5);
      }
    else
      {
	fHasym_yz->Reset();
      }

    for ( size_t i = 0; i < fAsym_gt_gt0.size(); ++i ) 
      {
	int is,iz,iy;
	EmcIndexer::decodeTowerId(i,is,iz,iy);
	fHasym_yz->Fill(iz,iy,fAsym_gt_gt0[i]);
      }
  }

 private:

  int fNumber;
  std::vector<float> fGt; // g(t)
  std::vector<float> fGt0; // g(t0)
  std::vector<float> fRatio_gt_gt0; // g(t)/g(t0)
  std::vector<float> fAsym_gt_gt0; // (g(t)-g(t0))(g(t)+g(t0))
  TH1* fHgt;
  TH1* fHgt0;
  TH1* fHratio;
  TH1* fHasym;

  TH2* fHgt_yz;
  TH2* fHgt0_yz;
  TH2* fHratio_yz;
  TH2* fHasym_yz;
};

//_____________________________________________________________________________
void norm(int runnumber, const char* outfile)
{
  emcCalibrationDataHelper cdh_t(runnumber, 
				 false, 
				 emcManageable::kDB_Pg, 
				 "pbsc");

  static const int runnumber_t0 = 92446;

  emcCalibrationDataHelper cdh_t0(runnumber_t0,
				  false,
				  emcManageable::kDB_Pg,
				 "pbsc");
  
  PHTimeStamp ts = cdh_t.timeStamp();

  for ( size_t is = 0; is < 6; ++is ) 
    {
      cdh_t0.getGainBaseLine(is);
    }

  std::vector<Sector> sectors;

  for ( size_t is = 0; is < 6; ++is )
    {
      sectors.push_back(Sector(is));
      cdh_t.getGainBaseLine(is);
    }

  for ( size_t ifem = 0; ifem < 108; ++ifem )
    {
      const emcCalFEM* g_t = cdh_t.getCalibration(ifem, "Gains");
      const emcCalFEM* g_t0 = cdh_t0.getCalibration(ifem,"Gains");

      for ( size_t channel = 0; channel < 144; ++channel )
        {
          float norm_t = g_t->getValue(channel,ts.getTics());
          float norm_t0 = g_t0->getValue(channel,
					 g_t0->GetXmax());
	  int towerid = EmcIndexer::PXSM144iCH_iPX(ifem,channel);
	  int is,ist;
	  EmcIndexer::iPXiSiST(towerid,is,ist);
	  sectors[is].set(ist,norm_t,norm_t0);
	}
    }

  for ( size_t is = 0; is < sectors.size(); ++is ) 
    {
      std::cout << "Sector " << EmcIndexer::EmcSectorId(is) << std::endl;
      sectors[is].print();
    }

  TFile fout(outfile,"RECREATE");

  for ( size_t is = 0; is < sectors.size(); ++is ) 
    {
      sectors[is].write(fout);
    }
  fout.Close();

}
