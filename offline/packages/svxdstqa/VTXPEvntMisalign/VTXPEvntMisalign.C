
#include "VTXPEvntMisalign.h"
#include "TFile.h"
#include <TTree.h>
#include <TH1.h>
#include <TF1.h>
#include <TMath.h>
#include <Rtypes.h>
#include <TLatex.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <TLine.h>

#include <iostream>
#include <cmath>

//////////////////////////////////////
// Default constructor
//
VTXPEvntMisalign::VTXPEvntMisalign():
  _tree(NULL),
  _outfilename(NULL),
  _ladder(0),
  _chip(0),
  _nbincorr(50),
  _nrebin(1),
  _sigma(20),
  _spike_threshold(0.5),
  _died_threshold(0.2),
  _threshold_multiplier(5),
  _did_make_corr_histos(false),
  _did_make_DoG_histos(false),
  _did_spike(false),
  _did_die(false),
  _did_set_thresh(false),
  _did_misalign_max(false),
  _sys("AuAu")
{
  // At least make shift zero correlation 
  _corrhists[0] = NULL;
}

///////////////////////////////////////////////////////////
// Standard constructor
//
VTXPEvntMisalign::VTXPEvntMisalign(
        TTree* tree, 
        int ladder, 
        int chip, 
        std::string sys):

  _tree(tree),
  _outfilename(NULL),
  _ladder(ladder),
  _chip(chip),
  _nbincorr(50),
  _nrebin(1),
  _sigma(20),
  _spike_threshold(0.5),
  _died_threshold(0.2),
  _threshold_multiplier(5),
  _did_make_corr_histos(false),
  _did_make_DoG_histos(false),
  _did_spike(false),
  _did_die(false),
  _did_set_thresh(false),
  _did_misalign_max(false),
  _sys(sys)
{
  // At least make shift zero correlation 
  _corrhists[0] = NULL;
}

///////////////////////////////////////////////////////////
// Destructor
//
VTXPEvntMisalign::~VTXPEvntMisalign()
{
  // Delete heap objects
  std::map<int,TH1F*>::iterator it;
  for(it = _corrhists.begin(); it != _corrhists.end(); ++it)
    delete it->second;
  for(it = _DoG_hists.begin(); it != _DoG_hists.end(); ++it)
    delete it->second;
}

///////////////////////////////////////////////////////////
// make_corr_histos()
//
// Reads the TTree into a histogram, then calculates pearsons correlation
// coefficient over the range _nbincorr. This is then stored in new 
// histograms with event offsets, and rebinned by _nrebin. 
//
// If whichsys == "BBC", correlate with the BBCq
// If whichsys == "ARM", correlate with the the sum of the hits in all other 
//   chips in that arm
//
void VTXPEvntMisalign::make_corr_histos(std::string whichsys)
{
  std::cout << std::endl;
  std::cout << "VTXPEvntMisalign::make_corr_histos()" << std::endl;

  if(whichsys.compare("BBC") == 0)
    std::cout << "Correlating with the BBC" << std::endl;
  else if(whichsys.compare("ARM") == 0)
    std::cout << "Correlating with the rest of the VTX ARM" << std::endl;
  else
  {
    std::cout << "Argument not supported!\n" << "Doing nothing" << std::endl;
    return;
  }

  // get things from the tree
  Int_t   event;
  Float_t bbcq;
  UInt_t  trigword;
  UChar_t hits[30][16];

  _tree->SetBranchAddress("event", &event);
  _tree->SetBranchAddress("bbcq", &bbcq);
  _tree->SetBranchAddress("trigword", &trigword);
  _tree->SetBranchAddress("hits", hits);

  Long64_t nentries = _tree->GetEntries();


  // initialize histograms
  //
  std::cout << std::endl;
  std::cout << "--> Initializing histograms" << std::endl;
  // Make sure number of bins is divisible by _nrebin
  Long64_t nbins = ((nentries + _nrebin)/_nrebin)*_nrebin;
  //
  // create the histograms with 1 bin per event (centered at event number)
  TH1I* hbbc = new TH1I("hbbc", ";Event Number;BBC Q",
                  nbins, -0.5, nbins - 0.5);
  TH1I* hchit = new TH1I("hchit", ";Event Number;hits",
                  nbins, -0.5, nbins - 0.5);

  // Histograms to hold the correlation for each shift
  std::map<int,TH1F*>::iterator it;
  for(it = _corrhists.begin(); it != _corrhists.end(); ++it)
  {
    it->second = new TH1F(Form("hcorr_s%d",it->first),
        "",nbins, -0.5, nbins - 0.5);
  }


  // Fill the histograms from the tree. These histos will be used to calculate
  // the correlation coefficient.
  //
  int darm = arm(_ladder);
  
  std::cout << std::endl;
  std::cout << "--> Looping over tree and filling histograms" << std::endl;
  for (int ientry = 0; ientry < nentries; ientry++)
  {
    _tree->GetEntry(ientry);
  
    hchit->SetBinContent(event + 1, hits[_ladder][_chip]);

    if(whichsys.compare("BBC") == 0)
      hbbc->SetBinContent(event + 1, bbcq);
    else if(whichsys.compare("ARM") == 0)
    {
      float avg = 0;
      for (int i = 0; i < 30; i++)
      {
        // skip ladders not in the desired arm
        if (arm(i) != darm) continue;
    
        for (int j = 0; j < 16; j++)
        {
          //skip the chip we're looking at
          if (i == _ladder && j == _chip) continue;
    
          avg += (float)hits[i][j];
          // nchips += 1.;
        }
      }
      hbbc->SetBinContent(event + 1, avg);
    }
  }// inetry


  std::cout << std::endl;
  std::cout << "--> Making correlation coefficient histos" << std::endl;
  // Calculate the Pearson correlation coefficient (r)
  // using a single-pass algorithm
  //
  //                     n*sum(x_i*y_i) - sum(x_i)sum(y_i)
  // r = -----------------------------------------------------------------
  //     sqrt(n*sum(x_i^2)-(sum(x_i)^2)) * sqrt(n*sum(y_i^2)-(sum(y_i)^2))
  //
  // where:
  //      x = bbc charge
  //      y = pixel hits
  //calculate the correlation for each offset value
  float s_x = 0;
  float s_y = 0;
  float s_xy = 0;
  float s_x2 = 0;
  float s_y2 = 0;
  float n = 0;

  for (it = _corrhists.begin(); it != _corrhists.end(); ++it)
  {
      std::cout << "     Shift: " << it->first << std::endl;
    // calculate the correlation
    for (int ix = 1; ix <= nbins; ix++)
    {
      // lower integration limit
      int lx = ix - _nbincorr;
      if (lx <= 0) continue;

      // Calculate the Pearson correlation coefficient (r)
      // See above
      s_x = 0;
      s_y = 0;
      s_xy = 0;
      s_x2 = 0;
      s_y2 = 0;
      n = 0;
      for (int is = lx; is <= ix; is++)
      {
        int x = hbbc->GetBinContent(is);
        int y = hchit->GetBinContent(is + it->first);

        s_x += (float)x;
        s_y += (float)y;
        s_xy += (float)x * (float)y;
        s_x2 += (float)x * (float)x;
        s_y2 += (float)y * (float)y;
        n += 1.0;
      }

      float r_num = n * s_xy - s_x * s_y;
      float r_denom = TMath::Sqrt(n * s_x2 - s_x * s_x);
      r_denom *= TMath::Sqrt(n * s_y2 - s_y * s_y);
      float r = 0;
      if (r_denom != 0)
        r = r_num / r_denom;
      

      (it->second)->SetBinContent(ix, r);
      (it->second)->SetBinError(ix, 0);

    } // ix
    (it->second)->Rebin(_nrebin);
    (it->second)->Scale(1./_nrebin);
  } // map iter

  // delete histograms
  delete hbbc;
  delete hchit;

  _did_make_corr_histos = true;
  return;
}


////////////////////////////////////////////////////////////////////////////
// run_search()
//
// Loops through the correlation histograms and searches for discontinuities.
// To do this, the histograms are convolved with the derivative of a gaussian
// whose distribution will yield spikes at the discontinuities. Then the 
// derivative plots are scanned to look for spikes over a threshold. 
// Information about the discontinuities is stored in _disco_map
//
// It only makes sense to call this method after make_corr_histos() has been
// called
//
bool VTXPEvntMisalign::run_search()
{
  std::cout << std::endl;
  std::cout << "VTXPEvntMisalign::run_search()" << std::endl;

  if(!_did_make_corr_histos)
  {
    std::cout << "    You must run make_corr_histos() before run_search()!!\n"
              << "    Doing nothing" << std::endl;
    return false;
  }

  // Make the derivative of a gaussian histograms
  make_DoG_histos();

  // calculate threshold by fitting gaus to derivative histo

  calculate_thresh();
  
  // Now scan the DoG histograms looking for bins above or below threshold
  // If there is a negative spike coincident with a positive spike somewhere 
  // else, set bool _did_spike to true and fill _disco_map with info.
  //
  // Also, to catch cases which are shifted for the whole run (without a
  // discontinuity) check if the correlation for shift zero has average of
  // zero within RMS AND a different shift has non-zero average
  float nbins = 0;
  float range = 0;
  if(_sys.compare("AuAu") == 0)
  {
    nbins = 500;
    range = 1;
  }
  if(_sys.compare("pAu") == 0)
  {
    nbins = 500;
    range = 0.7;
  }
  if(_sys.compare("pp") == 0)
  {
    nbins = 500;
    range = 0.3;
  }
  // Histos to find average correlation
  std::map<int,TH1F*> corr_proj;
  std::map<int,TH1F*> corr_proj_10pct;
  std::map<int,TH1F*>::iterator cit;
  for(cit = _corrhists.begin(); cit != _corrhists.end(); ++cit)
  {
    corr_proj[cit->first] =
          new TH1F(Form("corr_p_%d",cit->first),"",nbins,(-1)*range,range);
    corr_proj_10pct[cit->first] =
          new TH1F(Form("corr_p_10_%d",cit->first),"",nbins,(-1)*range,range);
  }


  std::cout << std::endl;
  std::cout << "--> Scanning for discontinuities" << std::endl;

  int current_shift = -999;
  _did_spike = false;
  for(int bin = 1; bin <= _DoG_hists[0]->GetNbinsX(); ++bin)
  {
    std::map<int,TH1F*>::iterator it1;
    for(it1 = _DoG_hists.begin(); it1 != _DoG_hists.end(); ++it1)
    {
      //fill histos for means
      corr_proj[it1->first]->Fill(_corrhists[it1->first]->GetBinContent(bin));
      if(bin > 0.9*_DoG_hists[0]->GetNbinsX())
        corr_proj_10pct[it1->first]->Fill(
              _corrhists[it1->first]->GetBinContent(bin));

      // find a negative discontinuity
      if((it1->second)->GetBinContent(bin) < (-1)*_spike_threshold)
      {
        // must be a different discontinuity
        if(it1->first == current_shift)
          continue;

        // then find the corresponding poitive disco in a *different shift
        std::map<int,TH1F*>::iterator it2;
        for(it2 = _DoG_hists.begin(); it2 != _DoG_hists.end(); ++it2)
        {
          // this discontinuity must be in a diferent histo 
          if(it1->first == it2->first)
            continue;

          // if found coincidence
          if((it2->second)->GetBinContent(bin-1) > _spike_threshold ||
             (it2->second)->GetBinContent(bin)   > _spike_threshold ||
             (it2->second)->GetBinContent(bin+1) > _spike_threshold) 
          {
            _did_spike = true;
            // if first discontinuity is from a shift other than 0 set an
            // additional one at zero
            if(_disco_map.size() == 0 && it1->first != 0)
              _disco_map[0] = std::make_pair(0,it1->first);

            int evnt = it1->second->GetXaxis()->GetBinCenter(bin+(int)_sigma);
            if(_disco_map.size() < _misalign_max - 1)
            {
              _disco_map[evnt] = std::make_pair(it1->first,it2->first);
              std::cout << "***" << std::endl;
              std::cout << (it1->second)->GetBinContent(bin) << " < " <<
                (-1)*_spike_threshold << std::endl;
            }
            else
              _did_misalign_max = true;

            current_shift = it1->first;
            break;
          } // if (coincidence)
        }// for it2 over other shifts (find positive  dicontinuity)
        break;
      }// if negative dicontinuity 
    }// for it1 (find negative dicontinuity)
  }// for(int bin..

  
  // Look for cases where the chip died by checking if all correlation histos
  // are zero in the last 10 percent
  _did_die = true;
  for(cit = corr_proj_10pct.begin(); cit != corr_proj_10pct.end(); ++cit)
  {
    float mean = (cit->second)->GetMean();
    float rms = (cit->second)->GetRMS();
    std::cout << "Mean = " << mean << ", RMS = " << rms << std::endl;
    if( rms > _died_threshold )
    {
      std::cout << "RMS >  _died_threshold = "<< _died_threshold <<  std::endl;
      _did_die = false;
    }
    if( (mean + rms) < 0 || (mean - rms) > 0 )
      _did_die = false;
  }
  if(_did_die == true)
  {
    std::cout << std::endl;
    std::cout << "--> Chip Died!!" << std::endl;
    for(cit = corr_proj.begin(); cit != corr_proj.end(); ++cit)
      delete cit->second;
    for(cit = corr_proj_10pct.begin(); cit != corr_proj_10pct.end(); ++cit)
      delete cit->second;

    return false;
  }

  
  if (_did_spike)
  {
    std::cout << std::endl;
    std::cout << "--> Event misalignment:" << std::endl;
  
    std::map< int, std::pair<int,int> >::iterator disc_it;
    for(disc_it = _disco_map.begin(); disc_it != _disco_map.end();  ++disc_it)
    {
      std::cout << "   Event: " << disc_it->first <<
                   ", from: " << (disc_it->second).first <<
                   ", to: " << (disc_it->second).second << 
                   std::endl;
    }
  }
  else
  {
    // Look for cases where it was shifted from the begining (no discontinuity)
    float s0mean = corr_proj[0]->GetMean();
    float s0rms  = corr_proj[0]->GetRMS();
    if( (s0mean + s0rms) > 0 && (s0mean - s0rms) < 0 )
    {
      std::cout <<"Mean0 = "<< s0mean <<", RMS0 = "<< s0rms << std::endl;
      for(cit = corr_proj.begin(); cit != corr_proj.end(); ++cit)
      {
        if(cit->first == 0)
          continue;
        float mean = (cit->second)->GetMean();
        float rms = (cit->second)->GetRMS();
        if( (mean + rms) < 0 || (mean - rms) > 0 )
        {
          std::cout <<"Shift: " << cit->first << std::endl;
          std::cout <<"Mean  = "<< mean <<", RMS  = "<< rms << std::endl;
          std::cout << std::endl;
          std::cout << "--> Event misalignment:" << std::endl;
          std::cout << "   Event: 0" <<
                       ", from: 0" <<
                       ", to: " << cit->first << 
                       std::endl;
          _disco_map[0] = std::make_pair(0,cit->first);
          _did_spike = true;
        }
      }
    }
  }
  for(cit = corr_proj.begin(); cit != corr_proj.end(); ++cit)
    delete cit->second;
  for(cit = corr_proj_10pct.begin(); cit != corr_proj_10pct.end(); ++cit)
    delete cit->second;


  return _did_spike;
}


////////////////////////////////////////////////////////////////////////////
// make_DoG_histos()
//
// Initializes _DoG_hists and fills them with the convolution of _corrhists
// with the derivative of a gaussian. 
//
// This is a helper menthod for run_search() and is thus private
//
void VTXPEvntMisalign::make_DoG_histos()
{
  std::cout << std::endl;
  std::cout << "VTXPEvntMisalign::make_DoG_histos()" << std::endl;

  // Initialize histograms to hold the convolution for each shift and store
  // pointers in _DoG_hists map
  //
  std::cout << "--> Initializing DoG histograms" << std::endl;
  std::map<int,TH1F*>::iterator it;
  for(it = _corrhists.begin(); it != _corrhists.end(); ++it)
  {
    _DoG_hists[it->first] = (TH1F*)(it->second)->Clone(
        Form("h_DoG_s%d",it->first));
    _DoG_hists[it->first]->Reset();
  }
  
  // set some basic variables
  Long64_t nbins = _corrhists[0]->GetNbinsX();
  Long64_t xmax  = nbins * _corrhists[0]->GetBinWidth(1);

	TF1* f_gaus = new TF1("f_gaus","gaus(0)",-xmax,xmax);
	f_gaus->SetParameter(0,1);
	f_gaus->SetParameter(1,0);
	f_gaus->SetParameter(2,_sigma);
	f_gaus->SetParameter(0,1./f_gaus->Integral(-10*_sigma,10*_sigma));
  // derivative of a gaussian function
	TF1* f_dgaus = new TF1(
      "f_dgaus","-[0]*((x-[1])/[2]**2)*exp(-0.5*((x-[1])/[2])**2)",-xmax,xmax);
	f_dgaus->SetParameter(0,1);
	f_dgaus->SetParameter(1,0);
	f_dgaus->SetParameter(2,_sigma);
	f_dgaus->SetParameter(0,1./f_gaus->Integral(-10*_sigma,10*_sigma));
  
	// Convolve _corrhists with derivative of a gaussian to calculate hist 
  // derivative
  std::cout << std::endl;
  std::cout << "--> Convolving with DoG" << std::endl;
	for (int i = 1 + 2*_sigma; i <= nbins - 2*_sigma; ++i)
	{
		for (int j = i - 2*_sigma; j <= i + 2*_sigma; ++j)
		{
      for(it = _corrhists.begin(); it != _corrhists.end(); ++it)
      {
	  		float oldcontent = _DoG_hists[it->first]->GetBinContent(i);
	  		float newcontent = 0;
	  		if(j > 0 && j < nbins+1)
        {
	  			newcontent = f_dgaus->Eval(i-j)*(it->second)->GetBinContent(j);
		  	  _DoG_hists[it->first]->SetBinContent( i, oldcontent + newcontent);
        }
      }
		}
	}
  _did_make_DoG_histos = true;
  
  delete f_gaus;
  delete f_dgaus;

  return;
}


////////////////////////////////////////////////////////////////////////////
// calculate_thresh() 
// 
// Sets the _spike_threshold to 8*sigma of a gaussian fit to a projection
// of the derivative histogram
//
void VTXPEvntMisalign::calculate_thresh()
{
  // if the threshold was set by user, don't calculate
  if(_did_set_thresh)
    return;

  float nbins = 0;
  float range = 0;
  if(_sys.compare("AuAu") == 0)
  {
    nbins = 200;
    range = 0.8;
  }
  if(_sys.compare("pAu") == 0)
  {
    nbins = 200;
    range = 0.2;
  }
  if(_sys.compare("pp") == 0)
  {
    nbins = 100;
    range = 0.1;
  }
  
  TH1F* h = new TH1F("f","Derivative bincontents",nbins,-1*range,range);
  int bins = _DoG_hists[0]->GetNbinsX();

  for(int i = 1; i <= bins; ++i)
    h->Fill(_DoG_hists[0]->GetBinContent(i));

  TF1* f_gaus = new TF1("f_gaus","gaus(0)",-1*range,range);
  f_gaus->SetParameter(0,h->GetMaximum());
  f_gaus->SetParameter(1,0);
  f_gaus->SetParameter(2,h->GetRMS());
  
  h->Fit("f_gaus");
  std::cout << "First fit sigma = " << f_gaus->GetParameter(2) << std::endl;
  h->Fit("f_gaus","","",-2*f_gaus->GetParameter(2),2*f_gaus->GetParameter(2));
  std::cout << "Second fit sigma = " << f_gaus->GetParameter(2) << std::endl;
  
  _spike_threshold = fabs(_threshold_multiplier*f_gaus->GetParameter(2));

  delete f_gaus;
  delete h;

  return;
}


////////////////////////////////////////////////////////////////////////////
// add_shift(int x)  -   Adds a number to the shift list
// returns the number of entries added
//
int VTXPEvntMisalign::add_shift(int x)
{
  if(_corrhists.find(x) == _corrhists.end())
  {
    _corrhists[x] = NULL;
    std::cout << "--> Added shift: " << x << std::endl;
    return 1;
  }
  else
    std::cout << "--> Shift already added to list: " << x << std::endl;
  return 0;
}

////////////////////////////////////////////////////////////////////////////
// add_shift(int* x, unsigned int n)  
//
// Adds an array of length n to the shift list
// returns the number of entries added
//
int VTXPEvntMisalign::add_shift(int* x, unsigned int n)
{
  int count = 0;
  for(unsigned int i=0; i<n; ++i)
  {
    if(_corrhists.find(x[i]) == _corrhists.end())
    {
      _corrhists[x[i]] = NULL;
      std::cout << "--> Added shift: " << x[i] << std::endl;
      count++;
    }
    else
      std::cout << "--> Shift already added to list: " << x[i] << std::endl;
  }
  return count;
}


///////////////////////////////////////////////////////////////////////////
// remove shift  -  removes shift from the shift list
//
int VTXPEvntMisalign::remove_shift(int x)
{

  // Delete heap objects
  std::map<int,TH1F*>::iterator it;
  it = _corrhists.find(x);
  if(it != _corrhists.end())
  {
    delete it->second;
    _corrhists.erase(it);
    std::cout <<"--> Removed shift " << x << " from the list" << std::endl;
    return 1;
  }
  else
    std::cout <<"--> Can't remove shift " << x << ", not in list" << std::endl;
  return 0;
}


////////////////////////////////////////////////////////////////////////////
// print_image - Makes a canvas and plots the correlation 
// Writes the plot as a .png to outfileimage
//
// If draw_DoG == true, the DoG histograms are plotted instead
//
void VTXPEvntMisalign::print_image(const char* outfileimage = "test.png", 
                                   bool draw_DoG = false)
{
  std::cout << std::endl;
  std::cout << "VTXPEvntMisalign::print_image()" << std::endl;

  gStyle->SetOptStat(0);

  const unsigned int nshifts = _corrhists.size();

  std::cout << std::endl;
  std::cout << "--> Plotting: draw_DoG = " << draw_DoG << std::endl;

  TLatex le;
  le.SetNDC();
  le.SetTextSize(0.15);

  TLatex lt;
  lt.SetNDC();
  lt.SetTextSize(0.35);
  lt.SetTextAlign(22);

  //
  // Make and format the canvas
  TCanvas *cchip = new TCanvas("cchip", "chip", 900, 750);
  cchip->SetTopMargin(0.0);
  cchip->SetRightMargin(0.0);
  cchip->SetBottomMargin(0.0);
  cchip->SetLeftMargin(0.0);

  //
  // Make and format the pads
  TPad* pads[nshifts + 2];
  for (unsigned int ipad = 0; ipad < nshifts + 2; ipad++)
  {
    float h   = 0.8/nshifts;
    float yhi = 0;
    float ylo = 0;

    if(ipad == 0)
    {
      yhi = 1.0;
      ylo = 0.9;
    }
    else if(ipad == nshifts + 1)
    {
      yhi = 0.1;
      ylo = 0.0;
    }
    else
    {
      yhi = 0.9 - (ipad-1)*h;
      ylo = 0.9 - (ipad)*h;
    }

    pads[ipad] = new TPad(Form("pad_%d",ipad),"", 0, ylo, 1, yhi);
    pads[ipad]->SetTopMargin(0);
    pads[ipad]->SetRightMargin(0.05);
    pads[ipad]->SetBottomMargin(0);
    pads[ipad]->SetLeftMargin(0.13);
    pads[ipad]->SetTicks(1, 1);
  }

  //title
  cchip->cd();
  pads[0]->Draw();
  pads[0]->cd();
  if(_did_spike)
    pads[0]->SetFillColor(kRed - 3);
  if(_did_misalign_max)
    pads[0]->SetFillColor(kGreen + 2);
  if(_did_die)
    pads[0]->SetFillColor(kBlue - 3);
  int lay = _ladder < 10 ? 0 : 1;
  int lad = _ladder < 10 ? _ladder : _ladder - 10;
  int sen = _chip / 4;
  int chi = _chip % 4;
  if(draw_DoG)
    lt.DrawLatex(0.5, 0.5,
               Form("DERIVATIVE - B%iL%iS%iC%i",
                    lay, lad, sen, chi));
  else
    lt.DrawLatex(0.5, 0.5,
               Form("CORRELATION - B%iL%iS%iC%i",
                    lay, lad, sen, chi));
  cchip->cd();
  pads[nshifts + 1]->Draw();
  pads[nshifts + 1]->cd();
  lt.DrawLatex(0.8, 0.5,"EVENT NUMBER");
  cchip->cd();

  // These lines will mark the discontinuities on the plots 
  // make a line for every one
  // 
  // first make some colors
  int colors[] = {2,8,4,6,7};
  int colorcount = 0;
  std::map<int,TLine*> lines;
  std::map< int, std::pair<int,int> >::iterator disc_it;
  for(disc_it = _disco_map.begin(); disc_it != _disco_map.end();  ++disc_it)
  {
    lines[disc_it->first] = new TLine(disc_it->first,0,disc_it->first,1);
    lines[disc_it->first]->SetLineStyle(4);
    lines[disc_it->first]->SetLineWidth(3);
    lines[disc_it->first]->SetLineColor( colors[colorcount % 5] );
    formatline( lines[disc_it->first], draw_DoG);
    ++colorcount;
  }

  // iterate over the histo map (choose the correct one)
  unsigned int count = 1;
  std::map<int,TH1F*>::iterator it;
  std::map<int,TH1F*>::iterator it_b;
  std::map<int,TH1F*>::iterator it_e;
  if(draw_DoG)
  {
    it_b = _DoG_hists.begin();
    it_e = _DoG_hists.end();
  }
  else
  {
    it_b = _corrhists.begin();
    it_e = _corrhists.end();
  }
  for(it = it_b; it != it_e; ++it)
  {
    pads[count]->Draw();
    pads[count]->cd();

    formathist( (it->second), draw_DoG);

    (it->second)->GetYaxis()->SetNdivisions(405);
    (it->second)->GetYaxis()->SetLabelSize(0.08);
    (it->second)->GetXaxis()->SetLabelSize(0.1);
    (it->second)->Draw("hist");

    // if there was a discontinuity, draw a line on the correct histogram and
    // mark the pad red
    if(_did_spike)
    {
      std::map< int, std::pair<int,int> >::iterator disc_it;
      for(disc_it = _disco_map.begin();
          disc_it != _disco_map.end();  ++disc_it)
      {
        if(it->first == (disc_it->second).first ||
           it->first == (disc_it->second).second)
          lines[disc_it->first]->Draw();
      }
    }

    le.DrawLatex(0.01, 0.5, Form("SHIFT: %i", it->first));
    cchip->cd();
    ++count;
  }

  std::cout << "--> Saving image as: " << outfileimage << std::endl;
  std::cout << std::endl;
  cchip->Print(outfileimage);

  // cleanup  
  for (unsigned int ipad = 0; ipad < nshifts + 2; ipad++)
    delete pads[ipad];

  std::map<int,TLine*>::iterator lit;
  for(lit = lines.begin(); lit != lines.end();  ++lit)
    delete lit->second;

  delete cchip;

  return;
}

////////////////////////////////////////////////////////////////////////////
// print_image - Makes a canvas and plots the correlation 
// Writes the plot as a .png to outfileimage
//
// If draw_DoG == true, the DoG histograms are plotted instead
//
void VTXPEvntMisalign::save_histos(const char* outname = "test.root", 
                                   bool draw_DoG = false)
{
  std::cout << std::endl;
  std::cout << "VTXPEvntMisalign::save_histos()" << std::endl;

  TFile* outfile = new TFile(outname,"RECREATE");

  // iterate over the histo map (choose the correct one)
  std::map<int,TH1F*>::iterator it;
  std::map<int,TH1F*>::iterator it_b;
  std::map<int,TH1F*>::iterator it_e;
  if(draw_DoG)
  {
    it_b = _DoG_hists.begin();
    it_e = _DoG_hists.end();
  }
  else
  {
    it_b = _corrhists.begin();
    it_e = _corrhists.end();
  }
  for(it = it_b; it != it_e; ++it)
  {      
      (it->second)->Write();
  }

  std::cout << "--> Saving histograms to root file: " << outname << std::endl;
  std::cout << std::endl;

  outfile->Close();

  // cleanup  
  delete outfile;

  return;
}


////////////////////////////////////////////////////////////////////////////
// Helper functions to format the histograms and lines based on species and
// whether or not the plots are of the derivatives
//
void VTXPEvntMisalign::formathist(TH1F* h, bool draw_DoG)
{
  if(draw_DoG)
  {
    if(_sys.compare("AuAu") == 0)
      h->GetYaxis()->SetRangeUser(-1.01,1);
    else if(_sys.compare("pAu") == 0)
      h->GetYaxis()->SetRangeUser(-0.301,0.3);
    else if(_sys.compare("pp") == 0)
      h->GetYaxis()->SetRangeUser(-0.101,0.1);
    h->SetLineColor(1);
  }
  else
  {
    if(_sys.compare("AuAu") == 0)
      h->GetYaxis()->SetRangeUser(0.001,1);
    else if(_sys.compare("pAu") == 0)
      h->GetYaxis()->SetRangeUser(0.001,0.5);
    else if(_sys.compare("pp") == 0)
      h->GetYaxis()->SetRangeUser(0.001,0.2);
    h->SetLineColor(1);
  }
  return;
}

void VTXPEvntMisalign::formatline(TLine* line, bool draw_DoG)
{
  if(draw_DoG)
  {
    if(_sys.compare("AuAu") == 0)
    {
      line->SetY1(-1.101);
      line->SetY2(1);
    }
    else if(_sys.compare("pAu") == 0)
    {
      line->SetY1(-0.301);
      line->SetY2(0.3);
    }
    else if(_sys.compare("pp") == 0)
    {
      line->SetY1(-0.101);
      line->SetY2(0.1);
    }
  }
  else
  {
    if(_sys.compare("AuAu") == 0)
    {
      line->SetY1(0.001);
      line->SetY2(1);
    }
    else if(_sys.compare("pAu") == 0)
    {
      line->SetY1(0.001);
      line->SetY2(0.5);
    }
    else if(_sys.compare("pp") == 0)
    {
      line->SetY1(0.001);
      line->SetY2(0.2);
    }
  }
  return;
}

 

////////////////////////////////////////////////////////////////////////////
//
// Return the arm in which the ladder is located
//
// Output:
//  -1: Invalid input
//   0: East
//   1: West
//
// Input:
//   ladder: ladder number [0-29] (B0:0-9, B1: 10-29)
//
////////////////////////////////////////////////////////////////////////////
int VTXPEvntMisalign::arm(int ladder)
{
  // check input
  if (ladder < 0 || ladder > 29)
  {
    std::cout << "ERROR!! arm(" << ladder << ") "
         << "valid ladder [0, 29]!" << std::endl;
    return -1;
  }

  // E arm
  // B0: 0-4, B1: 0-9
  if (ladder < 5 || (ladder > 9 && ladder < 19))
    return 0;

  // W arm
  // B0: 5-9, B1: 10-19
  if ( (ladder > 4 && ladder < 10) ||
       (ladder > 19 && ladder < 29) )
    return 1;

  return -1;

}


void VTXPEvntMisalign::get_misalignment_array(int array[_misalign_max][3])
{
  if(_did_misalign_max)
  {
    std::cout << "There were more than the allowed number of misalignments!!" 
      << std::endl;
    std::cout << "This chip is trash" << std::endl;
  }

  int i = 0;
  std::map< int, std::pair<int,int> >::iterator disc_it;
  for(disc_it = _disco_map.begin(); disc_it != _disco_map.end();  ++disc_it)
  {
    array[i][0] = disc_it->first;
    array[i][1] = (disc_it->second).first;
    array[i][2] = (disc_it->second).second;
    ++i;
  }
  return;
}

