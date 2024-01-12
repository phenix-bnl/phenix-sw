// ROOT Includes
#include<TH1.h>
#include<TH2.h>
#include<TSpectrum.h>

// Package Includes
#include<PeakQA.h>
#include<HistoQA.h>
#include<UtilityQA.h>
#include<MuonQAAnalyzer.h> 

void
PeakQA::process(TH1* histo)
{    
  _curRun = MuonQAAnalyzer::getRunActiveExt();
       
  TH1* htmp = 0;
  if ( _axis == 1 )
    htmp = static_cast<TH2*>(histo)->ProjectionX();
  else if ( _axis == 2 )
    htmp = static_cast<TH2*>(histo)->ProjectionY();

  boost::shared_ptr<TSpectrum> s = boost::make_shared<TSpectrum>();
  int npeaks = s->Search(htmp, 3, "goff", 0.75);
    
  double peakVal = (npeaks) ? s->GetPositionX()[0] : 0;
    
  if ( _curRun == _lastRun ) // Entry will exist
    _lastIt->second += peakVal;
  else // Insert a new entry            
    _lastIt = addOrIncrement(obsValByRun, 
                             _curRun,
                             peakVal);            
}

void
PeakQA::createSummary()
{
  if ( _outputHasBeenWritten ) return;
  std::string fname = _name + std::string(".root");
  std::string hname = _parentHisto->getName() + "_" + _name;

  int nbins = obsValByRun.rbegin()->first - obsValByRun.begin()->first + 1;
    
  _summary = new TH1D(hname.c_str(),_name.c_str(),nbins,
                      obsValByRun.begin()->first,
                      obsValByRun.rbegin()->first);
  
  normBySegments(_parentHisto, obsValByRun);

  int normFactor = ( _sumFlag & MuonQA::AVERAGE ) ? _parentHisto->getNHistos() : 1;

  std::cout << "PeakQA: Normalizing " << _name << " of " <<_parentHisto->getName() << " by "
            << normFactor << std::endl;
  
  std::for_each(obsValByRun.begin(),
                obsValByRun.end(),
                bind(AutoSetBinContent<double>(),
                     _summary,
                     bind(&MVT::first,_1),
                     bind(&MVT::second,_1),
                     normFactor
                     )
                );

  _summary->Write();
  _outputHasBeenWritten = true; 
}
