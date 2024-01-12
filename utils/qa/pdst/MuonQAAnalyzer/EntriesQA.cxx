// STL Includes
#include<utility>
#include<iterator>

// BOOST Includes

// ROOT Includes
#include<TH1.h>

// Package Includes
#include<HistoQA.h>
#include<UtilityQA.h>
#include<MuonQAAnalyzer.h>
#include<EntriesQA.h>

void
EntriesQA::process(TH1* histo)
{    
  _curRun = MuonQAAnalyzer::getRunActiveExt();
  if ( _curRun == _lastRun ) // Entry will exist
    _lastIt->second += histo->GetEntries();
  else // Insert a new entry
    _lastIt = addOrIncrement(obsValByRun, 
                             _curRun,
                             histo->GetEntries());            
}

void
EntriesQA::createSummary()
{
  if ( _outputHasBeenWritten ) return;
  std::string fname = _name + std::string(".root");
  std::string hname = _parentHisto->getName() + "_" + _name;

  int nbins = obsValByRun.rbegin()->first - obsValByRun.begin()->first + 1;
  
  _summary = new TH1D(hname.c_str(),_name.c_str(),nbins,
                      obsValByRun.begin()->first,
                      obsValByRun.rbegin()->first);
    
  normByEvents(_parentHisto,obsValByRun);

  int normFactor = ( _sumFlag & MuonQA::AVERAGE ) ? _parentHisto->getNHistos() : 1;

  std::cout << "EntriesQA: Normalizing " << _name << " of "
            <<_parentHisto->getName() << " by "
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
