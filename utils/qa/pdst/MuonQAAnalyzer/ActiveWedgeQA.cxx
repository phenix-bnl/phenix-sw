// STL Includes
#include<stdexcept>
#include<iostream>

// ROOT Includes
#include<TH1.h>
#include<TH2.h>
#include<TH3.h>

// Package Includes
#include<ActiveWedgeQA.h>
#include<UtilityQA.h>
#include<HistoQA.h>
#include<MuonQAAnalyzer.h>

void
ActiveWedgeQA::process(TH1* histo)
{    
  // This puts 2*sector+column on the x axis, station on the y axis
  TH1* histo_proj = static_cast<TH3*>(histo)->Project3D("zy");
  int curRun = MuonQAAnalyzer::getRunActiveExt();    

  ValMap::iterator it;
    
  if ( curRun == _lastRun )
    it = _lastIt;
  else
    it = obsValByRun.find(curRun);
  // If not then insert it and grab the iterator

  if ( it == obsValByRun.end() )
    it = obsValByRun.insert(std::make_pair(curRun,TupleSet())).first;

  _lastIt = it;

  // Grab a reference to the set of tuples
  TupleSet& ps = it->second;    
    
  if ( histo_proj->GetMinimum() > 0 ) return; // Skip if no empty bins

  std::string name = histo_proj->GetName();   
    
  int arm = 0, cage = 0;
    
  if ( name.rfind("0_0") != std::string::npos )
  {
    arm = 0; cage = 0;
  }
  else if ( name.rfind("0_1") != std::string::npos )
  {
    arm = 0; cage = 1;
  }
  else if ( name.rfind("1_0") != std::string::npos )
  {
    arm = 1; cage = 0;
  }
  else if ( name.rfind("1_1") != std::string::npos )
  {
    arm = 1; cage = 1;
  }
  else
    throw std::logic_error("Couldn't determine arm/cage");
 
  for (int i = 1; i <= histo_proj->GetNbinsX(); i+=2)
    for (int j = 1; j <= histo_proj->GetNbinsY(); j++)        
      if ( histo_proj->GetBinContent(i,j) == 0 &&
           histo_proj->GetBinContent(i+1,j) == 0 ) 
        ps.insert(boost::make_tuple(arm,cage,i,j));
}

void
ActiveWedgeQA::createSummary()
{    
  if ( _outputHasBeenWritten ) return;
  std::string fname = _name + std::string(".root");
  std::string hname = _parentHisto->getName() + "_" + _name;

  int nbins = obsValByRun.rbegin()->first - obsValByRun.begin()->first + 1;

  _summary = new TH1I(hname.c_str(),_name.c_str(),nbins,
                        obsValByRun.begin()->first,
                        obsValByRun.rbegin()->first);

  int normFactor = ( _sumFlag & MuonQA::AVERAGE ) ? _parentHisto->getNHistos() : 1;

  std::cout << "ActiveWedgeQA: Normalizing " << _name << " of " <<_parentHisto->getName() << " by "
            << normFactor << std::endl;  
  
  std::for_each(obsValByRun.begin(),
                obsValByRun.end(),
                bind(AutoSetBinContent<int>(),
                     _summary,
                     bind(&MVT::first,_1),
                     bind(&TupleSet::size,
                          bind(&MVT::second,_1)
                          ),
                     normFactor
                     )
                );

  _summary->Write();
  _outputHasBeenWritten = true;
}

