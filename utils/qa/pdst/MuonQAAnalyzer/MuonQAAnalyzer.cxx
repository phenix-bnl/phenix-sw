// STL Includes
#include<iostream>
#include<vector>

// Boost Includes
#include<boost/bind.hpp>
#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#endif

//  Root Includes

// Package Includes
#include<MuonQAAnalyzer.h>
#include<SubsystemQA.h>

using boost::bind;

MuonQAAnalyzer::MuonQAAnalyzer() :
    _subsystems()
{
}

MuonQAAnalyzer::~MuonQAAnalyzer()
{
}

MuonQAAnalyzer* 
MuonQAAnalyzer::getInstance()
{
  static MuonQAAnalyzer qa;
  return &qa;
}

#ifndef __CINT__
void 
MuonQAAnalyzer::addSubsystem(SubsystemQA* subsys)
{
  addSubsystem(SubsysPtr(subsys));
}
#endif

void 
MuonQAAnalyzer::addSubsystem(SubsysPtr subsys)
{
  _subsystems.insert(std::make_pair(subsys->getName(),
                                    subsys));
}

void
MuonQAAnalyzer::processQA()
{  
 std::for_each(_subsystems.begin(),_subsystems.end(),
               bind(&SubsystemQA::buildFileList,
                    bind(&MVT::second,
                         _1)));
  
 std::for_each(_subsystems.begin(),_subsystems.end(),
               bind(&SubsystemQA::processHistograms,
                    bind(&MVT::second,
                         _1)));
}

void
MuonQAAnalyzer::outputGoodRunList()
{
 std::for_each(_subsystems.begin(),_subsystems.end(),
               bind(&SubsystemQA::outputGoodRunList,
                    bind(&MVT::second,
                         _1)));  
}

void 
MuonQAAnalyzer::setActive(SubsysPtr ptr)
{
  MuonQAAnalyzer* qa = getInstance();
  qa->_setActive(ptr);
}

void 
MuonQAAnalyzer::_setActive(SubsysPtr ptr)
{
  _activeSubsys = ptr;
}

int 
MuonQAAnalyzer::getRun(std::string subsysName) const
{
  SubsysMap::const_iterator it = _subsystems.find(subsysName);
  if ( it != _subsystems.end() )
    return it->second->getRun();
  else 
  return -1;
}

int 
MuonQAAnalyzer::getRunActive() const
{
  return _activeSubsys->getRun();          
}

int
MuonQAAnalyzer::getRunActiveExt()
{
  MuonQAAnalyzer* qa = getInstance();
  return qa->getRunActive();
}

int
MuonQAAnalyzer::getRunExt(std::string subsysName)
{
  MuonQAAnalyzer* qa = getInstance();
  return qa->getRun(subsysName);
}

int 
MuonQAAnalyzer::getSegment(std::string subsysName) const
{
  SubsysMap::const_iterator it = _subsystems.find(subsysName);
  if ( it != _subsystems.end() )
    return it->second->getRun();
  else 
    return -1;
}

int 
MuonQAAnalyzer::getSegmentExt(std::string subsysName) 
{
  MuonQAAnalyzer* qa = getInstance();
  return qa->getSegment(subsysName);
}
