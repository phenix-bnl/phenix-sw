#ifndef PHHEPMCHELPER_h
#define PHHEPMCHELPER_h

#include <fstream>
#include <string>
#include <SubsysReco.h>

class PHCompositeNode;
class PHHepMCGenEvent;

namespace HepMC
{
  class GenEvent;
  class IO_GenEvent;
  class IO_AsciiParticles;
};

//! shifts particles vertex based on values read from a file
class PHHepMCHelper: public SubsysReco
{
  
  public:
  
  //! constructor
  PHHepMCHelper( const std::string& name = "PHHepMCHelper");
  
  //! destructor
  virtual ~PHHepMCHelper( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  
  //! full initialization
  int Init(PHCompositeNode *topNode);
    
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! end-of-job method
  int End(PHCompositeNode *topNode);
  //@}

  void SetVerbosity(int v = 1)
  { _verbosity = v; }

  void WriteEventsToText(const std::string& outName = "HepMCEvents.txt");
  void WriteHREventsToText(const std::string& outName = "HepMCEvents_HR.txt");

  void SetNodeName(std::string s){_node_name = s;}

  void PrintEventInfo(){_printInfo = true;}


  private:
  

  int _verbosity;
  bool _writeEvent;
  bool _writeReadableEvent;
  bool _printInfo;

  std::string _outputFileName;
  std::string _outputHRFileName;

  
  HepMC::GenEvent *_theHepMCEvt;
  HepMC::IO_GenEvent *_asciiOut;  
  HepMC::IO_AsciiParticles *_asciiReadableOut;  

  HepMC::GenEvent *hepmcevt;

  PHHepMCGenEvent *phhepmcevt;
  int eventCounter;
  double toMM;
  
  std::string _node_name;

};

#endif
