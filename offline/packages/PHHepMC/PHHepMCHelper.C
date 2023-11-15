#include <iostream>
#include <sstream>
#include <string>

#include <PHCompositeNode.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <PHDataNode.h>
#include <PHObject.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>


#include "HepMC/GenEvent.h"

#include "PHHepMCHelper.h"
#include "PHHepMCGenEvent.h"
#include "HepMC/IO_GenEvent.h"
#include "HepMC/IO_AsciiParticles.h"
#include "HepMC/GenEvent.h"
#include "HepMC/GenVertex.h"
#include "HepMC/GenParticle.h"


using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

//____________________________________________________________________________________________
PHHepMCHelper::PHHepMCHelper(const string &name): SubsysReco(name)
{
  _verbosity = 1;
  _writeEvent = false;
  _writeReadableEvent = false;
  _printInfo = false;
  hepmcevt = NULL;
  eventCounter = 0;
  toMM = 1e-12;
  _node_name = "PHHepMCGenEvent";
  //_asciiOut = new HepMC::IO_GenEvent(_outputFileName.c_str(),std::ios::out);
  //_asciiReadableOut = new HepMC::IO_AsciiParticles(_outputFileName.c_str(),std::ios::out);
}

//____________________________________________________________________________________________
int PHHepMCHelper::Init( PHCompositeNode* top_node )
{


  if(_outputFileName == _outputHRFileName && _writeEvent && _writeReadableEvent)
    {
      cout << "PHHepMCHelper::PHHepMCHelper - The output file name for HR and HepMC formats are the same!" << endl;
      return ABORTRUN;
    }


    
  return EVENT_OK;
}

//____________________________________________________________________________________________
int PHHepMCHelper::End( PHCompositeNode* )
{ 
  if(_asciiOut) delete _asciiOut;

  return EVENT_OK;
}

  
//____________________________________________________________________________________________
int PHHepMCHelper::process_event( PHCompositeNode* top_node )
{

  PHHepMCGenEvent* hepmcEvent = findNode::getClass<PHHepMCGenEvent>(top_node,_node_name.c_str());
  if (!hepmcEvent)
    {
      cout << "PHHepMCHelper::process_event - unable to get PHHepMCGenEvent, is Node missing?" << endl;
      return ABORTRUN;
    }
  _theHepMCEvt = hepmcEvent->getEvent();

  if(_printInfo)
    {
      cout << _node_name << " - Q: " << _theHepMCEvt->pdf_info()->scalePDF() << endl;
    }
  if(_writeEvent || _writeReadableEvent)
    {
      if(_verbosity > 10) hepmcEvent->PrintEvent();
      if(_verbosity > 1) cout << "PHHepMCHelper::process_event - writing event to " << _outputFileName << endl;
      if(_writeEvent) _asciiOut->write_event(_theHepMCEvt);
      if(_writeReadableEvent) _asciiReadableOut->write_event(_theHepMCEvt);
    }

  return EVENT_OK;  
}


  void PHHepMCHelper::WriteEventsToText(const std::string& outName)
{
  _outputFileName = outName;
  delete _asciiOut;
  _asciiOut = new HepMC::IO_GenEvent(_outputFileName.c_str(),std::ios::out);
  _writeEvent = true;
}

void PHHepMCHelper::WriteHREventsToText(const std::string& outName)
{
  _outputHRFileName = outName;
  delete _asciiReadableOut;
  _asciiReadableOut = new HepMC::IO_AsciiParticles(_outputHRFileName.c_str(),std::ios::out);
  _writeReadableEvent = true;
}

