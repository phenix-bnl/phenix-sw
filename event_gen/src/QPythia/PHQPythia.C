#include <PHQPythia.h>
#include <PHPythiaHeaderV2.h>
#include <PHPythiaContainerV2.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>
#include <Fun4AllReturnCodes.h>
#include <TQPythia6.h>
#include <TRandom3.h>
#include <TMCParticle.h>
#include <TClonesArray.h>
#include <fstream>
#include <sstream>
#include <algorithm>

#define pytune pytune_
extern "C" int  pytune(int *itune);

typedef PHIODataNode<PHObject> PHObjectNode_t;

PHQPythia::PHQPythia(unsigned int seed, const std::string& configFile, const std::string& name) 
  : SubsysReco(name), _configFile(configFile), _header(0), 
    _particleContainer(0){

  if(seed==0){
    TRandom dice(0);
    _seed = dice.GetSeed();
  } else {
    _seed = seed;
  }

  _pythia = new TQPythia6();

}

PHQPythia::~PHQPythia() {
  delete _pythia;
}

int PHQPythia::Init(PHCompositeNode *topNode){

  int ret = CreateNodeTree(topNode);
  if(ret!=EVENT_OK){
    return ret;
  }

  TQPythia6::DoQPYINIT();

  ReadConfig();

  PrintConfig();

  return EVENT_OK;
}

int PHQPythia::process_event(PHCompositeNode *topNode){
  static int nevts = 0;

  _pythia->GenerateEvent();

  TClonesArray *particleArray = (TClonesArray *)_pythia->ImportParticles();
  
  // number of generated particles
  Int_t numParticles = particleArray->GetLast() + 1;	
  for (Int_t ipart=0; ipart<numParticles; ipart++) { 
    // get the particle information
    TMCParticle *particle = (TMCParticle *)particleArray->At(ipart);	
    _particleContainer->addParticle(*particle);
  }
  
  _header->SetEvt(nevts);		// Event number
  _header->SetNpart(numParticles);	// Number of particle entries in entire history
  _header->SetProcessid(_pythia->GetMSTI(1));	// Process ID
  _header->SetParId_1(_pythia->GetMSTI(15));	// KF codes for partons participating in hard scattering
  _header->SetParId_2(_pythia->GetMSTI(16));	// KF codes for partons participating in hard scattering
  _header->SetX1(_pythia->GetPARI(33));		// Bjorken x1,x2
  _header->SetX2(_pythia->GetPARI(34));
  _header->SetSvar(_pythia->GetPARI(14));	// partonic s,t,u
  _header->SetTvar(_pythia->GetPARI(15));
  _header->SetUvar(_pythia->GetPARI(16));
  _header->SetQsqr(_pythia->GetPARI(22));	// Q squared
  _header->SetPt(_pythia->GetPARI(17));		// transverse momentum

  // primary vertex information. Assume position is at zero by default
  _header->SetPrimaryVertexX(0);
  _header->SetPrimaryVertexY(0);
  _header->SetPrimaryVertexZ(0);

  // rapidity of parton parton com frame
  _header->SetYcom(_pythia->GetPARI(37));	

  if(nevts<10){
    std::cout << " **************************" << std::endl; 
    std::cout << " ****** event = " << nevts   << std::endl;
    std::cout << " **************************" << std::endl;
    // print out particle listing information if desired (this shows decay modes and code numbers)
    _pythia->Pylist(1);   // list full pythia generated event
  }
  nevts++;

  return EVENT_OK;
}

int PHQPythia::ResetEvent(PHCompositeNode *topNode){
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd(Name()))
    {
      mainIter.forEach(reset);
    }

  return EVENT_OK;
}

int PHQPythia::End(PHCompositeNode *topNode){
  //-* dump out closing info (cross-sections, etc)
  _pythia->Pystat(1);

  return EVENT_OK;
}

int PHQPythia::CreateNodeTree(PHCompositeNode *topNode){
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    std::cout << PHWHERE << "DST Node missing doing nothing" << std::endl;
    return ABORTEVENT;
  }

  //-* pythia header information
  _header = new PHPythiaHeaderV2();
  PHObjectNode_t *PHPythiaHeaderNode = new PHObjectNode_t(_header, "PHPythiaHeader", "PHObject");
  dstNode->addNode(PHPythiaHeaderNode);

  //-* pythia particle information
  _particleContainer = new PHPythiaContainerV2();
  PHObjectNode_t *PHPythiaNode = new PHObjectNode_t(_particleContainer, "PHPythia", "PHObject");
  dstNode->addNode(PHPythiaNode);

  return EVENT_OK;
}

void PHQPythia::ReadConfig() {
  std::cout << "PHQPythia::ReadConfig - Reading " << _configFile << std::endl;
  
  std::ifstream infile( _configFile.c_str() ); 
  if (infile.fail()) {
    std::cout << "PHQPythia::ReadConfig - Failed to open file " << _configFile << std::endl;
    return;
  }

  // initialize variables
  Float_t _roots(0); 
  std::string  _proj;
  std::string  _targ;
  std::string  _frame;

  std::string FullLine;      // a complete line in the config file
  std::string label;         // the label

  int index = 999999;
  int ivalue = 999999;
  double value = 1e9;

  // get one line first 
  getline(infile, FullLine);
  while ( !infile.eof() )
  {
    
    // skip lines that begin with #, or "//"
    if ( FullLine[0]=='#' || FullLine.substr(0, 2) == "//" )
    {
      getline(infile,FullLine);
      continue;
    }
    
    // make FullLine an istringstream
    std::istringstream line( FullLine.c_str() );
    
    // get label
    line >> label;
    
    // to lower case
    std::transform(label.begin(), label.end(), label.begin(), (int(*)(int)) std::tolower);
    
    // based on label, fill correct item
    if ( label == "roots" )
    {
      line >> _roots;
      std::cout << "roots\t" << _roots << std::endl;
    }
    else if ( label == "proj" )
    {
      line >> _proj;
      std::cout << "proj\t" << _proj << std::endl;
    }
    else if ( label == "targ" )
    {
      line >> _targ;
      std::cout << "targ\t" << _targ << std::endl;
    }
    else if ( label == "frame" )
    {
      line >> _frame;
      std::cout << "frame\t" << _frame << std::endl;
    }
    else if ( label == "msel" )
    {
      line >> ivalue;
      _pythia->SetMSEL(ivalue);
      std::cout << "msel\t" << ivalue << std::endl;
    }
    else if ( label == "ckin" )
    {
      line >> index >> value;
      _pythia->SetCKIN(index,value);
      std::cout << "ckin\t" << index << " " << value << std::endl;
    }
    else if ( label == "qhat" )
    {
      line >> value;
      _pythia->SetQHat(value);
      std::cout << "qhat\t" << value << std::endl;
    }
    else if ( label == "pathlength" )
    {
      line >> value;
      _pythia->SetPathLength(value);
      std::cout << "pathlength\t" << value << std::endl;
    }
    else
    {
      // label was not understood
      std::cout << "************************************************************" << std::endl;
      std::cout << "PHQPythia::ReadConfig(), ERROR this option is not supported: " << FullLine << std::endl;
      std::cout << "Please email chiu@bnl.gov to correct this" << std::endl;
      std::cout << "or just add it yourself to " << PHWHERE << std::endl;
      std::cout << "************************************************************" << std::endl;
    }
    
    // get next line in file
    getline( infile, FullLine );
  }

  _pythia->SetSeed(_seed);
  _pythia->Initialize(_frame.c_str(), _proj.c_str(), _targ.c_str(), _roots);
  
  infile.close();
  
}

void PHQPythia::PrintConfig() const {
  _pythia->Pystat(3);
  _pythia->Pystat(4);
  _pythia->Pystat(5);
  _pythia->Pystat(6);
  
  std::cout << "Using seed " << _seed << std::endl;
}
