#include <PHPyquen.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHNodeIterator.h>
#include <PHPythiaHeaderV2.h>
#include <PHPythiaContainerV2.h>
#include <TPythia6.h>
#include <TFile.h>
#include <TTree.h>
#include <TClonesArray.h>
#include <TMCParticle.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>

#define pytune pytune_
extern "C" {
  void pyquen_(double*,int*,double*);
  int  pytune(int *itune);
  void phpyqueninit_(double* T0, double* tau0, int* nf, int* ienglu, int* ianglu, int* noquen);
}

typedef PHIODataNode<PHObject> PHObjectNode_t;

PHPyquen::PHPyquen(const std::string& name) : SubsysReco(name) {
  _pythia = TPythia6::Instance();
  _seed = 1234567890;
  _frame = "CMS";
  _projectile = "p";
  _target = "p";
  _roots = 200.;
  _eventcounter = 0;

  _configFile = "pyquen.cfg";
  _A = 197.;
  _bimpact = 0.0;
  _ifb = 0;
  _T0 = 1.0;
  _tau0 = 0.1;
  _nf = 0;
  _ienglu = 0;
  _ianglu = 0;
  _noquen = 0; //if = 1, no quenching is performed

  _phpythiaheader = 0;
  _phpythia = 0;

  _xsectfile = 0;
  _tp = 0;
}

PHPyquen::~PHPyquen() {
  delete _xsectfile;
}

int PHPyquen::Init(PHCompositeNode *topNode){

  //-* set up the node tree with the header and the particles
  int ret = CreateNodeTree(topNode);
  if(ret!=EVENT_OK){
    std::cout<< PHWHERE << "Problems creating the nodes on the node tree" << std::endl;
      return ABORTEVENT;
  }

  //-* set the seed
  _pythia->SetMRPY(1,_seed);
  _pythia->SetMRPY(2,0);

  //-* create the normalization tree
  TDirectory *orig_dir = gDirectory;
  _xsectfile = new TFile("phpy_xsec.root","RECREATE");  
  // Set up the normalization TTree
  _tp = new TTree("Tp","Pythia Normalization Tree");
  _tp->Branch("isub",&_tp_isub,"isub/i");
  _tp->Branch("nevt",&_tp_nevt,"nevt/i");
  _tp->Branch("sigma",&_tp_sigma,"sigma/D");
  _tp->Branch("nevt_sigma",&_tp_nevt_sigma,"nevt_sigma/D");
  _tp->Branch("integlumi",&_tp_integlumi,"integlumi/D");
  orig_dir->cd();

  //-* initialize pythia
  ReadConfig();

  //-* print out the configuration of pythia
  _pythia->Pystat(3);
  _pythia->Pystat(4);
  _pythia->Pystat(5);
  _pythia->Pystat(6);

  return EVENT_OK;
}

//__________________________________________________________
void PHPyquen::ReadConfig()
{

  std::cout << "PHPyquen::ReadConfig - Reading " << _configFile << std::endl;
  
  std::ifstream infile( _configFile.c_str() ); 
  if (infile.fail ()){
    std::cout << "PHPyquen::ReadConfig - Failed to open file " << _configFile << std::endl;
    exit(2);
  }

  // initialize variables

  std::string FullLine;      // a complete line in the config file
  std::string label;         // the label

  int index = 999999;
  int ivalue = 999999;
  double value = 1e9;

  // get one line first 
  std::getline(infile, FullLine);
  while ( !infile.eof() )
  {
    
    // skip lines that begin with #, or "//"
    if ( FullLine[0]=='#' || FullLine.substr(0, 2) == "//" )
    {
      std::getline(infile,FullLine);
      continue;
    }
    
    // make FullLine an istringstream
    std::istringstream line( FullLine.c_str() );
    
    // get label
    line >> label;
    
    // to lower case
    std::transform(label.begin(), label.end(), label.begin(), (int(*)(int)) std::tolower);
    
    // based on label, fill correct item
    if ( label == "a" )
    {
      line >> _A;
      std::cout << "A\t" << _A << std::endl;
    }
    else if ( label == "bimpact" )
    {
      line >> _bimpact;
      std::cout << "bimpact\t" << _bimpact << std::endl;
    }
    else if ( label == "minbias" )
    {
      line >> _ifb;
      std::cout << "minbias\t" << _ifb << std::endl;
    }
    // the rest is for pythia
    else if ( label == "roots" )
    {
      line >> _roots;
      std::cout << "roots\t" << _roots << std::endl;
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
    else if ( label == "msub" )
    {
      line >> index >> ivalue;
      _pythia->SetMSUB(index,ivalue);
      std::cout << "msub\t" << index << " " << ivalue << std::endl;
    }
    else if ( label == "mstp" )
    {
      line >> index >> ivalue;
      _pythia->SetMSTP(index,ivalue);
      std::cout << "mstp\t" << index << " " << ivalue << std::endl;
    }
    else if ( label == "mstj" )
    {
      line >> index >> ivalue;
      _pythia->SetMSTJ(index,ivalue);
      std::cout << "mstj\t" << index << " " << ivalue << std::endl;
    }
    else if ( label == "mstu" )
    {
      line >> index >> ivalue;
      _pythia->SetMSTU(index,ivalue);
      std::cout << "mstu\t" << index << " " << ivalue << std::endl;
    }
    else if ( label == "ckin" )
    {
      line >> index >> value;
      _pythia->SetCKIN(index,value);
      std::cout << "ckin\t" << index << " " << value << std::endl;
    }
    else if ( label == "parp" )
    {
      line >> index >> value;
      _pythia->SetPARP(index,value);
      std::cout << "parp\t" << index << " " << value << std::endl;
    }
    else if ( label == "parj" )
    {
      line >> index >> value;
      _pythia->SetPARJ(index,value);
      std::cout << "parj\t" << index << " " << value << std::endl;
    }
    else if ( label == "paru" )
    {
      line >> index >> value;
      _pythia->SetPARU(index,value);
      std::cout << "paru\t" << index << " " << value << std::endl;
    }
    else if ( label == "parf" )
    {
      line >> index >> value;
      _pythia->SetPARF(index,value);
      std::cout << "parf\t" << index << " " << value << std::endl;
    }
    else if ( label == "pytune" )
    {
      line >> ivalue;
      pytune(&ivalue);
      std::cout << "pytune\t" << ivalue << std::endl;
    }
    else if ( label == "imss" )
    {
      line >> index >> ivalue;
      _pythia->SetIMSS(index,ivalue);
      std::cout << "imss\t" << index << " " << ivalue << std::endl;
    }
    else if ( label == "rmss" )
    {
      line >> index >> value;
      _pythia->SetRMSS(index,value);
      std::cout << "rmss\t" << index << " " << value << std::endl;
    }
    else if ( label == "mdcy" )
    {
      int kc = 0;          // particle kc code
      line >> kc >> index >> ivalue;
      
      // if (index==1) turn on/off decay channel idc
      _pythia->SetMDCY(kc,index,ivalue);
      std::cout << "mdcy\t" << kc << " " << index << " " << ivalue << std::endl;
    }
    else if ( label == "mdme" )
    {
      int idc = 0;          // decay channel
      line >> idc >> index >> ivalue;
      
      // if (index==1) turn on/off decay channel idc
      _pythia->SetMDME(idc,index,ivalue);
      std::cout << "mdme\t" << idc << " " << index << " " << ivalue << std::endl;
    }
    else if ( label == "kfin" )
    {
      int i;	// 1=proj side, 2=target
      int j;	// flavor code (KF)
      line >> i >> j >> value;
      _pythia->SetKFIN(i,j,ivalue);
      std::cout << "kfin\t" << i << " " << j << " " << value << std::endl;
    }
    else if ( label == "kfpr")
    {
      int idc = 0;
      line >> index >> ivalue >> idc;
      _pythia->GetPyint2()->KFPR[ivalue-1][index-1] = idc;
      std::cout << "kfpr\t" << index << " " << ivalue << " " << idc << std::endl;
    }
    else if ( label == "pmas" )
    {
      int quark = 0;        // quark type
      line >> quark >> index >> value;
      
      // Pycomp converts KF to KC (different
      // particle indexing understood by pythia)
      _pythia->SetPMAS(_pythia->Pycomp(quark),index,value);
      std::cout << "pmas\t" << quark << " " << index << " " << value << std::endl;
    }
    else if ( label == "brat" )
    {
      line >> index >> value;
      _pythia->SetBRAT(index,ivalue);
      std::cout << "brat\t" << index << " " << value << std::endl;
    }
    else if ( label == "t0" )
    {
      line >> value;
      _T0 = value;
      std::cout<<"T0 = "<<_T0<<std::endl;
    }
    else if ( label == "tau0" )
    {
      line >> value;
      _tau0 = value;
      std::cout<<"tau0 = "<<_tau0<<std::endl;
    }
    else if ( label == "nf" )
    {
      line >> ivalue;
      _nf = ivalue;
      std::cout<<"nf = "<<_nf<<std::endl;
    }
    else if ( label == "ienglu" )
    {
      line >> ivalue;
      _ienglu = ivalue;
      std::cout<<"ienglu = "<<_ienglu<<std::endl;
    }
    else if ( label == "ianglu" )
    {
      line >> ivalue;
      _ianglu = ivalue;
      std::cout<<"ianglu = "<<_ianglu<<std::endl;
    }
    else if ( label == "noquen" )
    {
      line >> ivalue;
      _noquen = ivalue;
      std::cout<<"noquen = "<<_noquen<<std::endl;
    }
    else
    {
      // label was not understood
      std::cout << "************************************************************" << std::endl;
      std::cout << "PHPyquen::ReadConfig(), ERROR this option is not supported: " << FullLine << std::endl;
      std::cout << "Please email chiu@bnl.gov to correct this" << std::endl;
      std::cout << "or just add it yourself to " << PHWHERE << std::endl;
      std::cout << "************************************************************" << std::endl;
    }
    
    // get next line in file
    getline( infile, FullLine );
  }

  infile.close();

  //initialize pythia
  _pythia->Initialize(_frame.c_str(), _projectile.c_str(), _target.c_str(), _roots);

  //call the function that will get this into the fortran code
  phpyqueninit_(&_T0,&_tau0,&_nf,&_ienglu,&_ianglu,&_noquen);

}

int PHPyquen::process_event(PHCompositeNode *topNode){
  ++_eventcounter;

  //-* generate a pythia event without fragmentation
  _pythia->SetMSTJ(1,0);
  _pythia->Pyevnt();

  //-* quench the partons and generate radiated gluons
  pyquen_(&_A, &_ifb, &_bimpact);

  //-* now do the fragmenation and hadronization
  _pythia->SetMSTJ(1,1);
  _pythia->Pyexec();

  //-* fill the particles
  TClonesArray *particles = (TClonesArray*)_pythia->ImportParticles();
  int npart = particles->GetLast()+1;
  for(int ipart=0; ipart<npart; ipart++){
    TMCParticle *particle = (TMCParticle*)particles->At(ipart);
    _phpythia->addParticle(*particle);
  }

  //-* fill the event header
  _phpythiaheader->SetEvt(_eventcounter);	// Event number
  _phpythiaheader->SetNpart(npart);	        // Number of particle entries in entire history
  _phpythiaheader->SetProcessid(_pythia->GetMSTI(1));	// Process ID
  _phpythiaheader->SetParId_1(_pythia->GetMSTI(15));	// KF codes for partons participating in hard scattering
  _phpythiaheader->SetParId_2(_pythia->GetMSTI(16));	// KF codes for partons participating in hard scattering
  _phpythiaheader->SetX1(_pythia->GetPARI(33));	// Bjorken x1,x2
  _phpythiaheader->SetX2(_pythia->GetPARI(34));
  _phpythiaheader->SetSvar(_pythia->GetPARI(14));	// partonic s,t,u
  _phpythiaheader->SetTvar(_pythia->GetPARI(15));
  _phpythiaheader->SetUvar(_pythia->GetPARI(16));
  _phpythiaheader->SetQsqr(_pythia->GetPARI(22));	// Q squared
  _phpythiaheader->SetPt(_pythia->GetPARI(17));	// transverse momentum

  // primary vertex information. Assume position is at zero by default
  _phpythiaheader->SetPrimaryVertexX(0);
  _phpythiaheader->SetPrimaryVertexY(0);
  _phpythiaheader->SetPrimaryVertexZ(0);

  // rapidity of parton parton com frame
  _phpythiaheader->SetYcom(_pythia->GetPARI(37));	

  if ( _eventcounter<10 ) {
    std::cout << " **************************" << std::endl; 
    std::cout << " ****** event = " << _eventcounter   << std::endl;
    std::cout << " **************************" << std::endl;
    // print out particle listing information if desired (this shows decay modes and code numbers)
    _pythia->Pylist(1);   // list full pythia generated event
  }

  return EVENT_OK;
}

int PHPyquen::End(PHCompositeNode *topNode){
  //-* dump out closing info (cross-sections, etc)
  _pythia->Pystat(1);

  // Write out the normalization TTree, which has the x-sections, etc
  Pyint5_t *pyint5 = _pythia->GetPyint5();

  for (int i=0; i<500; i++) {
    if ( (i!=0) && (_pythia->GetMSUB(i) != 1) ) continue;

    _tp_isub = i;
    _tp_proc = _pythia->GetPROC(i);
    _tp_nevt = pyint5->NGEN[2][i];
    _tp_sigma = pyint5->XSEC[2][i];
    _tp_nevt_sigma = _tp_nevt*_tp_sigma;
    if ( _tp_sigma!=0. ){
      _tp_integlumi = 1e-9*_tp_nevt/_tp_sigma; // in pb^-1
    } else {
      _tp_integlumi = 0.;
    }
    _tp->Fill();
  }

  _xsectfile->Write();
  _xsectfile->Close();

  return EVENT_OK;
}

int PHPyquen::CreateNodeTree(PHCompositeNode *topNode){
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    std::cout << PHWHERE << "DST Node missing doing nothing" << std::endl;
    return ABORTEVENT;
  }

  //-* pythia header information
  _phpythiaheader = new PHPythiaHeaderV2();
  PHObjectNode_t *PHPythiaHeaderNode = new PHObjectNode_t(_phpythiaheader, "PHPythiaHeader", "PHObject");
  dstNode->addNode(PHPythiaHeaderNode);

  //-* pythia particle information
  _phpythia = new PHPythiaContainerV2();
  PHObjectNode_t *PHPythiaNode = new PHObjectNode_t(_phpythia, "PHPythia", "PHObject");
  dstNode->addNode(PHPythiaNode);

  return EVENT_OK;
}
