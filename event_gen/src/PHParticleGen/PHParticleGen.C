#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <sys/time.h>

//#include <TPythia6.h>
#include <TROOT.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif


#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <PHTimeStamp.h>
#include <Fun4AllReturnCodes.h>

#include <PHParticleGen.h>
#include <PHPythiaHeaderV2.h>
#include <PHPythiaContainerV2.h>

#include <TClonesArray.h>

#include <TSingleParticleGenerator.h>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

PHParticleGen::PHParticleGen(const std::string &name): SubsysReco(name)
{
  _generator = new TSingleParticleGenerator();
  phpythia = 0;		// array of pythia particles
  phpythiaheader = 0;	// pythia header
  eventcount = 0;	// event numbering will start from 1

  seed = 0;
  seedflag = 0;		// 0=use random seed, otherwise, use explicit seed
}

PHParticleGen::~PHParticleGen()
{
  delete _generator;
}

void PHParticleGen::SetGenerator(TSingleParticleGenerator *spg)
{
  if ( _generator != 0 ) delete _generator;
  _generator = spg;
}

int PHParticleGen::Init(PHCompositeNode *topNode)
{
  ReadConfig();

  CreateNodeTree(topNode);

  eventcount = 0;	// event numbering will start from 1

  // set the random seed by getting the time of day, to the microsecond
  if ( seedflag == 0 )
    {
      // first try getting seed from /dev/random
      ifstream devrandom;
      devrandom.open("/dev/random",ios::binary);
      devrandom.read((char*)&seed,sizeof(seed));

      // check that devrandom worked
      if ( !devrandom.fail() )
        {
          cout << PHWHERE << " Got seed from /dev/random" << endl;
          seed = abs(seed)%900000000;
        }
      else
        {
          // /dev/random failed, get the random seed from the time of day, to the microsecond
          cout << PHWHERE << " Getting seed from gettimeofday()" << endl;
          timeval xtime;
          int status = gettimeofday(&xtime,NULL);
          if ( status==0 )
            {
              seed = ((xtime.tv_sec << 12) + (xtime.tv_usec&0xfff))%900000000;
            }
          else
            {
              cout << PHWHERE << " something wrong with gettimeofday()" << endl;
            }
        }
      devrandom.close();
    }

  // Set the seed, either from explicit set or random
  _generator->SetParameter("seed",(double)seed);

  PrintConfig();

  // If the oscar output name is set, open up the OSCAR1999A file
  if ( oscar_fname.size() > 0 )
    {
      oscar_file.open("pythia.osc");
      oscar_file << "# OSC1999A" << endl;
      oscar_file << "# final_id_p_x" << endl;
      oscar_file << "# Hijing 1.383" << endl;
      oscar_file << "# " << endl;
    }

  return EVENT_OK;
}

int PHParticleGen::End(PHCompositeNode *topNode)
{
  //-* dump out closing info (cross-sections, etc)
/*
  cout << "Number Triggered Thrown "
       << numtriggered << "\t" << numberEvents << endl;

  cout << "Number MPC CENT BOTH "
       << num_mpctriggers << "\t" << num_centtriggers << "\t" << num_bothtriggers << endl;
*/

  if ( oscar_file.is_open() )
    {
      oscar_file.close();
    }

  return EVENT_OK;
}

int PHParticleGen::ReadConfig(const char *cfg_file)
{
  cout << "PHParticleGen::ReadConfig(), Reading " << cfg_file << endl;

  ifstream infile(cfg_file); 
  if (infile.fail ())
    {
      cout << "PHParticleGen::ReadConfig(), Failed to open file "
           << cfg_file << endl;
      return 0;
    }

  Int_t   _nevents = 0;

  string FullLine;      // a complete line in the config file
  string label;         // the label

  int index = 999999;
  //int ivalue = 999999;
  double value = 1e9;

  // get one line first 
  getline(infile, FullLine);
  while ( !infile.eof() )
    {
      // skip lines that begin with #
      if ( FullLine[0]=='#' )
        {
          getline(infile,FullLine);
          continue;
        }

      // make FullLine an istringstream
      istringstream line( FullLine.c_str() );

      // get label
      line >> label;

      // based on label, fill correct item
      if ( label == "nevents" )
        {
          line >> _nevents;
          cout << "nevents\t" << _nevents << endl;
        }
      else if ( label == "brat" )
        {
          line >> index >> value;
          //tpythia6->SetBRAT(index,ivalue);
          cout << "brat\t" << index << " " << value << endl;
        }
      else
        {
          // label was not understood
          cout << "************************************************************" << endl;
          cout << "PHParticleGen::ReadConfig(), ERROR this option is not supported: " << FullLine << endl;
          cout << "Please email chiu@bnl.gov to correct this" << endl;
          cout << "or just add it yourself to " << PHWHERE << endl;
          cout << "************************************************************" << endl;
        }

      // get next line in file
      getline( infile, FullLine );
    }

  infile.close();

  return _nevents;
}

//-* print pythia config info
void PHParticleGen::PrintConfig() const
{
  cout << "Using seed " << seed << endl;
}

int PHParticleGen::process_event(PHCompositeNode *topNode)
{
  if ( eventcount%10000 == 0 ) cout << __PRETTY_FUNCTION__ << ": Event count = " << eventcount << endl;
  ++eventcount;

  _generator->GenerateEvent();

  TClonesArray* particleArray = (TClonesArray *)_generator->ImportParticles();

  Int_t numParticles = particleArray->GetLast() + 1;	// number of generated particles

  int nstable = 0;

  if ( eventcount<10 )
    {
      cout << " **************************" << endl; 
      cout << " ****** event = " << eventcount   << endl;
      cout << " **************************" << endl;
    }

  for (Int_t ipart=0; ipart<numParticles; ipart++)
    { 
      TMCParticle* particle = (TMCParticle *)particleArray->At(ipart);	// get the particle information
      phpythia->addParticle(*particle);

      if ( phpythia->isStable(ipart) ) ++nstable;

      if ( eventcount<10 )
	{
	  std::cout << std::setw(10) << particle->GetName() << ": ";
	  particle->ls("");
          float px = particle->GetPx();
          float py = particle->GetPx();
          float pz = particle->GetPx();
          cout << sqrt(px*px + py*py + pz*pz) << endl;
	}

    }

  phpythiaheader->SetEvt(eventcount);		// Event number
  phpythiaheader->SetNpart(numParticles);	// Number of particle entries in entire history
  phpythiaheader->SetProcessid(0);		// Process ID
  const float CM2MM = 10.;
  phpythiaheader->SetPrimaryVertexX( _generator->GetParameter("xvtx")*CM2MM );
  phpythiaheader->SetPrimaryVertexY( _generator->GetParameter("yvtx")*CM2MM );
  phpythiaheader->SetPrimaryVertexZ( _generator->GetParameter("zvtx")*CM2MM );

  if ( oscar_file.is_open() )
    {
      oscar_file << nstable << " 0" << endl;

      int nout = 0;

      for (Int_t ipart=0; ipart<numParticles; ipart++)
        { 
    
          if ( phpythia->isStable(ipart) )
            {
              ++nout;

              TMCParticle *particle = phpythia->getParticle(ipart);	// get the particle information

              oscar_file << setw(8) << left << nout
                   << setw(8) << right << particle->GetKF()
                   << setw(4) << "0"
                   << setw(12) << particle->GetPx()
                   << setw(12) << particle->GetPy()
                   << setw(12) << particle->GetPz()
                   << setw(12) << particle->GetEnergy()
                   << setw(12) << particle->GetMass()
                   << setw(14) << particle->GetVx()
                   << setw(14) << particle->GetVy()
                   << setw(14) << particle->GetVz()
                   << setw(14) << particle->GetTime()
                   << endl;
            }
        }

      oscar_file << "0 0" << endl;	// end of oscar block
    }

  return EVENT_OK;
}

int PHParticleGen::CreateNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  //-* pythia header information
  phpythiaheader = new PHPythiaHeaderV2();
  PHObjectNode_t *PHPythiaHeaderNode = new PHObjectNode_t(phpythiaheader, "PHPythiaHeader", "PHObject");
  dstNode->addNode(PHPythiaHeaderNode);

  //-* pythia particle information
  phpythia = new PHPythiaContainerV2();
  PHObjectNode_t *PHPythiaNode = new PHObjectNode_t(phpythia, "PHPythia", "PHObject");
  dstNode->addNode(PHPythiaNode);

  return 0;
}

