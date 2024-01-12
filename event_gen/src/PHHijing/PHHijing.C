#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <ctime>
#include <sys/time.h>
#include <stdexcept>

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

#include <PHHijing.h>
#include <PHHijingHeaderV3.h>
#include <PHPythiaContainerV2.h>

#include "PHHepMCGenEvent.h"
#include <HepMC/GenEvent.h>
#include <HepMC/HeavyIon.h>
#include <HepMC/GenParticle.h>
#include <HepMC/GenVertex.h>

#include <TClonesArray.h>
#include <TRandom.h>

#include <THijing.h>
#include <hijingpar.h>
#include <histrng.h>
#include <hiparnt.h>

#include <ConfigParser.h>

#include <boost/algorithm/string.hpp>

using namespace std;

// Fortran common block used by PHENIX hijing
extern HIJINGPAR hijingpar_;
extern HISTRNG histrng_;
extern HIPARNT hiparnt_;

typedef PHIODataNode<PHObject> PHObjectNode_t;

PHHijing::PHHijing(const std::string &name, FORMAT outputformat):
  SubsysReco(name), eventcount(0), _generator(0), phhijingheader(0), 
  phpythia(0), _hijingpar(hijingpar_), _histrng(histrng_), seed(-1),
  hijingHepMCEvt(NULL),
  _node_name("PHHepMCGenEvent")
{
  _phirand = false;
  gRandom->SetSeed(time(NULL));

  //HepMC
  _useHepMC = false;
  if(outputformat == HEPMC) _useHepMC = true;
}

PHHijing::~PHHijing()
{
  std::cout << "PHHijing dtor" << std::endl;
  delete _generator;
}

int
PHHijing::Init(PHCompositeNode *topNode)
{
  ReadConfig();

  CreateNodeTree(topNode);

  _generator = new THijing();

  for(std::map<std::string,std::string>::const_iterator it = _kvc.begin();
      it != _kvc.end(); ++it)
    {
      std::cout << "PHHijing::Init: " << it->first << " = " << it->second << std::endl;
      std::string par = it->first;
      boost::erase_all(par, " "); 
      std::string val = it->second;
      char* endp = 0;
      double dval = strtod(val.c_str(),&endp);
      if ( val.c_str() != endp && *endp == '\0' )
	{
	  // Value successfully converted to double (which includes integers, too)
	  ; //_generator->SetParameter(par.c_str(),dval);
	}
      else
	{
	  int copylen = strlen(val.c_str());
	  if ( copylen > 4 ) copylen = 4;
	  memcpy(&dval,val.c_str(),copylen);
	}
      _generator->SetParameter(par.c_str(),dval);
    }


  eventcount = 0;	// event numbering will start from 1

  // set the random seed by getting the time of day, to the microsecond
  if ( seed < 0 )
    {
      // first try getting seed from /dev/random
      ifstream devrandom;
      devrandom.open("/dev/random",ios::binary);
      devrandom.read((char*)&seed,sizeof(seed));
      devrandom.close();

      if ( seed != -1 )
        {
          cout << PHWHERE << " Got seed from /dev/random" << endl;
        }
      else
        {
          // /dev/random failed, get the random seed from the time of day, to the microsecond
          cout << PHWHERE << " Getting seed from gettimeofday()" << endl;
          timeval xtime;
          int status = gettimeofday(&xtime,NULL);
          if ( status==0 )
            {
              seed = ((xtime.tv_sec << 12) + (xtime.tv_usec&0xfff));
              if ( seed<0 ) seed *= -1;
            }
          else
            {
              cout << PHWHERE << " something wrong with gettimeofday()" << endl;
            }
        }
    }


  if ( seed!=-1 )
    {
      if ( seed<0 ) seed *= -1;
      _generator->SetParameter("iseed",seed%215);
      int seed_skip = (seed/10000);
      seed_skip = seed_skip%1000;
      _generator->SetParameter("iseed_skip",seed_skip);
    }
  else 
    {
      std::cout << PHWHERE << " ERROR: seed " << seed << " is not valid" << std::endl;
      exit(2); 
    }

  _generator->Init();

  PrintConfig();

  // If the oscar output name is set, open up the OSCAR1999A file
  if ( oscar_fname.size() > 0 )
    {
      oscar_file.open("pythia.osc");
      oscar_file << "# OSC1999A" << std::endl;
      oscar_file << "# final_id_p_x" << std::endl;
      oscar_file << "# Hijing 1.383" << std::endl;
      oscar_file << "# " << std::endl;
    }

  // Print out the internal HIJING configuration (commons)
  std::cout << "================================================" << std::endl;
  std::cout << "        HIJING Parameter Configuration" << std::endl;
  std::cout << "================================================" << std::endl;

  for(int i=0; i<47; i++)
    {
      if ( (22<=i && i<=27) ||
	   (35<=i && i<=38) ||
	   (i == 40) ) continue; // Not used
      
      std::cout << "HIPR1(" << i+1 << ") = " << hiparnt_.hipr1[i] << std::endl;
    }
  std::cout << std::endl;

  for(int i=0; i<21; i++)
    {
      if ( 14<=i && i<=16 ) continue; // Not used
      std::cout << "IHPR2(" << i+1 << ") = " << hiparnt_.ihpr2[i] << std::endl;
    }
  std::cout << std::endl;

 
  return EVENT_OK;
}

int
PHHijing::End(PHCompositeNode *topNode)
{
  if ( oscar_file.is_open() ) oscar_file.close();

  return EVENT_OK;
}

int
PHHijing::ReadConfig(const char *cfg_file)
{
  std::cout << "PHHijing::ReadConfig: Reading " << cfg_file << std::endl;

  ifstream infile(cfg_file); 
  if (infile.fail ())
    {
      std::cout << "PHHijing::ReadConfig: Failed to open file "
		<< cfg_file << std::endl;
      return 1;
    }

  std::string FullLine;      // a complete line in the config file

  _kvc.clear();
  std::string key, value;
  int linenum = 0;
  while ( !std::getline(infile,FullLine).fail() )
    {
      ++linenum;

      // skip lines that begin with #
      if ( FullLine[0]=='#' ) continue;

      keyvalue_grammar keyvalue_parser(key, value);
      parse_info<> info = boost::spirit::classic::parse(FullLine.c_str(), 
							keyvalue_parser, 
							boost::spirit::classic::space_p);
      
      if ( ! info.full ) {
	std::cout << "------------------------------------\n";
	std::cout << "Parsing failed at line number " << linenum << "\n";
	std::cout << "stopped at: \"" << info.stop << "\"\n";
	std::cout << "------------------------------------\n" << std::endl;
	throw std::runtime_error("Failed to parse config file");
	break;
      }

      _kvc[key] = value;
    }

  return 0;
}

void
PHHijing::PrintConfig() const
{
  std::cout << "Using seed " << seed << std::endl;
  for(std::map<std::string,std::string>::const_iterator it = _kvc.begin();
      it != _kvc.end(); ++it)
    {
      std::cout << "PHHijing::PrintConfig: " << it->first << " = " << it->second << std::endl;
    }

  if(_useHepMC)  std::cout << "Output format will be PHHepMC!" << std::endl;
}

int
PHHijing::process_event(PHCompositeNode *topNode)
{
  ++eventcount;

  _generator->GenerateEvent();

  TClonesArray* particleArray = (TClonesArray *)_generator->ImportParticles();
  
  Int_t numParticles = particleArray->GetLast() + 1;	// number of generated particles
  
  int nstable = 0;
  // Count the number of binary collisions
  //
  int nbinary = 0;
  int iap = static_cast<int>(_generator->GetParameter("iap"));
  for(int i=0; i<iap; i++) nbinary += _histrng.nfp[10][i];

  float bimp = _generator->GetParameter("bimpact");
  int nt = _generator->GetParameter("nt");
  int np = _generator->GetParameter("np");

  if ( (eventcount < 10) ||
       (eventcount < 100 && eventcount%10 == 0) ||
       (eventcount < 1000 && eventcount%100 == 0) ||
       (eventcount%1000 == 0) )
    {
      std::cout << "Event " << eventcount << ": bimp=" << bimp << " nbin=" << nbinary << std::endl;
    }

  
  if(_useHepMC)
    {
      //if(hijingHepMCEvt) delete hijingHepMCEvt;
      hijingHepMCEvt = new HepMC::GenEvent(HepMC::Units::GEV,HepMC::Units::MM);
      makeHepMCEvent(particleArray,hijingHepMCEvt);
      //Fill in event details
      hijingHepMCEvt->set_event_number(eventcount);
      hijingHepMCEvt->set_signal_process_id(iap);
      HepMC::HeavyIon hi;
      hi.set_Npart_proj(_generator->GetParameter("np"));
      hi.set_Npart_targ(_generator->GetParameter("nt"));
      hi.set_Ncoll(nbinary);
      hi.set_impact_parameter(_generator->GetParameter("bimpact"));
      hijingHepMCEvt->set_heavy_ion(hi);
      hijingHepMCEvt->print();
      if(!phhepmcevt->addEvent(hijingHepMCEvt))
	{
	  cout << "PHHijing::process_event - Failed to add event to HepMC record!" << endl;
	  return ABORTEVENT;
	}
      else{ return EVENT_OK; }

    }

  if(!_phirand) // check for randomization only once per event
    {
      for (Int_t ipart=0; ipart<numParticles; ipart++)
	{ 
	  TMCParticle* particle = (TMCParticle *)particleArray->At(ipart);	// get the particle information
	  phpythia->addParticle(*particle);
      
	  if ( phpythia->isStable(ipart) ) ++nstable;
	}
    }
  else
    {
     for (Int_t ipart=0; ipart<numParticles; ipart++)
	{ 
	  TMCParticle* particle = (TMCParticle *)particleArray->At(ipart);	// get the particle information

	  double pt = sqrt(pow(particle->GetPx(),2)+pow(particle->GetPy(),2));

	  double phi = 2.0*acos(-1.0)*(gRandom->Rndm()-0.5);
	  particle->SetPx(pt*cos(phi));
	  particle->SetPy(pt*sin(phi));

	  phpythia->addParticle(*particle);
      
	  if ( phpythia->isStable(ipart) ) ++nstable;
	}
    }
  
  
  phhijingheader->SetEvt(eventcount);
  phhijingheader->SetNpart(numParticles);
  phhijingheader->SetATarg(_hijingpar.iat);
  phhijingheader->SetZTarg(_hijingpar.izt);
  phhijingheader->SetAProj(_hijingpar.iap);
  phhijingheader->SetZProj(_hijingpar.izp);
  phhijingheader->SetCollisionE(_hijingpar.efrm);
  phhijingheader->SetNbinary(nbinary);
  phhijingheader->SetBimpact(bimp);
  phhijingheader->SetNp(np);
  phhijingheader->SetNt(nt);

  // primary vertex information. Assume position is at zero by default
  phhijingheader->SetPrimaryVertexX(0);
  phhijingheader->SetPrimaryVertexY(0);
  phhijingheader->SetPrimaryVertexZ(0);
  
  if ( oscar_file.is_open() )
    {
      oscar_file << nstable << " 0" << std::endl;
      int nout = 0;
      for (Int_t ipart=0; ipart<numParticles; ipart++)
        { 
          if ( phpythia->isStable(ipart) )
            {
              ++nout;

              TMCParticle *particle = phpythia->getParticle(ipart);	// get the particle information

              oscar_file << std::setw(8) << std::left << nout
			 << std::setw(8) << std::right << particle->GetKF()
			 << std::setw(4) << "0"
			 << std::setw(12) << particle->GetPx()
			 << std::setw(12) << particle->GetPy()
			 << std::setw(12) << particle->GetPz()
			 << std::setw(12) << particle->GetEnergy()
			 << std::setw(12) << particle->GetMass()
			 << std::setw(14) << particle->GetVx()
			 << std::setw(14) << particle->GetVy()
			 << std::setw(14) << particle->GetVz()
			 << std::setw(14) << particle->GetTime()
			 << std::endl;
            }
        }
      oscar_file << "0 0" << std::endl;	// end of oscar block
    }


  return EVENT_OK;
}

int
PHHijing::CreateNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      std::cout << PHWHERE << "DST Node missing doing nothing" << std::endl;
      return -1;
    }

  if(_useHepMC)
    {
      phhepmcevt = new PHHepMCGenEvent();
      PHObjectNode_t *newNode = new PHObjectNode_t(phhepmcevt,_node_name.c_str(),"PHObject");
      dstNode->addNode(newNode);
      return 0;
    }

  //-* header information
  phhijingheader = new PHHijingHeaderV3();
  PHObjectNode_t *PHHijingHeaderNode = new PHObjectNode_t(phhijingheader, "PHHijingHeader", "PHObject");
  dstNode->addNode(PHHijingHeaderNode);

  //-* particle information
  phpythia = new PHPythiaContainerV2();
  PHObjectNode_t *PHPythiaNode = new PHObjectNode_t(phpythia, "PHHijing", "PHObject");
  dstNode->addNode(PHPythiaNode);

  return 0;
}

int
PHHijing::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd(Name()))
    {
      mainIter.forEach(reset);
    }

  if(hijingHepMCEvt) delete hijingHepMCEvt;

  return 0;
}


void
PHHijing::SetDecay(int pdgid, bool val)
{
  if ( ! _generator ) throw std::runtime_error("SetDecay: Generator not initialized");
  _generator->SetDecay(pdgid,val);
}

void
PHHijing::SetPhiRandomization(bool val)
{
  _phirand = val;
}


int PHHijing::makeHepMCEvent(TClonesArray *particles, HepMC::GenEvent *evt)
{
  if (!evt) throw std::runtime_error("PHHijing::makeHepMCEvent - Error! Passed null event...");
    
  // Create a particle instance for each entry and fill a map, and
  // a vector which maps from the particle index to the GenParticle address.
  int numParticles = particles->GetLast() + 1;
  std::vector<HepMC::GenParticle*> hepevt_particles( numParticles );
  for (int i = 0; i < numParticles; i++) 
    { 
      TMCParticle* particle = (TMCParticle *)particles->At(i);      // get the particle information                                                                                                                                                                                               
      // Fill the particle.
      if(verbosity > 5) std::cout << i << "  " << particle->GetKF() << "  " << particle->GetKS() << "    " << particle->GetPx() << "  " << particle->GetPy() << "  " << particle->GetPz() << std::endl;
      hepevt_particles[i] = new HepMC::GenParticle(HepMC::FourVector( particle->GetPx(), 
								      particle->GetPy(),
								      particle->GetPz(), 
								      particle->GetEnergy() ),
						   particle->GetKF(), particle->GetKS() );
      hepevt_particles[i]->suggest_barcode(i+1);
      hepevt_particles[i]->set_generated_mass( particle->GetMass() );

    }
  
  // Here we assume that the first two particles in the list
  // are the incoming beam particles.
  evt->set_beam_particles( hepevt_particles[0], hepevt_particles[1] );
  
  // Loop over particles AGAIN, this time creating vertices.
  // We build the production vertex for each entry in hepevt.
  // The HEPEVT pointers are bi-directional, so gives decay vertices as well.
  for (int i = 0; i < numParticles; i++) 
    {
      TMCParticle *thePart = (TMCParticle*)particles->At(i);
      if(!thePart) continue;
      HepMC::GenParticle *p = hepevt_particles[i]; 
      //Search to see if a production vertex already exists.
      //int iparent = thePart->GetParent()-1;
      //if(iparent < 0) iparent = 0;
      HepMC::GenVertex* prod_vtx = p->production_vertex();
      //No parents in Hijing
      //prod_vtx = hepevt_particles[iparent]->end_vertex();
      if ( prod_vtx ) prod_vtx->add_particle_out( p );

      // If no suitable production vertex exists - and the particle has
      // at least one mother or position information to store - make one.
      bool found = false;
      HepMC::FourVector prod_pos( thePart->GetVx(), thePart->GetVy(), thePart->GetVz(), thePart->GetTime() );
      if ( !prod_vtx ) 
	{
	  //See if the vertex is already in the event
	  for(HepMC::GenEvent::vertex_iterator v = evt->vertices_begin(); v != evt->vertices_end(); ++v)
	    {
	      HepMC::GenVertex *theV = *v;
	      if(theV->position().x() != prod_pos.x()) continue;
	      if(theV->position().y() != prod_pos.y()) continue;
	      if(theV->position().z() != prod_pos.z()) continue;
	      found = true;
	      theV->add_particle_out(p);
	    }
	  if(!found)
	    {
	      //Didn't find vertex, add it
	      prod_vtx = new HepMC::GenVertex(prod_pos);
	      prod_vtx->add_particle_out( p );
	      evt->add_vertex( prod_vtx );
	    }
	}

      // If prod_vtx doesn't already have position specified, fill it.
      if ( !found && prod_vtx && prod_vtx->position() == HepMC::FourVector() ) prod_vtx->set_position( prod_pos );
    
    }
  
  // Check for particles which come from nowhere, i.e. are without
  // mothers or daughters. These need to be attached to a vertex, or else
  // they will never become part of the event.
  for (int i = 0; i < numParticles; i++) 
    {
      if ( !hepevt_particles[i]->end_vertex() && !hepevt_particles[i]->production_vertex() ) 
	{
	  std::cerr << "PHHijing::makeHepMCEvent - hanging particle " << i << std::endl;
	  HepMC::GenVertex* prod_vtx = new HepMC::GenVertex();
	  prod_vtx->add_particle_out( hepevt_particles[i] );
	  evt->add_vertex( prod_vtx );
	}

      // Also check for free partons (= gluons and quarks; not diquarks?)
      if ( hepevt_particles[i]->pdg_id() == 21 && !hepevt_particles[i]->end_vertex() ) 
	{
	  std::cerr << "PHHijing::makeHepMCEvent - gluon without end vertex " << i << std::endl;
	}
      if ( abs(hepevt_particles[i]->pdg_id()) <= 6 && !hepevt_particles[i]->end_vertex() ) 
	{
	  std::cerr << "PHHijing::makeHepMCEvent - quark without end vertex " << i << std::endl;
	}
    }

  return 0;

}
