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

#include "PHHepMCFilter.h"
#include "PHHepMCGenEvent.h"

#include "HepMC/GenEvent.h"
#include "HepMC/IO_GenEvent.h"
#include "HepMC/GenEvent.h"
#include "HepMC/GenVertex.h"
#include "HepMC/GenParticle.h"
#include "HepMC/SimpleVector.h"

#include "TVector3.h"
#include <boost/foreach.hpp>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

//____________________________________________________________________________________________
PHHepMCFilter::PHHepMCFilter(const string &name): 
  SubsysReco(name),
  _theName(name),
  _saveOnlyFinalState(false),
  _saveFirstGen(false),
  _saveBeamParticles(true),
  _saveOnlyStatus(0),
  _theHepMCEvt(NULL),
  _theNewHepMCEvt(NULL),
  hepmcEvent(NULL),
  eventCounter(0),
  _node_name("PHHepMCGenEvent")
{
  _etaHighCut = 9999;
  _etaLowCut = -9999;
  _doEtaCut = false;
  _doAbsEtaCut = false;

  _pLowCut = -9999;
  _pHighCut = 9999;
  _doPCut = false;

  _pzLowCut = -9999;
  _pzHighCut = 9999;
  _doPzCut = false;

  _ptLowCut = -9999;
  _ptHighCut = 9999;
  _doPtCut = false;

}

PHHepMCFilter::~PHHepMCFilter()
{}

//____________________________________________________________________________________________
int PHHepMCFilter::Init( PHCompositeNode* top_node )
{

  return EVENT_OK;
}

//____________________________________________________________________________________________
int PHHepMCFilter::End( PHCompositeNode* )
{ 
  //if(_theHepMCEvt) delete _theHepMCEvt;
  
  return EVENT_OK;
}

int PHHepMCFilter::ResetEvent(PHCompositeNode *topNode)
{

  if(_theNewHepMCEvt) delete _theNewHepMCEvt;
  
  return EVENT_OK;
}


  
//---------------------------------------------------------------------------------------------
int PHHepMCFilter::process_event( PHCompositeNode* top_node )
{

  eventCounter++;

  hepmcEvent = findNode::getClass<PHHepMCGenEvent>(top_node,_node_name.c_str());
  if (!hepmcEvent)
    {
      cout << "PHHepMCFilter::process_event - unable to get PHHepMCGenEvent named " << _node_name << ", is Node missing?" << endl;
      return ABORTRUN;
    }
  
  _theHepMCEvt = hepmcEvent->getEvent();
  //hepmcEvent->PrintEvent();
  if(verbosity > 1)
    {
      cout << "PHHepMCFilter::process_event - event: " << eventCounter << "   size: " << _theHepMCEvt->particles_size() << endl;
    }
  if(_theHepMCEvt->particles_empty()){ cout << "PHHepMCFilter::process_event - event has no particles! Skipping..." << endl; return EVENT_OK;}
  if(verbosity > 0 && !_theHepMCEvt){ cout << "PHHepMCFilter::process_event - loaded event is null!" << endl; return ABORTEVENT;}

  //Form filtered event
  buildNewEvent(_theHepMCEvt);

  //cout << "PHHepMCFilter::process_event - Print" << endl;
  //_theNewHepMCEvt->print();

  hepmcEvent->swapEvent(_theNewHepMCEvt);
  //cout << "PASSED: " << passed << endl;

  //cout << "PHHepMCFilter::process_event - Print from NODE" << endl;
  //hepmcEvent->PrintEvent();

  if(verbosity > 1)
    {
      cout << "PHHepMCFilter::process_event - Filtered size: " << _theNewHepMCEvt->particles_size() << endl;
    }
  

  return EVENT_OK;  
}
//------------------------------------------------------------------------------------------------



bool PHHepMCFilter::checkCode(int code)
{
  if(code == 2) cout << "PHHepMCFilter::checkCode - event is empty after filtering!" << endl;
  else if(code == 1) cout << "PHHepMCFilter::checkCode - failed to add particle!" << endl;
  else if(code == 0) return true;
  
  return false;
  
}



void PHHepMCFilter::buildNewEvent(const HepMC::GenEvent *inputEvent)
{
  _theNewHepMCEvt = new HepMC::GenEvent(inputEvent->momentum_unit(),inputEvent->length_unit());
  _theNewHepMCEvt->set_event_number(inputEvent->event_number());
  _theNewHepMCEvt->set_signal_process_id(inputEvent->signal_process_id());
  _theNewHepMCEvt->set_event_scale (inputEvent->event_scale());
  _theNewHepMCEvt->set_alphaQCD(inputEvent->alphaQCD());
  _theNewHepMCEvt->set_alphaQED(inputEvent->alphaQED());
  _theNewHepMCEvt->weights() = inputEvent->weights();
  _theNewHepMCEvt->set_random_states (inputEvent->random_states());
  if (inputEvent->heavy_ion()) _theNewHepMCEvt->set_heavy_ion(*inputEvent->heavy_ion());
  if (inputEvent->pdf_info())  _theNewHepMCEvt->set_pdf_info(*inputEvent->pdf_info());
  _theNewHepMCEvt->set_cross_section(*inputEvent->cross_section());

  int CODE = doFiltering(inputEvent,_theNewHepMCEvt);
  if(!checkCode(CODE)) cout << "PHHepMC::buildNewEvent - WARNING! Event " << eventCounter << endl;


}



HepMC::GenEvent* PHHepMCFilter::filterEvent(const HepMC::GenEvent *inputEvent)
{
  if(_theNewHepMCEvt) delete _theNewHepMCEvt;
  buildNewEvent(inputEvent);
  return _theNewHepMCEvt;
}



int PHHepMCFilter::doFiltering(const HepMC::GenEvent *inEvent, HepMC::GenEvent* outEvent)
{

  if(_saveBeamParticles)
    {
      HepMC::GenParticle *pBeam1 = addBeamParticle(inEvent->beam_particles().first,outEvent);
      HepMC::GenParticle *pBeam2 = addBeamParticle(inEvent->beam_particles().second,outEvent);
      outEvent->set_beam_particles(pBeam1,pBeam2);
    }

  bool isEmpty = true;
  for(HepMC::GenEvent::particle_const_iterator p = inEvent->particles_begin(); p != inEvent->particles_end(); ++p)
    {
      if (isAccepted(*p))
	{
	  if(addParticle(*p, outEvent)) return 1;
	  if(_saveFirstGen) addFirstGen(*p, outEvent);
	  isEmpty = false;
	}
    }
  
  if(!isEmpty) return 0;
  return 2;

}



int PHHepMCFilter::addParticle(const HepMC::GenParticle* p, HepMC::GenEvent* ev)
{
  if (p->production_vertex()) addVertex (p->production_vertex(), ev);
  else if (p->end_vertex())   addVertex (p->end_vertex(), ev);
  else {
    throw std::runtime_error("PHHepMCFilter::addParticle - encountered particle with no vertices!");
  }
 
  HepMC::GenParticle* pnew = ev->barcode_to_particle (p->barcode());
  if (!pnew)
    {
      //Must recreate the particles...screw you HepMC
      HepMC::FourVector vec(p->momentum());
      int pdg = p->pdg_id();
      int status = p->status();
      HepMC::Flow flow(p->flow());
      HepMC::Polarization pol(p->polarization());
      pnew = new HepMC::GenParticle(vec,pdg,status,flow,pol);
      pnew->suggest_barcode(p->barcode());
      pnew->set_generated_mass(p->generated_mass());
    }
  if (p->production_vertex())
    {
      HepMC::GenVertex* v = ev->barcode_to_vertex (p->production_vertex()->barcode());
      if (v) v->add_particle_out (pnew);
    }
  if (p->end_vertex()) 
    {
      HepMC::GenVertex* v = ev->barcode_to_vertex (p->end_vertex()->barcode());
      if (v) v->add_particle_in (pnew);
    }

  return 0;
}



HepMC::GenParticle* PHHepMCFilter::addBeamParticle(const HepMC::GenParticle* p, HepMC::GenEvent* ev)
{
  if (p->production_vertex()) addVertex (p->production_vertex(), ev);
  else if (p->end_vertex())   addVertex (p->end_vertex(), ev);
  else {
    throw std::runtime_error("PHHepMCFilter::addParticle - encountered particle with no vertices!");
  }
 
  HepMC::GenParticle* pnew = ev->barcode_to_particle (p->barcode());
  if (!pnew)
    {
      //Must recreate the particles...screw you HepMC
      HepMC::FourVector vec(p->momentum());
      int pdg = p->pdg_id();
      int status = p->status();
      HepMC::Flow flow(p->flow());
      HepMC::Polarization pol(p->polarization());
      pnew = new HepMC::GenParticle(vec,pdg,status,flow,pol);
      pnew->suggest_barcode(p->barcode());
      pnew->set_generated_mass(p->generated_mass());
    }
  if (p->production_vertex())
    {
      HepMC::GenVertex* v = ev->barcode_to_vertex (p->production_vertex()->barcode());
      if (v) v->add_particle_out (pnew);
    }
  if (p->end_vertex()) 
    {
      HepMC::GenVertex* v = ev->barcode_to_vertex (p->end_vertex()->barcode());
      if (v) v->add_particle_in (pnew);
    }

  return pnew;
}


int PHHepMCFilter::addVertex(const HepMC::GenVertex* v, HepMC::GenEvent* ev)
{
  HepMC::GenVertex* vnew = ev->barcode_to_vertex(v->barcode());
  if (!vnew) 
    {
      vnew = new HepMC::GenVertex();
      vnew->set_position(v->position());
      vnew->set_id(v->id());
      vnew->suggest_barcode(v->barcode());
      vnew->weights() = v->weights();
      ev->add_vertex (vnew);
      // Fill in the existing relations of the new vertex.
      BOOST_FOREACH(const HepMC::GenParticle* p, std::make_pair (v->particles_in_const_begin(), v->particles_in_const_end()))
      {
	HepMC::GenParticle* pnew = ev->barcode_to_particle (p->barcode());
	if (pnew) vnew->add_particle_in (pnew);
      }

      BOOST_FOREACH(const HepMC::GenParticle* p, std::make_pair (v->particles_out_const_begin(), v->particles_out_const_end()))
      {
	HepMC::GenParticle* pnew = ev->barcode_to_particle (p->barcode());
	if (pnew) vnew->add_particle_out (pnew);
      }
    }

  return 0;
}



bool PHHepMCFilter::isAccepted (const HepMC::GenParticle *p)
{
  bool isAKeeper = true;
  
  //Check allowed parts
  if(_theParts.size() > 0)
    {
      bool tmpFound = false;
      for(unsigned int i = 0; i < _theParts.size(); i++)
	{
	  if(p->pdg_id() == _theParts[i]){ tmpFound = true; break;}
	}
      isAKeeper = tmpFound;
    }

  //Check status
  if(_saveOnlyStatus > 0)
    {
      if(p->status() != _saveOnlyStatus) isAKeeper = false;
    }

  //Check final state
  if(_saveOnlyFinalState && p->status() != 1) isAKeeper = false;

  //Eta Cuts
  if(_doAbsEtaCut)
    {
      if(abs(p->momentum().eta()) > _etaHighCut || abs(p->momentum().eta()) < _etaLowCut) isAKeeper = false;
    }
  if(_doEtaCut)
    {
      if(p->momentum().eta() > _etaHighCut || p->momentum().eta() < _etaLowCut) isAKeeper = false;
    }

  //Momentum Cuts
  if(_doPCut)
    {
      TVector3 mom(p->momentum().px(),p->momentum().py(),p->momentum().pz());
      if(mom.Mag() > _pHighCut || mom.Mag() < _pLowCut) isAKeeper = false;
    }
  if(_doPzCut)
    {
      if(abs(p->momentum().pz()) > _pzHighCut || abs(p->momentum().pz()) < _pzLowCut) isAKeeper = false;
    }
  if(_doPtCut)
    {
      if(abs(p->momentum().perp()) > _ptHighCut || abs(p->momentum().perp()) < _ptLowCut) isAKeeper = false;
    }

  //Check allowed parents
  if(_theParents.size() > 0)
    {
      if(!decayedFromParents(_theParents, p, 1)) isAKeeper = false;
    }
  
  if(verbosity > 2 && isAKeeper) cout << "PHHepMCFilter::isAccepted - ID: " << p->pdg_id() << "  Status: " << p->status() << endl;

  return isAKeeper;
}



bool PHHepMCFilter::decayedFromParents(std::vector<int> parents, const HepMC::GenParticle* track, int maxGenerations) 
{
  bool found = false;
  int maxLoops = maxGenerations;
  int loop = 0;
  if(maxGenerations<0) maxLoops=1000;
  
  // initialize current Production Vertex and mother particles
  // There should always be only one mother particle for decaying particles
  HepMC::GenVertex* current_vertex = track->production_vertex();
  HepMC::GenVertex::particle_iterator current_mother;
  
  // iterate through the mother particles and compare with mother
  // If found match, than stop the loop, otherwise try until there is no production vertex
  // or until you have reached the max number of loops specified
  while((current_vertex) && !(found) && (loop<maxLoops))
    {
      current_mother = current_vertex->particles_begin(HepMC::parents);
      
      //if no mother anymore, jump out the loop
      if(*current_mother==0)break;     

      for(unsigned int i = 0; i < parents.size(); i++)
	{
	  if((*current_mother)->pdg_id()==parents[i])
	    {
	      found=true;
	      break;
	    }
	}
      if(!found) current_vertex=(*current_mother)->production_vertex();
      loop++;
    }
  return found;
}


void PHHepMCFilter::addFirstGen(const HepMC::GenParticle* p, HepMC::GenEvent* ev)
{
  HepMC::GenVertex* current_vertex = p->production_vertex();
  HepMC::GenVertex::particle_iterator current_mother;

  //Loop over particles in to current_vertex
  //Add them to event 
  BOOST_FOREACH(const HepMC::GenParticle* ip, std::make_pair (current_vertex->particles_in_const_begin(), current_vertex->particles_in_const_end()))
    {
      HepMC::GenParticle* pnew = ev->barcode_to_particle(ip->barcode());
      if (!pnew)
	{
	  //Must recreate the particles...screw you HepMC
	  HepMC::FourVector vec(ip->momentum());
	  int pdg = ip->pdg_id();
	  int status = ip->status();
	  HepMC::Flow flow(ip->flow());
	  HepMC::Polarization pol(ip->polarization());
	  pnew = new HepMC::GenParticle(vec,pdg,status,flow,pol);
	  pnew->suggest_barcode(ip->barcode());
	  pnew->set_generated_mass(ip->generated_mass());
	}
      if (ip->end_vertex()) 
	{
	  HepMC::GenVertex* v = ev->barcode_to_vertex (ip->end_vertex()->barcode());
	  if (v) v->add_particle_in(pnew);
	}
    }

}


//------------------------------------------------------------------------------------------
//Set Cuts && Utilites
void PHHepMCFilter::SetEtaHigh(double eta)
{
  _etaHighCut = eta;
  _doEtaCut = true;
}
void PHHepMCFilter::SetEtaLow(double eta)
{
  _etaLowCut = eta;
  _doEtaCut = true;
}

void PHHepMCFilter::SetEtaHighLow(double etaHigh, double etaLow)
{
  _etaHighCut = etaHigh;
  _etaLowCut = etaLow;
  if(_etaHighCut < _etaLowCut)
    {
      _etaHighCut = etaLow;
      _etaLowCut = etaHigh;
    }
 
  _doEtaCut = true;
}

void PHHepMCFilter::SetAbsEtaHigh(double eta)
{
  _etaHighCut = eta;
  _doAbsEtaCut = true;
}

void PHHepMCFilter::SetAbsEtaLow(double eta)
{
  _etaLowCut = eta;
  _doAbsEtaCut = true;
}

void PHHepMCFilter::SetAbsEtaHighLow(double etaHigh, double etaLow)
{
  _etaHighCut = etaHigh;
  _etaLowCut = etaLow;
  if(_etaHighCut < _etaLowCut)
    {
      _etaHighCut = etaLow;
      _etaLowCut = etaHigh;
    }

  _doAbsEtaCut = true;
}


void PHHepMCFilter::SetPLow(double p)
{
  _pLowCut = p;
  _doPCut = true;
}

void PHHepMCFilter::SetPHigh(double p)
{
  _pHighCut = p;
  _doPCut = true;
}

void PHHepMCFilter::SetPzLow(double pz)
{
  _pzLowCut = pz;
  _doPzCut = true;
}

void PHHepMCFilter::SetPzHigh(double pz)
{
  _pzHighCut = pz;
  _doPzCut = true;
}

void PHHepMCFilter::SetPtLow(double pt)
{
  _ptLowCut = pt;
  _doPtCut = true;
}

void PHHepMCFilter::SetPtHigh(double pt)
{
  _ptHighCut = pt;
  _doPtCut = true;
}


void PHHepMCFilter::AddParents(std::string parents)
{
  std::vector<int> addedParents = convertToInts(parents);
  _theParents.insert(_theParents.end(),addedParents.begin(),addedParents.end());
}

void PHHepMCFilter::AddParents(int parent)
{
  _theParents.push_back(parent);
}

void PHHepMCFilter::AddParents(std::vector<int> parents)
{
  _theParents.insert(_theParents.end(),parents.begin(),parents.end());
}

void PHHepMCFilter::AddParticles(std::string parts)
{
  std::vector<int> addedParts = convertToInts(parts);
  _theParts.insert(_theParts.end(),addedParts.begin(),addedParts.end());
}

void PHHepMCFilter::AddParticles(int part)
{
  _theParts.push_back(part);
}

void PHHepMCFilter::AddParticles(std::vector<int> parts)
{
  _theParts.insert(_theParts.end(),parts.begin(),parts.end());
}


std::vector<int> PHHepMCFilter::convertToInts(std::string s)
{

  vector<int> theVec;
  stringstream ss(s);
  int i;
  while (ss >> i)
    {
      theVec.push_back(i);
      if (ss.peek() == ',' || ss.peek() == ' ' || ss.peek() == ':' || ss.peek() == ';') ss.ignore();
    }
  return theVec;
 
}
