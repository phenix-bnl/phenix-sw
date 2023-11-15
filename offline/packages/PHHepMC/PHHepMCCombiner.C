#include "PHHepMCCombiner.h"

#include <HepMC/GenEvent.h>
#include <HepMC/IO_GenEvent.h>
#include <HepMC/GenEvent.h>
#include <HepMC/GenVertex.h>
#include <HepMC/GenParticle.h>
#include <HepMC/SimpleVector.h>

#include <TVector3.h>

#include <boost/foreach.hpp>

#include <iostream>
#include <sstream>
#include <string>

using namespace std;


//____________________________________________________________________________________________

void PHHepMCCombiner::combineEvents(const HepMC::GenEvent *inputEvent, HepMC::GenEvent *outputEvent)
{
  if(outputEvent == NULL) outputEvent = new HepMC::GenEvent(inputEvent->momentum_unit(),inputEvent->length_unit());
  if(outputEvent->event_number() < 0)       outputEvent->set_event_number(inputEvent->event_number());
  if(outputEvent->signal_process_id() <= 0) outputEvent->set_signal_process_id(inputEvent->signal_process_id());
  if(outputEvent->event_scale() <= 0)       outputEvent->set_event_scale (inputEvent->event_scale());
  if(outputEvent->alphaQCD() <= 0)          outputEvent->set_alphaQCD(inputEvent->alphaQCD());
  if(outputEvent->alphaQED() <= 0)          outputEvent->set_alphaQED(inputEvent->alphaQED());
  if(outputEvent->weights().empty())        outputEvent->weights() = inputEvent->weights();
  if(outputEvent->random_states().size() == 0)outputEvent->set_random_states (inputEvent->random_states());
  if(!outputEvent->heavy_ion() && inputEvent->heavy_ion()) outputEvent->set_heavy_ion(*inputEvent->heavy_ion());
  if(!outputEvent->pdf_info() && inputEvent->pdf_info())   outputEvent->set_pdf_info(*inputEvent->pdf_info());
  if(outputEvent->cross_section())     outputEvent->set_cross_section(*inputEvent->cross_section());
  
  for(HepMC::GenEvent::particle_const_iterator p = inputEvent->particles_begin(); p != inputEvent->particles_end(); ++p)
    {
      addParticle(*p, outputEvent);
    }
}


int PHHepMCCombiner::addParticle(const HepMC::GenParticle* p, HepMC::GenEvent* ev)
{
  if (p->production_vertex()) addVertex (p->production_vertex(), ev);
  else if (p->end_vertex())   addVertex (p->end_vertex(), ev);
  else {
    throw std::runtime_error("PHHepMCCombiner::addParticle - encountered particle with no vertices!");
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
      //pnew->suggest_barcode(p->barcode());
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


int PHHepMCCombiner::addVertex(const HepMC::GenVertex* v, HepMC::GenEvent* ev)
{
  HepMC::GenVertex* vnew = ev->barcode_to_vertex(v->barcode());
  if (!vnew) 
    {
      vnew = new HepMC::GenVertex();
      vnew->set_position(v->position());
      vnew->set_id(v->id());
      //vnew->suggest_barcode(v->barcode());
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
