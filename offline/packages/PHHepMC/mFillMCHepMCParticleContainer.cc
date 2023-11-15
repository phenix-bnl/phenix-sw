#include <PHGlobal.h>
#include <RunHeader.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <getClass.h>
#include <Tools.h>
#include <RunHeader.h>
#include <SyncObject.h>
#include <MCHepMCParticle.h>
#include <MCHepMCParticleContainer.h>
#include <PHHepMCGenEvent.h>

#include "HepMC/GenEvent.h"
#include "HepMC/GenParticle.h"
#include "HepMC/GenVertex.h"

#include "mFillMCHepMCParticleContainer.h"

using namespace std;
typedef PHIODataNode<PHObject> PHObjectNode_t;

//___________________________________________________________________
int mFillMCHepMCParticleContainer::Init(PHCompositeNode *top_node)
{
    //create output node in the node tree
  PHNodeIterator iter(top_node);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode *>(iter.findFirst("PHCompositeNode","DST"));
  if(!dstNode) {
    cout << "dstNode not found"<<endl;
  } else {
    cout << "dstNode is found"<<endl;
  }

  MCHepMCParticleContainer *partCont = new MCHepMCParticleContainer();
  if(partCont && dstNode) 
    {
      PHObjectNode_t *partNode = new PHIODataNode<PHObject>(partCont,_output_node_name.c_str(),"PHObject");
      dstNode->addNode(partNode);
      cout << "MCHepMCParticleContainer is added" <<endl;
    } 
  else {
    cout << ThisName << " Init() failed to create output object"<<endl;
    return ABORTRUN;
  }

  nevents = 0;

  return 0;
}

//______________________________________________________
int mFillMCHepMCParticleContainer::InitRun(PHCompositeNode *top_node)
{
  cout << "------------------- mFillMCHepMCParticleContainer::InitRun ---------------------" << endl;
  return 0;
}

//______________________________________________________
int mFillMCHepMCParticleContainer::process_event(PHCompositeNode *top_node)
{
  nevents++;

  phhepmcevt = findNode::getClass<PHHepMCGenEvent>(top_node,_node_name.c_str());
  if (!phhepmcevt)
    {
      cout << "PHHepMCHelper::process_event - unable to get PHHepMCGenEvent, is Node missing?" << endl;
      return ABORTRUN;
    }
  hepmcevt = phhepmcevt->getEvent();
  
  MCHepMCParticleContainer *hepmcPartCont = findNode::getClass<MCHepMCParticleContainer>(top_node,_output_node_name.c_str());
  if (!hepmcPartCont)
    {
      cout << "mFillHepMCParticleContainer::process_event - MCHepMCParticleContainer not in Node Tree" << endl;
      return ABORTRUN;
    }

  int npart = 0;

  // Loop over hepmcparts to fill container
  for(HepMC::GenEvent::particle_iterator p = hepmcevt->particles_begin(); p != hepmcevt->particles_end(); ++p)
    {
      HepMC::GenParticle *thePart = hepmcevt->barcode_to_particle((*p)->barcode());
      MCHepMCParticle *hepmcpart = new MCHepMCParticle();
      //----------------Define MC Variables -----------------------
      int MC_N_PART=-999;
      int MC_BARCODE=-999;
      float MC_PX=-999;        // the major MC particle's px
      float MC_PY=-999;
      float MC_PZ=-999;
      float MC_E=-999;
      float MC_PTOT =-999;      // the major MC particle's lastGap
      int MC_PID =-999;
      int MC_STATUS =-999;
      //-------------------------------------Start Search--------------------------------------
      MC_N_PART = npart;
      MC_BARCODE = thePart->barcode();
      MC_PX = thePart->momentum().px();
      MC_PY = thePart->momentum().py();
      MC_PZ = thePart->momentum().pz();
      MC_E = thePart->momentum().e();
      MC_PTOT = sqrt(MC_PX*MC_PX+MC_PY*MC_PY+MC_PZ*MC_PZ);
      MC_PID = thePart->pdg_id();
      MC_STATUS = thePart->status();

      //-----------------Fill the MC information------------------ 
      hepmcpart->mc_n_part = MC_N_PART;
      hepmcpart->mc_barcode = MC_BARCODE;
      hepmcpart->mc_px = MC_PX;
      hepmcpart->mc_py = MC_PY;
      hepmcpart->mc_pz = MC_PZ;
      hepmcpart->mc_e = MC_E;
      hepmcpart->mc_ptot = MC_PTOT;
      hepmcpart->mc_pid = MC_PID;
      hepmcpart->mc_status = MC_STATUS;

      _mcpartmap.insert(std::pair<int,MCHepMCParticle*>(MC_BARCODE,hepmcpart));

      npart++;
    }
  
  //re-associate vertices to particles
  for(HepMC::GenEvent::vertex_iterator v = hepmcevt->vertices_begin(); v != hepmcevt->vertices_end(); ++v)
    {
      HepMC::GenVertex *theVertex = hepmcevt->barcode_to_vertex((*v)->barcode());
      
      int barcode = theVertex->barcode();
      if(barcode == 0) continue;
      float X = theVertex->position().x();
      float Y = theVertex->position().y();
      float Z = theVertex->position().z();
      
      for(HepMC::GenVertex::particles_in_const_iterator in = theVertex->particles_in_const_begin(); in != theVertex->particles_in_const_end(); ++in)
	{
	  HepMC::GenParticle *partIn = *in;
	  MCHepMCParticle *thePart = _mcpartmap.find(partIn->barcode())->second;
	  thePart->mc_x_end = X;
	  thePart->mc_y_end = Y;
	  thePart->mc_z_end = Z;
	  thePart->mc_vertex_end = barcode;
	}

      for(HepMC::GenVertex::particles_out_const_iterator out = theVertex->particles_out_const_begin(); out != theVertex->particles_out_const_end(); ++out)
	{
	  HepMC::GenParticle *partOut = *out;
	  MCHepMCParticle *thePart = _mcpartmap.find(partOut->barcode())->second;
	  thePart->mc_x_begin = X;
	  thePart->mc_y_begin = Y;
	  thePart->mc_z_begin = Z;
	  thePart->mc_vertex_begin = barcode;
	}
    }

  //Fill container
  for(std::map<int,MCHepMCParticle*>::iterator it = _mcpartmap.begin(); it != _mcpartmap.end(); ++it)
    {
      MCHepMCParticle part = *(it->second);
      hepmcPartCont->AddMCHepMCParticle(part);
    }

  //clear map
  _mcpartmap.clear();
  if (hepmcPartCont->get_nMCHepMCParticles() > 0) return EVENT_OK;

  return DISCARDEVENT;
}

//______________________________________________________
int mFillMCHepMCParticleContainer::End(PHCompositeNode *top_node)
{
  cout << "---------------------- mFillMCHepMCParticleContainer::End ----------------------" << endl;
  return 0 ;
}
