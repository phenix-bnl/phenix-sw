#include "EmcEmbedEventReco.h"

#include <cassert>
#include <vector>

#include "emcEmbedEventv1.h"
#include "emcNodeHelper.h"
//INCLUDECHECKER: Removed this line: #include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "primaryWrapper.h"
#include "TDatabasePDG.h"
#include "VtxOut.h"
#include "PHGlobal.h"

namespace
{
  const std::vector<int>& knownparticles()
  {
    static std::vector<int> particles;

    if (particles.empty())
      {
	particles.push_back(111); // pizero
	particles.push_back(221); // eta
	particles.push_back(310); // K0s
      }

    return particles;
  }
}

//_____________________________________________________________________________
EmcEmbedEventReco::EmcEmbedEventReco() 
  : SubsysReco("EmcEmbedEventReco"),
    fIsValid(false)
{
}

//_____________________________________________________________________________
int
EmcEmbedEventReco::InitRun(PHCompositeNode* topNode)
{  
  primaryWrapper* primary = findNode::getClass<primaryWrapper>(topNode,"primary");
  if (!primary)
    {
      // Running this module on pure real data is not error, just a nop.

      std::cout << PHWHERE << "No primary in input file. This might not be a problem if the input is pure real data. If you intended to run on a merged input, then you have a problem."
		<< std::endl;
      fIsValid=false;
      return 0;
    }

  // emcEmbedEvent is the object we're creating.
  emcEmbedEvent* test =
    emcNodeHelper::addObject<emcEmbedEventv1>(topNode,"emcEmbedEvent",true);

  fIsValid=(test!=0); // invalid if creation failed for whatever reason.
  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedEventReco::process_event(PHCompositeNode* topNode)
{
  if (!fIsValid)
    {
      return 0;
    }

  emcEmbedEvent* eee = findNode::getClass<emcEmbedEvent>(topNode,"emcEmbedEvent");
  assert(eee!=0);

  eee->Reset();

  primaryWrapper* primary = findNode::getClass<primaryWrapper>(topNode,"primary");
  assert(primary!=0);

  if ( primary->RowCount() != 2 )
    {
      std::cerr << PHWHERE << " This module currently only understands "
		<< "forced acceptance single meson->gg simulation."
		<< std::endl;
      return ABORTRUN;
    }

  if ( primary->get_idpart(0) != 1 && // photon 
       primary->get_idpart(1) != 1 )
    {
      std::cerr << PHWHERE << " Expecting two photons there!"
		<< std::endl;
      return ABORTRUN;
    }

  double px1 = primary->get_px_momentum(0);
  double py1 = primary->get_py_momentum(0);
  double pz1 = primary->get_pz_momentum(0);

  double px2 = primary->get_px_momentum(1);
  double py2 = primary->get_py_momentum(1);
  double pz2 = primary->get_pz_momentum(1);

  double p1 = sqrt(px1*px1+py1*py1+pz1*pz1);
  double p2 = sqrt(px2*px2+py2*py2+pz2*pz2);
  
  double cosinus = (px1*px2+py1*py2+pz1*pz2)/(p1*p2);

  double minv = sqrt(2*p1*p2*(1-cosinus));

  TDatabasePDG* pdg = TDatabasePDG::Instance();
  
  double pmass = -1;
  int pid = -1;

  for ( size_t i = 0; i < knownparticles().size(); ++i ) 
    {      
      TParticlePDG* particle = pdg->GetParticle(knownparticles()[i]);

      double mass = particle->Mass();

      double diff = fabs(mass-minv)/mass;

      if ( diff < 1E-3 )
	{
	  pmass=mass;
	  pid = particle->PdgCode();
	  break;
	}
    }

  if ( pmass < 0 )
    {
      std::cerr << PHWHERE << " Do not know this particle. Check me : "
		<< "minv=" << minv
		<< std::endl;
      return ABORTRUN;
    }

  double px = px1+px2;
  double py = py1+py2;
  double pz = pz1+pz2;
  double psquare = px*px+py*py+pz*pz;
  double energy = sqrt(psquare+pmass*pmass);

  PHGlobal* global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  assert(global!=0);  

  float zreal = global->getBbcZVertex();

  VtxOut* vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut");
  assert(vtxout!=0);

  float zsimu = vtxout->get_ZVertex();

  eee->set_primary(0,pid,energy,px,py,pz,zreal,zsimu);

  return EVENT_OK;
}
