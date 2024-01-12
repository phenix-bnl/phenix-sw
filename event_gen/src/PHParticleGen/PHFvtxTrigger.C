#include <iostream>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>

#include <TLorentzVector.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <PHPyTrigger.h>
#include <PHPythiaHeader.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>

#include <PHFvtxTrigger.h>

PHFvtxTrigger::PHFvtxTrigger(const std::string &name) : 
  PHPyTrigger(name)
{
}

int
PHFvtxTrigger::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
PHFvtxTrigger::End(PHCompositeNode *topNode)
{
  //-* dump out trigger statistics
  std::cout << "PHFvtxTrigger: Number_Triggered Number_Considered Percentage "
	    << ntriggered << " " << nconsidered;
  if ( nconsidered>0 ) std::cout << " " << float(ntriggered)/nconsidered*100.0 << std::endl;
  else                 std::cout << " nan" << std::endl;

  return EVENT_OK;
}

int
PHFvtxTrigger::process_event(PHCompositeNode *topNode)
{
  // Get PYTHIA Header (only if you want to trigger on event information)
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
    {
      std::cout << PHWHERE << "Unable to get PHPythiaHeader, is Node missing?" << std::endl;
      return ABORTEVENT;
    }
  
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
    {
      std::cout << PHWHERE << "Unable to get PHPythia, is Node missing?" << std::endl;
      return ABORTEVENT;
    }
  
  ++nconsidered;

  int triggered = FvtxAcceptance(phpythia);

  if ( triggered )
    {
      ++ntriggered;
      return EVENT_OK;
    }

  return ABORTEVENT;	// trigger condition not found, don't write out event
}

int
PHFvtxTrigger::FvtxAcceptance(PHPythiaContainer *phpylist)
{
  int npart = phpylist->size();
  for (int ipart=0; ipart<npart; ipart++)
    {
      TMCParticle* part = phpylist->getParticle(ipart);

      double x = part->GetVx();
      double y = part->GetVy();
      //      double z = part->GetVz();
      
      float px = part->GetPx();
      float py = part->GetPy();
      float pz = part->GetPz();
      
      double sigma_z = copysign(10.0,pz); // assume VTX design of 10 cm 
      const double Z2 = copysign(24.0,pz); // approx location of 2nd plane
      const double Z4 = copysign(40.0,pz); // approx location of 4th plane
      const double R_INNER = 3;            // make these a little wide for safety's sake
      const double R_OUTER = 14;

      // Smallest acceptable angle is defined by most backward
      // vtx and forward particle hitting inner radius of 4th plane
      double x4 = x + px/pz * (Z4+sigma_z);
      double y4 = y + py/pz * (Z4+sigma_z);
      double r4 = sqrt(x4*x4+y4*y4);

      // Similar logic for largest angle
      double x2 = x + px/pz * (Z2-sigma_z);
      double y2 = y + py/pz * (Z2-sigma_z);
      double r2 = sqrt(x2*x2+y2*y2);

      
      // If the projection has a greater r than the inner edge of the
      // 4th plane and a smaller r than the outer edge of the second
      // plane, trigger on it.
      if ( r4 > R_INNER && r2 < R_OUTER ) return 1;
    }

  // Went through all particles and did not find a trigger
  return 0;
}

int
PHFvtxTrigger::ResetEvent(PHCompositeNode *topNode)
{
  return 0;
}
