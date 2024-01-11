#include "EmcEmbedTowerMerger.h"

#include <iostream>
#include <cassert>
//INCLUDECHECKER: Removed this line: #include <fstream>

#include "emcTowerContainer.h"
#include "emcTowerContent.h"

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"

// uncomment the line below to get real.towers, simu.towers and merged.towers
// output text files. WARNING: those might be big files !
//
//#define FULLDEBUG 

namespace
{
  bool incompatible(const char* method, const char* feature)
  {
    std::cerr << "EmcEmbedTowerMerger::" << method << " : incompatible feature "
	      << feature << " in emcTowerContent "
	      << std::endl;
    return false;
  }

  const unsigned int kTOPREAL = 0;
  const unsigned int kTOPSIMU = 1;
  const unsigned int kTOPMERGED = 2;
}

//_____________________________________________________________________________
EmcEmbedTowerMerger::EmcEmbedTowerMerger(const char* topNode1Name,
                               const char* topNode2Name,
                               const char* topNodeMergedName)
  : SubsysReco("EmcEmbedTowerMerger")
{
  fTopNodeNames.push_back(topNode1Name);
  fTopNodeNames.push_back(topNode2Name);
  fTopNodeNames.push_back(topNodeMergedName);
}

//_____________________________________________________________________________
bool 
EmcEmbedTowerMerger::copy(const emcTowerContent& source, 
		     emcTowerContent& destination)
{
  // OK. You might wonder why do I write a separate copy method
  // instead of putting this as a method in emcTowerContent base class.
  // Well, the answer is that in the following code :
  //
  // emcTowerContainer* tc1 = ...
  // emcTowerContainer* tc2 = ...
  // emcTowerContent* t1 = tc1->getTower(i1);
  // emcTowerContent* t2 = tc2->getTower(i2);
  //
  // *t1 = *t2
  //
  // The last line does not always make sense. It does only if the
  // underlying concrete classes (that is, the versionned ones) are
  // either the same (in which case the operator= in the versionned
  // classes will do the job just right) or "compatible" (one way to view this
  // is to replace the base class emcTowerContainer by let's say a Shape
  // base class, and the versionned concrete classes by e.g. Circle and
  // Rectangle...).
  //
  // Compatible is of course a matter of definition, but I assume here that
  // real towers and simulated towers are *not* compatible, as
  // the simulated towers have much less informations available
  // than real ones.
  // so (real tower)=(simulated tower) should be fine,
  // but (simulated tower)=(real tower) should not be fine (you'd loose
  // too much information in doing so).
  //
  // So the code below assumes that destination is "big enough" to
  // retain all the information of source. As it turns, it means
  // that destination concrete class is not pure real nor pure simulated
  // but a mix (merged) of the two.
  //
  // Fortunately, the emcTowerContent has a set of methods to determine
  // if a given tower has some "features", so we can use those
  // to decide on "compatibility".
  // But please note that the decisions here are driven by the
  // embedding needs, and so are not 100% general. That's why
  // this code is not in the base class.

  if ( !destination.canHaveMerSimStatus() )
    {
      // Here we impose that the destination be a merged tower.      
      return incompatible("copy","canHaveMerSimStatus");
    }

  destination.SetID(source.FEM(),source.Channel());
  destination.SetNeighbours(source.ErrorNeighbours(),
			    source.WarnNeighbours());

  destination.SetSimFrac(source.SimFrac());
  bool simulated = source.isSimulated();
  bool merged = source.isMerged();
  destination.SetMerSimStatus(simulated,merged);

  // Raw part
  if ( source.hasRaw() )
    {
      if ( destination.canHaveRaw() == false )
	{
	  return incompatible("copy","canHaveRaw");
	}
      else
	{
	  destination.SetRaw(source.HGPost(),source.HGPre(),
			     source.LGPost(),source.LGPre(),
			     source.TAC(),
			     source.AMUPre(),source.AMUPost(),source.AMUTAC(),
			     source.BeamClock());
	  destination.SetDataError(source.DataError());
	}
    }

  // DC part
  
  if ( source.hasDC() )
    {
      if ( destination.canHaveDC() == false )
	{
	  return incompatible("copy","canHaveDC");
	}
      else
	{
	  int hg=0;
	  int lg=0;

	  if ( destination.hasRaw() )
	    {
	      hg = destination.HG();
	      lg = destination.LG();
	    }
	  destination.SetADCTDC(source.ADC(),source.TDC(),hg,lg);
	}
    }

  // Calib part.

  if ( source.hasCalib() )
    {
      if ( destination.canHaveCalib() == false )
	{
	  return incompatible("copy","canHaveCalib");
	}
      else
	{
	  destination.SetCalibrated(source.Energy(),source.ToF());
	}      
    }

  if ( source.hasGain() )
    {
      // Gain propagation is not considered a must.
      if ( destination.canHaveGain() )
	{
	  destination.SetGain(source.Gain());
	}
    }

  return true;
}

//_____________________________________________________________________________
bool
EmcEmbedTowerMerger::copy(const emcTowerContainer& source,
		     emcTowerContainer& destination)
{
  destination.Reset();

  for ( size_t i = 0; i < source.size(); ++i ) 
    {
      emcTowerContent* src = source.getTower(i);
      assert(src!=0);

      emcTowerContent* dest = destination.addTower(destination.size());
      assert(dest!=0);

      bool ok = copy(*src,*dest);
      if (!ok)
	{
	  destination.Reset();
	  return false;
	}
    }

  return true;
}

//_____________________________________________________________________________
int
EmcEmbedTowerMerger::InitRun(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  const std::string cmergedTopNode = fTopNodeNames[kTOPMERGED];

  PHCompositeNode* mergedTopNode = se->topNode(cmergedTopNode.c_str());
  assert(mergedTopNode!=0);

  emcTowerContainer* tm = 
    findNode::getClass<emcTowerContainer>(mergedTopNode,"emcTowerContainer");

  assert(tm!=0);

  // Check that the container will contain merged towers.
  emcTowerContent* t = tm->addTower(0);
  if ( !t->canHaveMerSimStatus() )
    {
      std::cerr << PHWHERE << " The emcTowerContainer in "
		<< cmergedTopNode << " does not contain the right tower type "
		<< "(merged ones, that is)."
		<< std::endl;
      tm->Reset();
      return 1;
    }

  tm->Reset();

  return 0;
}

//_____________________________________________________________________________
void
EmcEmbedTowerMerger::merge(const emcTowerContent& src,
		      emcTowerContent& dest)
{
  // merge src and dest into dest.
  float energy = src.Energy() + dest.Energy();
  unsigned int deadmap = src.ErrorNeighbours() | dest.ErrorNeighbours();
  unsigned int warnmap = src.WarnNeighbours() | dest.WarnNeighbours();
  float tof = std::min(src.ToF(),dest.ToF());
  
  float simfrac = 0;
  if ( energy > 0 )
    {
      float simenergy = src.SimFrac()*src.Energy() 
	+ dest.SimFrac()*dest.Energy();
      simfrac = simenergy/energy;
    }

  dest.SetNeighbours(deadmap,warnmap);
  dest.SetCalibrated(energy,tof);
  dest.SetSimFrac(simfrac);
}

//_____________________________________________________________________________
void
EmcEmbedTowerMerger::merge(const emcTowerContainer& input1,
                      const emcTowerContainer& input2,
                      emcTowerContainer& result)
{
  result.Reset();

  // copy all towers of input1 into result.
  bool ok = copy(input1, result);

  if (!ok)
    {
      std::cerr << PHWHERE << " Incompatible emcTowerContainers!!!"
 		<< std::endl;
      return;
    }

  for ( size_t i = 0; i < input2.size(); ++i )
    {
      const emcTowerContent* t2 = input2.getTower(i);
      assert(t2 != 0);
      emcTowerContent* tr = result.findTower(t2->towerid());
      if ( tr )
        {
          // merge tr and t2 into result
#ifdef FULLDEBUG
	  std::cout << PHWHERE << " Will merge towers :" << std::endl;
	  t2->print();
	  tr->print();
#endif
	  emcTowerContent* tr2 = tr->create(); // to get same type 
	  copy(*t2,*tr2);
	  merge(*tr2,*tr);
        }
      else
        {
          // add t2 to result
          emcTowerContent* t = result.addTower(result.size());
          // copy t2 into t
          bool ok = copy(*t2, *t);
	  if (!ok)
	    {
	      std::cerr << PHWHERE << "Trying to merge incompatible towers!!!"
			<< std::endl;
	      result.Reset();
	      return;
	    }
        }
    }
}

//_____________________________________________________________________________
int
EmcEmbedTowerMerger::process_event(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  std::vector<emcTowerContainer*> towers;

  for ( size_t i = 0; i < fTopNodeNames.size(); ++i )
    {
      PHCompositeNode* topNode = se->topNode(fTopNodeNames[i].c_str());
      if (!topNode)
        {
          std::cerr << PHWHERE << " Could not find topNode "
		    << fTopNodeNames[i].c_str()
		    << std::endl;
          return ABORTEVENT;
        }

      emcTowerContainer* tc = 
	findNode::getClass<emcTowerContainer>(topNode, "emcTowerContainer");
      assert(tc!=0);

      towers.push_back(tc);
		       
      if (!towers[i])
        {
          std::cerr << PHWHERE << " Could not find emcTowerContainer under "
		    << fTopNodeNames[i].c_str()
		    << std::endl;
          return ABORTEVENT;
        }
    }

  merge(*towers[0], *towers[1], *towers[2]);

#ifdef FULLDEBUG

  // In full debug mode, output 3 files with all the real,simulated and
  // merged towers, for the events in which merging actually took place.
  //
  const char* onames[] = { "real.towers","simu.towers","merged.towers" };

  for ( size_t i = 0; i < 3; ++i )
    {
      std::ofstream os(onames[i],std::ios_base::app|std::ios_base::out);
      os << std::string(80,'-') << std::endl;
      
    }

  if ( towers[2]->size() < towers[0]->size() + towers[1]->size() )
    {
      // some merging occured

      for ( size_t i = 0; i < 3; ++i )
	{
	  std::ofstream os(onames[i],std::ios_base::app|std::ios_base::out);
	  towers[i]->print(os);    
	}
      
      for ( size_t i = 0; i < towers.size(); ++i )
	{
	  std::cout << "towers " << i << " size=" << towers[i]->size()
		    << std::endl;
	}
    }

#endif

  return EVENT_OK;
}
