// The recalreco module corrects the energy scales in pairs of 
// sectors in the EMCal. The functions were derived using 
// pi0 peak positions from the Run4 62.4 GeV data set.
// Author: Peter Tarjan
// $Author: pinkenbu $
// $Date: 2009/06/30 02:08:12 $
// $Name:  $
// $Source: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/framework/preco/EmcAbsEScaleRecalReco.C,v $

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>
#include <PHGlobal.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>

#include <EmcAbsEScaleRecalReco.h>

#include <cassert>
#include <cmath>
#include <fstream>
#include <iostream>


using namespace std;

typedef PHIODataNode <emcClusterContainer> emcClusterContainer_t;


EmcAbsEScaleRecalReco::EmcAbsEScaleRecalReco(const char *name)
{
  ThisName = name;
  return ;
}

int EmcAbsEScaleRecalReco::Init(PHCompositeNode *topNode)
{
  if (verbosity > 0)
    {
      cout << "Calling Init in EmcAbsEScaleRecalReco" << endl;
    }
  // Create Histograms here - later you will have to do file magic
  // to make sure they are not deleted when the input file is closed

return EVENT_OK;
}

int EmcAbsEScaleRecalReco::InitRun(PHCompositeNode *topNode)
{
//   recoConsts *rc = recoConsts::instance();
//   // this rc flag is set by the framework
//   if ( (run = rc->get_IntFlag("RUNNUMBER")) )
//   {
//       cout << "This is run " << run << endl;
//   }
//       return EVENT_OK;
//   }
//   else
//   {
//       cerr << "<E> Run number could not be determined, aborting..." << endl;
//       return ABORTRUN;
//   }

    return EVENT_OK;
}


float EmcAbsEScaleRecalReco::get_correction( int iarm, int isect, float ecore=0){
 
    float corr = 1.;
    // Henner's new corrections as of Aug 20, 2004
    // PbGl has two consecutive corrections, here's the first

    const float a0 = 9.62944e-01;
    const float a1 = 1.35644e-01;
    const float a2 = -1.10904e+00;
    if(iarm==1&&isect<2)
      {
	corr *= a0+a1*exp(a2*ecore);
      }

    // Second correction, same functional form for all sectors
      float ak_0 = 1;
      float ak_1 = 0;
      float ak_2 = 0;

    
    switch(4*arm + sector)
    {
	// W0 & W1
	case 0:
	  break;
	case 1: 
	  ak_0 = 9.87004e-01; 
	  ak_1 = 2.00000e-02;
	  ak_2 =-1.20876e+00;
	  //corr = 0.987 + 0.03*exp(-0.5*pow((ecore/0.719),2)); 
	  break;
	// W2 & W3
	case 2:
	  ak_0 = 9.67663e-01; 
	  ak_1 = 2.00000e-02;
	  ak_2 =-9.76488e-01;
	  break;
	case 3: 
	  ak_0 = 9.61509e-01;
	  ak_1 = 4.00000e-02;
	  ak_2 =-1.42812e+00;  
	  //corr = 0.952 + 0.04*exp(-0.5*pow((ecore/0.697),2)); 
	  break;
	// E0 & E1: PbGl, don't do anything
	case 4: 
	case 5: 	  break;
	// E2 & E3
	case 6:
	  ak_0 = 9.39270e-01; 
	  ak_1 = 6.03399e-02;
	  ak_2 = -1.60249e+00;
	  break;
	case 7: 
	  ak_0 = 9.41385e-01;
	  ak_1 = 4.37485e-02;
	  ak_2 =-1.03802e+00;
	  break;
	default: cerr << "<E> " << __FILE__ << " : " << __LINE__ 
		      << " : Invalid arm/sector: " << arm 
		      << "/" << sector << endl; 
	    return -9999.0;
    }
    corr *= ak_0+ak_1*exp(ak_2*ecore);

    return corr;
}


int EmcAbsEScaleRecalReco::process_event(PHCompositeNode *topNode)
{
  if (verbosity > 1)
    {
      cout << "Calling process_event in EmcAbsEScaleRecalReco......." << endl;
    }
  int iret = EVENT_OK;


  emcClusterContainer *clusters = NULL;
  PHTypedNodeIterator<emcClusterContainer> clusteriter(topNode);
  emcClusterContainer_t *emcClusterContainerNode = 
      clusteriter.find("emcClusterContainer");
  
  if(emcClusterContainerNode)
  {
      clusters = emcClusterContainerNode->getData();
  }
  else
  {
      cout << PHWHERE << "emcClusterContainer Node missing, aborting" << endl;
      return ABORTRUN;
  }
  
  float ecore=-1;

  if ( !clusters->isValid() ) return DISCARDEVENT;
  
  for (size_t i = 0; i<clusters->size();i++)
    {
      emcClusterContent *clus = clusters->getCluster(i);
      ecore      = clus->ecore();
      arm        = (int)clus->arm();
      sector     = (int)clus->sector();

      float corr = get_correction(arm, sector, ecore);
      assert(corr>-9998);
      
      clus->set_ecore(ecore*corr);
    }
  return iret;
    
}

int EmcAbsEScaleRecalReco::End(PHCompositeNode *topNode)
{
  if (verbosity > 0)
    {
      cout << "Calling End" << endl;
    }
  return EVENT_OK;
}

int EmcAbsEScaleRecalReco::Reset(PHCompositeNode *topNode)
{
  if (verbosity > 0)
    {
      cout << "Calling Reset" << endl;
    }
  return EVENT_OK;
}
int EmcAbsEScaleRecalReco::ResetEvent(PHCompositeNode *topNode)
{
  if (verbosity > 0)
    {
      cout << "Calling ResetEvent" << endl;
    }
  return EVENT_OK;
}

// $Log: EmcAbsEScaleRecalReco.C,v $
// Revision 1.4  2009/06/30 02:08:12  pinkenbu
// assert needs cassert include
//
// Revision 1.3  2004/09/20 02:07:23  dave
// include cmath to pick up some funcs
//
// Revision 1.2  2004/08/20 17:35:52  david
// Replacing E corrections with Henners new parametrization
//
// Revision 1.1  2004/05/28 23:56:02  ptarjan
// Added EmcAbsEScaleRecalReco
//
