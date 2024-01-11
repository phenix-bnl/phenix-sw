//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
// 
// Copyright (C) PHENIX collaboration, 2000 
//
// Implementation of class : mEmcRealEvent.h
//
// Hisayuki Torii
//-------------------------------------------------------------------------
#include "mEmcRealEventModule.h"
#include "VtxOut.h"
#include "dEmcEventWrapper.h"

#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHDataNodeIterator.h"

#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

typedef PHIODataNode<VtxOut> VtxOutNode_t;

static int ievno = 0;
int j;

PHBoolean mEmcRealEventModule::event(PHCompositeNode *root) 
{  
  
  // Set up dEmcEvent
  PHNodeIterator i(root);
  PHIODataNode<PHTable>* dEmcEventNode = (PHIODataNode<PHTable>*)i.findFirst("PHIODataNode","dEmcEvent");
  dEmcEventWrapper * dEmcEvent = static_cast<dEmcEventWrapper*>(dEmcEventNode->getData());

  float z_value = 0;
  PHTypedNodeIterator<VtxOut> vtxiter(root);
  VtxOutNode_t *VtxOutNode = vtxiter.find("VtxOut");
  if (VtxOutNode)
    {
      vtxout = VtxOutNode->getData();
      vertex = vtxout->get_Vertex();
      z_value = vertex.getZ();
    }
  else
    {
      cout << PHWHERE << "No VtxOut Object, using Z-Vtx = 0" << endl;
    }
  for( j = 0; j < 2; j++)
    {
      dEmcEvent->set_xyz(j,0,0.0) ;
    }

  if(fabs(z_value)>100.0) 
    {
      z_value = 100.0;
    }

  dEmcEvent->set_xyz(2,0,z_value) ;

  // Fill the rest for completeness
  ievno++;
  

  dEmcEvent->set_id(0,1);
  dEmcEvent->set_evtyp(0,2);  /* BEAM */
  dEmcEvent->set_evno(0,0);
  dEmcEvent->set_runno(0,0);
  dEmcEvent->set_serialno(0,0);
  dEmcEvent->set_impact(0,0.0);
  dEmcEvent->set_twrmultlo(0,0.0);
  dEmcEvent->set_twrmulthi(0,0.0);
  for ( j = 0; j < 3; j++)
    {
      dEmcEvent->set_trigsum(j,0,0.0);
    }
  dEmcEvent->set_tote(0,0.0);
  dEmcEvent->set_totet(0,0.0);
 
  dEmcEvent->SetRowCount(1);


  return true;
  
}
//-------------------------------------------------------------------------
