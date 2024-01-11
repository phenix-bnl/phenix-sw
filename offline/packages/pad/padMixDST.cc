//--------------------------------------------------- 
// Class: padMixDST (implementation)
// 
// Created by: David Silvermyr
// 
// Description: 
// see .hh file
//
//--------------------------------------------------- 

#include <padMixDST.hh>

#include <dPadClusterWrapper.h>

#include <PHNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTable.hh>

#include <cmath>
#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<TObject> ObjectNode_t;

extern "C" float utiRandom(long*);
//**************************************************************************
// Constructor for padMixDST
padMixDST::padMixDST(long seedval)
{

  Debug = 0; // do not Debug

  StoreOnNode=-1; // do not overwrite node 0 or 1
  IgnoreOverlap=-1; // do not allow overlaps from node 0 and 1

  // if another hit is less than WireCut*Type away in wire-space (same goes in s-space), 
  // they are treated as overlapping and of them is removed.
  WireCut=0.5; 
  ZCut=0.5;

  // set cluster type default limits
  minClusterType = 1; // needs to be at least one cell in the cluster, and no splitting
  maxClusterType = 137; // no clusters larger than a ROC (106*58/45). Not 1/fine-structure-constant..

  utiseed = seedval;

  // init counters (reset every time event & compare are called respectively)
  for (int i=0; i<2; i++) {
    for (int j=0; j<3; j++) {
      numAccHits[i][j]=0;
      removedHits[i][j]=0;
    }
  }
}  // end method padMixDST::padMixDST
//**************************************************************************
// Destructor for padMixDST
padMixDST::~padMixDST() {
}  // end method padMixDST::~padMixDST
//**************************************************************************
// Event method for padMixDST
PHBoolean padMixDST::event(PHCompositeNode* topNode0,PHCompositeNode* topNode1) {

  if (Debug > 0)
    cout << "padMixDST::event: Executing...\n";

  // unpack
  PHBoolean status;
  status = getInfo(topNode0,0);
  if (!status) {
    cerr << "padMixDST::event: Could not get info from node 0\n";
    return False;
  }
  status = getInfo(topNode1,1);
  if (!status) {
    cerr << "padMixDST::event: Could not get info from node 1\n";
    return False;
  }

  // merge?
  if (IgnoreOverlap!=1) {
    status = compare01();
    if (!status) {
      cerr << "padMixDST::event: Could not compare node01 info (remove overlaps)\n";
      return False;
    }
  }

  // store/overwrite node?
  if ( (StoreOnNode==0) || (StoreOnNode==1) ) {
    if (StoreOnNode==0) status = setInfo(topNode0,0);
    else status = setInfo(topNode1,1);

    if (!status) {
      cerr << "padMixDST::event: Could not store/overwrite node " << StoreOnNode << " info \n";
      return False;
    }
  }
  return status;
}
//**************************************************************************
PHBoolean padMixDST::getInfo(PHCompositeNode* topNode, short nodeindex) {

  if (Debug > 0)
    cout << "padMixDST::getInfo: Executing node " << nodeindex << "...\n";

  PHNodeIterator topIter(topNode);

  TableNode_t *dPc1ClusterNode;
  dPadClusterWrapper *dPc1Cluster;
  TableNode_t *dPc2ClusterNode;
  dPadClusterWrapper *dPc2Cluster;
  TableNode_t *dPc3ClusterNode;
  dPadClusterWrapper *dPc3Cluster;

  //***********************************************************************
  // Find the input tables
  dPc1ClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc1Cluster"));
  if (!dPc1ClusterNode) {
    cout << "padMixDST::event ERROR: dPc1Cluster not found.\n";
    return False;
  }
  dPc1Cluster = static_cast<dPadClusterWrapper*>(dPc1ClusterNode->getData());
  
  dPc2ClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc2Cluster"));
  if (!dPc2ClusterNode) {
    cout << "padMixDST::event ERROR: dPc2Cluster not found.\n";
    return False;
  }
  dPc2Cluster = static_cast<dPadClusterWrapper*>(dPc2ClusterNode->getData());

  dPc3ClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc3Cluster"));
  if (!dPc3ClusterNode) {
    cout << "padMixDST::event ERROR: dPc3Cluster not found.\n";
    return False;
  }
  dPc3Cluster = static_cast<dPadClusterWrapper*>(dPc3ClusterNode->getData());

  // number of entries in each table
  if (Debug > 1) {
    cout << "padMixDST::getInfo: dPcXCluster rowcount (PC1,2,3) \n";
    cout << dPc1Cluster->RowCount() << " \t" << dPc2Cluster->RowCount() << " \t" << dPc3Cluster->RowCount() << endl;
  }

  // zero counters
  numAccHits[nodeindex][0] = 0;
  numAccHits[nodeindex][1] = 0;
  numAccHits[nodeindex][2] = 0;

  int j,ipc;

  ipc=0; // PC1
  j=0;  
  for (unsigned i=0; i<dPc1Cluster->RowCount(); i++) {
    if ( (dPc1Cluster->get_type(i)>=minClusterType) && (dPc1Cluster->get_type(i)<=maxClusterType) ) {
      if (Debug>1) printf(" PC1 cluster i=%u x= %f y= %f z= %f\n",i,dPc1Cluster->get_xyz(0,i),dPc1Cluster->get_xyz(1,i),dPc1Cluster->get_xyz(2,i)); 

      padClust[nodeindex][ipc][j].id=dPc1Cluster->get_id(i);
      padClust[nodeindex][ipc][j].arm=dPc1Cluster->get_arm(i);
      padClust[nodeindex][ipc][j].sector=dPc1Cluster->get_sector(i);
      padClust[nodeindex][ipc][j].wire=dPc1Cluster->get_wire(i);
      padClust[nodeindex][ipc][j].cell=dPc1Cluster->get_cell(i);
      for (int l=0; l<3; l++) {
	padClust[nodeindex][ipc][j].xyz[l]=dPc1Cluster->get_xyz(l,i);
	padClust[nodeindex][ipc][j].dxyz[l]=dPc1Cluster->get_dxyz(l,i);
      }
      padClust[nodeindex][ipc][j].type=dPc1Cluster->get_type(i);
      padClust[nodeindex][ipc][j].accepted=1;
      j++;      
    } // type acceptable
  } // i loop 
  numAccHits[nodeindex][ipc] = j;

  ipc=1; // PC2
  j=0;
  for (unsigned i=0; i<dPc2Cluster->RowCount(); i++) {
    if ( (dPc2Cluster->get_type(i)>=minClusterType) && (dPc2Cluster->get_type(i)<=maxClusterType) ) {
      if (Debug>1) printf(" PC2 cluster i=%u x= %f y= %f z= %f\n",i,dPc2Cluster->get_xyz(0,i),dPc2Cluster->get_xyz(1,i),dPc2Cluster->get_xyz(2,i)); 

      padClust[nodeindex][ipc][j].id=dPc2Cluster->get_id(i);
      padClust[nodeindex][ipc][j].arm=dPc2Cluster->get_arm(i);
      padClust[nodeindex][ipc][j].sector=dPc2Cluster->get_sector(i);
      padClust[nodeindex][ipc][j].wire=dPc2Cluster->get_wire(i);
      padClust[nodeindex][ipc][j].cell=dPc2Cluster->get_cell(i);
      for (int l=0; l<3; l++) {
	padClust[nodeindex][ipc][j].xyz[l]=dPc2Cluster->get_xyz(l,i);
	padClust[nodeindex][ipc][j].dxyz[l]=dPc2Cluster->get_dxyz(l,i);
      }
      padClust[nodeindex][ipc][j].type=dPc2Cluster->get_type(i);
      padClust[nodeindex][ipc][j].accepted=1;
      j++;      
    } // type acceptable
  } // i loop 
  numAccHits[nodeindex][ipc] = j;

  ipc=2; // PC3
  j=0;
  for (unsigned i=0; i<dPc3Cluster->RowCount(); i++) {
    if ( (dPc3Cluster->get_type(i)>=minClusterType) && (dPc3Cluster->get_type(i)<=maxClusterType) ) {
      if (Debug>1) printf(" PC3 cluster i=%u x= %f y= %f z= %f\n",i,dPc3Cluster->get_xyz(0,i),dPc3Cluster->get_xyz(1,i),dPc3Cluster->get_xyz(2,i)); 

      padClust[nodeindex][ipc][j].id=dPc3Cluster->get_id(i);
      padClust[nodeindex][ipc][j].arm=dPc3Cluster->get_arm(i);
      padClust[nodeindex][ipc][j].sector=dPc3Cluster->get_sector(i);
      padClust[nodeindex][ipc][j].wire=dPc3Cluster->get_wire(i);
      padClust[nodeindex][ipc][j].cell=dPc3Cluster->get_cell(i);
      for (int l=0; l<3; l++) {
	padClust[nodeindex][ipc][j].xyz[l]=dPc3Cluster->get_xyz(l,i);
	padClust[nodeindex][ipc][j].dxyz[l]=dPc3Cluster->get_dxyz(l,i);
      }
      padClust[nodeindex][ipc][j].type=dPc3Cluster->get_type(i);
      padClust[nodeindex][ipc][j].accepted=1;
      j++;      
    } // type acceptable
  } // i loop 
  numAccHits[nodeindex][ipc] = j;

  return True; // in case we made it all the way here, we must have succeded.. 
}  // end method padMixDST::getInfo
//**************************************************************************
PHBoolean padMixDST::compare01() {

  if (Debug > 0)
    cout << "padMixDST::compare01 (remove overlaps) \n";

  int ipc,i,j;
  short wirediff,zdiff;
  float wirecover,zcover;
  float rndmval;

  for (ipc=0; ipc<3; ipc++) { // PC1,2,3
    removedHits[0][ipc]=0;
    removedHits[1][ipc]=0;

    for (i=0; i<numAccHits[0][ipc]; i++) { // src 0
      for (j=0; j<numAccHits[1][ipc]; j++) { // src 1

	// arm & sector must match if there should be a reasonable chance for a merge
	if ( (padClust[0][ipc][i].arm==padClust[1][ipc][j].arm) &&
	     (padClust[0][ipc][i].sector==padClust[1][ipc][j].sector) ) {

	  wirediff=padClust[0][ipc][i].wire-padClust[1][ipc][j].wire;
	  zdiff=padClust[0][ipc][i].cell-padClust[1][ipc][j].cell;

	  if (wirediff<0) wirediff=-wirediff;
	  if (zdiff<0) zdiff=-zdiff;

	  zcover=(padClust[0][ipc][i].type+padClust[1][ipc][j].type)*ZCut;
	  wirecover=(padClust[0][ipc][i].type+padClust[1][ipc][j].type)*WireCut;

	  if (Debug>2) 
	    printf("zdiff= %d wirediff=%d zcover=%f wirecover=%f\n",zdiff,wirediff,zcover,wirecover);

	  if ( (zdiff<zcover) && (wirediff<wirecover) && (padClust[0][ipc][i].accepted==1) && (padClust[1][ipc][j].accepted==1) ) { 
	    // one of these guys have to go, we'll let (pseudo)chance decide..
	    rndmval =  utiRandom(&utiseed);

	    if (Debug>1) {
	      printf("i=%d j=%d rndmval=%f\n",i,j,rndmval);
	      printf("zdiff= %d wirediff=%d zcover=%f wirecover=%f\n",zdiff,wirediff,zcover,wirecover);	    }

	    if (rndmval<0.5) {
	      padClust[0][ipc][i].accepted--;
	      removedHits[0][ipc]++;
	    }
	    else {
	      padClust[1][ipc][j].accepted--;
	      removedHits[1][ipc]++;
	    }
	  }
	}
      }
    }
    if (Debug>0) {
      cout << "padMixDST::compare01 PC" << ipc+1 << endl;
      cout << "padMixDST::compare01 Removed src0 " << removedHits[0][ipc] << endl;
      cout << "padMixDST::compare01 Removed src1 " << removedHits[1][ipc] << endl;
    }
  }
  return True;
}  // end method padMixDST::compare01
//**************************************************************************
PHBoolean padMixDST::setInfo(PHCompositeNode* topNode, short nodeindex) {

  if (Debug > 0)
    cout << "padMixDST::setInfo: Executing node " << nodeindex << "...\n";

  PHNodeIterator topIter(topNode);

  TableNode_t *dPc1ClusterNode;
  dPadClusterWrapper *dPc1Cluster;
  TableNode_t *dPc2ClusterNode;
  dPadClusterWrapper *dPc2Cluster;
  TableNode_t *dPc3ClusterNode;
  dPadClusterWrapper *dPc3Cluster;

  //***********************************************************************
  // Find the tables
  dPc1ClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc1Cluster"));
  if (!dPc1ClusterNode) {
    cout << "padMixDST::event ERROR: dPc1Cluster not found.\n";
    return False;
  }
  dPc1Cluster = static_cast<dPadClusterWrapper*>(dPc1ClusterNode->getData());
  
  dPc2ClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc2Cluster"));
  if (!dPc2ClusterNode) {
    cout << "padMixDST::event ERROR: dPc2Cluster not found.\n";
    return False;
  }
  dPc2Cluster = static_cast<dPadClusterWrapper*>(dPc2ClusterNode->getData());

  dPc3ClusterNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode","dPc3Cluster"));
  if (!dPc3ClusterNode) {
    cout << "padMixDST::event ERROR: dPc3Cluster not found.\n";
    return False;
  }
  dPc3Cluster = static_cast<dPadClusterWrapper*>(dPc3ClusterNode->getData());

  int isrc,ipc,i,j,ntotal;

  ipc=0; // PC1
  i=0;
  ntotal=numAccHits[0][ipc]+numAccHits[1][ipc]-removedHits[0][ipc]-removedHits[1][ipc];
  dPc1Cluster->SetRowCount(ntotal);
  for (isrc=0; isrc<=1; isrc++) {
    for (j=0; j<numAccHits[isrc][ipc]; j++) {
      if (padClust[isrc][ipc][j].accepted==1) { // not removed
	dPc1Cluster->set_id(i,padClust[isrc][ipc][j].id);
	dPc1Cluster->set_arm(i,padClust[isrc][ipc][j].arm);
	dPc1Cluster->set_sector(i,padClust[isrc][ipc][j].sector);
	dPc1Cluster->set_wire(i,padClust[isrc][ipc][j].wire);
	dPc1Cluster->set_cell(i,padClust[isrc][ipc][j].cell);
	for (int l=0; l<3; l++) {
	  dPc1Cluster->set_xyz(i,l,padClust[isrc][ipc][j].xyz[l]);
	  dPc1Cluster->set_dxyz(i,l,padClust[isrc][ipc][j].dxyz[l]);
	}
	dPc1Cluster->set_type(i,padClust[isrc][ipc][j].type);
	i++;
      }
    }
  }

  ipc=1; // PC2
  i=0;
  ntotal=numAccHits[0][ipc]+numAccHits[1][ipc]-removedHits[0][ipc]-removedHits[1][ipc];
  dPc2Cluster->SetRowCount(ntotal);
  for (isrc=0; isrc<=1; isrc++) {
    for (j=0; j<numAccHits[isrc][ipc]; j++) {
      if (padClust[isrc][ipc][j].accepted==1) { // not removed
	dPc2Cluster->set_id(i,padClust[isrc][ipc][j].id);
	dPc2Cluster->set_arm(i,padClust[isrc][ipc][j].arm);
	dPc2Cluster->set_sector(i,padClust[isrc][ipc][j].sector);
	dPc2Cluster->set_wire(i,padClust[isrc][ipc][j].wire);
	dPc2Cluster->set_cell(i,padClust[isrc][ipc][j].cell);
	for (int l=0; l<3; l++) {
	  dPc2Cluster->set_xyz(i,l,padClust[isrc][ipc][j].xyz[l]);
	  dPc2Cluster->set_dxyz(i,l,padClust[isrc][ipc][j].dxyz[l]);
	}
	dPc2Cluster->set_type(i,padClust[isrc][ipc][j].type);
	i++;
      }
    }
  }

  ipc=2; // PC3
  i=0;
  ntotal=numAccHits[0][ipc]+numAccHits[1][ipc]-removedHits[0][ipc]-removedHits[1][ipc];
  dPc3Cluster->SetRowCount(ntotal);
  for (isrc=0; isrc<=1; isrc++) {
    for (j=0; j<numAccHits[isrc][ipc]; j++) {
      if (padClust[isrc][ipc][j].accepted==1) { // not removed
	dPc3Cluster->set_id(i,padClust[isrc][ipc][j].id);
	dPc3Cluster->set_arm(i,padClust[isrc][ipc][j].arm);
	dPc3Cluster->set_sector(i,padClust[isrc][ipc][j].sector);
	dPc3Cluster->set_wire(i,padClust[isrc][ipc][j].wire);
	dPc3Cluster->set_cell(i,padClust[isrc][ipc][j].cell);
	for (int l=0; l<3; l++) {
	  dPc3Cluster->set_xyz(l,i,padClust[isrc][ipc][j].xyz[l]);
	  dPc3Cluster->set_dxyz(l,i,padClust[isrc][ipc][j].dxyz[l]);
	}
	dPc3Cluster->set_type(i,padClust[isrc][ipc][j].type);
	i++;
      }
    }
  }
  
  return True; // in case we made it all the way here, we must have succeded.. 
}  // end method padMixDST::event
//**************************************************************************
void padMixDST::print() {

  cout << "padMixDST::print \n";

  // setup info
  cout << "Debug = " << Debug << "\n";
  
  // rec. info
  cout << "numAccSrc0Hits (PC1,2,3)= " << numAccHits[0][0] << ", " << numAccHits[0][1] << ", " << numAccHits[0][2] << "\n";
  cout << "numAccSrc1Hits (PC1,2,3)= " << numAccHits[1][0] << ", " << numAccHits[1][1] << ", " << numAccHits[1][2] << "\n";

  cout << "removedSrc0Hits (PC1,2,3)= " << removedHits[0][0] << ", " << removedHits[0][1] << ", " << removedHits[0][2] << "\n";
  cout << "removedSrc1Hits (PC1,2,3)= " << removedHits[1][0] << ", " << removedHits[1][1] << ", " << removedHits[1][2] << "\n";

  cout << "remainingSrc0Hits (PC1,2,3)= " << numAccHits[0][0]-removedHits[0][0] << ", " << numAccHits[0][1]-removedHits[0][1] << ", " << numAccHits[0][2]-removedHits[0][2] << "\n";
  cout << "remainingSrc1Hits (PC1,2,3)= " << numAccHits[1][0]-removedHits[1][0] << ", " << numAccHits[1][1]-removedHits[1][1] << ", " << numAccHits[1][2]-removedHits[1][2] << "\n";

  cout << endl;

}  // end method padMixDST::print
//**************************************************************************
