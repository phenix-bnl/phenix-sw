#ifndef __NODEEMCEMBED_H
#define __NODEEMCEMBED_H

#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "PHIODataNode.h"

#ifdef MAIN
PHCompositeNode* topNode;    // *  Working top node containing (in the following order):
PHCompositeNode* emcNodeMix; // 1) Memory node for EMC clustering (called "EMC")
PHCompositeNode* evaNodeOut; // 2) Persistent output node after EMC merging & evaluation (called "EVA")
                             // 3) Real DST (dstNodeReal) passed by pdst (called "DST")

PHCompositeNode* topNodeSim; // *  Top node for Simulated input:
PHCompositeNode* dstNodeSim; // 1) Simulated DST opened by embed via dfileopen2(char *)
PHCompositeNode* evaNodeIn;  // 2) rawrel.root contains the GEANT relational tables (input kin.)    

//PHCompositeNode* parNode;
//PHCompositeNode* evaNode; 
//PHCompositeNode* geaNode;

#else
extern PHCompositeNode* topNode;    // *  Working top node containing (in the following order):
extern PHCompositeNode* emcNodeMix; // 1) Memory node for EMC clustering (called "EMC")
extern PHCompositeNode* evaNodeOut; // 2) Persistent output node after EMC merging & evaluation (called "EVA")
                                    // 3) Real DST (dstNodeReal) passed by pdst (called "DST")

extern PHCompositeNode* topNodeSim; // *  Top node for Simulated input:
extern PHCompositeNode* dstNodeSim; // 2) Simulated DST opened by embed via dfileopen2(char *)
extern PHCompositeNode* evaNodeIn;  // 1) rawrel.root contains the GEANT relational tables (input kin.)    
//extern PHCompositeNode* parNode; 
//extern PHCompositeNode* evaNode; 
//extern PHCompositeNode* geaNode; 
#endif

#endif // __NODEEMCEMBED_H
