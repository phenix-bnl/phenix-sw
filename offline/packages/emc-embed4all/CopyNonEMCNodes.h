/*
 * copies non-emcal node tree. used in embedding to 
 * keep the other nodes (vtx, flags, etc) in the merged
 * output.
 *
 */


#ifndef __COPYNONEMCNODES_H__
#define __COPYNONEMCNODES_H__

#include <set>
#include <string>

#include <Rtypes.h>

#include <SubsysReco.h>




class CopyNonEMCNodes: public SubsysReco {
public:
  CopyNonEMCNodes(PHCompositeNode * from, const char * name = "CopyNonEMCNodes");
  virtual ~CopyNonEMCNodes(){};
  

public:
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);


protected:
  PHCompositeNode * from;
  std::set<std::string> banlist;


  ClassDef(CopyNonEMCNodes, 0)
};


#endif /* !__COPYNONEMCNODES_H__*/
