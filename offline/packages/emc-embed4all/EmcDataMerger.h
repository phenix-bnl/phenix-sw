/*
 * this class merges a set of EmcTables to another
 * set of tables.  used durring embedding.
 *
 */

#ifndef __EMC_DATAMERGER_H__
#define __EMC_DATAMERGER_H__


#include <string>
#include <list>
#include <map>

#include <Rtypes.h>

#include <SubsysReco.h>
#include <PHCompositeNode.h>

#include <emctypes.h>


class emcGeaTrackContainer;
class emcGeaTowerContainer;


class EmcDataMerger: public SubsysReco {
public:
  EmcDataMerger();
  virtual ~EmcDataMerger();



public:
  virtual int InitRun(PHCompositeNode * root);
  virtual int ResetEvent(PHCompositeNode * root);
  virtual int Reset(PHCompositeNode * root){ return ResetEvent(root); }
  virtual int process_event(PHCompositeNode * root);



public:
  void AddSourceNode(const std::string name){ srcnodes.push_back( name ); /*setname();*/ }



protected:
  int merge(std::string srcname, emcGeaTrackContainer * src, emcGeaTrackContainer * dest);
  int merge(std::string srcname, emcGeaTowerContainer * src, emcGeaTowerContainer * dest);



protected:
  std::list< std::string > srcnodes;   /// name of source topnodes
  bool seentracks, seentowers;         /// what nodes do we manage


  // these structures kept durring the lifetime of an object
  typedef std::pair<std::string, int> my_input_t;
  std::map< std::pair<std::string, int>, int> filenomap; /// maps fileno for geatrack


  // these structures are cleared for every event
  std::map<std::string, emc_trkno_t> trknooffs; /// offset added to trkno-s in this input


  ClassDef(EmcDataMerger, 0)
};



#endif
