/*
 * class that does the embedding: reads in two datasets,
 * merges them, unclusterizes and reclusterizes the merged
 * datasets.
 *
 * you can add to the processing chain additional modules
 * to be run after the input datasets are imported but before
 * they are merged (e.g. calibrators, afterburners etc). to do
 * that use EmcEmbedDriver2::realimporter()->push_back() and
 * EmcEmbedDriver2::simimporter()->push_back().
 *
 */


#ifndef __EMC_EMBEDDRIVER2_H__
#define __EMC_EMBEDDRIVER2_H__

#include <string>

#include <Rtypes.h>

#include <SubsysRecoStack.h>
#include <mEmcGeometryModule.h>





class EmcEmbedDriver2: public SubsysRecoStack {
public:
  typedef enum {
    REAL = 1, SIM = 2, MERGED = 4, 
    INPUT = REAL | SIM, ALL = INPUT | MERGED 
  } calib_t;

  typedef enum {
    LIBEMCV1 = 1, LIBEMCV2 = 2
  } inputtype_t;



public:
  EmcEmbedDriver2(std::string realnode = "REAL", std::string simnode = "SIM");
  virtual ~EmcEmbedDriver2();

  void setcopy(bool copy = true){ this->copynonemc = copy; }
  void setinputtype(inputtype_t real = LIBEMCV1, inputtype_t sim = LIBEMCV2);
  void setcalib(calib_t calib = INPUT);
  void setgeomrealm(mEmcGeometryModule::ERealm realm = mEmcGeometryModule::kReal);

  SubsysRecoStack * realimporter(){ return inited ? realimp : tmprealimp; }
  SubsysRecoStack * simimporter(){ return inited ? simimp : tmpsimimp; }


public:
  int Init(PHCompositeNode * root);
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);


protected:
  inputtype_t realtype, simtype;

  std::string realnode;
  std::string simnode;
  calib_t calib;
  bool copynonemc;
  
  mEmcGeometryModule::ERealm realm;
  mEmcGeometryModule * geom;

  SubsysRecoStack * tmprealimp, * tmpsimimp;
  SubsysRecoStack * realimp, * simimp;

  bool firstevent;
  bool inited;

  
  ClassDef(EmcEmbedDriver2, 0)
};


#endif /* !__EMC_EMBEDDRIVER2_H__*/
