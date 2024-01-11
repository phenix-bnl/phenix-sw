//////////////////////////////////////////////////////////////////////////////////
//
// calculates detector response for simulated events. rewrote to fit into 
// new version of emc Fri Aug 22 01:26:51 EDT 2003-2008 mazsi
//
// old descriptions:
//
// 
//
//  This is the core module in calculating detector response for
//  simulated data.  It is fairly complicated and has lots of
//  functionality, governed by the dEmcRespPar parameter table.
//  If you don't know what you are doing darned well, stick to
//  the default values...
//  Detailed Documentation:
//  http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99
//  @author Gabor David \URL{mailto:david@bnl.gov}
//  @version 1.0
//
//
// ***************************************************  
//
//	Drastic changes, based on PISORP EMC_USER_NEW
//	Dec 19, 1997 G. David
//
//	August 95, G. David
//
//	Main steering routine for EMC in PISORP
//
//	The code has been converted to C on Feb 27, 1998.
//	By : 
//		PhoolChand,		phool@phenix.barc.ernet.in
//		Dipanwita Dutta,	dipa@phenix.barc.ernet.in
//
//	Updated with changes since Jan 98 in memcgeamakeraw.F
//	and put in the repository  May 21, 98 G. David
//
//	Updated Sep. 7, 98 - G. David
//	        (Cleanup, comments, fkin search dropped for dio_ptrkstack,
//
//	Updated Sep. 20, 98 - G. David
//
//
//////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#define NODEBUG(format...) {}
#define SIMPLEDEBUG(format...) { printf(format); fflush(stdout);}
#define VERBDEBUG(format...) { printf("%d:%s: ", __LINE__, __PRETTY_FUNCTION__); printf(format); printf("\n"); fflush(stdout); }

#define DEBUG NODEBUG
#define IODEBUG NODEBUG
#define CUMDEBUG NODEBUG
#define DISTDEBUG NODEBUG
#define RESPDEBUG NODEBUG


#include <string.h>
#include <math.h>

#include <iostream>
#include <ostream>
#include <cassert>
#include <algorithm>

#include <CachedFunction.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <Fun4AllReturnCodes.h>

#include <emcNodeHelper.h>
#include <emcGeaParams.h>
#include <emcGeaGeometry.h>
#include <emcGeaEdep.h>
#include <emcGeaEdepContainer.h>

#include <emcGeaDeposit.h>
#include <emcGeaDepositContainer.h>

#include <EmcIndexer.h>
#include <dio_trk.hh>

#include <EmcGeaRawDataSimMaker.h>


ClassImp(EmcGeaRawDataSimMaker);


using namespace std;






#ifndef max
#define	max(x,y)	( ( (x) > (y)  ) ? (x) : (y) )
#endif


#ifndef min
#define	min(x,y)	( ( (x) < (y)  ) ? (x) : (y) )
#endif






namespace {

  class PulseShape: public my_unary_function< float, double > {
  public:
    PulseShape(double alpha, double tdecay){
      this->alpha = alpha;
      this->tdecay = tdecay;
      norm = pow(alpha/tdecay, alpha) * exp(- alpha);
    }

    double operator()(float x){
      return ::pow(x, alpha) * exp( -x * tdecay) / norm;
    }

    double alpha;
    double tdecay;
    double norm;
  };


  
  // TODO: for the time being this object never gets freed. but it is 
  // only one object per program execution.
  // TODO: make it auto_ptr
  CachedFunction<float, double> * testfunction = NULL;

}

// Default constructor and destructor to pacify CINT
EmcGeaRawDataSimMaker::EmcGeaRawDataSimMaker(pbsc_timing_t pbsc, pbgl_timing_t pbgl): SubsysReco("EmcGeaRawDataSimMaker"){
  pbsc_timing = pbsc;
  pbgl_timing = pbgl;
}



EmcGeaRawDataSimMaker::~EmcGeaRawDataSimMaker(){}



int EmcGeaRawDataSimMaker::Init(PHCompositeNode * root){
  /*
   * just make sure, that setting memory to hex 0x00 really represent 0
   */

#define TESTMEMSET(_type) 			\
  {						\
    _type data[8];				\
    memset(data, 0, sizeof(data) );		\
    assert(data[0] == 0);			\
  }

  TESTMEMSET(int);
  TESTMEMSET(float);
  TESTMEMSET(double);

#undef TESTMEMSET     
  return 0;
}  





int EmcGeaRawDataSimMaker::InitRun(PHCompositeNode * root){
  // check input
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  emcGeaParams * geaparms = getGeaObject(emcGeaParams, emcGeaParams, "emcGeaParams", dstnode);
  EMCNODEASSERT( geaparms );

  emcGeaGeometry * geom = getGeaObject(emcGeaGeometry, emcGeaGeometry, "emcGeaGeometry", dstnode);
  EMCNODEASSERT( geom );

  emcGeaEdepContainer * edeps = getGeaObject(emcGeaEdepContainer, emcGeaEdepContainer, "emcGeaEdepContainer", emcnode);
  EMCNODEASSERT( edeps );



  // create  output
  emcGeaDepositContainer * deposits = emcGeaDepositContainer::createdef();
  emcNodeHelper::insertObject< emcGeaDepositContainer >(dstnode, deposits, "emcGeaDepositContainer");



  // create cached function
  if(testfunction == NULL)
    //testfunction = new CachedFunction<float, double>(new PulseShape((*geapar)[0].tfac, (*geapar)[0].decaytime), 0.0, 0.0005);
    testfunction = new CachedFunction<float, double>(new PulseShape((*geaparms)[0].tfac, 0.5), 0.0, 0.0005); // hardwired 0.5

  return 0;
}











int EmcGeaRawDataSimMaker::Reset(PHCompositeNode * root){

  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  emcGeaDepositContainer * deposits = getGeaObject(emcGeaDepositContainer, emcGeaDepositContainer, "emcGeaDepositContainer", dstnode);
  EMCNODEASSERT( deposits );
  deposits->Reset();

  return 0;
}




int EmcGeaRawDataSimMaker::process_event(PHCompositeNode *root) {

  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );


  emcGeaParams * geaparms = getGeaObject(emcGeaParams, emcGeaParams, "emcGeaParams", dstnode);
  EMCNODEASSERT( geaparms );

  emcGeaGeometry * geom = getGeaObject(emcGeaGeometry, emcGeaGeometry, "emcGeaGeometry", dstnode);
  EMCNODEASSERT( geom );

  emcGeaEdepContainer * edeps = getGeaObject(emcGeaEdepContainer, emcGeaEdepContainer, "emcGeaEdepContainer", emcnode);
  EMCNODEASSERT( edeps );

  // If Cherenkov photons were generated in PISA, disallow user to steer PbGl response
  if(geaparms->operator[](0).emc_response_option == 1.0) pbgl_timing = PBGL_TIMING_CHERENKOV;

  emcGeaDepositContainer * deposits = getGeaObject(emcGeaDepositContainer, emcGeaDepositContainer, "emcGeaDepositContainer", dstnode);
  EMCNODEASSERT( deposits );


  DEBUG("looping over %d entries\n", edeps->size());
  if( edeps->size() == 0 ) return 0;

  

  //
  // first pass: add up small depostis to reduce CPU usage
  //                store results in two lists: sorted by tower, and sorted by ( tower & track )
  //
  //                than add up for each ( track & tower ) and save as gea_edep, gea_tof
  //                this is what geant provides as deposited energy and theoretical tof
  //
  typedef std::pair<emc_towerid_t, emc_trkno_t> xkey_t;
  typedef std::map<xkey_t, vector<xdep_t *> > xmap_tt_t;
  typedef std::map<emc_towerid_t, vector<xdep_t *> > xmap_t_t;
  xmap_tt_t deposits_by_towertrack;
  xmap_t_t deposits_by_tower;


  for(size_t i = 0; i < edeps->size(); i++){
    xdep_t * prevhit = (i == 0) ? NULL : edeps->get(i-1);
    xdep_t * thishit = edeps->get(i);
    xdep_t * nexthit = (i+1 == edeps->size()) ? NULL : edeps->get(i+1);

    IODEBUG("istaf = %d,  trkno = %d,  smodind = %d,  towerind = %d,  tof = %f,  edep = %f\n", 
	    distance(edeps->begin(), i), thishit->trkno, 
	    thishit->smodind, thishit->towerind, thishit->geatof, thishit->geaedep);

    
    xdep_t * deposit = deposit_energy(prevhit, thishit, nexthit);

    //    xdep_t * deposit = deposit_energy( *i, (i+1 == edeps->end()) ? NULL : *(i+1) ); // ugly-ish

    if(deposit == NULL) continue; // we are still cumlating small deposits
    deposits_by_tower[ deposit->swkey ].push_back( deposit );
    deposits_by_towertrack[ xkey_t(deposit->swkey, deposit->trkno) ].push_back( deposit );
  }

  for(xmap_tt_t::iterator i = deposits_by_towertrack.begin(); i != deposits_by_towertrack.end(); i++){
    xdep_adder sum = for_each(i->second.begin(), i->second.end(), xdep_adder(i->second[0]));
    
    emcGeaDeposit * deposit = emcGeaDeposit::createdef();
    deposit->set_trkno( sum.trkno ); deposit->set_towerid( EmcIndexer::TowerID(sum.swkey) );
    deposit->set_simulated( true );
    deposit->set_gea_edep( sum.edep ); deposit->set_gea_tof( sum.tof );

    if( deposits->add( deposit ) == -1 ) return ABORTRUN;
  }
  


  //
  // second pass: propagate light along the tower (attenuate light), modifies edep & tof
  //                than add up for each ( track & tower ) and save as calib_edep, calib_tof
  //
  for(xmap_tt_t::iterator i = deposits_by_towertrack.begin(); i != deposits_by_towertrack.end(); i++){
    for(std::vector<xdep_t *>::iterator j = i->second.begin(); j != i->second.end(); j++){
      emcGeaEdep * deposit = *j;
      emc_gea_params_t * geaparam = &(*geaparms)[deposit->sector];
      emc_geometry_t * geometry = &(*geom)[deposit->sector][deposit->iy][deposit->iz];
      
      distribute_light(deposit, geaparam, geometry);
    }

  }
  


  //
  // third pass: calculate detector response (pulse shape reconstruction), modifies edep & tof
  //                than add up for each ( track & tower ) and save as orig_edep, orig_tof
  //

  for(xmap_t_t::iterator i = deposits_by_tower.begin(); i != deposits_by_tower.end(); i++){
    calculate_response(i->second, geaparms);
  }

  for(xmap_tt_t::iterator i = deposits_by_towertrack.begin(); i != deposits_by_towertrack.end(); i++){
    xdep_adder sum = for_each(i->second.begin(), i->second.end(), xdep_adder(i->second[0]));

    // correcting bug: appling rescale only to pbsc (see logbook p59)
    //    if(sum.type == SECTOR_TYPE_PBSC) sum.edep *= e_rescale;
    
    emcGeaDeposit::key_type key(sum.trkno, EmcIndexer::TowerID(sum.swkey), EMC_INVALID_CLUSTERID);
    emcGeaDeposit * deposit = deposits->find( key );
    deposit->set_orig_edep( sum.edep ); deposit->set_orig_tof( sum.tof );
    deposit->set_calib_edep( sum.edep ); deposit->set_calib_tof( sum.tof );
  }





  //
  // clean up: iterating over only one of the two lists
  // 
  
  for(xmap_t_t::iterator i = deposits_by_tower.begin(); i != deposits_by_tower.end(); i++)
    for(std::vector<xdep_t *>::iterator j = i->second.begin(); j != i->second.end(); j++)
      delete *j;
  


  return 0;
}




#ifdef _COMPATIBLE_VERSION

inline xdep_t * EmcGeaRawDataSimMaker::deposit_energy(xdep_t const * const prevhit,
							xdep_t const * const hit, 
							xdep_t const * const nexthit){

  enum { GATEMAX, CUTOFF, NEWTOWER, LIMIT, CUMLATING, NEWTRACK, NEWEVENT };
  static int last_was_this = NEWEVENT, before_last_was_this = NEWEVENT;
  xdep_t * ret = NULL;
  static double esum = 0;


  before_last_was_this = last_was_this;

  
  if(nexthit == NULL  ||  (prevhit && hit->trkno != prevhit->trkno) ){
    goto bypass_single_edep_checks;
  }

  if(hit->type == SECTOR_TYPE_PBSC){
    
    if(hit->tof > p_gatemax){
      last_was_this = GATEMAX;
      CUMDEBUG("under limit, throwing: gatemax\n");
      return NULL;
    }
    
    if(hit->edep < e_cutoff){
      last_was_this = CUTOFF;
      CUMDEBUG("under limit, throwing: cutoff\n");
      return NULL;
    }
    
  }
  
 bypass_single_edep_checks:

  // mazsi::compat last event triggers new tower...
  if(nexthit == NULL){
    last_was_this = NEWTOWER;
    CUMDEBUG("processing: new tower\n");
    ret = new xdep_t(*hit); ret->edep += esum; esum = 0;
    return ret;
  }
  
  // new tower?
  else if( (last_was_this != NEWTOWER) && (prevhit && prevhit->swkey != hit->swkey) ){
    last_was_this = NEWTOWER;
    CUMDEBUG("processing: new tower\n");
    ret = new xdep_t(*hit); ret->edep += esum; esum = 0;
  }
  else if(nexthit && nexthit->swkey != hit->swkey){
    last_was_this = NEWTOWER;
    CUMDEBUG("processing: new tower\n");
    ret = new xdep_t(*hit); ret->edep += esum; esum = 0;
  }



  
  // edep > edep_small?
  else if(hit->edep > dele_small){
    last_was_this = LIMIT;
    CUMDEBUG("processing: out of collect limits: dele_small (%f)\n", dele_small);
    ret = new xdep_t(*hit); ret->edep += esum; esum = 0;
  } 
  
  // esum > dele_max?
  else if(esum > dele_max){
    last_was_this = LIMIT;
    CUMDEBUG("processing: out of collect limits: dele_sum (%f)\n", dele_max);
    ret = new xdep_t(*hit); ret->edep += esum; esum = 0;
  }
  
  // mazsi::compat anomaly at first deposit triggers new track
  else if( prevhit == NULL ){
    last_was_this = NEWTRACK;
    CUMDEBUG("cumlating (hitsum->edep = %f)\n", esum);
    CUMDEBUG("processing: new track\n");
      ret = new xdep_t(*hit); ret->edep += esum; esum = hit->edep;
  }
  
  // collect deposits
  else if( prevhit->trkno == hit->trkno ){
    last_was_this = CUMLATING;
    esum += hit->edep;
    CUMDEBUG("cumlating (hitsum->edep = %f)\n", esum);
  }
  
  // new track
  // compatiblity stuff: esum is not celared to 0, but the current 
  // edep is saved.. (so it is counted twice).
  //
  else {
    last_was_this = NEWTRACK;
    CUMDEBUG("processing: new track\n");
    ret = new xdep_t(*hit); esum += hit->edep;
  }
  
  return ret;
}




#else




xdep_t * EmcGeaRawDataSimMaker::deposit_energy(xdep_t const * prevhit, xdep_t const * hit, xdep_t const * nexthit){
  //
  // handles elemntary deposits: collects them to reduce CPU usage
  //

  static xdep_t * saved = NULL;


  enum { RETURN, DROP, CARRYON } decision = CARRYON;

  // last deposit? return what we have cumlated up to now
  if( nexthit == NULL ) decision = RETURN;

  // will be new tower or new track?
  else if( hit->trkno != nexthit->trkno  ||  hit->swkey != nexthit->swkey ) decision = RETURN;

  // next one will be a tough one?
  else if( nexthit->edep > dele_small ) decision = RETURN;

  // we already have too much?
  else if( saved  &&  saved->edep > dele_max ) decision = RETURN;

  // too small? (todo: how could be too small? crazy...)
  else if( hit->type == SECTOR_TYPE_PBSC  &&  ( hit->tof > p_gatemax || hit->edep < e_cutoff) ){
    DEBUG("dropping deposit! %d %d\n", hit->tof > p_gatemax, hit->edep < e_cutoff);
    decision = DROP;
  }  


  xdep_t * ret = NULL;

  switch(decision){
  case RETURN: if(saved){ *saved += *hit; ret = saved; saved = NULL; } else { ret = new xdep_t(*hit); } break;
  case DROP: ret = NULL; break;
  case CARRYON: if(saved){ *saved += *hit; } else { saved = new xdep_t(*hit); ret = NULL; } break;
  }

  return ret;
}


#endif




inline void EmcGeaRawDataSimMaker::distribute_light(xdep_t * deposit, emc_gea_params_t * geapar, emc_geometry_t * geom){
  //
  // modifies tof and edep according to imapct point, cal. medium, parameretes, weathe...
  //
  //
  //    Process hit information (taken from emc_digi.f in PISA as of 8/15/96)
  //    
  //    Get the distance from the readout device.  This is
  //    the difference (RPOS + LSIZ) - RPROJ, where LSIZ is
  //    the (full) longitudinal size of the cell (PbGl, crystal,
  //    whatever), RPOS is the distance of the center of the
  //    front face of the detector, RPROJ is the distance of the
  //    hit PROJected on the vector pointing to the center of
  //    the detector
  //
  
  assert(deposit != NULL);

  DISTDEBUG("*** distributing trkno = %ld,  (smod, twr) = (%d, %d),  edep = %f,  tof = %f\n", 
	    deposit->trkno, deposit->smodind, deposit->towerind, deposit->edep, deposit->tof);
  DISTDEBUG("***   (x, y, z) = (%f, %f, %f)\n", deposit->x, deposit->y, deposit->z);

  //  short sector = deposit->sector;
  //  short iy = deposit->iy;
  //  short iz = deposit->iz;
  //  float tof_star = deposit->tof;
  //  float dele_star = deposit->edep;
  float tof_star, dele_star;


  if(deposit->edep < dele_small){
  
    dele_star = deposit->edep;
    tof_star = 0.0;
    DISTDEBUG("  %16s,  tof = %f,  edep = %f\n", "edep < dele_small", tof_star, dele_star);
    
  } else {
    
    /*  ********************************************************************
	Recalculate energy and time of flight: correct time of flight
	with light propagation speed (TOF_STAR), correct energy
	with light attenuation over the distance between the current
	position (where energy was deposited) and the end of the
	module, where light is read out.
	*************************************** ******************************  */
	
    if( (deposit->type == SECTOR_TYPE_PBGL) && (pbgl_timing == PBGL_TIMING_PARAMETRIC) ){
      /* PbGl and response parametrization is chosen, do NOT attenuate */
      dele_star = deposit->edep;
      tof_star = deposit->tof;
      DISTDEBUG("  %16s,  tof = %f,  edep = %f\n", "pbgl::param", tof_star, dele_star);
    }
    
    else if( (deposit->type == SECTOR_TYPE_PBGL) && (pbgl_timing == PBGL_TIMING_CHERENKOV) ){
      /* If Cherenkov photons were generated, do NOT attenuate! */
      dele_star = deposit->edep;
      tof_star = deposit->tof;
      DISTDEBUG("  %16s,  tof = %f,  edep = %f\n", "pbgl::cheren", tof_star, dele_star);
    }

    else {
      double work1 = deposit->x * geapar->cosangle;
      double work2 = deposit->y * geapar->sinangle;
      float  proj  = fabs(work1) + fabs(work2);
      
      /* Failed to account for global translations in the
	 geometry; this is still not quite clean
	 but works   Aug. 14, 2000  GD
      */
      float distance =
	max(0.0, geapar->rpos + geapar->lsize +
	    fabs(  geapar->translate[0]  ) - proj);
      
      float work = ( - distance / geapar->att_length );
      dele_star = deposit->edep * exp (work);
      tof_star = deposit->tof + distance / geapar->speed_of_light;

      DISTDEBUG("  %16s,  tof = %f,  edep = %f\n", "atten", tof_star, dele_star);
    }
    
  }

  //  assert(deposit->edep > 0.0);


  /* ************************************************************************** 
     Charlie is right: position uniformity correction should come
     before the pulse shape reconstruction
     
     If position nonuniformity correction is requested,
     do it now (for Shish-Kebab only)
     
     IF we include this in here, we need dEmcGeometry
     ********************************************* ******************************  */

  if(l_correct_unif  &&  deposit->type == SECTOR_TYPE_PBSC){
    float centx = (deposit->x - geom->x);
    
    float centy = geom->y;
    if( geapar->cosangle  != 0.0 )
      centy += geapar->sinangle * centx / geapar->cosangle;
    
    float centz = geom->z;


    float disty = deposit->y - centy;
    float distz = deposit->z - centz;
    
    int j = (int)( 12 + disty * 24.0 / cellsize );
    int k = (int)( 12 + distz * 24.0 / cellsize );
    
    j = max(1,min(j,24))	-1;
    k = max(1,min(k,24))	-1;
    
    dele_star = dele_star * emc_pos_unif[j][k];
 
    DISTDEBUG("  %16s,  tof = %f,  edep = %f\n", "uniformity", tof_star, dele_star);
  }
  
  

  
  /* Restore dele_star to dele in case of PbGl and leave the
     response to Muenster */
  if ( deposit->type == SECTOR_TYPE_PBGL ){
    dele_star = deposit->edep;
    DISTDEBUG("  %16s,  tof = %f,  edep = %f\n", "pbgl::muenster", tof_star, dele_star);
  }  


  { // 1.0 for pbgl, so can be safely applyed
    dele_star /= geapar->sampfrac;
  }    

  

  deposit->edep = dele_star;
  deposit->tof = tof_star;
}
    
    












inline void EmcGeaRawDataSimMaker::calculate_response(std::vector< xdep_t * > & deposits, emcGeaParams * geaparms){
  //
  // calculates the response of detectors
  //

  assert(deposits.size() != 0);
  size_t maxindex = deposits.size();

  /*  *************************************************************** 
      Time of flight is now the earliest time in the module
      (this is NOT the real, measured TOF, but it will serve
      as a starting point when we build up the pulse-shape to
      get a good simulation of timing)
      ********************************** ******************************  */
  

  // calculate these values for the diferent timing modules

  
  float tof = NAN, edep = 0;


  for(size_t i = 0; i < maxindex; i++){
    edep += deposits[i]->edep;
    if( !isnan(deposits[i]->tof) )
      if( isnan(tof) || (deposits[i]->tof < tof) ) tof = deposits[i]->geatof; // mazsi::comp this should be tof!
    DISTDEBUG("  storing (sector,y,z) = (%1d,%2d,%2d)   :%2d   edep = %10f   tof = %10f   work\n", 
	      deposits[i]->sector, deposits[i]->iy, deposits[i]->iz, 
	      i,
	      deposits[i]->edep, deposits[i]->tof
	      );
  }


  /* Sort emc_parent for decreasing energy; you will need the
     particle with highest deposited energy - the "dominant contributor"
     to make the empirical correction to the PbGl response */
  
  std::sort(deposits.begin(), deposits.end(), xdep_more());

  float origedep = edep;
  emc_gea_params_t * geapar = &geaparms->operator[]( deposits[0]->sector );
  if (deposits[0]->type == SECTOR_TYPE_PBGL) calculate_response_pbgl(deposits, tof, edep, geapar);
  else                                       calculate_response_pbsc(deposits, tof, edep, geapar);
  

  // if not equal scale small deposits to preserve modifications
  if(edep != origedep){
    float scale = (origedep == 0) ? 0.0 : edep/origedep;
    
    for(vector< xdep_t *>::iterator i = deposits.begin(); i != deposits.end(); i++) 
      (*i)->edep *= scale;
  }
  

  // save calculated tof
  for(vector< xdep_t *>::iterator i = deposits.begin(); i != deposits.end(); i++) // save calculated tof
    (*i)->tof = tof;
}  




	 
void EmcGeaRawDataSimMaker::calculate_response_pbsc(std::vector< xdep_t * > & deposits, float & tof, float & edep, emc_gea_params_t * geapar){

  //  assert(edep > 0);

  // make the correction with fudge factor to bring back peek
  // to correct position
  edep *= e_rescale;
  DISTDEBUG("  %16s,  edep = %f\n", "rescale", edep * geapar->sampfrac);
  
  RESPDEBUG("pbsc: (sect, y, z) = (%1d, %2d, %2d),  nodeps = %3d,  edep = %10f  tof = %10f  work  mode=%d\n",
	    sector, iy, iz, deposits.size(), edep, tof, pbsc_timing
	    );

  
  
  //  double thresh = ra_det[93][sector];	 // TOF threshold


  //  if(pbsc_timing == PBSC_TIMING_SIMPLE ) return; // skip full pulsehape reconstrction, use min(tof)
  if(pbsc_timing == PBSC_TIMING_SIMPLE ) goto bailout;


  // if not PBSC_TIMING_SIMPLE the default value is 0 for tof
  tof = 0;


  /*  ************************************************************ 
      If number of photons/GeV is specified, we can generate
      the statistical term in the energy resolution
      ******************************* ******************************  */
  if( geapar->photoe_per_gev > 0.0){
    double tmp = edep * geapar->photoe_per_gev;
    float ran; norran_(&ran);
    tmp = tmp + ran * sqrt(tmp); /*	 Mean + ran*sigma	*/
    edep = tmp / geapar->photoe_per_gev;
  }
   

  /*  ****************************************************************** 
      If noise is specified, put uncorrelated noise on top of
      the signal in every channel.  Channels with no signal are
      deliberately omitted (left as 0.0).  If you really want to
      have noise in every channel (no matter if it is hit or not),
      change the code, the zeroing of the array EMC_DELE at the
      beginning of this routine.  Who cares?  Its your CPU time.
      ************************************* ******************************  */
  {
    float ran; norran_(&ran);
    edep += (ran * pbsc_noise);
  }


  //  if(edep <= thresh) return;                    // was ??
  if(edep <= timethresh){
    RESPDEBUG("(%1d,%2d,%2d) work cut failed\n", deposits[0]->sector, deposits[0]->iy, deposits[0]->iz);
    goto bailout;
  } else {
    RESPDEBUG("(%1d,%2d,%2d) work cut passed\n", deposits[0]->sector, deposits[0]->iy, deposits[0]->iz);
  }    



  /*	DO pulse reconstruction		*/
  float ra_pulserecon[2*maxtimebin];
  memset(ra_pulserecon, 0, sizeof(ra_pulserecon));
  
  for(std::vector<xdep_t *>::iterator i = deposits.begin(); i != deposits.end(); i++){
    xdep_t * deposit = *i;
    
    
    /*  ****************************************************************************** 
	
	Here you are reconstructing the pulse itself, then compare
	it with the timing threshold (ra_det[94][sector])
	This takes lots of time; we shortcut the pulse reconstruction
	FROM A SPECIFIC E DEPOSIT AT A SPECIFIC TIME, 
	if the pulse already passes the timing threshold.
	(Watch out: you cannot just stop and say: that's my time, because
	the time can still be pushed EARLIER, particularly for very
	low energies with large slewing.  But it definitely cannot be
	pushed LATER; therefore, one you have a valid threshold crossing,
	you can stop reconstructing the rest of the pulse.)
	NOTE THAT THIS WOULD NOT WORK WITH A CFD OR A TRAILING EDGE
	TIMING  --  AS IS THE CASE FOR LEAD GLASS!
	
	! You already had a pulse crossing
	! the threshold, and now waited
	! 5 ns, more than the maximum slewing
	! no pulse, however big, that comes
	! later could influence your measurement
	! and result in earlier times
	
	************************************************* ******************************  */

    /*	Do not generate pulse from noise!	*/
    if(deposit->edep <= geapar->noise) continue;

    // this sequence is to reproduce original behaviour
    int startup = (int)( (deposit->tof-timeoffset) / timebin );
    startup = max(startup, 1);
    startup = min(startup, maxtimebin);
    startup -= 1;

    /*
    RESPDEBUG("(%1d,%2d,%2d) generating pulse at timebin=%3d work with pulse=%10f "
	      "(tof=%10f offs=%10f bin=%10f)\n",
	      deposit->sector, deposit->iy, deposit->iz,
	      startup, deposit->edep,
	      deposit->tof, timeoffset, timebin
	      );
    */

    for (int k = startup; k < 2*maxtimebin-1; k++ ){
      //      double t =  (k+1) * timebin - deposit->tof; // mazsi: why k+1 ?
      double t =  (k + 1 - startup) * timebin;
      
      //if((t * geapar->decaytime) <= 75.0)
      if((t * 0.5) <= 75.0) // hardwired
	ra_pulserecon[k] += testfunction->operator()(t) * deposit->edep;

      //      testfunction->operator[]( t );
      
      // todo mazsi : unused variable: earliest valid tof
      //      if( (ra_pulserecon[k] > 1.3*ra_det[92][sector]) && (earliest_valid_tof == 0.0) )
      //	earliest_valid_tof = t + timeoffset;
      
    }
  }
  
  
  {
  // now find the peek 
  int k = 0;
  
  switch(pbsc_timing){
  case PBSC_TIMING_LED:
    for ( k = 1;  k < 2 * maxtimebin - 2; k++ ) // skipp last bin for code uniformity..
      /* if(i_tofindex == 0 && ra_pulserecon[k] > ra_det[93][sector]) */
      if(ra_pulserecon[k] > timethresh) break;
    RESPDEBUG("(%1d,%2d,%2d) work finished loop with %d\n",
	      sector, iy, iz, (k == 2*maxtimebin-2)?0:k
	      );
    break;
    
    
  case PBSC_TIMING_DZC:
    /* Do "constant fraction" a la PHENIX */
    /* First implementation: just look for the first
       peak of the signal (true story: the signal
       is Differentiated and the Zero Crossing of the
       derivative is measured) */
    
    for ( k = 1;  k < 2 * maxtimebin - 2; k++  )
      /* You still want to keep r_timethresh to avoid timing on channels
	 with too low energy */
      if( (ra_pulserecon[k] > timethresh) &&
	  (ra_pulserecon[k] > ra_pulserecon[k+1]) &&
	  (ra_pulserecon[k] > ra_pulserecon[k+2]) )
	break;
    break;
    

  default: 
    assert( (pbsc_timing == PBSC_TIMING_LED) || (pbsc_timing == PBSC_TIMING_DZC));
    
  }
  
  if( k < 2 * maxtimebin - 2 ) tof = (k+1) * timebin + timeoffset;
  else                         tof =  -99.0;
  }


 bailout:
  RESPDEBUG("  calculated pbsc response (sector,y,z) = (%1d,%2d,%2d)   edep = %10f   tof = %10f   work\n",
	    sector, iy, iz, edep, tof
	    );
}








void EmcGeaRawDataSimMaker::calculate_response_pbgl(std::vector< xdep_t * > & deposits, float & tof, float & edep, emc_gea_params_t * geapar){

  //  assert(edep > 0);
  assert(deposits.size() > 0);

  RESPDEBUG("pbgl: (sect, y, z) = (%1d, %2d, %2d),  nodeps = %3d,  edep = %10f  tof = %10f  work\n", 
	    sector, iy, iz, deposits.size(), edep, tof
	    );


  switch(pbgl_timing){
  case PBGL_TIMING_SIMPLE:
    /* Leave everything unchanged; just exp. attenuation */
    break;



	     
  case PBGL_TIMING_SMEARING:
    /* Just smear for photoelectrons, resolution, noise */
    
    if( geapar->photoe_per_gev > 0.0){  /* Photoelectrons */
      edep *= geapar->photoe_per_gev;
      float ran; norran_(&ran);
      edep += ran * sqrtf(edep);
      edep /= geapar->photoe_per_gev;
    }
    
    /* Overall resolution */
    {
      float ran; norran_(&ran);
      edep += ( ran * sqrtf(edep) * pbgl_resolution );
    }
    
    /* Uncorrelated noise */
    {
      float ran; norran_(&ran);
      edep += ( ran * geapar->noise );
    }

    /* Rescale to get photons right February 15, 2000  G. David */
    edep *= pbgl_rescale;
    
    break;
    
    


  case PBGL_TIMING_PARAMETRIC:
    {    
      /* GEANT energies have been added.
	 Now it is time for corrections, a la Henner 
	 First you have to figure out particle ID */
      int trkno = deposits[0]->trkno;
      
      /* The following call IS necessary because
	 Henner needs the total momentum of incident part */ 
      
      float ptot,ptheta,pphi,r_vertex,z_vertex,theta_vertex,phi_vertex;
      int nfile, error, itparent,idparent, idpart;
      int status = dio_ptrkstack(&trkno, &nfile, &error, 
				 &ptot, &ptheta, &pphi,
				 &r_vertex, &z_vertex, 
				 &theta_vertex, &phi_vertex,
				 &itparent, &idparent, &idpart);
      assert(status == 0);
      
      float r_eexp = edep;
      float r_porig = ptot;
      float r_etracking = r_eexp;    /* Default value */
      
      switch(idpart){
      case 1:    /* Photons */
	r_etracking = r_eexp * ( 1.003 - 0.0531*exp(0.2905*r_porig) );
	r_etracking = r_etracking / 0.88; 	/* Fudge factor */
	break;
	
	
      case 2: case 3:      /* Electrons */
	r_etracking = r_eexp * ( 0.9777 - 0.0469*exp(0.4567*r_porig) );
	r_etracking = r_etracking / 0.88; 	/* Fudge factor */
	break;
	
	
      case 5: case 6: case 7: case 8: case 9: /* Muons, pions */
	if(r_porig > 0.5){
	  /* May be min.ion. */
	  if(r_eexp < 0.35) r_etracking = r_eexp * 1.85;
	  else              r_etracking = r_etracking * 0.82; /* Fudge factor */
	}
	break;
	
	
      case 11: case 12: case 13: case 14: case 15: case 16:
	if(r_porig > 0.9){
	  /* May be min.ion. */
	  if(r_eexp < 0.4) r_etracking = r_eexp * (2.055 - 0.03 * r_porig);
	  else             r_etracking = r_etracking * 0.9; 	    /* Fudge factor */
	}
	break;
      }
      
      edep = r_etracking;
    }

    break;


    
    
  case PBGL_TIMING_CHERENKOV:
    /* Cherenkov photons were generated; leave it alone */
    
    /* Fudge factor added when Markus introduced
       new parametrzation: should be double-checked */
    
    edep *= pbgl_ctrk_rescale;
    break;


  default: 
    assert( (pbgl_timing == PBGL_TIMING_SIMPLE) || (pbgl_timing == PBGL_TIMING_SMEARING) || (pbgl_timing == PBGL_TIMING_PARAMETRIC) || (pbgl_timing == PBGL_TIMING_CHERENKOV) );

  }



  /* Take care of TOF */
  {
    tof += pbgl_tofoffset;
    float ran; norran_(&ran);
    tof += ( ran * ran * pbgl_tofresolution );
  }    

  RESPDEBUG("  calculated pbgl response (sector,y,z) = (%1d,%2d,%2d)   edep = %10f   tof = %10f   work\n",
	    sector, iy, iz, edep, tof
	    );
}
   













float EmcGeaRawDataSimMaker::emc_pos_unif[24][24];
float EmcGeaRawDataSimMaker::cellsize;




