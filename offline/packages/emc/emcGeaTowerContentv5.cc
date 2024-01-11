//////////////////////////////////////////////////////////////////////////////////
//
// v5 of emcGeaTowerContent. this version _does_ cache some of the data. for a
// non caching version see emcGeaTowerContentv4.
//
//////////////////////////////////////////////////////////////////////////////////

#include <iostream>

#include <EmcIndexer.h>

#include <emcGeaDeposit.h>
#include <emcGeaTowerContentv5.h>
#include <emcGeaTowerContainer.h>


ClassImp(emcGeaTowerContentv5);



namespace {

  void copy(emcTowerContent * to, emcTowerContent const * from){

    if( from->hasRaw() && to->canHaveRaw() ){
      to->SetRaw( from->HGPost(), from->HGPre(), from->LGPost(), from->LGPre(), from->TAC(),
		  from->AMUPre(), from->AMUPost(), from->AMUTAC(),
		  from->BeamClock()
		  );
    }
    
    //    to->set_towerid( from->towerid() );
    to->SetID( from->FEM(), from->Channel() );
    
    
    //    if( from->hasGain() && to->canHaveGain() ){
    //      to->SetGain( from->Gain() );
    //    }
    
    
    if( from->hasDC() && to->canHaveDC() ){
      to->SetADCTDC( from->ADC(), from->TDC(), from->HG(), from->LG() );
    }
    
    
    if( from->hasCalib() && to->canHaveCalib() ){
      to->SetCalibrated( from->Energy(), from->ToF() );
      //to->SetToF( from->UncorrectedToF() );
    }
    
    
    if( from->hasMerSimStatus() && to->canHaveMerSimStatus() ){
      to->SetMerSimStatus( from->isMerged(), from->isSimulated() );
      to->SetSimFrac( from->SimFrac() );
    }
    
    
    if( from->hasReference() ){
      // /// Whether this tower is a reference one.
      // virtual bool isReference(void) const;
      
      // /// Pointer to reference (if available, see hasReference()).
      // virtual emcTowerContent* Reference(void) const;
    }
    
    
    to->SetNeighbours( from->ErrorNeighbours(), from->WarnNeighbours() );
    to->SetDataError( from->DataError() );
  }



  void copygea(emcGeaTowerContent * to, emcGeaTowerContent const * from){
    /*
      tracks = from->tracks;
      for(emc_tracklist_t::iterator i = from->tracklist.begin(); i != from->tracklist.end(); i++)
      tracklist.insert(*i);
    */
    
    //to->set_trackcontainer( from->get_trackcontainer() );

    /*
    to->clear_tracks();
    emc_tracklist_t trklist = from->get_track_list();
    for(emc_tracklist_t::iterator i = trklist.begin(); i != trklist.end(); i++)
      to->add_track(*i);
    */
  }

}





emcGeaTowerContentv5::emcGeaTowerContentv5(emc_towerid_t towerid){
  set_towerid(towerid);

  hasgain = false;
  higain = logain = tdc = 0;
  
  hasraw = false;
  hgpost = hgpre = lgpost = lgpre = tac = 0;
  amupre = amupost =  amutac = beamclock = 0;

  dataerror = errormap = warnmap = 0;

  cacheok = false;
}



emcGeaTowerContentv5::emcGeaTowerContentv5(const emcGeaTowerContentv5 & t){
  set_towerid(EMC_INVALID_TOWERID);

  hasgain = false;
  higain = logain = tdc = 0;
  
  hasraw = false;
  hgpost = hgpre = lgpost = lgpre = tac = 0;
  amupre = amupost =  amutac = beamclock = 0;

  dataerror = errormap = warnmap = 0;

  cacheok = false;

  this->copy(&t); 
}



void emcGeaTowerContentv5::Copy(const emcTowerContent & from){
  ::copy(this, &from);

  emcGeaTowerContent const * xfrom = dynamic_cast<emcGeaTowerContent const *>(&from);
  if( xfrom ) ::copygea(this, xfrom);
}



void emcGeaTowerContentv5::copy(emcGeaTowerContent const * from){
  ::copy(this, from);
  ::copygea(this, from);
  cacheok = false;
}



void emcGeaTowerContentv5::Reset(){
  emcGeaTowerContent::Reset();

  hasgain = false;
  higain = logain = tdc = 0;
  
  hasraw = false;
  hgpost = hgpre = lgpost = lgpre = tac = 0;
  amupre = amupost =  amutac = beamclock = 0;

  dataerror = errormap = warnmap = 0;

  ctracklist.clear(); cclusterlist.clear(); cacheok = false;
}



void emcGeaTowerContentv5::set_towerid(emc_towerid_t towerid){
  this->_towerid = towerid;
}



void emcGeaTowerContentv5::SetID(int fem, int channel){ 
  set_towerid( EmcIndexer::PXSM144iCH_iPX(fem, channel) );
}



int emcGeaTowerContentv5::Channel() const {
  int PXSM144, CH; 
  EmcIndexer::PXPXSM144CH(_towerid, PXSM144, CH);
  return CH;
}



int emcGeaTowerContentv5::FEM() const {
  int PXSM144, CH; 
  EmcIndexer::PXPXSM144CH(_towerid, PXSM144, CH);
  return PXSM144;
}
  



void emcGeaTowerContentv5::SetCalibrated(float energy, float tof){
  std::cerr << __PRETTY_FUNCTION__ << ": you should not use this function. "
	    << "energy and tof values are calculated on the fly from track data."
	    << std::endl;
}




void emcGeaTowerContentv5::SetRaw(int hgpost, int hgpre, int lgpost, int lgpre, int tac,
			int amupre, int amupost, int amutac, int beamclock){
  this->hgpost = hgpost;
  this->hgpre = hgpre;
  this->lgpost = lgpost;
  this->lgpre = lgpre;

  this->amupre = amupre;
  this->amupost = amupost;
  this->amutac = amutac;
  this->beamclock = beamclock;

  hasraw = true;
}



void emcGeaTowerContentv5::SetADCTDC(int adc, int tdc, int hg, int lg){
  this->higain = hg;
  this->logain = lg;
  this->tdc = tdc;

  hasgain = true;
}



emc_tracklist_t const emcGeaTowerContentv5::get_track_list() const {
  if( !cacheok ){
    ctracklist.clear();
    ctracklist = emcGeaTowerContent::get_track_list(); 

    cclusterlist.clear();
    cclusterlist = emcGeaTowerContent::get_cluster_list(); 

    cacheok = true;
  }

  return ctracklist;
}



emc_clusterlist_t const emcGeaTowerContentv5::get_cluster_list() const {
  if( !cacheok ){
    ctracklist.clear();
    ctracklist = emcGeaTowerContent::get_track_list(); 

    cclusterlist.clear();
    cclusterlist = emcGeaTowerContent::get_cluster_list(); 

    cacheok = true;
  }

  return cclusterlist;
}



void emcGeaTowerContentv5::invcache(cachetype_t type){ 
  ctracklist.clear();
  cclusterlist.clear();
  cacheok = false; 
}

