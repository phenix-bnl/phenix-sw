//////////////////////////////////////////////////////////////////////////////////
//
// v4 of emcGeaTowerContent. this version does not do any caching of the data.
// this makes it _very_ slow, but still usefull if you suspect errors in the 
// caching mechanism of the libemc simulated content classes (emcGea*Content).
//
//////////////////////////////////////////////////////////////////////////////////

#include <iostream>

#include <EmcIndexer.h>

#include <emcGeaDeposit.h>
#include <emcGeaTowerContentv4.h>


ClassImp(emcGeaTowerContentv4);



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





emcGeaTowerContentv4::emcGeaTowerContentv4(emc_towerid_t towerid){
  set_towerid(towerid);

  hasgain = false;
  higain = logain = tdc = 0;
  
  hasraw = false;
  hgpost = hgpre = lgpost = lgpre = tac = 0;
  amupre = amupost =  amutac = beamclock = 0;

  dataerror = errormap = warnmap = 0;

  //  tracks = NULL;
}



void emcGeaTowerContentv4::Copy(const emcTowerContent & from){
  ::copy(this, &from);

  emcGeaTowerContent const * xfrom = dynamic_cast<emcGeaTowerContent const *>(&from);
  if( xfrom ) ::copygea(this, xfrom);
}



void emcGeaTowerContentv4::copy(emcGeaTowerContent const * from){
  ::copy(this, from);
  ::copygea(this, from);
}



void emcGeaTowerContentv4::Reset(){
  emcGeaTowerContent::Reset();
  //tracklist.clear();

  hasgain = false;
  higain = logain = tdc = 0;
  
  hasraw = false;
  hgpost = hgpre = lgpost = lgpre = tac = 0;
  amupre = amupost =  amutac = beamclock = 0;

  dataerror = errormap = warnmap = 0;
}



void emcGeaTowerContentv4::set_towerid(emc_towerid_t towerid){
  this->_towerid = towerid;
}



void emcGeaTowerContentv4::SetID(int fem, int channel){ 
  set_towerid( EmcIndexer::PXSM144iCH_iPX(fem, channel) );
}



int emcGeaTowerContentv4::Channel() const {
  int PXSM144, CH; 
  EmcIndexer::PXPXSM144CH(_towerid, PXSM144, CH);
  return CH;
}



int emcGeaTowerContentv4::FEM() const {
  int PXSM144, CH; 
  EmcIndexer::PXPXSM144CH(_towerid, PXSM144, CH);
  return PXSM144;
}
  



void emcGeaTowerContentv4::SetCalibrated(float energy, float tof){
  std::cerr << __PRETTY_FUNCTION__ << ": you should not use this function. "
	    << "energy and tof values are calculated on the fly from track data."
	    << std::endl;
}




void emcGeaTowerContentv4::SetRaw(int hgpost, int hgpre, int lgpost, int lgpre, int tac,
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



void emcGeaTowerContentv4::SetADCTDC(int adc, int tdc, int hg, int lg){
  this->higain = hg;
  this->logain = lg;
  this->tdc = tdc;

  hasgain = true;
}
