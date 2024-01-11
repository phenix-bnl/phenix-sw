// ====================
/// \file SvxStripixel.h
/// \brief Parent class for the sensor clustering
/// \author Michael P. McCumber (under the svx documentation effort)
// ====================

#ifndef __SVXSTRIPIXEL_H__
#define __SVXSTRIPIXEL_H__

// standard includes
#include <iostream>
//#include <iomanip>
//#include <math.h>

// root includes
#include <TRandom3.h>

// svx includes
#include "SvxSensor.h"
#include "SvxPixStruct.h"
#include "SvxGhit.h"
#include "SvxGhitList.h"
#include "SvxRawhit.h"
#include "SvxRawhitList.h"
#include "SvxGhitRawhit.h"
#include "SvxGhitRawhitList.h"
#include "svxAddress.hh"

/// \class SvxStripixel
/// \brief The base class for both pixel and stripixel sensors
///
/// This implements functionalities common to pixel and stripixel sensors,
/// such as makeRawhits().
///
/// This class holds the parameters of (stri)pixel sensor, 
/// such as the number and the type of sections per sensor, 
/// and the gain (ADC/charge).
/// The actual values are specified in SvxStripixel.C.
///
/// Created  by V. L. Rykov: 18-Mar-2004
/// Modified by V. L. Rykov: 15-May-2004
/// Sorted hit containers and fast index searches are expolited
/// Documented/Clean-up by M. McCumber: 13-Feb-2013
///
/// \todo Templating this inheritance for 2 classes is over-kill and makes
/// support of these classes more difficult. It's a big-ish job, but this should
/// probably be fixed at some point. -MPM
///
template <int TYPE>
class SvxStripixel : public SvxSensor 
{
public:

  SvxStripixel(const int sc=-1,const int lr=-1,const int ld=-1,const int sn=-1);
   virtual ~SvxStripixel();

  /// standard PHObject reset method
  virtual void Reset();

  /// standard identification streamer
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: SvxStripixel<" << TYPE
       << "> object" << std::endl;
  }
  
  /// get sensor template used
  virtual int   get_sensorType() const {return sensorType;}
  /// get number of sensor sections
  virtual int   get_nSection() const {return nSection;}
  /// get number of readouts, e.g. x&u==2
  virtual int   get_nReadout() const {return nReadout;}
  /// get adc gain, counts per ion. electron
  virtual float get_adcgain() const {return adcgain;}
  /// get adc saturation value
  virtual int   get_adctop() const {return adctop;}
  /// get threshold for accepted raw hits
  virtual float get_adcthresh(const int sc = 0, const int rd = 0) const {
    return adcthresh;
  }
  /// get type in SvxPixStruct<TYPE> template
  virtual int   get_secTYPE(const int sc=0, const int rd=0) const {
    return secTYPE[sc][rd];
  }
  /// get x-positions of section central line, cm
  virtual double get_secXpos(const int sc=0, const int rd=0) const {
    return secXpos[sc][rd];
  }
  /// get z-positions of section central line, cm
  virtual double get_secZpos(const int sc=0, const int rd=0) const {
    return secZpos[sc][rd];
  }
  /// get true if reverse counting of x-rows
  virtual bool  get_xcntRvrs(const int sc=0, const int rd=0) const {
    return xcntRvrs[sc][rd];
  }
  /// get true if reverse counting of z-columns
  virtual bool  get_zcntRvrs(const int sc=0, const int rd=0) const {
    return zcntRvrs[sc][rd];
  }
  /// get channel number offset
  virtual int   get_chanOffs(const int sc=0, const int rd=0) const {
    return chanOffs[sc][rd];
  } 
  /// get channel upper limit
  virtual int   get_chanUplim(const int sc=0, const int rd=0) const {
    return chanUplim[sc][rd];
  }
  /// get "x-slope" for chan # in the formula: 
  /// chan = ix*xSlope+iz*zSlope+chanOffs
  virtual int   get_xSlope(const int sc=0, const int rd=0) const {
    return xSlope[sc][rd];
  }
  /// get "z-slope" for chan # in the formula: 
  /// chan = ix*xSlope+iz*zSlope+chanOffs
  virtual int   get_zSlope(const int sc=0, const int rd=0) const {
    return zSlope[sc][rd];
  }
  /// get pitch (channels/cm? or offset?) in x
  virtual int   get_nXpitch(const int sc=0, const int rd=0) const {
    return senSec[sc][rd]->get_nXpitch();
  }
  /// get pitch (cm/channel) in x
  virtual double get_xPitch(const int sc=0, const int rd=0) const {
    return senSec[sc][rd]->get_xPitch();
  }
  /// get half width of x channel, cm
  virtual double get_xhalfWidth(const int sc=0, const int rd=0) const {
    return senSec[sc][rd]->get_xhalfWidth();
  }
  /// get pitch (channels/cm? or offset?) in z
  virtual int   get_nZpitch(const int sc=0, const int rd=0) const {
    return senSec[sc][rd]->get_nZpitch();
  }
  /// get pitch (cm/channel) in z
  virtual double get_zPitch(const int sc=0, const int rd=0) const {
    return senSec[sc][rd]->get_zPitch();
  }
  /// get half width of z channel, cm
  virtual double get_zhalfWidth(const int sc=0, const int rd=0) const {
    return senSec[sc][rd]->get_zhalfWidth();
  }

  /// print parameters
  virtual void printPar() const;

  /// build raw hits from GEANT hits
  int  makeRawhits(SvxGhitList* glist,
                   SvxRawhitList* rawlist,
                   SvxGhitRawhitList* g2raw,
                   svxAddress* SvxAddressObject) const;

 protected:

  /// main function to build raw hits
  virtual int makeRaw(SvxGhit* ghit,
		      SvxRawhitList* rawlist,
		      SvxGhitRawhitList* g2raw,
		      svxAddress* SvxAddressObject,
		      const int rawFirst = -1) const;

  //=================================
  // Parameters and sensor structures
  //=================================

  static const int          sensorType;  ///< sensor template setting
  static const unsigned int nSection;    ///< number of sensor sections
  static const unsigned int nReadout;    ///< number of readouts, e.g x&u==2
  static const float        adcgain;     ///< adc gain, counts per ion. electron
  static const int          adctop;      ///< adc saturation
  static const int          adcthresh;   ///< threshold for accepted rawhits

  /// section type in term of SvxPixStruct<secTYPE>
  static const int      secTYPE[][2]; // 2 = max. number of readouts 
  /// x-positions of section central line, cm
  static const double   secXpos[][2];
  /// z-positions of section central line, cm
  static const double   secZpos[][2];
  /// if true, reverse counting of x-rows
  static const bool   xcntRvrs[][2];
  /// if true, reverse counting of z-columns
  static const bool   zcntRvrs[][2];
  /// channel number offset
  static const int    chanOffs[][2];
  /// channel number upper limit
  static const int   chanUplim[][2];
  /// "x-slope" for chan # in the formula: chan = ix*xSlope+iz*zSlope+chanOffs
  static const int      xSlope[][2];
  /// "z-slope" for chan # in the formula: chan = ix*xSlope+iz*zSlope+chanOffs
  static const int      zSlope[][2];
  /// instrumented channel cut-off
  static       int chanCutOff[][2];

  /// random number generator for setting charge asymmetry on simulated hits
  TRandom* rnd;

  SvxPixStructure *(*senSec)[2]; //! omit from ROOT I/O

  ClassDefT(SvxStripixel,1)
};

ClassDefT2(SvxStripixel,TYPE)
ClassImpT(SvxStripixel,TYPE)

//========================================
//
//       Method Implementations
//
//========================================

template <int TYPE>
SvxStripixel<TYPE>::SvxStripixel(const int sc,
				 const int lr,
				 const int ld,
				 const int sn) : 
SvxSensor::SvxSensor(sc, lr, ld, sn) 
{
  // create subsensor storage object
  senSec = new SvxPixStructure *[nSection][2];
  for (unsigned int i=0; i<nSection; ++i) {
    for (unsigned int j=0; j<2; ++j) {
      senSec[i][j] = NULL;
    }
  }

  // fill storage with correct subsensor struture
  for (unsigned int i=0; i<nSection; ++i) {
    for (unsigned int j=0; j<nReadout; ++j) {

      if        (secTYPE[i][j]==1) {
	senSec[i][j] = new SvxPixStruct<1>();
      } else if (secTYPE[i][j]==2) {
	senSec[i][j] = new SvxPixStruct<2>();
      } else if (secTYPE[i][j]==3) {
	senSec[i][j] = new SvxPixStruct<3>();
      } else if (secTYPE[i][j]==10) {
	senSec[i][j] = new SvxPixStruct<10>();
      } else if (secTYPE[i][j]==20) {
	senSec[i][j] = new SvxPixStruct<20>();
      } else {
	std::cout << "SvxStripixel<" << TYPE
		  << ">: Unknown pixel structure SvxPixStruct<"
		  << secTYPE[i][j] << "> -- " << "SvxStripixel<" << TYPE
		  << "> constructor terminated" << std::endl;
      }
    }
  }

  // seed the random number generator to something different for each area
  rnd = new TRandom3(sc*1000+lr*100+ld*10+sn);
}

/// Clean up dynamically memory
///
template <int TYPE>
SvxStripixel<TYPE>::~SvxStripixel() 
{
  for (unsigned int i=0; i<nSection; ++i) {
    for (unsigned int j=0; j<nReadout; ++j) {
      delete senSec[i][j];
    }
  }

  delete [] senSec;
  delete rnd;
}

/// Reset internal storage.
///
template <int TYPE>
void SvxStripixel<TYPE>::Reset() 
{
  for (unsigned int i=0; i<nSection; ++i) {
    for (unsigned int j=0; j<nReadout; ++j) {
      senSec[i][j]->Reset();
    }
  }

  return;
}

/// Print out object details.
///
template <int TYPE>
void SvxStripixel<TYPE>::printPar() const 
{
  SvxSensor::printPar();

  std::cout << "SvxStripixel<" << TYPE << "> object: sensorType = "
	    << sensorType << ", nSection = " << nSection
	    << ", nReadout = " << nReadout
	    << ", adcgain = " << adcgain << " counts/electron"
	    << ", adctop = " << adctop
	    << ", adcthreshold = " << adcthresh
	    << std::endl;

  for (unsigned int i=0; i<nSection; ++i) {
    std::cout << " Section #" << i << ":" << std::endl;
    for (unsigned int j=0; j<nReadout; ++j) {
      std::cout << "  Readout #" << j << ":"
		<< " Xpos = " << secXpos[i][j]
		<< " cm, Zpos = " << secZpos[i][j] << " cm" << std::endl
		<< "   Reversed x-row counting? " << xcntRvrs[i][j]
		<< ", Reversed z-column counting? " << zcntRvrs[i][j]
		<< std::endl << "   chanOffs = " << chanOffs[i][j]
		<< ", chanUplim = " << chanUplim[i][j]
		<< ", xSlope = " << xSlope[i][j]
		<< ", zSlope = " << zSlope[i][j]
		<< std::endl << "   ";
      senSec[i][j]->printPar();
    }
  }

  return;
}  

/// This method creates raw detector hits from simulated ghits, then applies
/// digitization on the adc values, and applies the threshold rejection.
///
/// \param[in] glist pointer to input ghits
/// \param[in,out] rawlist pointer to output raw list
/// \param[in,out] g2raw pointer to ancestry container
/// \param[in] SvxAddressObject pointer to vtx hardware info
///
/// \return number of raw hits created
///
template <int TYPE>
int SvxStripixel<TYPE>::makeRawhits(SvxGhitList* glist,
				    SvxRawhitList* rawlist,
				    SvxGhitRawhitList* g2raw,
				    svxAddress* SvxAddressObject) const 
{
  // bail if no ghits
  int nghit = glist->get_nGhits();
  if (nghit<=0) return 0;

  // bail if what? -- looks something like a test if the contained ghits
  // belong to this sensor (see SvxGhitList)
  int idx_ghit_first = 0;
  int idx_ghit_last = 0;
  if (!glist->indexOfGhit(this, idx_ghit_first, idx_ghit_last)) return 0;

  int nrawhit       = 0;     // number of filled rawhits
  int nhit_removed  = 0;     // number of removed rawhits under threshold
  SvxRawhit* rawhit = NULL;  // work rawhit pointer

  int rawFirst   = rawlist->get_nRawhits();   // index of 1st rhit to fill
  int g2rawFirst = g2raw->get_nGhitRawhits(); // index of 1st g2r to fill

  // make a rawhit for each ghit
  for (int ihit=idx_ghit_first; ihit<=idx_ghit_last; ++ihit) {
    SvxGhit* ghit = glist->get_Ghit(ihit);
    //--int nrawhit_pre = nrawhit;

    nrawhit += makeRaw(ghit, rawlist, g2raw, SvxAddressObject, rawFirst);

    //if(ghit->get_layer()==2||ghit->get_layer()==3) 
    //--{
    //--  float xin  = ghit->get_xyzlocalin(0);
    //--  float zin  = ghit->get_xyzlocalin(2);
    //--  float xout = ghit->get_xyzlocalout(0);
    //--  float zout = ghit->get_xyzlocalout(2);

    //--  std::cout<<"rawhit : layer="<<ghit->get_layer()<<" "<<ghit->get_ladder()<<" ";
    //--  std::cout<<"    nrawhit= "<<nrawhit_pre<<" - "<<nrawhit<<" (";
    //--  std::cout<<xin<<","<<zin<<" - "<<xout<<","<<zout<<")"<<std::endl;
    //--}
  }

  // digitize and mask ADC values
  SvxGhitRawhit* g2r = NULL;
  int n_g2r = g2raw->get_nGhitRawhits();
  // loop over all new rawhits
  for (int ihit=rawFirst; ihit<rawlist->get_nRawhits(); ++ihit) {
    rawhit = rawlist->get_Rawhit(ihit);

    // apply the gain per electron and then digitize
    int adc = (int) floor(adcgain*static_cast<float>(rawhit->get_adc()));

    //--float adcraw = rawhit->get_adc();
    //--if(rawhit->get_layer()==2||rawhit->get_layer()==3){
    //--  std::cout<<"  adc="<<adc<<" (thre="<<adcthresh<<") "<<rawhit->get_layer()<<" ";
    //--  std::cout<<rawhit->get_ladder()<<" "<<rawhit->get_channel()<<" ";
    //--  std::cout<<rawhit->get_sensorReadout()<<" "<<rawhit->get_sensorSection()<<" raw="<<adcraw<<std::endl;
    //--}
    if (adc<adcthresh) {
      // remove hits below threshold
      int rawid = rawhit->get_hitID();
      rawlist->removeRawhit(ihit);

      // invalidate the rawhit ancestry to the ghit
      //      int ghitid = -1;
      for (int i=g2rawFirst; i<n_g2r; ++i) {
	g2r = g2raw->get_GhitRawhit(i);
	if (g2r->get_rawhitID()==rawid) {
	  g2r->set_rawhitID(-1);
	  //	  ghitid = g2r->get_ghitID();
	}
      }

      nhit_removed++;
    } else {
      /// \todo I believe this logic is backward, ADC saturation happens
      /// prior to the threshold. One could imagine a case where the threshold
      /// is set to the saturation value. But it looks like this threshold
      /// doesn't change channel-to-channel---but should be checked. -MPM

      // saturate high ADC values
      if (adc>adctop) adc = adctop;

      // set new adc value
      rawhit->set_adc(adc);
    }
  }

  // clean up g2raw list of duplicate ghitID references to rawhitID<0
  SvxGhitRawhit* r2g = NULL;
  for (int i=g2rawFirst; i<n_g2r; ++i) {

    /// \todo split up assignment and logic statement
    if (!(g2r = g2raw->get_GhitRawhit(i))) continue; // if g2r=NULL, skip
    if (g2r->get_rawhitID()>=0)            continue; // skip if still valid
    
    int  g_id = g2r->get_ghitID();
    for (int j=g2rawFirst; j<n_g2r; ++j) {

      /// \todo split up assignment and logic statements
      if ((j==i)||!(r2g = g2raw->get_GhitRawhit(j))) continue;
      if (r2g->get_ghitID()!=g_id) continue; // (i!=j) and (gID(i)!=(gID(j))

      // \todo not sure why the code is picking one or the other here -MPM
      if (r2g->get_rawhitID()<0) {
	g2raw->removeGhitRawhit(j);
      } else if (g2r) {
	g2raw->removeGhitRawhit(i);
      }
    } // j
  } // i

  // restack the output if something was removed
  if (nhit_removed>0) {
    rawlist->Compress();
    g2raw  ->Compress();
  }
  
  // return the number of newly created hits
  return nrawhit - nhit_removed;
}

/// This method creates raw detector hits from a single simulated ghit. 
/// ADC is left in units of charge (no gain yet applied).
///
/// \param[in] glist pointer to input ghits
/// \param[in,out] rawlist pointer to output raw list
/// \param[in,out] g2raw pointer to ancestry container
/// \param[in] SvxAddressObject pointer to vtx hardware info
/// \param[in] rawFirst offset location for new storage
///
/// \return number of channels fired
///
template <int TYPE>
int SvxStripixel<TYPE>::makeRaw(SvxGhit*         ghit,
				SvxRawhitList*   rawlist,
				SvxGhitRawhitList* g2raw,
				svxAddress* SvxAddressObject,
				const int  rawFirst) const 
{
  short maptype = SvxAddressObject->get_maptype(); // ???
  bool           rhit_new; // new rawhit = true, merging raw hit = false
  int            npixel;   // number of fired pixels in a section
  int            ix, iz;   // x-row, z-column for fired pixel

  SvxRawhit     *cur_rhit   = NULL; // current Rawhit     pointer
  SvxGhitRawhit *cur_g2raw  = NULL; // current GhitRawhit pointer

  int   g_id = ghit->get_hitID();
  float xin  = ghit->get_xyzlocalin(0);
  float zin  = ghit->get_xyzlocalin(2);
  float xout = ghit->get_xyzlocalout(0);
  float zout = ghit->get_xyzlocalout(2);
  float chrg = ghit->get_dele();

  //--std::cout<<"  dE/dx="<<chrg<<std::endl;

  int raw_first   = ( rawFirst < 0 ) ? 0 : rawFirst;
  int nextraw     = rawlist->get_nRawhits();   // index of the next raw hit
  int g2raw_first = g2raw->get_nGhitRawhits(); // index of the 1st g2raw obj
  int nextg2raw   = g2raw_first;               // index of the next g2raw obj
  int nchan       = 0 ; // counter of the filled channels in this call    

  // loop over readouts and sections 
  // for ( unsigned int ird = 0; ird < nReadout; ird++ ) {
  /// \todo figure out if this really needs to be a loop anymore.
  /// this loop may pre-date charge sharing between x&u (which should only be
  /// done once)
  for (unsigned int ird=0; ird<1; ++ird) {
    for (unsigned int isec=0; isec<nSection; ++isec) {

      // charge sharing in both x and z directions 
      // and between x-u readouts with sharing parameters
      npixel = senSec[isec][ird]->firePixelsAndShareChargeXZ
	(xin  - secXpos[isec][ird], zin  - secZpos[isec][ird],
	 xout - secXpos[isec][ird], zout - secZpos[isec][ird],
	 chrg);

      int sectype = get_secTYPE(isec,ird);

      // cycle over fired (stri)pixels
      for (int jpix=0; jpix<npixel; ++jpix) {

	// locate fired channels for this deposit
	// fired channel ix and iz are within sensor section
	ix = senSec[isec][ird]->get_xrow(jpix);
	iz = senSec[isec][ird]->get_zcolumn(jpix);
	int ch[2] = {0,0};
	int iroc = -1;
	int imodule = -1;
	int mychannel1 = -1;

	// ---this may be a stale comment---
	// Use SvxAddressObject to get SENSOR BASED channel numbers 
	// corresponding to X and Z positions. For PIXELS, strips are still 
	// sensor section based.

	int tmpz = iz;
	for(unsigned int itmp=0; itmp<isec; ++itmp) {
	  tmpz += senSec[itmp][0]->get_nZpitch(); 
	}
	
	/// \todo maptype 0 is deprecated, see SvxAddress
	if (maptype==0) {
	  mychannel1 = SvxAddressObject->getChannelSensor0(ix,tmpz);
	} else {
	  iroc = SvxAddressObject->getROCSensor0(layer, ladder, 
						 sensor, ix, tmpz); 
	  imodule = SvxAddressObject->getModuleSensor0(layer, ladder, 
						       sensor, ix,tmpz);
	}

	int mychannel2 = 0; 
	if (sectype<10) {
	  mychannel1 = SvxAddressObject->getChannelSensor0(layer, ladder, 
							   sensor, ix, tmpz, 0);
	} else if (sectype==10) { 
	  // still section based counting for strips
	  mychannel1 = SvxAddressObject->getChannelSensor0(layer, ladder, 
							   sensor, ix, tmpz, 0);
	  mychannel2 = SvxAddressObject->getChannelSensor0(layer, ladder, 
							   sensor, ix, tmpz, 1);
	}

	ch[0] = mychannel1;
	ch[1] = mychannel2;

	// set charge sharing parameters
	float ShareRatioXU[2]={0}; // 0:x 1:u
	if (nReadout==1) {       
	  // pixel channels don't share charge with anyone
	  ShareRatioXU[0] = 1.0;
	} else if (nReadout==2) { 
	  // stripixel share between x and u
	  float ChargeAsym = rnd->Gaus(0,sAQ);
	  ShareRatioXU[0] = 0.5*(1.0 - ChargeAsym);      
	  ShareRatioXU[1] = 0.5*(1.0 + ChargeAsym);      
	  //--std::cout<<"   share = "<<ShareRatioXU[0]<<" "<<ShareRatioXU[1]<<" : chrg="<<chrg
          //--         <<" ch_pix="<<senSec[isec][ird]->get_charge(jpix)<<std::endl;
	} else {
	  std::cout << "Read out more than 2; Unknow sensor type." << std::endl;
	}

	// produce ADC (actually charge units) values
	int xuADC[2] = {0};
	xuADC[0] = (int)(senSec[isec][ird]->get_charge(jpix)*ShareRatioXU[0]);
	xuADC[1] = senSec[isec][ird]->get_charge(jpix) - xuADC[0];

	for(unsigned int xu=0; xu<nReadout; ++xu) {

	  // Merging raw hits?
	  rhit_new = true;
	  for (int i=raw_first; i<nextraw; ++i) {

	    cur_rhit = rawlist->get_Rawhit(i);

	    if (ch[xu]==cur_rhit->get_channel()) {
	      if (cur_rhit->get_sensorSection()==(int)isec) {
		if (cur_rhit->get_sensorReadout()==(int)xu) {
		  if (rawFirst<0) {
		    rhit_new = !(checkID(cur_rhit));
		  } else {
		    rhit_new = false;
		  } // rawFirst
		} //sensorReadout
	      } //sensorSection
	    } // chan

	    if (!rhit_new) break;
	  } 

	  if (rhit_new) { // this is a distinct new hit
	    cur_rhit = rawlist->addRawhit();
	    cur_rhit->set_svxSection(svxSection);
	    cur_rhit->set_layer(layer);
	    cur_rhit->set_ladder(ladder);
	    cur_rhit->set_sensor(sensor);
	    cur_rhit->set_sensorSection(isec);
	    cur_rhit->set_sensorReadout(xu);
	    cur_rhit->set_sensorType(sensorType);
	    cur_rhit->set_channel(ch[xu]);
	    cur_rhit->set_adc(xuADC[xu]);
	    cur_rhit->set_pixelROC(iroc);
	    cur_rhit->set_pixelModule(imodule);

	    cur_g2raw = g2raw->addGhitRawhit();
	    cur_g2raw->set_ghitID  (g_id);
	    cur_g2raw->set_rawhitID(cur_rhit->get_hitID());

	    ++nchan;
	    ++nextraw;
	    ++nextg2raw;

	  } else { // overlapping merged rawhit

	    // add in the new charge counts
	    cur_rhit->set_adc(cur_rhit->get_adc() + xuADC[xu]);

	    // check and add GhitRawhit into the g2raw list
	    // if the different geant track enter same pixel, 
	    // then share same rawhitID 
	    int r_id = cur_rhit->get_hitID();
	    rhit_new = true;
	    for (int i=g2raw_first; i<nextg2raw; ++i) {
	      if (r_id==g2raw->get_GhitRawhit(i)->get_rawhitID()) {
		rhit_new = false;
	      }
	      if (!rhit_new) break;
	    }

	    if (rhit_new) {		
	      cur_g2raw = g2raw->addGhitRawhit();
	      cur_g2raw->set_ghitID(g_id);
	      cur_g2raw->set_rawhitID(r_id);
	      ++nextg2raw;
	    }

	  } // new or merged hit
	} // cycle for x-u  
      } // jpix
    } // isec
  } // ird
  
  return nchan;
}

#endif // __SVXSTRIPIXEL_H__
