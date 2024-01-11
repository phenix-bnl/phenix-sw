// $Id: mRxnpUnpackPRDF.cxx,v 1.10 2007/11/30 19:49:56 hpereira Exp $
/*!
  \file    mRxnpUnpackPRDF.cxx
  \brief   unpack raw data hits. Generates interface class TRxnpRawScint_v1.
  \author  Chun Zhang
  \version $Revision: 1.10 $
  \date    $Date: 2007/11/30 19:49:56 $
*/

// std include
//
#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <list>
#include <algorithm>

// boost
//
#include <boost/shared_ptr.hpp>

// PHENIX include
//
#include <Event.h>
#include <PHCompositeNode.h>
#include <PHException.h>
#include <getClass.h>
#include <RunHeader.h>
#include <Bbc.hh>
#include <BbcOut.h>
#include <PHTimeStamp.h>

// MUTOO include
//
#include <TMutNode.h>
#include <MUTOO.h>
#include <PHTimer.h>

//Rxnp include
//
#include <TRxnpGeom.h>
#include <TRxnpFEM.h>
#include <TRxnpRawScintMap.h>
#include <TRxnpScintMap.h>

#include "mRxnpUnpackPRDF.h"

// use name space findNode 
//
using namespace findNode;

//@{ 
//file scope utility functions
// getting bbc bin for calibration const
int get_BbcBin(PHCompositeNode*);

const int RXNP_ADC_MAX = 4096;
const int RXNP_ADC_BIN = 100;

// invert grey coding
//For zero or positive values of is, return the Gray code of n; if is is negative, return the inverse Gray code of n.
// From http://lib-www.lanl.gov/numerical/bookcpdf/c20-2.pdf
unsigned long FromGray(unsigned long n, int is);
//@}

// constructor
//
mRxnpUnpackPRDF::mRxnpUnpackPRDF():
  _timer(PHTimeServer::get()->insert_new("mRxnpUnpackPRDF"))
{
  MUTOO::TRACE("initializing module mRxnpUnpackPRDF");
}

// Event method
//
PHBoolean mRxnpUnpackPRDF::event(PHCompositeNode* top_node)
{
  // start the timer
  //
  _timer.get()->restart();

  // event loop put into a try-catch block
  //
  // reset the interface ptrs
  // call unpack method to fill the TRxnpScintRawMap
  // call calibrate method to fill the TRxnpScintMap
  //
  try 
    {
      // reset IOC pointers
      //
      set_interface_ptrs(top_node);
      // clear TRxnpRawScintMap and TrxnpScintMap
      //
      _raw_map->clear();
      _scint_map->clear();
      // protection over no physics events.
      //
      if(_evt->getEvtSequence() < 0)
	return false;
      // call unpack method
      //
      unpack_prdf();
      //      _raw_map->print();
      // call calibrate method
      //
      calibrate();
      //_scint_map->print();
    } 
  catch ( std::exception& e) 
    {
      MUTOO::TRACE(e.what());
      return False;
    }
      
  // stop the timer
  //
  _timer.get()->stop();
    
  // if verbose dump the information
  //
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) 
    {
      _raw_map->print();
      _scint_map->print();
    }
  if(_mod_par->get_verbosity() >= MUTOO::SOME)
    _timer.get()->print();
  
  return True;
}

// set IOC pointers
//
void mRxnpUnpackPRDF::set_interface_ptrs(PHCompositeNode* top_node)
{
  
  // module runtime parameters
  _mod_par = TMutNode<mRxnpUnpackPRDFPar>::find_node(top_node, "mRxnpUnpackPRDFPar");
  
  // raw scint map pointer
  _raw_map = TMutNode<TRxnpRawScintMap>::find_node(top_node, "TRxnpRawScintMap");

  // scint map pointer
  _scint_map = TMutNode<TRxnpScintMap>::find_node(top_node, "TRxnpScintMap");
  
  // event pointer
  _evt = TMutNode<Event>::find_node(top_node, "PRDF");
	
}

// unpack prdf
//
void mRxnpUnpackPRDF::unpack_prdf()
{
  // get data packet
  //
  // 
  //  std::cout << "look for packid "<< RXNP::RXNP_PACKID << std::endl;
  //std::cout << "evt number is "<< _evt->getEvtSequence() << std::endl;


  Packet *p = _evt->getPacket(RXNP::RXNP_PACKID);
  if(!p)
    {
      return;
      /*
      std::ostringstream THROW_EXP;
      THROW_EXP << "mRxnpUnpackPRDF::No data packet "<< RXNP::RXNP_PACKID << " is found. ";
      throw std::runtime_error(DESCRIPTION(THROW_EXP.str()));
      */
    }

  // Geomtry
  //
  boost::shared_ptr<TRxnpGeom> rxnp_geom = TRxnpGeom::instance();

  // get adc's and Tdc's for each channel 
  // we only look for the channels that are in the TRxnpFEM
  // 
  boost::shared_ptr<TRxnpFEM> rxnp_fem = TRxnpFEM::instance();

  const TRxnpFEM::ChanList* chan_list = rxnp_fem->get_chan_list(); 

  TRxnpFEM::ChanList::const_iterator iter = chan_list->begin(); 

  for(; iter!= chan_list->end(); ++iter)
    {
      // get electric channel id
      int channel = *iter;

      // insert a new RawScint object into raw map
      //

      int arm = rxnp_fem->get_arm(channel);
      int ring = rxnp_fem->get_ring(channel);
      int scint = rxnp_fem->get_scint(channel);

      TRxnpRawScintMap::iterator raw_iter = _raw_map->insert_new(arm, ring, scint);

      // fill the data members of new RawScint object
      //
      raw_iter->get()->set_chanid((unsigned short)(channel));
      // adc
      raw_iter->get()->set_high_post(p->iValue(channel,1));
      raw_iter->get()->set_low_post(p->iValue(channel,2));
      raw_iter->get()->set_high_pre(p->iValue(channel,3));
      raw_iter->get()->set_low_pre(p->iValue(channel,4));
      // tdc
      raw_iter->get()->set_tdc(p->iValue(channel,0));
      // amu cell
      //
      raw_iter->get()->set_amu_pre(FromGray(p->iValue(1, "AMU"), -1));
      raw_iter->get()->set_amu_post(FromGray(p->iValue(2, "AMU"), -1));
      raw_iter->get()->set_amu_tdc(FromGray(p->iValue(0, "AMU"), -1));
      // Geometry location
      //
      raw_iter->get()->set_phi(rxnp_geom->get_phi_at(arm, ring, scint));
      raw_iter->get()->set_theta(rxnp_geom->get_theta_at(arm, ring, scint));
    }
  
  if(p)
    delete p;  
}

// calibrate raw hits
//
void mRxnpUnpackPRDF::calibrate(int ibbc)
{
  // need calibration data base infor.
  //
  
  boost::shared_ptr<TRxnpFEM> fem = TRxnpFEM::instance();
  
  // 
  if(fem.unique())
    std::cout << PHWHERE << " TRxnpFEM is unique" << std::endl;

  // Loop through raw map, for each RawScint objects, construct a new Scint object and insert it in the scint map
  //
  TRxnpRawScintMap::const_iterator raw_iter = _raw_map->range();
  
  while(TRxnpRawScintMap::const_pointer raw_ptr = raw_iter.next())
    {
      
      unsigned short arm   = raw_ptr->get()->get_arm();
      unsigned short ring  = raw_ptr->get()->get_ring();
      unsigned short scint = raw_ptr->get()->get_scint(); 

      // amu cell number of pre and tdc
      //
      unsigned short amu_pre = raw_ptr->get()->get_amu_pre();     
      unsigned short amu_tdc = raw_ptr->get()->get_amu_tdc();     
      
      // electronic channel
      int ichann = raw_ptr->get()->get_chanid();
      
      TRxnpScintMap::iterator scint_iter = _scint_map->insert_new(arm,
								  ring,
								  scint);
      // fill the contents for this new scint object, need calibration data base infor.
      //
      float high_e = (raw_ptr->get()->get_high_post()-raw_ptr->get()->get_high_pre()-fem->get_high_int(ichann, amu_pre))*fem->get_high_slop(ichann, ibbc);
      //int high_nhits = (int) (high_e/loc->get_high_mip());
      float low_e = (raw_ptr->get()->get_low_post()-raw_ptr->get()->get_low_pre()-fem->get_low_int(ichann, amu_pre))*fem->get_low_slop(ichann, ibbc);;

      // threshold for tdc = pedestel - 20*rms
      //
      float thr = fem->get_tdc_int(ichann, amu_tdc) - 20.0*fem->get_tdc_wid(ichann, amu_tdc);
      float tdc = raw_ptr->get()->get_tdc();
      if (tdc>0 && tdc<thr) 
	{
	  // determine lg adc bin for slew correction
	  //
	  unsigned short low_adc = (unsigned short)( raw_ptr->get()->get_low_post()-raw_ptr->get()->get_low_pre()-fem->get_low_int(ichann, amu_pre) );	  
	  // copy from Wei's online code
	  float dadc = log((double)RXNP_ADC_MAX)/RXNP_ADC_BIN;
	  unsigned short iadc = (unsigned short )(log((double)low_adc)/dadc);
	  // protect for no hit
	  //
	  if(iadc > 99) 
	    iadc = 0;
	  tdc-=(fem->get_tdc_slop(ichann, ibbc)+fem->get_tdc_lg_slewcoeff(ichann,iadc));
	}
      else 
	tdc=-9999.9;
      //tof = raw_ptr->get()->get_tdc() - fem->get_tdc_int(ichann, amu_tdc);
      
      if (ichann==11) {high_e=0; low_e=0; tdc=-9999.9;}
      scint_iter->get()->set_high_e(high_e);
      scint_iter->get()->set_low_e(low_e);
      scint_iter->get()->set_tof(tdc);
      scint_iter->get()->set_phi(raw_ptr->get()->get_phi());
      scint_iter->get()->set_theta(raw_ptr->get()->get_theta());
    }
}

int get_BbcBin(PHCompositeNode* topNode)
{
  //Get BBC charge sum from south and north. In Oncal, the calibration consts
  //are determined in each BBC charge bin
  //
  // Total bbc bins
  const int NBIN = 20;
  // bbc charge coverage per bin 
  const double dbbc = 100.0;
  // return bin
  // 
  int bin = 0;

  // get bbc data node
  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");

  if(bbcout)
    {
      double charge = bbcout->get_ChargeSum(Bbc::North) + bbcout->get_ChargeSum(Bbc::South);
      bin = (int(charge/dbbc) < NBIN ) ?  int(charge/dbbc) : 19;
    }
  else
    {
      std::cerr<< PHWHERE << " No BBC Node is found. bbc bin is set to 0 "<< std::endl; 
    }

  return bin;
}

unsigned long FromGray(unsigned long n, int is)
{
  int ish;
  unsigned long ans,idiv;
  if ( is >= 0 ) return n ^ (n >> 1);
  ish=1;
  ans=n;
  for (;;) 
    {
      ans ^= (idiv=ans >> ish);
      if (idiv <= 1 || ish == 16) return ans;
      ish <<= 1;
    }
}
