//----------------------------------------------------------------------
//  Implementation of class TofEvent
//
//  Author: Tatsuya Chujo (BNL)
//
//  History: 06/11/00  T.Chujo      First Version
//           06/13/00  A.Kiyomichi  Add q1 q2 t3 t4 in dtofRaw
//           07/08/00  T.Chujo      RawToDst update
//           07/12/00  A.Kiyomichi  Add cut parameter
//           07/20/00  A.Kiyomichi  dTofRaw move to dstNode
//           07/20/00  A.Kiyomichi  Add dTofRawRec relational table
//           07/27/00  A.Kiyomichi  delete dTofDCM; renew Unpack part
//           08/05/00  A.Kiyomichi  delete AMU info.
//           11/01/00  A.Kiyomichi  Add DstReCycle
//           12/21/00  A.Kiyomichi  set evaNode (dTofRawRec)
//           06/26/01  T.Chujo      Add new hitformat (1007:Q1,Q3,T3,T4)
//           07/19/01  T.Chujo      Add zero-suppression 1107 hitformat
//           07/19/01  T.Chujo      Change indent format
//           11/29/01  A.Kiyomichi  Add GeaToRaw [response chain]
//           12/14/01  T.Chujo      Introduced PHTypedNodeIterator (dTofRaw)
//           01/11/02  A.Kiyomichi  Add TC4 in DST with RHIC clothing time
//           12/08/05  H.Masui      Clean up
//----------------------------------------------------------------------

#include "TofEvent.hh"
#include "dTofRawWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "dTofRawRecWrapper.h"

#include "tofghitWrapper.h"
#include "dTofGhitRawWrapper.h"
#include "utiPrototype.hh"

//INCLUDECHECKER: Removed this line: #include "Tof.hh"
#include "TofAddressObject.hh"
#include "TofGeometryObject.hh"
#include "TofCalibObject.hh"

#include <phool.h>
//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include <PHTable.hh>
#include <Event.h>
#include <getClass.h>

#include <gsl/gsl_randist.h>

#include <cassert>
#include <cmath>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<dTofRawWrapper> TofRawNode_t;

TofEvent::TofEvent()
{
  //set default values as 0
  iDebug = 0;
  setCutParameter(0.0,10.0); // ChargeCut, TvcPedeCut
  setCrossingTime(106.5805); // RHIC crossing time [ns] 

  setTimingResolution(0.080);  // intrinsic timing resolution [ns]
  setAttenuationLength(128.0); // Scintillator attenuation length [cm]
  rng = gsl_rng_alloc (gsl_rng_mt19937);
  gsl_rng_set(rng, 987654);
}

TofEvent::~TofEvent()
{
  gsl_rng_free (rng);
  return;
}

PHBoolean TofEvent::setCutParameter(const float chargecut , const float tvcpedecut)
{
  for (int islat=0; islat<TOF_NSLAT; islat++)
    {
      setChargeCut(islat, chargecut);
      for (int lu=0; lu < 2; lu++)
	{
	  setTvcPedeCut(lu, islat,tvcpedecut);
	}
    }
  return True;
}

void TofEvent::setChargeCut(const int slatid, const float v)
{
  if(slatid<0 || slatid>=TOF_NSLAT){
    cout << PHWHERE << " Error : Invarid slatid, slatid= " << slatid << endl;
    return;
  }

  chargecut[slatid] = v;
}

void TofEvent::setTvcPedeCut(const int lu, const int slatid, const float v)
{
  if(lu<0 || lu>=2){
    cout << PHWHERE << " Error : Invalid pmtid, lu= " << lu << endl;
    return;
  }

  if(slatid<0 || slatid>=TOF_NSLAT){
    cout << PHWHERE << " Error : Invarid slatid, slatid= " << slatid << endl;
    return;
  }

  tvcpedecut[slatid][lu] = v;
}

void TofEvent::setCrossingTime(const float v)
{
  crossingtime = v; 
}

PHBoolean
TofEvent::DcmToRaw(PHCompositeNode *root, TofAddressObject *address)

{
  Event* event = findNode::getClass<Event>(root, "PRDF");

  //
  // get dTofRaw table
  //
  dTofRawWrapper* TofRawWrapper = findNode::getClass<dTofRawWrapper>(root, "dTofRaw");
  if (!TofRawWrapper)
    {
      cout << PHWHERE << " TofEvent::DcmToRaw: dTofRaw Node not found" << endl;
      return False;
    }

  DTOFRAW_ST* tofraw = TofRawWrapper->TableData();
  
  // Loop for DCM
  // Hit format (= scheme) 607 for Run-01 non-suppressed  format (Q1,Q2,T3,T4)
  //                       407 for simulation
  //                      1007 for Run-02 non-suppressed  format (Q1,Q3,T3,T4)
  //                      1107 for Run-02 zero-suppressed format (Q1,Q3,T3,T4)
  static const int id_base     = 7001;
  static const int max_packets = 8;

  short cell[TOF_NSLAT][2];
  short qvc[TOF_NSLAT][2] ;
  short q1[TOF_NSLAT][2]  ;
  short q2[TOF_NSLAT][2]  ;
  short tvc[TOF_NSLAT][2] ;
  short t3[TOF_NSLAT][2]  ;
  short t4[TOF_NSLAT][2]  ;

  for(int islat=0;islat<TOF_NSLAT;islat++){
    for(int lu=0;lu<2;lu++){
      cell[islat][lu] = -1;
      qvc[islat][lu]  = -1;
      q1[islat][lu]   = -1;
      q2[islat][lu]   = -1;
      tvc[islat][lu]  = -1;
      t3[islat][lu]   = -1;
      t4[islat][lu]   = -1;
    }
  }

  for (int k = 0; k < max_packets; k++)
    { 
      const int id = id_base + k;
      Packet* pTOF = 0;

      if((pTOF =  event->getPacket(id))) 
	{
	  const int scheme = pTOF->getHitFormat();
	  const int nword  = pTOF->getDataLength();
	  
	  // Scheme 607 selection(IDTOF_DCM0) for Run-01
	  //            selection(IDTOF_DCM2) for Run-02
	  // (Q1,Q2,T3,T4)
	  if (scheme == 607)
	    {
	      //  ## commentout by AK   Aug-05-2000 ##
	      //int detid    =  pTOF->iValue(0, "DETID") & 0xff;
	      //int event_n  =  pTOF->iValue(0, "EVTNR") & 0xfff;
	      //int icrate   = (pTOF->iValue(0, "MODULE")& 0xf) - 1;

	      int icrate = k;  // ## add by AK Aug-05-2000 ##
	      for (int iboard = 0; iboard < TOF_NBOARD; iboard++)
		{ // board loop

		  // Check slot validity
		  if( !address->getSlotValidity(icrate, iboard)) continue;

		  //  ## commentout by AK   Aug-05-2000 ##
		  //short icell1 = pTOF->iValue(iboard, "AMU1");
		  //short icell2 = pTOF->iValue(iboard, "AMU2");
		  //short icell = icell1;

		  short icell = 0;  // ## add by AK Aug-05-2000 ##
		  for(int ich = 0; ich < TOF_NCHANNEL; ich++)
		    { // channel loop

		      // Check channel validity
		      if( !address->getChannelValidity(icrate, iboard, ich)) continue;

		      int channel = iboard*TOF_NBOARD + ich; // channel seq. [0-255]
		      short qvc_1 = pTOF->iValue(channel, "QC1");
		      short qvc_2 = pTOF->iValue(channel, "QC2");
		      short tvc_3 = pTOF->iValue(channel, "TC3");
		      short tvc_4 = pTOF->iValue(channel, "TC4");
		      
		      // find slat ID and PMT ID by TofAddressObject
		      int slatid = address->getSlatID(icrate,iboard,ich);
		      int lu     = address->getPmt(icrate,iboard,ich);
		      
		      cell[slatid][lu] = icell;
		      qvc[slatid][lu]  = qvc_1 - qvc_2;
		      q1[slatid][lu]   = qvc_1;
		      q2[slatid][lu]   = qvc_2;
		      tvc[slatid][lu]  = tvc_3;
		      t3[slatid][lu]   = tvc_3;
		      t4[slatid][lu]   = tvc_4;
		    } // end of channel loop I
		} // end of board loop
	    } // end of sheme selection (607)
	  
	  // Scheme 1007 selection(IDTOF_FPGA) for Run-02 non-suppressed
	  // Scheme 1107 selection(IDTOF_FPGA0SUP) for Run-02 zero-suppressed
	  // (Q1,Q3,T3,T4)
	  else if(scheme == 1007 || scheme == 1107) 
	    {
	      int icrate = k;  
	      for (int iboard = 0; iboard < TOF_NBOARD; iboard++)
		{ // board loop

		  // Check slot validity
		  if( !address->getSlotValidity(icrate, iboard)) continue;

		  short icell = 0;  
		  for(int ich = 0; ich < TOF_NCHANNEL; ich++)
		    { // channel loop

		      // Check channel validity
		      if( !address->getChannelValidity(icrate, iboard, ich)) continue;

		      int channel = iboard*TOF_NBOARD + ich; // channel seq. [0-255]
		      short qvc_1 = pTOF->iValue(channel, "QC1");
		      short qvc_3 = pTOF->iValue(channel, "QC3");
		      short tvc_3 = pTOF->iValue(channel, "TC3");
		      short tvc_4 = pTOF->iValue(channel, "TC4");
		      
		      // find slat ID and PMT ID by TofAddressObject
		      int slatid = address->getSlatID(icrate,iboard,ich);
		      int lu     = address->getPmt(icrate,iboard,ich);
	      
		      cell[slatid][lu] = icell;
		      qvc[slatid][lu]  = qvc_1 - qvc_3;
		      q1[slatid][lu]   = qvc_1;
		      q2[slatid][lu]   = qvc_3;
		      tvc[slatid][lu]  = tvc_3;
		      t3[slatid][lu]   = tvc_3;
		      t4[slatid][lu]   = tvc_4;
		    } // end of channel loop
		} // end of board loop
	    } // end of sheme selection (1007 or 1107)
	  
	  // Scheme 407 selection (IDTOF_DCM0)
	  else{
	    //if (scheme == 407)
	    int idcm = 5; // Skip DCM header
	    
	    while (idcm < nword) 
	      {//pTOF->iValue
		long idcm0  = idcm;
		short icrate = (short)((pTOF->iValue(idcm0+1) & 0xf00) / 0x100);
		short iboard = (short)((pTOF->iValue(idcm0+1) & 0x0f0) / 0x010);
		short icell  = (short) (pTOF->iValue(idcm0+2) & 0xffff);
		
		for (idcm += 5; idcm < nword; idcm +=3)
		  {      
		    short ich = (short)((pTOF->iValue(idcm) & 0xff00000)/0x100000);
		    unsigned short tvc_3 = (pTOF->iValue(idcm)   & 0xfff);
		    unsigned short qvc_1 = (pTOF->iValue(idcm+1) & 0xfff);
		    unsigned short qvc_2 = (pTOF->iValue(idcm+2) & 0xfff);
		    
		    unsigned short data_type = (pTOF->iValue(idcm) & 0xf0000) / 0x10000;
		    if (data_type != 12) 
		      {
			break;
		      }
		    
		    // find slat ID and PMT ID by TofAddressObject
		    int slatid = address->getSlatID(icrate, iboard, ich);
		    int lu     = address->getPmt(icrate,iboard,ich);
		    assert(lu>=0);		    
		    if(slatid >= 0)
		      {
			cell[slatid][lu] = icell; 
			qvc[slatid][lu]  = qvc_2 - qvc_1;
			q1[slatid][lu]   = qvc_1;
			q2[slatid][lu]   = qvc_2;
			tvc[slatid][lu]  = tvc_3;
			t3[slatid][lu]   = tvc_3;
			t4[slatid][lu]   = -1; //t4 is not defined in scheme 407
		      }
		  } // end of ch roop (407)
	      } // end of dcm roop (407)
	  } // end of scheme selection (407)
	  delete pTOF;
	} //end if getPacket
    } // end of max_packets (crate loop)
  
  // Filling Raw data
  int iraw = 0;
  for ( int islat=0;islat<TOF_NSLAT;islat++ ) 
    {
      //if((qvc[islat][0]>0&&qvc[islat][0]<4095)||
      //   (qvc[islat][1]>0&&qvc[islat][1]<4095)||
      //   (tvc[islat][0]>0&&tvc[islat][0]<4095)||
      //   (tvc[islat][1]>0&&tvc[islat][1]<4095)){  // IF zero-suppression 
      //}

      tofraw[iraw].id     = iraw;
      tofraw[iraw].slatid = islat;
      tofraw[iraw].sector = address->getSector(islat);
      tofraw[iraw].side   = address->getSide(islat);
      tofraw[iraw].panel  = address->getPanel(islat);
      tofraw[iraw].slat   = address->getSlat(islat);
      for( int lu=0;lu<2;lu++)
	{
	  tofraw[iraw].cell[lu] = cell[islat][lu];
	  tofraw[iraw].qvc[lu]  = qvc[islat][lu];
	  tofraw[iraw].q1[lu]   = q1[islat][lu];
	  tofraw[iraw].q2[lu]   = q2[islat][lu];
	  tofraw[iraw].tvc[lu]  = tvc[islat][lu];
	  tofraw[iraw].t3[lu]   = t3[islat][lu];
	  tofraw[iraw].t4[lu]   = t4[islat][lu];
	}
      iraw++;
    }  
  TofRawWrapper->SetRowCount(iraw); // Adjust size of dTofRaw
  return True;

}

PHBoolean
TofEvent::RawToDst(PHCompositeNode *root, TofAddressObject *address,
		   TofGeometryObject *geom, TofCalibObject *calib) 
{

  //
  // get dTofRaw table
  //
  dTofRawWrapper* TofRawWrapper = findNode::getClass<dTofRawWrapper>(root,"dTofRaw");
  if (!TofRawWrapper)
    {
      cout << PHWHERE << " TofEvent::RawToDst: dTofRaw Node not found" << endl;
      exit(1);
    }

  //
  // get dTofReconstructed table
  //
  dTofReconstructedWrapper* TofReconstructedWrapper = findNode::getClass<dTofReconstructedWrapper>(root,"dTofReconstructed");
  if (!TofReconstructedWrapper)
    {
      cout << PHWHERE << "dTofReconstructed Node not found, exiting" << endl;
      exit(1);
    }

  // Extract the data from the dTofRawRec
  dTofRawRecWrapper *TofRawRecWrapper = findNode::getClass<dTofRawRecWrapper>(root,"dTofRawRec");
  if (!TofRawRecWrapper)
    {
      cout << PHWHERE << "dTofRawRec Node not found, exiting" << endl;
      exit(1);
    }

  DTOFRAW_ST           *tofraw           = TofRawWrapper->TableData();
  DTOFRECONSTRUCTED_ST *tofreconstructed = TofReconstructedWrapper->TableData();
  DTOFRAWREC_ST        *tofrawrec        = TofRawRecWrapper->TableData();
  
  // Calculate end products
  int    hitcwn =0;
  
  for (unsigned int k=0; k<TofRawWrapper->RowCount(); k++) 
    {
      int rawid = tofraw[k].id;
      int islat = tofraw[k].slatid;

      float  ftvc_3[2], ftvc_4[2], fqvc[2];
      float  ftvc_pede[2];

      // Get TVC and QVC, pedestal and slewing parameters
      ftvc_3[0] = tofraw[k].t3[0]; // same value with tofraw[k].tvc[0]
      ftvc_3[1] = tofraw[k].t3[1]; // same value with tofraw[k].tvc[1]
      ftvc_4[0] = tofraw[k].t4[0];
      ftvc_4[1] = tofraw[k].t4[1];
      fqvc[0]   = tofraw[k].qvc[0];
      fqvc[1]   = tofraw[k].qvc[1];
      
      ftvc_pede[0] = calib->getTvcPede(0,islat);
      ftvc_pede[1] = calib->getTvcPede(1,islat);
      
      float sqrt_qvc = sqrt(fqvc[0] * fqvc[1]);
      
      bool t3_lower = (ftvc_pede[0] - ftvc_3[0] > getTvcPedeCut(0,k));
      bool t4_lower = (ftvc_pede[0] - ftvc_4[0] > getTvcPedeCut(0,k));
      bool t3_upper = (ftvc_pede[1] - ftvc_3[1] > getTvcPedeCut(1,k));
      bool t4_upper = (ftvc_pede[1] - ftvc_4[1] > getTvcPedeCut(1,k));

      // Hit slat selection by TVC and QVC / calculate end products 
      if (sqrt_qvc > getChargeCut(k) && 
	  (t3_lower||t4_lower)&&(t3_upper||t4_upper))
	{
      
	  tofreconstructed[hitcwn].id          = hitcwn;
	  tofreconstructed[hitcwn].slatid      = islat;
	  tofreconstructed[hitcwn].sector      = address->getSector(islat);
	  tofreconstructed[hitcwn].side        = address->getSide(islat);
	  tofreconstructed[hitcwn].panel       = address->getPanel(islat);
	  tofreconstructed[hitcwn].slat        = address->getSlat(islat);
	  
	  tofreconstructed[hitcwn].qvc[0]      = short(fqvc[0]);
	  tofreconstructed[hitcwn].qvc[1]      = short(fqvc[1]);
	  if(t3_lower)
	    {
	      tofreconstructed[hitcwn].tvc[0]  = short(ftvc_3[0]);
	    }
	  else
	    {
	      tofreconstructed[hitcwn].tvc[0]  = short(ftvc_4[0])+0x1000;
	    }

	  if(t3_upper)
	    {
	      tofreconstructed[hitcwn].tvc[1]  = short(ftvc_3[1]);
	    }
	  else
	    {
	      tofreconstructed[hitcwn].tvc[1]  = short(ftvc_4[1])+0x1000;
	    }
	  //
	  // Filling relational table
	  //	
	  tofrawrec[hitcwn].rawid  = rawid;
	  tofrawrec[hitcwn].slatid = islat;
	  tofrawrec[hitcwn].recid  = hitcwn;
	  
	  hitcwn++;
	}
    }

  //
  // Adjust size of dTofReconstructed
  TofReconstructedWrapper->SetRowCount(hitcwn);
  TofRawRecWrapper->SetRowCount(hitcwn);
  
  return DstCalculator(root, address, geom, calib);
}

PHBoolean TofEvent::DstReCycle(PHCompositeNode *root, TofAddressObject *address,
		     TofGeometryObject *geom, TofCalibObject *calib) 
{
  // DstReCycle: Use in DST analysis  dTofReconstructed => dTofReconstructed
  //                                    slatid,qvc,tvc  =>   tof,xtof,eloss
  
  return DstCalculator(root, address, geom, calib);
}


PHBoolean TofEvent::DstCalculator(PHCompositeNode *root, TofAddressObject *address,
			TofGeometryObject *geom, TofCalibObject *calib) 
{

  //
  // get dTofReconstructed table
  //
  dTofReconstructedWrapper *TofReconstructedWrapper = findNode::getClass<dTofReconstructedWrapper>(root,"dTofReconstructed");

  
  DTOFRECONSTRUCTED_ST *tofreconstructed = TofReconstructedWrapper->TableData();
  
  // Calculate end products
  //const  float c0 = 0.150;    // timing resolution at the edge of PMT [ns]
  const  float c0 = 0.080;    // timing resolution at the center of Scinti.[ns]
  
  
  for (unsigned int hitcwn=0; hitcwn<TofReconstructedWrapper->RowCount(); hitcwn++) 
    {
      int islat = tofreconstructed[hitcwn].slatid;
      float ftvc[2];
      float fqvc[2];
      ftvc[0] = tofreconstructed[hitcwn].tvc[0] & 0xfff;
      ftvc[1] = tofreconstructed[hitcwn].tvc[1] & 0xfff;
      fqvc[0] = tofreconstructed[hitcwn].qvc[0];
      fqvc[1] = tofreconstructed[hitcwn].qvc[1];
      
      float sqrt_qvc = sqrt(fqvc[0] * fqvc[1]);
      float timing[2];
      timing[0] = ftvc[0]*calib->getTvcConv(0,islat);
      timing[1] = ftvc[1]*calib->getTvcConv(1,islat);
      
      if(tofreconstructed[hitcwn].tvc[0] > 0xfff) // This is TC4
	{
	  timing[0] += crossingtime;
	}
      if(tofreconstructed[hitcwn].tvc[1] > 0xfff) // This is TC4
	{
	  timing[1] += crossingtime;
	}

      float slat_length     = geom->getSlatLength(islat);
//      float slat_halflength = slat_length/2.;
      double velocity = calib->getVelocity(islat);
      float yoffset  = calib->getYoffset(islat);

      // timing difference
      float tdiff = (timing[0] - timing[1])/2.0;
      
      // slewing effect
      float slewpar[2];
      slewpar[0] = calib->getSlewPar_a(0,islat)
	+ calib->getSlewPar_b(0,islat)/sqrt(fqvc[0]);
      
      slewpar[1] = calib->getSlewPar_a(1,islat)
	+ calib->getSlewPar_b(1,islat)/sqrt(fqvc[1]);
      
      float tofslew = (slewpar[0] + slewpar[1])/2.;
      
      // Time of Flight calculation
      float ftof = (timing[0] + timing[1])/2.;      
      float tof = ftof - tofslew - calib->getToffset(islat) - calib->getGlobalT();
      
      if(tof < 0.0) tof = 0.0;
      
      // Energy loss calculation
      float eloss = sqrt_qvc * calib->getElossConv(islat);
      
      // Hit position calculation
      float ptof = (timing[0] - timing[1])/2.;
      float ypos = ptof*velocity;
      
      PHPoint hitpoint(0,0,0);    // TOF hit position by PHPoint
      hitpoint = geom->getSlatXYZ(islat) + 
	(PHPoint)geom->getSlatVector(islat)*(ypos - yoffset);
      
      // TOF error calculation
      float atten   = attenuation[islat];
      double tof_err = c0*exp(slat_length/(4.*atten))*sqrt(cosh(2*ypos/(2.*atten)));
      
      // Hit position error calculation
      PHPoint hiterror(geom->getSlatVector(islat));    // TOF hit position error by PHPoint
      double velocity_tof_err = velocity*tof_err;
      hiterror = hiterror * velocity_tof_err;
      
      // energy loss error calculation
      float eloss_err = 0.0;
      //
      // Filling DST data
      //
      tofreconstructed[hitcwn].tof         = tof;
      tofreconstructed[hitcwn].tof_err     = tof_err;
      tofreconstructed[hitcwn].eloss       = eloss;
      tofreconstructed[hitcwn].eloss_err   = eloss_err;
      tofreconstructed[hitcwn].xtof[0]     = hitpoint.getX();
      tofreconstructed[hitcwn].xtof[1]     = hitpoint.getY();
      tofreconstructed[hitcwn].xtof[2]     = hitpoint.getZ();
      tofreconstructed[hitcwn].xtof_err[0] = hiterror.getX();
      tofreconstructed[hitcwn].xtof_err[1] = hiterror.getY();
      tofreconstructed[hitcwn].xtof_err[2] = hiterror.getZ();
      tofreconstructed[hitcwn].tdiff       = tdiff;
    }
  return True;
}

// public function
//=====================
// TOF response chain
//=====================

PHBoolean TofEvent::setTimingResolution(const float sigma)
{
  for (int islat=0; islat<TOF_NSLAT; islat++)
    {
      setTimingResolution(islat, sigma);
    }
  return True;
}

PHBoolean TofEvent::setAttenuationLength(const float atten)
{
  for (int islat=0; islat<TOF_NSLAT; islat++)
    {
      setAttenuationLength(islat, atten);
    }
  return True;
}

PHBoolean 
TofEvent::GeaToRaw(PHCompositeNode *root, TofAddressObject *address,
		   TofGeometryObject *geom, TofCalibObject *calib) 
{
  //
  // get tofghit table
  //
  tofghitWrapper *tofghitWrap = findNode::getClass<tofghitWrapper>(root,"tofghit");
  if (!tofghitWrap)
    {
      cout << PHWHERE << "tofghit Node missing, exiting" << endl;
      exit(1);
    }

  TOFGHIT_ST *tofghit = tofghitWrap->TableData();

  //
  // get dTofRaw table
  //
  dTofRawWrapper *TofRawWrapper = findNode::getClass<dTofRawWrapper>(root,"dTofRaw");
  if (!TofRawWrapper)
    {
      cout << PHWHERE << "dTofRaw Node missing, exiting" << endl;
      exit(1);
    }
  DTOFRAW_ST* tofraw = TofRawWrapper->TableData();
  
  //
  // get dTofGhitRaw table
  //
  dTofGhitRawWrapper *TofGhitRawWrapper = findNode::getClass<dTofGhitRawWrapper>(root,"dTofGhitRaw");
  if (!TofGhitRawWrapper)
    {
      cout << PHWHERE << "dTofGhitRaw Node missing, exiting" << endl;
      exit(1);
    }

  DTOFGHITRAW_ST* tofghitraw = TofGhitRawWrapper->TableData();
  
  //=======================================================================
  // Convert from mTofGhitRawModule.C
  //=======================================================================

  short  nghit_total = tofghitWrap->RowCount();
  if(iDebug>0)
    {
      cout<<"Number of rows in TOFGHIT = "<<nghit_total<<endl;
    }

  // Initialize
  short  raw_slatid[TOF_NSLAT];
  float  raw_eloss_l[TOF_NSLAT];
  float  raw_eloss_u[TOF_NSLAT];
  float  raw_tof_l[TOF_NSLAT];
  float  raw_tof_u[TOF_NSLAT];
  short  nraw = 0;

  for(int islat=0; islat<TOF_NSLAT; islat++)
    {
      raw_slatid[islat] = -1;
      raw_eloss_l[islat] = 0.;
      raw_eloss_u[islat] = 0.;
      raw_tof_l[islat] = 4096.*calib->getTvcConv(0, islat);
      raw_tof_u[islat] = 4096.*calib->getTvcConv(1, islat);
    }

  // Add dele and find fastest tof for each PMT
  for(int ighit=0; ighit<nghit_total; ighit++)
    {
      short partl          = tofghit[ighit].partl;
      long mctrack         = tofghit[ighit].mctrack;
      float g_pos_hit_slat = tofghit[ighit].pos_hit_slat;
      float g_tof          = tofghit[ighit].tof;
      float g_dele         = tofghit[ighit].dele;
//      short g_subvol       = tofghit[ighit].subvol;
      short g_panel        = tofghit[ighit].panel;
      short g_column       = tofghit[ighit].column;
      short g_pslat        = tofghit[ighit].pslat;
      short ghit_slatid    = address->getSlatIDfromPISA(g_panel, g_column, g_pslat);

      float ypos;
      if(g_column%2 == 0)
	{
	  ypos = - g_pos_hit_slat; // distance from center of scintillator
	} 
      else 
	{
	  ypos = g_pos_hit_slat;
	}
      
      if((partl <= 0)||(mctrack <= 0))
	{
	  // No data in tofghit
	  tofghitraw[ighit].ghitid = ighit;
	  tofghitraw[ighit].slatid = -1;
	  tofghitraw[ighit].rawid  = -1;
	} 
      else 
	{
	  short tmpid = -1;
	  // Search new hit
	  for(int iraw = 0; iraw < nraw; iraw++)
	    {
	      if(raw_slatid[iraw] == ghit_slatid)
		{
		  tmpid = iraw;
		  break;
		}
	    }
	  // Add dele and find fastest tof for each PMT
	  short rawid;
	  if(tmpid >= 0)
	    {
	      rawid = tmpid;
	    } 
	  else 
	    {
	      rawid = nraw;
	      raw_slatid[nraw] = ghit_slatid;
	      nraw++;
	    }
	  short islat = raw_slatid[rawid];
	  float velocity = calib->getVelocity(islat);
	  float yoffset  = calib->getYoffset(islat);
	  float Toffset  = calib->getToffset(islat) + calib->getGlobalT();
//    	    float slat_length     = geom->getSlatLength(islat);
//          float slat_halflength = slat_length/2.;

	  // add dele
	  float atten = attenuation[islat];
	  raw_eloss_l[rawid] += g_dele*exp(-( + ypos)/atten);
	  raw_eloss_u[rawid] += g_dele*exp(-( - ypos)/atten);
	  //raw_eloss_l[rawid] += g_dele*exp(-(slat_halflength + ypos)/atten);
	  //raw_eloss_u[rawid] += g_dele*exp(-(slat_halflength - ypos)/atten);

	  // find fastest tof
	  float ftof[2];
	  ftof[0] = g_tof + Toffset + (ypos + yoffset)/velocity;
	  ftof[1] = g_tof + Toffset - (ypos + yoffset)/velocity;
	  if(ftof[0] < raw_tof_l[rawid]) raw_tof_l[rawid] =  ftof[0];
	  if(ftof[1] < raw_tof_u[rawid]) raw_tof_u[rawid] =  ftof[1];

	  // Relational table between tofghit and dTofRaw
	  tofghitraw[ighit].ghitid = ighit;
	  tofghitraw[ighit].slatid = islat;
	  tofghitraw[ighit].rawid  = rawid;
	} // if((partl <= 0)||(mctrack <= 0)){ -- } else {
    } // for(i=0; i<nghit_total; i++){

  // Digitize qvc and tvc, and fill to dTofRaw
  for(int iraw=0; iraw<nraw; iraw++)
    {
      short islat = raw_slatid[iraw];
      float eloss_l = raw_eloss_l[iraw];
      float eloss_u = raw_eloss_u[iraw];
      float tof_l = raw_tof_l[iraw];
      float tof_u = raw_tof_u[iraw];

      short tvc_pede[2];
      short qvc_pede[2];
      tvc_pede[0] = (short)calib->getTvcPede(0,islat);
      tvc_pede[1] = (short)calib->getTvcPede(1,islat);
      qvc_pede[0] = (short)calib->getQvcPede(0,islat);
      qvc_pede[1] = (short)calib->getQvcPede(1,islat);

      short raw_cell[2];
      raw_cell[0] = 0;
      raw_cell[1] = 0;

      // convert qvc
      float fqvc[2];
      fqvc[0] = eloss_l/calib->getElossConv(islat);
      fqvc[1] = eloss_u/calib->getElossConv(islat);

      short raw_qvc[2];
      raw_qvc[0] = (short)fqvc[0];
      raw_qvc[1] = (short)fqvc[1];
      if(raw_qvc[0]>qvc_pede[0]) raw_qvc[0] = qvc_pede[0];
      if(raw_qvc[0]<0)           raw_qvc[0] = 0;
      if(raw_qvc[1]>qvc_pede[1]) raw_qvc[1] = qvc_pede[1];
      if(raw_qvc[1]<0)           raw_qvc[1] = 0;

      // Add time resolution
      float sigma = timingsigma[islat]*sqrt(2.0);
      tof_l += gsl_ran_gaussian(rng, sigma);
      tof_u += gsl_ran_gaussian(rng, sigma);

      // Add slewing effect [ns]
      float slewpar[2];
      slewpar[0] = calib->getSlewPar_a(0,islat)
	+ calib->getSlewPar_b(0,islat)/sqrt(fqvc[0]);
      
      slewpar[1] = calib->getSlewPar_a(1,islat)
	+ calib->getSlewPar_b(1,islat)/sqrt(fqvc[1]);

      float ftof[2];
      ftof[0] = tof_l + slewpar[0];
      ftof[1] = tof_u + slewpar[1];

      // convert tvc
      float ftvc[2];
      ftvc[0] = ftof[0]/calib->getTvcConv(0, islat);
      ftvc[1] = ftof[1]/calib->getTvcConv(1, islat);
      short raw_tvc[2];
      raw_tvc[0] = (short)ftvc[0];
      raw_tvc[1] = (short)ftvc[1];

      if(raw_tvc[0]>tvc_pede[0]) raw_tvc[0] = tvc_pede[0];
      if(raw_tvc[0]<0)           raw_tvc[0] = 0;
      if(raw_tvc[1]>tvc_pede[1]) raw_tvc[1] = tvc_pede[1];
      if(raw_tvc[1]<0)           raw_tvc[1] = 0;

      tofraw[iraw].id     = iraw;
      tofraw[iraw].slatid = islat;
      tofraw[iraw].sector = address->getSector(islat);
      tofraw[iraw].side   = address->getSide(islat);
      tofraw[iraw].panel  = address->getPanel(islat);
      tofraw[iraw].slat   = address->getSlat(islat);
      for( int lu=0;lu<2;lu++)
	{
	  tofraw[iraw].cell[lu] = raw_cell[lu];
	  tofraw[iraw].qvc[lu]  = raw_qvc[lu];
	  tofraw[iraw].q1[lu]   = qvc_pede[lu];
	  tofraw[iraw].q2[lu]   = qvc_pede[lu] - raw_qvc[lu];
	  tofraw[iraw].tvc[lu]  = raw_tvc[lu];
	  tofraw[iraw].t3[lu]   = raw_tvc[lu];
	  tofraw[iraw].t4[lu]   = tvc_pede[lu];
	}
    }

  TofGhitRawWrapper->SetRowCount(nghit_total);
  TofRawWrapper->SetRowCount(nraw);

  if(iDebug>0)
    {
      cout<<"Number of rows in         dTofRaw   = "<<nraw<<endl;
    }
  if (TofGhitRawWrapper->RowCount() > TofGhitRawWrapper->MaxRowCount())
    {
      cout<<"  dTofGhitRawWrapper->RowCount() = ";
      cout<<TofGhitRawWrapper->RowCount();
      cout<<", dTofGhitRawWrapper->MaxRowCount() = ";
      cout<<TofGhitRawWrapper->MaxRowCount();
      cout << " this means memory Corruption, exiting" << endl;
      exit(1);
    }
  if (TofRawWrapper->RowCount() > TofRawWrapper->MaxRowCount())
    {
      cout<<PHWHERE << "  dTofRawWrapper->RowCount() = ";
      cout<<TofRawWrapper->RowCount();
      cout<<", dTofRawWrapper->MaxRowCount() = ";
      cout<<TofRawWrapper->MaxRowCount();
      cout << " this means memory Corruption exiting" << endl;
      exit(1);
    }
  
  return True;
}
