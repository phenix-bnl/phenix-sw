// EMCalFEE.cc Describes class EMCalFEE methods.
// This class uses Eventiterator to fill EmcDynamicData data arrays.
// You can use it as an example or inherit from this class to create your own data readout.
// Created by Sergei Belikov 04/20/99.
#include "EmcFEM.h"
#include "EmcDynamicData.h"
#include "Eventiterator.h"
#include "Event.h"
#include "packetConstants.h"

#define LONG_FORMAT 808          // long format - 5 words for each of 192 channels per FEM
#define SHORT_FORMAT 908          // short format - 3 words for each of 144 channels per FEM
// data readout errors
#define OK        0x0            // no any problems.
#define NO_PACKET 0x1            // current event has no packet from this FEM 
#define WRONG_CELL_NUMBER 0x2    // the delay between TAC and Pre samples or Post and Pre does 
                                 // not correspond to expected value. 
#define HG_PRE_TOO_LOW 0x4       // high gain pre-sample value is less then minimal acceptable.
#define HG_POST_TOO_LOW 0x8      // high gain post-sample value is less then minimal acceptable.
#define HG_PRE_TOO_HIGH 0x10     // high gain pre-sample value is higher then maximal acceptable.
#define HG_POST_TOO_HIGH 0x20    // high gain post-sample value is higher then maximal acceptable.

#define LG_PRE_TOO_LOW 0x40       // low gain pre-sample value is less then minimal acceptable.
#define LG_POST_TOO_LOW 0x80      // low gain post-sample value is less then minimal acceptable.
#define LG_PRE_TOO_HIGH 0x100     // low gain pre-sample value is higher then maximal acceptable.
#define LG_POST_TOO_HIGH 0x200    // low gain post-sample value is higher then maximal acceptable.
#define TAC_TOO_HIGH 0x400        // TAC value is higher then maximal acceptable.


EMCalFEE::EMCalFEE(EmcDynamicData* d, FEMlimits * l, int& status)
{
  lim=l;
  dd=d;
  FEMstatus=NULL;
  DataErrors=NULL;
  RefErrors=NULL;
  EvtCells=NULL;
  // clear event information structure
  evtIn.evtLength=0;
  evtIn.evtType=0;
  evtIn.evtSequence=0;
  evtIn.evtRunNumber=0;
  event        =0;
  FEMstatus=new int[dd->getnSM()];
  if(!FEMstatus)
    {
      status=1;
      return;
    }
  DataErrors=new int[dd->getEmcSize()];
  if(!DataErrors)
    {
      status=2;
      return;
    }

  RefErrors=new int[dd->getRefSize()];
  if(!RefErrors)
    {
      status=3;
      return;
    }
  EvtCells=new cells[dd->getnSM()];
  if(!EvtCells)
    {
      status=4;
      return;
    }
  status=0;
}
EMCalFEE::~EMCalFEE()
{
  delete [] FEMstatus;
  delete [] DataErrors;
  delete [] RefErrors;
  delete [] EvtCells;
}
int EMCalFEE::getNextEvent(Eventiterator *it)
{
  if(event) delete event;
  int rv = 0 ;
  event = it->getNextEvent();
  if (event) {
    rv = processEvent(event) ;
    delete event ;
    event = 0;
  }
  return rv ;
}

int EMCalFEE::readNextEvent(Eventiterator *it)
{
  event = it->getNextEvent();
  return ((event)? event->getEvtSequence() : 0);
}

int EMCalFEE::convertNextEvent()
{
  if(!event) return 0;
  int rv = processEvent(event);
  delete event;
  event = 0;
  return rv;
}

int EMCalFEE::processEvent(Event* evt)
{
  int pre,post,i,j;
  Packet *p;
  int npack=0;
  int ok=0;
  int emcCh;
  int refCh;
  int iEmc=0;
  int iRef=0;
  int hitFormat=0;
  float** rEmc=dd->getEmcRaw();
  float** rRef=dd->getRefRaw();
  // fill event information
  evtIn.evtLength=evt->getEvtLength();
  evtIn.evtType=evt->getEvtType();
  evtIn.evtSequence=evt->getEvtSequence();
  evtIn.evtRunNumber=evt->getRunNumber();
  for(i=0;i<dd->getnSM();i++)
    {
      FEMstatus[i] = 0;
      //		p=evt->getPacket((dd->getSmMap())[i].packet, IDEMC_OLDSTYLE);		
      p=evt->getPacket((dd->getSmMap())[i].packet,IDEMC_OLDSTYLE);

      if(p)
	{
	  hitFormat=p->getHitFormat();
	  npack++;
	  EvtCells[i].tac = p->iValue(0,"AMU");
	  EvtCells[i].pre = p->iValue(1,"AMU");
	  EvtCells[i].post= p->iValue(2,"AMU");
	  //			cout<<"FEM "<<i<<": "<<EvtCells[i].tac<<"  "<<EvtCells[i].pre<<"  "<<EvtCells[i].post<<endl;
	  if(((EvtCells[i].pre+(dd->getSmMap())[i].tac_pre)&0x3F)!=EvtCells[i].tac || 
	     ((EvtCells[i].pre+(dd->getSmMap())[i].post_pre)&0x3F)!=EvtCells[i].post)
	    {
	      ok=2;
	      FEMstatus[i] = ok; //WRONG_CELL_NUMBER;
	    }
	  else
	    ok=0;
	}
      else
	{
	  ok=1;
	  FEMstatus[i] = ok;   //NO_PACKET;
	}
      // =========== Towers data =================================
      for(j=0;j<(dd->getSmMap())[i].nch;j++)
	{
	  iEmc=(dd->getSmMap())[i].startTad+j;
	  DataErrors[iEmc]=0;
	  emcCh=(dd->getSmMap())[i].femCh[j];
	  if(hitFormat==SHORT_FORMAT)
	    {
	      if((emcCh%16)>11)emcCh=144;
	      else emcCh=emcCh-emcCh/16*4;
	    }
	  // High Gain
	  if(ok!=1)
	    {
	      if(rEmc)
		{
		  //														cout<<emcCh<<":";
		  for(int ra=0;ra<5;ra++)
		    {
		      rEmc[ra][iEmc]=p->iValue(emcCh,ra);
		      //												cout<<rEmc[ra][iEmc]<<"  ";
		    }
		  //	cout<<endl;
		}
	      // just fix - to get rid of 4095 in TAC
	      // if(rEmc[0][iEmc]>=4095) rEmc[0][iEmc] = 0;
	      pre=p->iValue(emcCh,3);
	      post=p->iValue(emcCh,1);
	      if(pre<lim->minAmp){DataErrors[iEmc]|=HG_PRE_TOO_LOW;}
	      if(post<lim->minAmp){DataErrors[iEmc]|=HG_POST_TOO_LOW;}
	      if(pre>lim->maxAmp){DataErrors[iEmc]|=HG_PRE_TOO_HIGH;}
	      if(post>lim->maxAmp){DataErrors[iEmc]|=HG_POST_TOO_HIGH;}
	      (dd->getEmcHG())[iEmc]=float(pre-post);
	      //(dd->getEmcHG())[iEmc]=float(post);
	      // Low Gain
	      pre=p->iValue(emcCh,4);
	      post=p->iValue(emcCh,2);
	      if(pre<lim->minAmp)(DataErrors[iEmc])|=LG_PRE_TOO_LOW;
	      if(post<lim->minAmp)(DataErrors[iEmc])|=LG_POST_TOO_LOW;
	      if(pre>lim->maxAmp)(DataErrors[iEmc])|=LG_PRE_TOO_HIGH;
	      if(post>lim->maxAmp)(DataErrors[iEmc])|=LG_POST_TOO_HIGH;
	      (dd->getEmcLG())[iEmc]=float(pre-post);
	      //(dd->getEmcLG())[iEmc]=float(post);
	      // TAC
	      (dd->getEmcTAC())[iEmc]=float(p->iValue(emcCh,0));
	      if(p->iValue(emcCh,0)>lim->maxAmp) (DataErrors[iEmc])|= TAC_TOO_HIGH;
	    }
	  else
	    {
	      (DataErrors[iEmc])|=ok;
	      (dd->getEmcHG())[iEmc]=0.;
	      (dd->getEmcLG())[iEmc]=0.;
	      (dd->getEmcTAC())[iEmc]=0.;
	    }

	}
      // =========== References ===================
      for(j=0;j<(dd->getSmMap())[i].nrefCh;j++)
	{
	  iRef=(dd->getSmMap())[i].startRad+j;
	  refCh=(dd->getSmMap())[i].refCh[j];
	  if(hitFormat==SHORT_FORMAT)
	    {
	      refCh=refCh-refCh/16*4;
	      if((refCh%16)>11)refCh=-1;
	    }
	  // High Gain
	  if(ok!=1)
	    {
	      if(rRef)
		{
		  for(int rf=0;rf<5;rf++)
		    rRef[rf][iRef]=p->iValue(refCh,rf);
		}
	      pre= p->iValue(refCh,3);
	      post=p->iValue(refCh,1);
	      if(pre<lim->minAmp)(RefErrors[iRef])|=HG_PRE_TOO_LOW;
	      if(post<lim->minAmp)(RefErrors[iRef])|=HG_POST_TOO_LOW;
	      if(pre>lim->maxAmp)(RefErrors[iRef])|=HG_PRE_TOO_HIGH;
	      if(post>lim->maxAmp)(RefErrors[iRef])|=HG_POST_TOO_HIGH;
	      (dd->getRefHG())[iRef]=float(pre-post);
	      // Low Gain
	      pre=p->iValue(refCh,4);
	      post=p->iValue(refCh,2);
	      if(pre<lim->minAmp)(RefErrors[iRef])|=LG_PRE_TOO_LOW;
	      if(post<lim->minAmp)(RefErrors[iRef])|=LG_POST_TOO_LOW;
	      if(pre>lim->maxAmp)(RefErrors[iRef])|=LG_PRE_TOO_HIGH;
	      if(post>lim->maxAmp)(RefErrors[iRef])|=LG_POST_TOO_HIGH;
	      (dd->getRefLG())[iRef]=float(pre-post);
	      // TAC
	      (dd->getRefTAC())[iRef]=float(p->iValue(refCh,0));
	      if(p->iValue(refCh,0)>lim->maxAmp) (RefErrors[iRef])|= TAC_TOO_HIGH;
	    }
	  else
	    {
	      (RefErrors[iRef])|=ok;
	      (dd->getRefHG())[iRef]=0.;
	      (dd->getRefLG())[iRef]=0.;
	      (dd->getRefTAC())[iRef]=0.;
	    }
	}

      if(p)delete p;
    }
  //	delete evt; //this is done in processEvent now as we are
  // not owner of the pointer anymore
  if(!npack)return -1;
  return 1;
}

