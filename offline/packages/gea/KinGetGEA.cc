// $Id: KinGetGEA.cc,v 1.13 2009/08/17 23:42:34 hpereira Exp $
//////////////////////////////////////////////////////////////////
/*! 
  \file KinGetGea.cc
  \brief convert pisa Kin objects into phool objects
  \author	 Hugo Pereira
  \version $Revision: 1.13 $
  \date	$Date: 2009/08/17 23:42:34 $
*/
//////////////////////////////////////////////////////////////////


#include <iostream>
#include <PISAEventHeader.h>
#include <KinPISAHit.h>
#include <PriPISAHit.h>
#include <getClass.h>

#include "fkinWrapper.h"
#include "primaryWrapper.h"
#include "headerWrapper.h"
#include "pythiaWrapper.h"

using namespace std;


long KinGetGEA(PHCompositeNode* topNode)
{

  // Primary table information
  PriPISAHit *prihits = PriPISAHit::GetPriHitEvt();
  int priRows = PriPISAHit::GetPriCount(); 

  // Instantiate the primary table for this event, and attach it to
  // the EVA "directory" node.
  primaryWrapper* wprim = findNode::getClass<primaryWrapper>(topNode,"primary");
  wprim->SetMaxRowCount(priRows);

  PRIMARY_ST* primary = wprim->TableData();

  for(int iprim=0; iprim<priRows; iprim++) 
  {
    primary[iprim].key             = iprim;  
    primary[iprim].event_track     = prihits->GetEvttrack();
    primary[iprim].subevent_track  = prihits->GetNtrack();
    primary[iprim].subevent        = prihits->GetIsubevent();
    primary[iprim].true_track      = prihits->GetTrue_track();
    primary[iprim].idpart          = prihits->GetIdpart();
    primary[iprim].nfile           = prihits->GetNfile();
    primary[iprim].px_momentum     = prihits->GetPx();
    primary[iprim].py_momentum     = prihits->GetPy();
    primary[iprim].pz_momentum     = prihits->GetPz();    
    prihits++;
  } // loop over primary rows
  wprim->SetRowCount(priRows);

  //
  // FKIN table information
  //
  KinPISAHit *event = KinPISAHit::GetKinHitEvt();
  Int_t kinRows = KinPISAHit::GetKinCount();   
  
  // Instantiate the fkin table for this event, and attach it to
  // the EVA "directory" node.
  fkinWrapper* w = findNode::getClass<fkinWrapper>(topNode,"fkin");
  w->SetMaxRowCount(kinRows);
  
  FKIN_ST* fkin = w->TableData();
  for (int i = 0; i < kinRows; i++)
  {
    
    /* Globally unique track ID */
    fkin[i].true_track = event[i].GetTrue_track();  
    
    /* Subevent for this vertex */
    fkin[i].subevent = event[i].GetIsubevent();	    
    
    /* input track value (original value in subevent)*/
    fkin[i].ntrack = event[i].GetEvttrack();
    
    /* output momentum */
    fkin[i].ptot = event[i].GetPtot();		    
    
    /* output theta direction */
    fkin[i].pthet = event[i].GetPthet();	  
    
    /* output phi direction */
    fkin[i].pphi = event[i].GetPhi();		    
    
    /* output vertex r position */
    fkin[i].r_vertex = event[i].GetRvertex();	    
    
    /* output vertex z position */
    fkin[i].z_vertex = event[i].GetZvertex();	    
    
    /* output vertex theta position */
    fkin[i].th_vertx = event[i].GetThvertx();	    
    
    /* output vertex phi position */
    fkin[i].ph_vertx = event[i].GetPhvertx();	    
    
    /* output parent track number */
    fkin[i].itparent = event[i].GetItparent();	   
    
    /* output parent ID number */
    fkin[i].idparent = event[i].GetIdparent();	
    
    /* output particle ID number */
    fkin[i].idpart = event[i].GetIdpart();	    
    
    /* file number (0 = primary, rest MERGE files) */
    fkin[i].nfile = event[i].GetNfile();              
    
  }  // loop over Kin hits
  
  w->SetRowCount(kinRows);
  
  PISAEventHeader *evtheader = PISAEventHeader::GetEventHeader();
  int headerRows = 1;
  
  // Instantiate the header table for this event, and attach it to
  // the EVA "directory" node.
  headerWrapper* whead = findNode::getClass<headerWrapper>(topNode,"header");
  whead->SetMaxRowCount(headerRows);
  
  HEADER_ST* header = whead->TableData();
  for (int ihead = 0; ihead < headerRows; ihead++)
  {
    header[ihead].run = evtheader[ihead].GetIDRun();
    header[ihead].event = evtheader[ihead].GetEvent();
    header[ihead].multiplicity = evtheader[ihead].GetNptls();
    header[ihead].b = evtheader[ihead].GetImpactParameter();
    header[ihead].a1 = evtheader[ihead].GetAproj();
    header[ihead].z1 = evtheader[ihead].GetZproj();
    header[ihead].a2 = evtheader[ihead].GetAtarg();
    header[ihead].z2 = evtheader[ihead].GetZtarg();
    header[ihead].sqrt_s = evtheader[ihead].GetSqrtS();
    header[ihead].bmin = evtheader[ihead].GetBmin();
    header[ihead].bmax = evtheader[ihead].GetBmax();
    header[ihead].vertex[0] = evtheader[ihead].GetXvertex();
    header[ihead].vertex[1] = evtheader[ihead].GetYvertex();
    header[ihead].vertex[2] = evtheader[ihead].GetZvertex();
    
    // date
    Int_t igdate = evtheader[ihead].GetIgdate();
    if (igdate >= 990101 && igdate <= 991124) header[ihead].t0femto = 0;
    else header[ihead].t0femto = evtheader[ihead].GetT0femto();
    header[ihead].idate = igdate;

    // time
    header[ihead].itime = evtheader[ihead].GetIgtime();
    
    header[ihead].nrndm[0] = evtheader[ihead].GetNrndm0();
    header[ihead].nrndm[1] = evtheader[ihead].GetNrndm1();
    header[ihead].isqStart = evtheader[ihead].GetIsqstart();
    header[ihead].iSeconds = evtheader[ihead].GetIseconds();
    header[ihead].maxTrueTrack = 0; 
      
    Int_t event_code = evtheader[ihead].GetEventCode();
    
    if (event_code == 7)
    {
      // Instantiate the pythia table for this event, and attach it to
      // the EVA "directory" node.
      pythiaWrapper* wpythia = findNode::getClass<pythiaWrapper>(topNode, "pythia");
      
      PYTHIA_ST* pythia = wpythia->TableData();

      // PYTHIA event specific information
      pythia[0].pyth_proc_id = evtheader[ihead].GetEventInt(0);
      pythia[0].pyth_bjork[0] = evtheader[ihead].GetEventFloat(0);  
      pythia[0].pyth_bjork[1] = evtheader[ihead].GetEventFloat(1); 
      pythia[0].pyth_parstu[0] = evtheader[ihead].GetEventFloat(2);
      pythia[0].pyth_parstu[1] = evtheader[ihead].GetEventFloat(3);
      pythia[0].pyth_parstu[2] = evtheader[ihead].GetEventFloat(4);
      pythia[0].pyth_qsqr = evtheader[ihead].GetEventFloat(5); 
      pythia[0].pyth_ptrans = evtheader[ihead].GetEventFloat(6); 
      int kpart;
      for (kpart = 0; kpart < 4; kpart++)
      {
        pythia[0].intr_part_id[kpart] = evtheader[ihead].GetEventInt(1 + kpart);
        int ixyze;
        for (ixyze = 0; ixyze < 4; ixyze++)
        {
          int index = 7 + ixyze + kpart * 4;
          pythia[0].intr_part_p[ixyze][kpart] = evtheader[ihead].GetEventFloat(index);
        }  
      } 
      wpythia->SetRowCount(1);
    } 
    
  }
  
  
  whead->SetRowCount(headerRows);
  
  return 0;
}

