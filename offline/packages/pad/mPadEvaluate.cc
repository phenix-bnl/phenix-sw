// :>--------------------------------------------------------------------
// : FILE:       mPadEvaluate.c.template
// : HISTORY:
// :             00jan93-v000a-hpl- Created by stic Version
// :  Id: idl.y,v 1.17 1997/03/25 19:22:52 ward Exp  
// :<--------------------------------------------------------------------
#include <mPadEvaluate.h>
#include <emlLib.h>

#include <TROOT.h>
#include <TFile.h>
#include <TNtuple.h>

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

long mPadEvaluate_(
  TABLE_HEAD_ST    *dPadEvalPar_h,    DPADEVALPAR_ST      *dPadEvalPar ,
  TABLE_HEAD_ST       *dPadGeom_h,       DPADGEOM_ST         *dPadGeom ,
  TABLE_HEAD_ST        *pcXghit_h,         PCGHIT_ST          *pcXghit ,
  TABLE_HEAD_ST       *dPadXRaw_h,        DPADRAW_ST         *dPadXRaw ,
  TABLE_HEAD_ST   *dPadXCluster_h,    DPADCLUSTER_ST     *dPadXCluster ,
  TABLE_HEAD_ST   *dPadXGhitRaw_h,    DPADGHITRAW_ST     *dPadXGhitRaw ,
  TABLE_HEAD_ST   *dPadXRawClus_h,    DPADRAWCLUS_ST     *dPadXRawClus ,
  TABLE_HEAD_ST  *dPadXGhitClus_h,   DPADGHITCLUS_ST    *dPadXGhitClus ,
  TABLE_HEAD_ST      *dPadXEval_h,       DPADEVAL_ST        *dPadXEval )
{
  // :>--------------------------------------------------------------------
  // : ROUTINE:    mPadEvaluate_
  // :
  // : DESCRIPTION:  Evaluates the performance of the pad chamber cluster
  // :               reconstruction.
  // :
  // : AUTHOR:   Jeffery T. Mitchell (BNL)
  // :
  // : ARGUMENTS:
  // :       IN:
  // :        dPadEvalPar    - Pad Chamber evaluation control parameters
  // :       dPadEvalPar_h   - header Structure for dPadEvalPar
  // :           dPadGeom    - Pad Chamber geometry parameters
  // :          dPadGeom_h   - header Structure for dPadGeom
  // :            pcXghit    - PCX GEANT hits
  // :           pcXghit_h   - header Structure for pcXghit
  // :           dPadXRaw    - PCX fired pads information
  // :          dPadXRaw_h   - header Structure for dPadXRaw
  // :       dPadXCluster    - PCX reconstructed clusters
  // :      dPadXCluster_h   - header Structure for dPadXCluster
  // :       dPadXGhitRaw    - Relates pcXghit to dPadXRaw
  // :      dPadXGhitRaw_h   - header Structure for dPadXGhitRaw
  // :       dPadXRawClus    - Relates dPadXRaw to dPadXCluster
  // :      dPadXRawClus_h   - header Structure for dPadXRawClus
  // :    INOUT:
  // :      dPadXGhitClus    - Relates pcXghit to dPadXCluster
  // :     dPadXGhitClus_h   - header Structure for dPadXGhitClus
  // :      OUT:
  // :          dPadXEval    - PCX evaluation information
  // :         dPadXEval_h   - header Structure for dPadXEval
  // : RETURNS:    STAF Condition Value
  // :>------------------------------------------------------------------


  // **************************************************************************
  // DESCRIPTION: Performs an evaluation of the cluster reconstruction. It
  //   compares the reconstructed cluster position with the GEANT hit position.

  // Author/CONTACT: Jeffery T. Mitchell, BNL

  // REVISIONS:
  //   Date		Author		Description

  //   11/30/99        Nilsson         Minor bug fix: rcut variable
  //   10/27/98        Mitchell        Converted to ROOT output
  //   9/9/97          Mitchell        Converted to STAF
  //   4/9/96          Mitchell        Converted to C for LUXOR
  //   2/23/96         K. Filimonov    Modified dx-entry: dx=xtrue-xmeas in
  //                                   local, not global coordinates
  //   1/25/96         K. Filimonov    Modified for pixel 
  //   7/2/95          Mitchell        Updated for CVS  
  //   4/18/95	       J.T. Mitchell   Original
  // ************************************************************************

  static TNtuple *PadClus;
  float padclusnt[13];
  static TNtuple *PadGhit;
  float padghitnt[14];
  static TNtuple *PadEval;
  float padevalnt[9];
  static TNtuple *PadStat;
  float padstatnt[10];

  static short first = 0;

  int i,iclus,clusid,iraw,nraw,ich;
  int rawid[60],connum[60],conid[60];
  int itest,ncontrib,igr,ghitid,icont,iarm,isect;
  int maxcon,domconid,igc,domconent,ighit,itype,ipceval;
  float xglobal,yglobal,rglobal,phiglobal;
  float dx,dy,dz,rtrue,rmeas,dr,ds;
  //  float dxlocal;
  // float dzlocal;
  float rcut;
  int NClusMatch,NClusGhost,NClusR;
  int NGeaMatch,NGeaR,NGeaOver,NGeaNone;
  int NPadDomCon,PadDomList[2000],PadDomRList[2000];
  int rflag,ngdom,grflag;
  float PctClusMatch,PctClusGhost,PctClusR;
  float PctGeaMatch,PctGeaR,PctGeaOver,PctGeaNone;

  // EXECUTABLE STATEMENTS:
  
  if (dPadEvalPar[0].verbose > 0)
    {
      cout << "mPadEvaluate-I: Entering the module for PC " <<
	dPadEvalPar[0].pcnumber << "\n";
    }

  // First-time call set-up
  if (first == 0)
    {
      first = 1;
      if (dPadEvalPar[0].fillclus == 1)
	{
	  PadClus = new TNtuple("PadClus","Pad Chamber Clusters",
	   "id:pc:arm:sector:wire:cell:x:y:z:dx:dy:dz:type");
	  if (dPadEvalPar[0].verbose > 0)
	    cout << "mPadEvaluate-I: Ntuple PadClus created.\n";
	}
      if (dPadEvalPar[0].fillghit == 1)
	{
	  PadGhit = new TNtuple("PadGhit","Pad Chamber GEANT hits",
	   "id:pc:arm:sector:xloc:yloc:zloc:x:y:z:tof:dedx:path:mctrack");
	  if (dPadEvalPar[0].verbose > 0)
	    cout << "mPadEvaluate-I: Ntuple PadGhit created.\n";
	}
      if (dPadEvalPar[0].filleval == 1)
	{
	  PadEval = new TNtuple("PadEval","Pad Chamber Evaluation",
	   "id:pc:clusid:ghitid:dx:dz:dr:nhits:type");
	  if (dPadEvalPar[0].verbose > 0)
	    cout << "mPadEvaluate-I: Ntuple PadEval created.\n";
	}
      if (dPadEvalPar[0].fillstat == 1)
	{
	  PadStat = new TNtuple("PadStat","Pad Chamber Evaluation Stats",
	   "pc:eff:ghost:reff:domcon:rgeant:overdom:noclus:nclus:nghit");
	  if (dPadEvalPar[0].verbose > 0)
	    cout << "mPadEvaluate-I: Ntuple PadStat created.\n";
	}
    }   // first==0

  // Fill the cluster ntuple
  if (dPadEvalPar[0].fillclus == 1)
    {
      for (iclus=0; iclus<dPadXCluster_h->nok; iclus++)
	{
	  padclusnt[0] = dPadXCluster[iclus].id;
	  padclusnt[1] = dPadEvalPar[0].pcnumber;
	  padclusnt[2] = dPadXCluster[iclus].arm;
	  padclusnt[3] = dPadXCluster[iclus].sector;
	  padclusnt[4] = dPadXCluster[iclus].wire;
	  padclusnt[5] = dPadXCluster[iclus].cell;
	  padclusnt[6] = dPadXCluster[iclus].xyz[0];
	  padclusnt[7] = dPadXCluster[iclus].xyz[1];
	  padclusnt[8] = dPadXCluster[iclus].xyz[2];
	  padclusnt[9] = dPadXCluster[iclus].dxyz[0];
	  padclusnt[10] = dPadXCluster[iclus].dxyz[1];
	  padclusnt[11] = dPadXCluster[iclus].dxyz[2];
	  padclusnt[12] = dPadXCluster[iclus].type;
	  PadClus->Fill(padclusnt);
	}  // iclus=0,dPadXCluster_h->nok
    }  // dPadEvalPar.fillclus == 1

  // Fill the GEANT hit ntuple
  if (dPadEvalPar[0].fillghit == 1)
    {
      for (iclus=0; iclus<pcXghit_h->nok; iclus++)
	{
	  padghitnt[0] = pcXghit[iclus].id;
	  padghitnt[1] = dPadEvalPar[0].pcnumber;
	  padghitnt[2] = pcXghit[iclus].arm;
	  iarm = pcXghit[iclus].arm;
	  ich = dPadEvalPar[0].pcnumber;
	  isect = pcXghit[iclus].sector;
	  if (iarm == 1)
	    isect = dPadGeom[0].sectperarm[ich] - isect - 1;
	  if (iarm == 0)
	    isect = isect - dPadGeom[0].sectperarm[ich];
	  padghitnt[3] = isect;
	  padghitnt[4] = pcXghit[iclus].xyzinloc[0];
	  padghitnt[5] = pcXghit[iclus].xyzinloc[1];
	  padghitnt[6] = pcXghit[iclus].xyzinloc[2];
	  padghitnt[7] = pcXghit[iclus].xyzinglo[0];
	  padghitnt[8] = pcXghit[iclus].xyzinglo[1];
	  padghitnt[9] = pcXghit[iclus].xyzinglo[2];
	  padghitnt[10] = pcXghit[iclus].tof;
	  padghitnt[11] = pcXghit[iclus].dedx;
	  padghitnt[12] = pcXghit[iclus].pathLength;
	  padghitnt[13] = pcXghit[iclus].mctrack;
	  PadGhit->Fill(padghitnt);
	}  // iclus=0,pcXghit_h->nok
    }  // dPadEvalPar.fillghit == 1

  // Initialize for this event
  NClusMatch = 0;
  NClusGhost = 0;
  NClusR = 0;
  NGeaMatch = 0;
  NGeaR = 0;
  NGeaOver = 0;
  NGeaNone = 0;
  NPadDomCon = 0;
  for (i=0; i<2000; i++)
    {
      PadDomList[i] = -1;
      PadDomRList[i] = 0;
    }
  ich = dPadEvalPar[0].pcnumber;
  rcut = 0;
  if (ich == 0) rcut = dPadEvalPar[0].rcutpc1;
  if (ich == 1) rcut = dPadEvalPar[0].rcutpc2;
  if (ich == 2) rcut = dPadEvalPar[0].rcutpc3;

  // Do one evaluation for each reconstructed cluster
  for (iclus=0; iclus<dPadXCluster_h->nok; iclus++)
    {

      // Grab the cluster id
      clusid = dPadXCluster[iclus].id;

      // Determine the raw hit entry numbers and id number.
      // Allow a maximum of 60 pads associated to each cluster.
      for (i=0; i<60; i++)
	{
	  rawid[i] = -1;
	  conid[i] = -1;     // list GEANT hit id's contributing to cluster
	  connum[i] = 0;     // total number of contributions for each
	}
      nraw = 0;
      iraw = 0;
      while (nraw<60 && iraw<dPadXRawClus_h->nok)
	{
	  if (dPadXRawClus[iraw].clusid == clusid)
	    {
	      rawid[nraw] = dPadXRawClus[iraw].rawid;
	      nraw++;
	      if (nraw > 59) nraw = 59;   // protection
	    }   // dPadXRawClus.clusid == clusid
	  iraw++;
	}   // nraw<60 && iraw<dPadXRawClus_h->nok

      // There can be more than one GEANT hit associated with each raw
      // entry.  We will count up the contributions made from each GEANT
      // hit and choose the most common (dominant) one as our association.
      // That association will be recorded in the dPadXGhitClus table and be
      // used in the physical comparisons performed by this module.

      ncontrib = 0;
      for (i=0; i<nraw; i++)
	{
	  for (igr=0; igr<dPadXGhitRaw_h->nok; igr++)
	    {
	      if (dPadXGhitRaw[igr].rawid == rawid[i])
		{
		  ghitid = dPadXGhitRaw[igr].ghitid;
		  // Is this GEANT hit id already in the contributor list?
		  itest = 0;
		  icont = 0;
		  while (itest==0 && icont<ncontrib)
		    {
		      if (conid[icont] == ghitid) 
			{
			  itest = 1;
			  // increment counter for this contributor
			  connum[icont]++;
			} 
		      icont++;
		    }   // itest==0 && icont<ncontrib

		  // If this is a new contributor, add it to the list
		  if (itest == 0)
		    {
		      conid[ncontrib] = ghitid;
		      connum[ncontrib]++;
		      ncontrib++;
		      if (ncontrib > 59) ncontrib = 59;   // protection
		    }   // itest == 0
		}   // dPadXGhitRaw.rawid == rawid
	    }   // igr=0,dPadXGhitRaw_h->nok
	}   // i=0,nraw

      // pick out the dominant contributor from the contributor list
      maxcon = -999;
      domconid = -1;
      for (i=0; i<ncontrib; i++)
	{
	  if (connum[i] > maxcon)
	    {
	      maxcon = connum[i];
	      domconid = conid[i];
	    }
	}   // i=0,ncontrib

      // Save the dominant contributor in the dPadXGhitClus table
      if (domconid >= 0)
	{
	  igc = dPadXGhitClus_h->nok;
	  dPadXGhitClus[igc].ghitid = domconid;
	  dPadXGhitClus[igc].clusid = clusid;
	  dPadXGhitClus_h->nok++;
	}   // domconid >= 0

      // Grab the entry number for the dominant contributor GEANT hit
      domconent = -1;
      if (domconid >= 0)
	{
	  itest = 0;
	  ighit = 0;
	  while (itest==0 && ighit<pcXghit_h->nok)
	    {
	      if (pcXghit[ighit].id == domconid)
		{
		  domconent = ighit;
		  itest = 1;
		}   // pcXghit.id == domconid
	      ighit++;
	    }   // itest==0 && ighit<pcXghit_h->nok
	}   // domconid >= 0

      // for the matched GEANT-reconstructed clusters, compare
      //	 the observables
      if (domconent == -1) NClusGhost++;
      if (domconent >= 0) 
	{
	  
	  NClusMatch++;

	  // calculate the simulated-reconstructed positions
	  dx = (pcXghit[domconent].xyzinglo[0] -
		dPadXCluster[iclus].xyz[0]);
	  dy = (pcXghit[domconent].xyzinglo[1] -
		dPadXCluster[iclus].xyz[1]);
	  dz = (pcXghit[domconent].xyzinglo[2] - 
		dPadXCluster[iclus].xyz[2]);
	  //dxlocal = ((pcXghit[domconent].xyzinloc[0] +
	  //      pcXghit[domconent].xyzoutloc[0])/2. -
	  //     dPadXCluster[iclus].dxyz[0]);
	  //dzlocal = ((pcXghit[domconent].xyzinloc[2] +
	  //      pcXghit[domconent].xyzoutloc[2])/2. - 
	  //     dPadXCluster[iclus].dxyz[2]);
	  // dxlocal = pcXghit[domconent].xyzinloc[0] -
	  // 	     dPadXCluster[iclus].dxyz[0];
	  // dzlocal = pcXghit[domconent].xyzinloc[2] -
	  // 	     dPadXCluster[iclus].dxyz[2];
	  ds = sqrt(dx*dx + dy*dy + dz*dz);
	  //ds = sqrt(dxlocal*dxlocal + dzlocal*dzlocal);

	  // If the difference is within the cuts, count the cluster
	  // as successfully reconstructed
	  // Bug fix: P. Nilsson has replaced rcutpc1 with rcut
	  rflag = 0;
	  if (ds <= rcut) 
	    {
	      NClusR++;
	      rflag = 1;
	    }
	  
	  // Take care of GEANT hit accounting
	  if (NPadDomCon < 2000)
	    {
	      PadDomList[NPadDomCon] = domconent;
	      PadDomRList[NPadDomCon] = rflag;
	      NPadDomCon++;
	    }

	  // for x-coordinate take the difference in global phi
	  xglobal = pcXghit[domconent].xyzinglo[0];
	  yglobal = pcXghit[domconent].xyzinglo[1];
	  rglobal = sqrt(xglobal*xglobal+yglobal*yglobal);
	  if (xglobal != 0.0)
	    {
	      phiglobal = atan(yglobal/xglobal)*57.2957795;
	    }
	  else
	    {
	      phiglobal = 90.0;
	    }
	  if (xglobal < 0) phiglobal += 180.0;
	  rtrue = rglobal;

	  xglobal = dPadXCluster[iclus].xyz[0];
	  yglobal = dPadXCluster[iclus].xyz[1];
	  rglobal = sqrt(xglobal*xglobal+yglobal*yglobal);
	  if (xglobal != 0.0)
	    {
	      phiglobal = atan(yglobal/xglobal)*57.2957795;
	    }
	  else
	    {
	      phiglobal = 90.0;
	    }
	  if (xglobal < 0) phiglobal += 180.0;
	  rmeas = rglobal;
      
	  dr = rtrue - rmeas;
      
	  // save the number of cells per cluster
	  itype = dPadXCluster[iclus].type;
      
	  // fill the dPadXEval table
	  ipceval = dPadXEval_h->nok;
	  dPadXEval[ipceval].id = ipceval;
	  dPadXEval[ipceval].clusid = clusid;
	  dPadXEval[ipceval].ghitid = domconid;
	  dPadXEval[ipceval].deltax = dx;
	  dPadXEval[ipceval].deltaz = dz;
	  dPadXEval[ipceval].deltar = dr;
	  dPadXEval[ipceval].nhits = maxcon;
	  dPadXEval[ipceval].type = itype;
	  dPadXEval_h->nok++;

	}   // domconent <= 0

    }   // iclus=0,dPadXCluster_h->nok

  // Fill the evaluation ntuple
  if (dPadEvalPar[0].filleval == 1)
    {
      for (i=0; i<dPadXEval_h->nok; i++)
	{
	  padevalnt[0] = dPadXEval[i].id;
	  padevalnt[1] = dPadEvalPar[0].pcnumber;
	  padevalnt[2] = dPadXEval[i].clusid;
	  padevalnt[3] = dPadXEval[i].ghitid;
	  padevalnt[4] = dPadXEval[i].deltax;
	  padevalnt[5] = dPadXEval[i].deltaz;
	  padevalnt[6] = dPadXEval[i].deltar;
	  padevalnt[7] = dPadXEval[i].nhits;
	  padevalnt[8] = dPadXEval[i].type;
	  PadEval->Fill(padevalnt);
	}  // i=0,dPadXEval_h->nok
    }  // dPadEvalPar.filleval == 1


  // Check the GEANT dominant contributor list and calculate stats
  for (ighit=0; ighit<pcXghit_h->nok; ighit++)
    {
      ngdom = 0;
      grflag = 0;
      // Loop over the GEANT contributor list
      for (i=0; i<NPadDomCon; i++)
	{
	  // Is there a match?
	  if (PadDomList[i] == ighit)
	    {
	      ngdom++;
	      if (PadDomRList[i] > 0) grflag = 1;
	    }  // PadDomList == ighit
	}  // i=0,NPadDomCon
      // Increment counters if necessary
      if (ngdom > 0) NGeaMatch++;
      if (ngdom > 1) NGeaOver++;
      if (ngdom == 0) NGeaNone++;
      if (grflag == 1) NGeaR++;
    }  // ighit=0,pcXghit_h->nok

  // Calculate the efficiencies
  PctClusMatch = 0.0;
  PctClusGhost = 0.0;
  PctClusR = 0.0;
  PctGeaMatch = 0.0;
  PctGeaR = 0.0;
  PctGeaOver = 0.0;
  PctGeaNone = 0.0;
  if (dPadXCluster_h->nok != 0)
    {
      PctClusMatch = (float (NClusMatch))/(float (dPadXCluster_h->nok));
      PctClusMatch *= 100.0;
      PctClusGhost = ((float) NClusGhost)/(float (dPadXCluster_h->nok));
      PctClusGhost *= 100.0;
      PctClusR = (float (NClusR))/(float (dPadXCluster_h->nok));
      PctClusR *= 100.0;
    }  // dPadXCluster_h->nok != 0
  if (pcXghit_h->nok != 0)
    {
      PctGeaMatch = (float (NGeaMatch))/(float (pcXghit_h->nok));
      PctGeaMatch *= 100.0;
      PctGeaR = (float (NGeaR))/(float (pcXghit_h->nok));
      PctGeaR *= 100.0;
      PctGeaOver = (float (NGeaOver))/(float (pcXghit_h->nok));
      PctGeaOver *= 100.0;
      PctGeaNone = (float (NGeaNone))/(float (pcXghit_h->nok));
      PctGeaNone *= 100.0;
    }  // pcXghit_h->nok

  // Store the efficiencies in the statistics ntuple
  if (dPadEvalPar[0].fillstat == 1)
    {
      padstatnt[0] = dPadEvalPar[0].pcnumber;
      padstatnt[1] = PctClusMatch;
      padstatnt[2] = PctClusGhost;
      padstatnt[3] = PctClusR;
      padstatnt[4] = PctGeaMatch;
      padstatnt[5] = PctGeaR;
      padstatnt[6] = PctGeaOver;
      padstatnt[7] = PctGeaNone;
      padstatnt[8] = dPadXCluster_h->nok;
      padstatnt[9] = pcXghit_h->nok;
      PadStat->Fill(padstatnt);
    }  // dPadEvalPar.fillstat

  // Report the efficiencies to the standard output
  if (dPadEvalPar[0].printstat == 1)
    {
      cout << "\nPad Chamber reconstruction efficiency report.\n";
      cout << "=============================================\n";
      cout << "Pad chamber number: " << dPadEvalPar[0].pcnumber << "\n";
      cout << "Number of input GEANT clusters = " << 
	pcXghit_h->nok << "\n";
      cout << "Number of reconstructed clusters = " << 
	dPadXCluster_h->nok << "\n";
      cout << "% clusters corresponding to a GEANT hit: (" <<
	NClusMatch << "/" << dPadXCluster_h->nok << ") = " << 
	PctClusMatch << "\n";
      cout << "% clusters not corresponding to a GEANT hit: (" <<
	NClusGhost << "/" << dPadXCluster_h->nok << ") = " <<
	PctClusGhost << "\n";
      cout << "% cluster matches within r=" <<
	rcut << " : (" << NClusR << "/" << dPadXCluster_h->nok <<
	") = " << PctClusR << "\n";
      cout << "% GEANT hits that are dominant contributors: (" <<
	NGeaMatch << "/" << pcXghit_h->nok << ") = " << PctGeaMatch << "\n";
      cout << "% GEANT hits matched to within r=" <<
	rcut << ": (" << NGeaR << "/" << pcXghit_h->nok << ") = " << 
	PctGeaR << "\n";
      cout << "% GEANT hits that dominate more than once: (" <<
	NGeaOver << "/" << pcXghit_h->nok << ") = " <<
	PctGeaOver << "\n";
      cout << "% GEANT hits with no associated cluster: (" <<
	NGeaNone << "/" << pcXghit_h->nok << ") = " <<
	PctGeaNone << "\n\n";
    }  // dPadEvalPar.printstat == 1

  // Check the counters on the output tables
  if ((dPadXGhitClus_h->maxlen <= 0) ||
      (dPadXGhitClus_h->nok > dPadXGhitClus_h->maxlen))
    {
      cout << "mPadEvaluate-F: Too many dPadXGhitClus rows: " <<
	dPadXGhitClus_h->nok << "\n";
      return STAFCV_BAD;
    }
  if ((dPadXEval_h->maxlen <= 0) ||
      (dPadXEval_h->nok > dPadXEval_h->maxlen))
    {
      cout << "mPadEvaluate-F: Too many dPadXEval rows:" <<
	dPadXEval_h->nok << "\n";
      return STAFCV_BAD;
    }

  return STAFCV_OK;

}   // end mPadEvaluate module

