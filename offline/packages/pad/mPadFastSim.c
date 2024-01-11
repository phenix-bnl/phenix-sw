/*:>--------------------------------------------------------------------
**: FILE:       mPadFastSim.c.template
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.17 1997/03/25 19:22:52 ward Exp  
**:<------------------------------------------------------------------*/
#include <mPadFastSim.h>

#include <emlLib.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/* external functions and utilities */
extern float utiRandom(long *ldum);
extern float utiGaussian(float *lmean, float *lsig, 
			 long *lrandseed, float *lgauss);
extern void utiXYtoRPhi(float *lx, float *ly, float *lr, float *lphi);
extern void utiRPhitoXY(float *ir, float *iphi, float *ix, float *iy);

long mPadFastSim_(
  TABLE_HEAD_ST        *pcXghit_h,         PCGHIT_ST          *pcXghit ,
  TABLE_HEAD_ST *dPadFastSimPar_h, DPADFASTSIMPAR_ST   *dPadFastSimPar ,
  TABLE_HEAD_ST   *dPadXCluster_h,    DPADCLUSTER_ST     *dPadXCluster ,
  TABLE_HEAD_ST  *dPadXGhitClus_h,   DPADGHITCLUS_ST    *dPadXGhitClus )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    mPadFastSim_
**: DESCRIPTION: Fast cluster simulation for all of the pad chambers.
**:
**: AUTHOR:     Jeffery T. Mitchell (BNL)
**: ARGUMENTS:
**:       IN:
**:              pcXghit    - The pad chamber GEANT hits for one PC
**:             pcXghit_h   - header Structure for pcXghit
**:    INOUT:
**:       dPadFastSimPar    - input parameters for the pcFastSim module
**:      dPadFastSimPar_h   - header Structure for dPadFastSimPar
**:      OUT:
**:         dPadXCluster    - Reconstructed clusters for one pad chamber
**:        dPadXCluster_h   - header Structure for dPadXCluster
**:        dPadXGhitClus    - Relates pcXGhit to dPadXCluster
**:       dPadXGhitClus_h   - header Structure for dPadXGhitClus
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

  int ii,j,jj,n,nn,l;
  int indx;
  int ihit,iphit,ihit2;

  long irndm;
    
  int ifire1;
  float fire1;

  float mean,sigma,xgauss;
  
  int itemp;
  float phii,phil;
  float hitr,hitphi,hitz;

  float x,y,z,r;
  float z1,z2,dz;
  float phi1,phi2,dphi,radius,drphi;
  int ichoose;

  int pcindex,sectperarm,isect;

  int itecgh;
  
  int nhitpln,usedhit[2000],hitpln[2000];

  /* get the index for the pad chamber being simulated. */
  pcindex = dPadFastSimPar[0].pcnumber;

  /* set up random number generator seed */
  irndm = dPadFastSimPar[0].randseed[pcindex];

  /* loop over PCX GEANT hits. */
  for (ihit=0; ihit<pcXghit_h->nok; ihit++) 
    {
      iphit = ihit;     /* current pointer to tofghit in this loop */

      /* Determine if the hit fired based on the wire efficiency */
      ifire1=1;
      fire1 = utiRandom(&irndm);
      if (fire1 > dPadFastSimPar[0].efficiency[pcindex]) ifire1=0;

      /* only fill if it fired */
      /* Aligning GEANT tables to the standard numbering conventions now.
	 Needs to be fixed in GEANT. */
      if (ifire1 != 0) 
	{
	  ihit2 = dPadXCluster_h->nok;
	  dPadXCluster[ihit2].id = ihit2;
	  dPadXCluster[ihit2].arm = pcXghit[iphit].arm;
	  sectperarm = 4;
	  if (pcindex == 0) sectperarm = 8;   /* PC1 has more */
	  if (dPadXCluster[ihit2].arm == 1)
	    {
	      isect = sectperarm - pcXghit[iphit].sector - 1;
	    }
	  else if (dPadXCluster[ihit2].arm == 0)
	    {
	      isect = pcXghit[iphit].sector - sectperarm;
	    }
	  else {

            /* CFM November 29, safety for possibly uninitialized value of isect */
            /* should really throw an exception if this occurs */

	    printf("\n mPadFastSim error, illegal arm value %d\n", dPadXCluster[ihit2].arm);
	    exit(1);
	  }
	  dPadXCluster[ihit2].sector = isect;
	  dPadXCluster[ihit2].wire = 0;
	  dPadXCluster[ihit2].cell = 0;
	  dPadXCluster[ihit2].xyz[0] = pcXghit[iphit].xyzinglo[0];
	  dPadXCluster[ihit2].xyz[1] = pcXghit[iphit].xyzinglo[1];
	  dPadXCluster[ihit2].xyz[2] = pcXghit[iphit].xyzinglo[2];
	  dPadXCluster[ihit2].dxyz[0] = 0.0;
	  dPadXCluster[ihit2].dxyz[1] = 0.0;
	  dPadXCluster[ihit2].dxyz[2] = 0.0;
	  dPadXCluster[ihit2].type = -1;   /* flag a simulated cluster */
	  dPadXCluster_h->nok++;
	  
	  /* Fill the dPadXGhitClus structure */
	  itecgh = dPadXGhitClus_h->nok;
	  /* pcXghit needs an id! */
	  dPadXGhitClus[itecgh].ghitid = iphit;
	  dPadXGhitClus[itecgh].clusid = ihit2;
	  dPadXGhitClus_h->nok++;
		  
	  x = dPadXCluster[ihit2].xyz[0];
	  y = dPadXCluster[ihit2].xyz[1];
	  z = dPadXCluster[ihit2].xyz[2];
	  utiXYtoRPhi(&x,&y,&hitr,&hitphi);
		
	  /* smear the hit in the phi and z direction by the 
	     input resolution. The input resolution should be in 
	     r-phi coordinates. Converted here. */

	  if (dPadFastSimPar[0].phires[pcindex]>0.0) 
	    {
	      mean = hitphi;
	      sigma = fabs(atan(dPadFastSimPar[0].phires[pcindex]/hitr));
	      sigma *= 57.2957795;   /* to degrees */
	      utiGaussian(&mean,&sigma,&irndm,&xgauss);
	      hitphi = xgauss;
	      utiRPhitoXY(&hitr,&hitphi,&x,&y);
	      dPadXCluster[ihit2].xyz[0] = x;
	      dPadXCluster[ihit2].xyz[1] = y;
	    }

	  if (dPadFastSimPar[0].zres[pcindex]>0.0) 
	    {
	      mean = z;
	      sigma = dPadFastSimPar[0].zres[pcindex];
	      utiGaussian(&mean,&sigma,&irndm,&xgauss);
	      hitz = xgauss;
	      dPadXCluster[ihit2].xyz[2] = hitz;
	    }

	}   /* ifire1!=0 */
	  
    }   /* ihit=0, pcXghit_h->nok */

  /* This section will determine if two hits are close enough to be 
     merged into one hit.  If so, one of the two dPadXClusters will be chosen 
     at random and its plane number will be set to a negative value to 
     preserve its merged status through the rest of the analysis chain. */

  if (dPadFastSimPar[0].phisep[pcindex]>0.0) 
    {

      /* First, order the dPadXCluster arrays in order of increasing phi. */

      /* initialize */
      nhitpln = 0;
      for (j=0; j<2000; j++) 
	{
	  hitpln[j] = 0;
	  usedhit[j] = 0;
	}

      /* Create a search list called hitpln that will contain
	 the index of the hits belonging to each plane. */

      /* loop over all PCX clusters in an arm - one at a time */
      for (ihit=0; ihit<dPadXCluster_h->nok; ihit++) 
	{
	  if (dPadXCluster[ihit].arm == 1) 
	    {
	      nhitpln++;
	      if (nhitpln >= 2000) 
		{
		  printf("mPadFastSim-W: too many hits in region \n");
		  printf("Excess hits ignored. Pad chamber %d. \n",pcindex);
		  nhitpln = 1999;
		}
	      hitpln[nhitpln] = ihit;
	    }
	}   /* ihit=0,dPadXCluster->nok */

      /* Now sort hitpln in order of increasing phi for each plane
	 Will use the simple bubble sort. */

      n = nhitpln;
      nn = n-1;
      for (j=1; j<=nn; j++) 
	{
	  l=j;
	  jj=j+1;
	  for (ii=jj; ii<=n; ii++) 
	    {
	      x = dPadXCluster[hitpln[l]].xyz[0];
	      y = dPadXCluster[hitpln[l]].xyz[1];
	      utiXYtoRPhi(&x,&y,&r,&phil);
	      x = dPadXCluster[hitpln[ii]].xyz[0];
	      y = dPadXCluster[hitpln[ii]].xyz[1];
	      utiXYtoRPhi(&x,&y,&r,&phii);
	      phii = hitphi;
	      if (phil >= phii) l=ii;
	    }
	  itemp = hitpln[l];
	  hitpln[l] = hitpln[j];
	  hitpln[j] = itemp;
	}   /* j=1,nn */

      /* with the sort complete, now just compare neighboring phi 
	 values pointed to by hitpln. If they are within the r-phi 
	 limit set by twotrksep, then pick one and flag it as a 
	 merged hit.  This will keep all techits elements available 
	 down the line. */
      
      /* loop over hits in the region */
      for (ihit2=2; ihit2<nhitpln; ihit2++) 
	{
	  /* already merged? */
	  if (usedhit[ihit2] == 0) 
	    {
	      indx = hitpln[ihit2-1];
	      x = dPadXCluster[indx].xyz[0];
	      y = dPadXCluster[indx].xyz[1];
	      z1 = dPadXCluster[indx].xyz[2];
	      utiXYtoRPhi(&x,&y,&hitr,&phi1);
	      indx = hitpln[ihit2];
	      x = dPadXCluster[indx].xyz[0];
	      y = dPadXCluster[indx].xyz[1];
	      z2 = dPadXCluster[indx].xyz[2];
	      utiXYtoRPhi(&x,&y,&hitr,&phi2);

	      /* how far apart are they? */
	      dphi = fabs(phi1-phi2);
	      dphi *= 0.0174533;    /* convert to radians */
	      radius = hitr;    /* fetch an r */
	      drphi = dphi*radius;  /* sep in r-phi coords. */
	      dz = fabs(z1-z2);
	      /* merge the puppy */
	      if (drphi<dPadFastSimPar[0].phisep[pcindex] &&
		  dz<dPadFastSimPar[0].zsep[pcindex])
		{
		  ichoose = utiRandom(&irndm);
		  /* merge ihit-1 if less than 0.5 */
		  if (ichoose <= 0.5) 
		    {
		      usedhit[ihit2-1] = 2;
		      indx = hitpln[ihit2-1];
		      dPadXCluster[indx].wire = -dPadXCluster[indx].wire;
		    }
		  else
		    {
		      usedhit[ihit2] = 2;
		      indx = hitpln[ihit2];
		      dPadXCluster[indx].wire = -dPadXCluster[indx].wire;
		    }
		}   /* drphi<twotrksep */
	    }   /* usedhit==0 */
	}   /* ihit2 loop */

    }   /* twotrksep>0 */

  /* Pass the random number seed back for the next event */
  dPadFastSimPar[0].randseed[pcindex] = irndm;
    
  /* Check the counters on the output tables */
  if ((dPadFastSimPar_h->maxlen <= 0) ||
      (dPadFastSimPar_h->nok > dPadFastSimPar_h->maxlen))
    {
      printf("pcFastSim-F: Too many dPadFastSimPar rows: %ld\n",
	     dPadFastSimPar_h->nok);
      return STAFCV_BAD;
    }
  if ((dPadXCluster_h->maxlen <= 0) ||
      (dPadXCluster_h->nok > dPadXCluster_h->maxlen))
    {
      printf("pcFastSim-F: Too many dPadXCluster rows: %ld\n",
	     dPadXCluster_h->nok);
      return STAFCV_BAD;
    }
  if ((dPadXGhitClus_h->maxlen <= 0) ||
      (dPadXGhitClus_h->nok > dPadXGhitClus_h->maxlen))
    {
      printf("pcFastSim-F: Too many dPadXGhitClus rows: %ld\n",
	     dPadXGhitClus_h->nok);
      return STAFCV_BAD;
    }

   return STAFCV_OK;

}   /* end pcFastSim module */

