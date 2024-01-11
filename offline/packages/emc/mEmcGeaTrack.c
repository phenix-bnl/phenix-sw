#include <math.h>
#include <assert.h>
#include <gsl/gsl_math.h>
#include "dio_trk.hh"
#include "mEmcGeaTrack.h"
#include "emlLib.h"

/** This module fetches information from "fkin" (hidden, accessed via
    dio_ptrkstack) for each track that deposited energy in the calorimeter.
    (The info here is essentially a subset of the fkin info).
    Also, for each particle reaching the calorimeter it will tell you
    whether it is a primary (generated in the vertex), or it has
    "ancestors" - and if so, lists data of the ancestors up to an
    arbitrary number of "generations".  The default is that the
    search is cut off at the third generation.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
*/
long mEmcGeaTrack_(
  TABLE_HEAD_ST *dEmcGeaTrackTower_h, DEMCGEATRACKTOWER_ST *dEmcGeaTrackTower ,
  TABLE_HEAD_ST   *dEmcGeaTrack_h,   DEMCGEATRACK_ST     *dEmcGeaTrack )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    mEmcGeaTrack_
**: DESCRIPTION: Physics Analysis Module ANSI C template.
**:             This is an ANSI C Physics Analysis Module template
**:             automatically generated by stic from mEmcGeaTrack.idl.
**:             Please edit comments and code.
**: AUTHOR:     hpl - H.P. Lovecraft, hplovecraft@cthulhu.void
**: ARGUMENTS:
**:       IN:
**:  dEmcGeaTrackTower    - PLEASE FILL IN DESCRIPTION HERE
**:  dEmcGeaTrackTower_h   - header Structure for dEmcGeaTrackTower
**:    INOUT:
**:      OUT:
**:       dEmcGeaTrack    - PLEASE FILL IN DESCRIPTION HERE
**:      dEmcGeaTrack_h   - header Structure for dEmcGeaTrack
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

    long j,k,k1,k2;
    long i_gtr_id;
    long i_ancestry;
    int i_max_ancestry;
    int ll;
    
    static float rmass[25] = 
    {0.0,    0.0005, 0.0005, 0.0,    0.1057,
     0.1057, 0.1349, 0.1395, 0.1395, 0.4977,
     0.4936, 0.4936, 0.9396, 0.9383, 0.9383,
     0.4977, 0.5475, 1.1156, 1.1894, 1.1926,
     1.1975, 1.3149, 1.3213, 1.6724, 0.9396
    };

    float xyz[3];
    double /*todeg,*/ torad;
    short l_found;
    long i_last_trkno;
    long twrhit;
    float edep;
    double d_thvtx,d_phvtx,d_work0,d_work1,d_work2,d_work;
    float r_work;
    
 int error;
 int nfile;
 int status;
 int true_track;
 int idpart;
 float ptot,ptheta,pphi,r_vertex,z_vertex,theta_vertex,phi_vertex;
 int itparent,idparent;
    
/*
**	executable    
*/

    i_max_ancestry = 10;
    torad = M_PI / 180.0;
    /* todeg = 180.0 / M_PI; */

    if (dEmcGeaTrackTower_h->nok < 0) return (STAFCV_BAD);
    else if (dEmcGeaTrackTower_h->nok == 0) return (STAFCV_OK);

    for (ll = 0; ll < sizeof(xyz)/sizeof(xyz[0]); ll++)
      {
	*(xyz + ll) = 0.0;
      }
    
    i_gtr_id =0;
    i_last_trkno = 0;
    for (j = 0; j < dEmcGeaTrackTower_h->nok; j++)
    {
       if (dEmcGeaTrackTower[j].trkno <= 0) goto orphan;
       for (k = 0; k <= 2; k++)
	 {
	   xyz[k] = dEmcGeaTrackTower[j].xyz[k];
	 }
       
       if(dEmcGeaTrackTower[j].trkno != i_last_trkno)
       {
	  l_found = 0;
	  i_ancestry = 1;
	  i_last_trkno = dEmcGeaTrackTower[j].trkno;
	  true_track = i_last_trkno;

	  /* Iterate until max. ancestry level is reached */

	  while (l_found > -1 && i_ancestry <= i_max_ancestry)
	    {
	      
	  /* Get track data */

	      //        The negative or positive number was introduced as a redundancy
	      //check for the user, as explained on the WWW site
	      //"Documentation for PISA Track Ancestry Accessor Functions"
	      //http://www.phenix.bnl.gov/phenix/WWW/simulation/pisaAncestry.html
	      //
	      //************************************************
	      //Redundancy checks 
	      //
	      //As a redundancy check on the dio_ptrkstack ancestry search, 
	      //the parent particle ID idparent is returned as a negative value if the parent
	      //particle itself is a secondary . This is not a mistake by the ancestry 
	      //software, but a deliberate attempt to help you in your coding of your own
	      //searches. On the other hand, if the particle is a primary, then the parent 
	      //ID is returned as 0. There are also redundancy checks on the
	      //itparent track number values. If the particle is itself a primary, 
	      //then the itparent is returned as a negative value equal to the
	      //true_track value. It the particle is not a primary, then the itparent 
	      //track value is returned as a positive number, and that positive must
	      //be seen to be different from the input true_track value, of course. "


	      // OK: prevent dio_ptrkstack from looking for a negative track

	      if(true_track<0)
		{
		  error = 1;
		}
	      else{
		// also, zero out all variables
		nfile = 0; error = 0; itparent = 0; idparent = 0; idpart = 0;
		ptot = 0.0; ptheta = 0.0; pphi = 0.0;
		r_vertex = 0.0; z_vertex = 0.0; theta_vertex = 0.0; phi_vertex = 0.0;

		status = dio_ptrkstack(&true_track, &nfile,&error, 
				     &ptot, &ptheta, &pphi,
				     &r_vertex, &z_vertex, 
				     &theta_vertex, &phi_vertex,
				       &itparent, &idparent, &idpart);
		assert(status == 0);
	      }
	      

	      if(error == 0)
		{
		  /* Get number of towers where it deposited energy 
		     and total energy deposition */
		  edep = 0.0;
		  twrhit = 0;
		  for (k2 = 0; k2 < 3; k2++)  /* There is at least on entry */
		    {
		      if(dEmcGeaTrackTower[j].edep[k2] > 0.0)
			{
			  twrhit++;
			  edep = edep + dEmcGeaTrackTower[j].edep[k2];
			}
		    }
		  k1 = 0;
		  while(dEmcGeaTrackTower[j+k1].nextid > 0 && k1 < 19)
		    {
		      for (k2 = 0; k2 < 3; k2++)  /* There is at least on entry */
			{
			  if(dEmcGeaTrackTower[j+k1+1].edep[k2] > 0.0)
			    {
			      twrhit++;
			      edep = edep + dEmcGeaTrackTower[j+k1+1].edep[k2];
			    }
			}
		      k1++;
		    }
		  dEmcGeaTrack[i_gtr_id].twrhit = twrhit;
		  dEmcGeaTrack[i_gtr_id].edep = edep;

		  /* Write basic data */

		  dEmcGeaTrack[i_gtr_id].id = i_gtr_id;
		  dEmcGeaTrack[i_gtr_id].trkno = true_track;
		  dEmcGeaTrack[i_gtr_id].input = nfile;
		  dEmcGeaTrack[i_gtr_id].anclvl = i_ancestry;
		  dEmcGeaTrack[i_gtr_id].pid = idpart;
		  for (k1 = 0; k1 <= 2; k1++)
		    {

		      dEmcGeaTrack[i_gtr_id].impxyz[k1] = xyz[k1];
		    }
		  
		  dEmcGeaTrack[i_gtr_id].ptot = ptot;
		  dEmcGeaTrack[i_gtr_id].ekin = ptot;
/*		
**	Watch out!  total momentum instead of kinetic energy !	
*/		

		  k1 = idpart - 1;
		  if ( k1 > -1 && k1 < 25)
		    {
		      r_work = rmass[k1] * rmass[k1] +
			ptot * ptot;
		      d_work = r_work;
		      d_work = sqrt(d_work);
		      r_work = d_work - rmass[k1];
		      if (r_work > 0.0)
			{
			  dEmcGeaTrack[i_gtr_id].ekin = r_work;
			}
		    }

/*
**      Position where the particle was born
**      do it with double precision
*/

		  d_thvtx = theta_vertex;
		  d_phvtx = phi_vertex;
		  d_thvtx = torad * d_thvtx;
		  d_phvtx = torad * d_phvtx;
		  d_work1 = sin(d_thvtx);
		  d_work2 = cos(d_phvtx);
		  d_work0 = r_vertex;
		
		  d_work = d_work0 * d_work1 * d_work2;
		  if(z_vertex != 0.0)
		    {
		      d_work = d_work0 * d_work2;
		    }
		  dEmcGeaTrack[i_gtr_id].xyz[0] = d_work;
	      

		  d_work2 = sin(d_phvtx);
		  d_work = d_work0 * d_work1 * d_work2;
		  if(z_vertex != 0.0)
		    {
		      d_work = d_work0 * d_work2;
		    }
		  dEmcGeaTrack[i_gtr_id].xyz[1] = d_work;
		
		  dEmcGeaTrack[i_gtr_id].xyz[2] =
		    z_vertex;

/*
**      Momentum 
**      do it with double precision
*/
		  d_thvtx = ptheta;
		  d_phvtx = pphi;
		  d_thvtx = torad * d_thvtx;
		  d_phvtx = torad * d_phvtx;
		  d_work1 = sin(d_thvtx);
		  d_work2 = cos(d_phvtx);
		  d_work0 = ptot;


		  d_work = d_work0 * d_work1 * d_work2;
		  dEmcGeaTrack[i_gtr_id].pxyz[0] = d_work;
		

		  d_work2 = sin(d_phvtx);
		  d_work = d_work0 * d_work1 * d_work2;
		  dEmcGeaTrack[i_gtr_id].pxyz[1] = d_work;

		  d_work1 = cos(d_thvtx);
		  d_work = d_work0 * d_work1;
		  dEmcGeaTrack[i_gtr_id].pxyz[2] = d_work;
		  dEmcGeaTrack[i_gtr_id].itparent = itparent;

		  dEmcGeaTrack[i_gtr_id].idparent = idparent;

		  if(idparent == 0 || i_ancestry >= i_max_ancestry)
		    {
		      l_found = -1;
		      dEmcGeaTrack[i_gtr_id].itparent = 0;
		      dEmcGeaTrack[i_gtr_id].parent_ptr = -1;
		    }
		  else
		    {
		      i_ancestry = i_ancestry + 1;
		      /*  Continue searching on level back*/
		      true_track = itparent;
		      dEmcGeaTrack[i_gtr_id].parent_ptr = i_gtr_id + 1;
		      
		    }
		  i_gtr_id = i_gtr_id + 1;
		  
		
		}    /* endif error = 0; track found by dio_ptrkstack */

	      else   /* error != 0  Track not found by dio_ptrkstack */
		{
		  l_found = -1;
		  /*
		  printf(" Error in mEmcGeaTrack: true_track not found \n");
		  */
		}
	      
	    }   /* while (l_found >= 0 && i_ancestry <= i_max_ancestry) */
	  
       }        /* End if(dEmcGeaTrackTower[j].trkno != i_last_trkno) */


    orphan:
       continue;       /* Track number was zero, nothing to do */

    }        /* End for (j = 0; j < dEmcGeaTrackTower_h->nok; j++)  */

    dEmcGeaTrack_h->nok = i_gtr_id;
    
   return STAFCV_OK;
}