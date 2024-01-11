#include <math.h>
#include <assert.h>
#include <map>
#include "dio_trk.hh"
#include "mEmcGeaMakeRaw.h"
#include "emlLib.h"

#include <phool.h>

#include <iostream>
#include <cstdlib>

using namespace std;

/** This is the core module in calculating detector response for
    simulated data.  It is fairly complicated and has lots of
    functionality, governed by the dEmcRespPar parameter table.
    If you don't know what you are doing darned well, stick to
    the default values...
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
*/
long type_of_call mEmcGeaMakeRaw_(
  float r_lowgain_convfac, float r_highgain_convfac,
  TABLE_HEAD_ST         *header_h,         HEADER_ST           *header ,
  TABLE_HEAD_ST     *dEmcGeaHit_h,     DEMCGEAHIT_ST       *dEmcGeaHit ,
  TABLE_HEAD_ST  *dEmcGeaParams_h,  DEMCGEAPARAMS_ST    *dEmcGeaParams ,
  TABLE_HEAD_ST    *dEmcRespPar_h,    DEMCRESPPAR_ST      *dEmcRespPar ,
  TABLE_HEAD_ST   *dEmcGeometry_h,   DEMCGEOMETRY_ST     *dEmcGeometry ,
  TABLE_HEAD_ST *dEmcGeaTrackTower_h, DEMCGEATRACKTOWER_ST *dEmcGeaTrackTower ,
  TABLE_HEAD_ST *dEmcGeaTowerTrack_h, DEMCGEATOWERTRACK_ST *dEmcGeaTowerTrack ,
  TABLE_HEAD_ST    *dEmcRawData_h,    DEMCRAWDATA_ST      *dEmcRawData )
{
/* ***************************************************  
	Drastic changes, based on PISORP EMC_USER_NEW
	Dec 19, 1997 G. David

	August 95, G. David

	Main steering routine for EMC in PISORP

	The code has been converted to C on Feb 27, 1998.
	By : 
		PhoolChand,		phool@phenix.barc.ernet.in
		Dipanwita Dutta,	dipa@phenix.barc.ernet.in

	Updated with changes since Jan 98 in memcgeamakeraw.F
	and put in the repository  May 21, 98 G. David

	Updated Sep. 7, 98 - G. David
	        (Cleanup, comments, fkin search dropped for dio_ptrkstack,

	Updated Sep. 20, 98 - G. David

***************************************************  */

#define	max(x,y)	( ( (x) > (y)  ) ? (x) : (y) )
#define	min(x,y)	( ( (x) < (y)  ) ? (x) : (y) )

#ifndef TRUE
#define	TRUE	1
#endif

#ifndef FALSE
#define	FALSE	0
#endif

/*	--------------------------------------------------	*/

   static	int	l_first =  TRUE;

	/*	bit unpacking variables	*/

   static	int	iwall;
   static	int	itype,iarm;
   static	float	dele;
   static	float	pos_x;
   static	float	pos_y;
   static	float	pos_z;
   static	float	tof;
   static	int	i_index[3];
   static	int	numed;

	/*	variables for doing the pulse shape timing	*/

   static	float	r_tdecay;
   static	float	r_ttune;
   static	int	i_tdelay;
   static	float	r_tfac;
   static	float	r_thresh;
   static	float	r_tmaxval;

   /* variables for Year-2 slewing */

   static	float	r_slew_e;
   static	float	r_meas_t;
   static	float	r_slew_t;

#define	p_timemax	1000

   static	float	r_timeoffset = 17.0;
   static	float	r_talpha;
   static	float	r_t;
   static	float	r_earliest_valid_tof;
   static       float   r_timethresh = 0.05;

	/*	other variables from emc_digi in PISA	*/

   static	float	rproj;
   static	float	r_distance;
   static	float	dele_star;
   static	float	tof_star;
   static	int	iz_off;
   static	int	iy_off;
   static	int	iz_rel;
   static	int	iy_rel;

	/*  **************************************************************** 
		This array is for response corrections for different impact
		positions
	********************************* ******************************  */

   static	int	l_correct_unif = FALSE; 
   static	float	emc_pos_unif[24][24];
   static	float	r_cellsize;
   static	float	r_disty,r_distz;
   static	float	r_centx,r_centy,r_centz;

   static	int	 i_tofindex;
   static	float	r_work;
   static	float	r_work1;
   static	float	r_work2;
   static	float	r_work3;
   static	float	r_timebin = 0.05;              /*	 50 ps bins	*/

#define	max_chanz	96 
#define	max_chany	48 
#define	i_detmax	8 

   static	float	emc_dele_cal[i_detmax][max_chany][max_chanz];
   static	float	emc_tof[i_detmax][max_chany][max_chanz];
   static	float	emc_tof_first[i_detmax][max_chany][max_chanz];

   static	float	emc_geom[i_detmax][max_chany][max_chanz][3];

   /* emc_parent[...][0] = energy
      emc_parent[...][1] = true track number
      emc_parent[...][2] = tof_first
   */
   static	float	emc_parent[i_detmax][max_chany][max_chanz][4][3];

   static	int	i_lopre,i_hipre,i_lopost,i_hipost,i_tof;

   static	float	ra_det[120][8];

   static       int i_gtotr_id;    /* Counter of dEmcGeaTowerTrack rows */

   static       int nfile,isubevt;
   static       int itrack_prev,isubevt_prev,nfile_prev;
   //   static       float r_lowgain_convfac;
   //   static       float r_highgain_convfac;
   //#define r_lowgain_convfac  0.001   
   //#define r_highgain_convfac  0.008   
#define r_tdc_convfac  0.05

   /*
     #define i_low_ped 100
     #define i_high_ped 100
   */
#define i_low_ped 4000
#define i_high_ped 4000
#define i_minvalue 100

	/*	Pulse reconstruction from flash + 10 ns
		mostly (17 to 27 ns) in 50 ps bins	*/

   /* #define	p_maxtimebin	300 */
#define	p_maxtimebin	1200
   // This function was originally designed for a large ra_pulse array, but it
   // was mostly empty, so use a multidimensional map instead (it has the same
   // interface as the array). It will adapt dynamically to the required
   // dimensions.
   std::map<size_t,
     std::map<size_t,
     std::map<size_t,
     std::map<size_t, float> > > > ra_pulse;
   static	float	ra_pulserecon[2*p_maxtimebin];

   static	int	idpart,itrack;

   static	int	i,j,k,k1;
   static	int	i1;
   static	int	iy,iz;

   static	int	l_found;

   static       int     l_continue;
   static       int     is1,is2,i_work;

   static	int	istaf,iout_ok;

#define	p_maxtowerhit	30 
   /*#define	p_maxtowerhit	100 */

	/*	Variables needed to build dEmcGeaTrackTower	*/

   static	int	i_twrkey;			/*	 Translates iz,iy,i1 to single number tower key	*/
   static	int	i_gtrto_id;			/*	 ID number in dEmcGeaTrackTower	*/
   static	int	i_last_trkno;			/*	 Last valid track number	*/
   static	int	ia_twrkey[p_maxtowerhit];	/*	 Temp. storage for hit tower keys	*/
   static	float	ra_edep[p_maxtowerhit];		/*	 Temp. storage for hit tower E	*/

   int	l_last_hit;

   static int true_track;
   static int true_track_current;
   static int true_track_prev;

#define         p_gatemax 127.0        /* 17 ns + 110 ns for clock */

   double d_work,d_work1,d_work2,d_work3,d_work4;

   float xyz[3],newxyz[3];

   long ismodz,ismody,irelz,irely,ismoffset,isector,ihwkey;
   long itower;

   static int sim_timing    = 0;    /* Steering timing simulation */
   static int pbgl_response = 0;    /* Steering PbGl response simulation */
   static float pbgl_resolution = 0.06;
   static float pbgl_tofresolution     = 0.25;
   static float pbgl_tofoffset         = 2.5;
   static float pbgl_rescale = 1.18;

   // Try to implement the safer part of Markus's changes
   // G. David, Dec. 2, 2000

   //   Do not eliminate the possibility of rescale, just 
   //   set it to 1.0 right now 
   static float pbgl_ctrk_rescale = 1.0;

   static float noise = 0.0;

   static float pbsc_noise = 0.003;

   static int next_tower = 0;

   // End changes Dec. 2, 2000

   float r_etracking,r_eexp,r_porig;

 int status;
 float ptot,ptheta,pphi,r_vertex,z_vertex,theta_vertex,phi_vertex;
 int itparent,idparent;

 /* For dio_truetrack call */
 /* dio_truetrack thrown out, since now the root output of PISA99
    provides you also with true_track number, even if files are
    merged   Jan. 3, 2000, Gabor David  */

 int error;
 size_t ll;

 /* Cutoff low energy GEANT hits */

 static float r_e_cutoff      =    0.00001;
 /* Change rescale - calibration - factor because the attenuation
    length has been changed,  Dec. 5, 2000,  G. David
 static float r_e_rescale     =    1.02;
 New rescale factor based upon 1.5 GeV photons: 1.02 * 1.08
 */
 static float r_e_rescale     =    1.08;
 static float r_dele_small    =    0.0005;
 static float r_dele_max      =    0.002;

 float dele_sum;
 int i_key[2];

/*-----------------------------------------------------------------------------*/

	/*  ************************************************** 
			Executable
	******************************************************/

   if(l_first)	/*	 Read in	*/
   {
      l_first =  FALSE;

      if(dEmcRespPar_h->nok > 0)
	{
	  if(dEmcRespPar[0].anyset == 1)
	    {
	      sim_timing      = dEmcRespPar[0].sim_timing;
	      pbgl_response   = dEmcRespPar[0].pbgl_response;
	    }
	}

      if(dEmcGeaParams_h->nok <= 0)
      {
	 printf(" error in memcgeamakeraw: missing params \n");
	 return ( STAFCV_OK );
      }
      for ( j = 0; j< dEmcGeaParams_h->nok; j++ )
      {
	 for ( i = 0; i < 120; i++ )
	 {
	    ra_det[i][j] = dEmcGeaParams[j].detarray[i];
	 }
      }

      /*
	If Cherenkov photons were generated in PISA, disallow user
	to steer PbGl response
       */
      if(ra_det[79][6] == 1.0)
	{
	  pbgl_response = 3;
	}

      for (ll = 0; ll < sizeof(emc_geom)/sizeof(emc_geom[0][0][0][0]); ll++)
	{
	  *((float *)emc_geom + ll) = 0.0;
	}
      for ( j = 0; j < dEmcGeometry_h->nok; j++ )
      {
	/* Changed indexing in dEmcGeometry Nov. 20, 1998 G. David 
	 iz = dEmcGeometry[j].ind[0]	-1;
	 iy = dEmcGeometry[j].ind[1]	-1;
	 i1 = dEmcGeometry[j].sector	-1;
	*/
	 iz = dEmcGeometry[j].ind[0];
	 iy = dEmcGeometry[j].ind[1];
	 if(dEmcGeometry[j].arm == 0)
	   {
	     i1 = dEmcGeometry[j].sector;
	   }
	 else
	   {
	     i1 = 7 - dEmcGeometry[j].sector;
	   }

	 emc_geom[i1][iy][iz][0] = dEmcGeometry[j].nomxyz[0];
	 emc_geom[i1][iy][iz][1] = dEmcGeometry[j].nomxyz[1];
	 emc_geom[i1][iy][iz][2] = dEmcGeometry[j].nomxyz[2];
      }
   }

   /* Fudge factors to make up for leaving out low energy hits */

   for (ll = 0; ll < sizeof(emc_dele_cal)/sizeof(emc_dele_cal[0][0][0]); 
	ll++)
     {
       *((float *)emc_dele_cal + ll) = 0.0;
     }
   for (ll = 0; ll < sizeof(emc_tof)/sizeof(emc_tof[0][0][0]); ll++)
     {
       *((float *)emc_tof + ll) = 0.0;
     }
   for (ll = 0; ll < sizeof(emc_tof_first)/sizeof(emc_tof_first[0][0][0]); 
	ll++)
     {
       *((float *)emc_tof_first + ll) = 0.0;
     }
   for (ll = 0; ll < sizeof(emc_parent)/sizeof(emc_parent[0][0][0][0][0]); ll++)
     {
       *((float *)emc_parent + ll) = 0.0;
     }
   for (ll = 0; ll < sizeof(ia_twrkey)/sizeof(ia_twrkey[0]); ll++)
     {
       *(ia_twrkey + ll) = 0;
     }
   for (ll = 0; ll < sizeof(ra_edep)/sizeof(ra_edep[0]); ll++)
     {
       *(ra_edep + ll) = 0.0;
     }
   i_gtrto_id = 0;       /*	 Always start with zero	*/
   i_gtotr_id = 0;       /*	 Always start with zero	*/
   l_last_hit =  FALSE ;

   itrack_prev = 0;
   isubevt_prev = 0;
   nfile_prev = 0;

   true_track_prev = 0;

   /*	Loop over all subevents		*/

   /*
     CFM: May 28, 2005
     Single particle events can miss EMC entirely
   */
   if(dEmcGeaHit_h->nok <= 0) 
   {
      /* printf (" Error in mEmcGeaMakeRaw: no hits \n");  (use verbosity or iDebug check, CFM) */
      return ( STAFCV_OK );
   }

   i_key[0] = -1;
   i_key[1] = -1;
   dele_sum = 0.0;

   for ( istaf = 0;istaf < dEmcGeaHit_h->nok; istaf++ )
   {
     /* You can skip very small hits, but only if they are not
	the very last entry in the hit table for this part. - otherwise you
	get screwed with the TrackTower table */

      i1    = dEmcGeaHit[istaf].sector	-1;
      iwall = dEmcGeaHit[istaf].sector	-1;
      itype = dEmcGeaHit[istaf].type;
      dele  = dEmcGeaHit[istaf].deltae;
      pos_x = dEmcGeaHit[istaf].xyz[0];
      pos_y = dEmcGeaHit[istaf].xyz[1];
      pos_z = dEmcGeaHit[istaf].xyz[2];
      tof   = dEmcGeaHit[istaf].tof;
      i_index[0] = dEmcGeaHit[istaf].smodind;
      i_index[1] = dEmcGeaHit[istaf].towerind;
      numed = dEmcGeaHit[istaf].numed;
      idpart = dEmcGeaHit[istaf].partid;
      itrack = dEmcGeaHit[istaf].itrack;
      isubevt = dEmcGeaHit[istaf].isubevt;
      nfile = dEmcGeaHit[istaf].nfile;
      true_track_current = dEmcGeaHit[istaf].true_track;

      /* Got here the first time */

      /* This part deleted, now true_track comes from PISA99 root
	 output - Jan. 3, G. David */
      /*
      if( itrack_prev == 0 && isubevt_prev == 0
	 && nfile_prev ==0 )
      {
	 itrack_prev = itrack;
	 isubevt_prev = isubevt;
	 nfile_prev = nfile;
	 l_found = FALSE;
	 xyz[0] = pos_x;
	 xyz[1] = pos_y;
	 xyz[2] = pos_z;
      }

      */

      if( true_track_prev == 0)
      {
	 true_track_prev = true_track_current;
	 l_found = FALSE;
	 xyz[0] = pos_x;
	 xyz[1] = pos_y;
	 xyz[2] = pos_z;
      }

      /* Check if (itrack,isubevt,nfile) changed */

      /* Modified: true track now comes from PISA99 root output
	 directly  Jan. 3, 2000,  G. David */
      /*
      if( itrack_prev != itrack || isubevt_prev != isubevt ||
	 nfile_prev != nfile) l_found = FALSE;
      */

      /* Added Markus's changes, Dec. 17, 2000 G. D. */

      if (istaf < dEmcGeaHit_h->nok - 1) {
        next_tower = 100000*(dEmcGeaHit[istaf+1].sector)
                     +1000*(dEmcGeaHit[istaf+1].smodind)
                     +(dEmcGeaHit[istaf+1].towerind);
      }

      /* End Markus's change */

      if( true_track_prev != true_track_current) l_found = FALSE;

      if(istaf == dEmcGeaHit_h->nok - 1 ) l_last_hit =  TRUE ;

	 /*  Take care of readout gate and hits that are too small 
	     but only if this is not the last hit from a track */
      /* Don't do it for PbGl, says Markus, Dec 17, 2000, G. David 
      if(l_found != 0 && l_last_hit == 0)
      */
      if((l_found != 0 && l_last_hit == 0)&&(i1<6))
	{
	  if(dEmcGeaHit[istaf].tof > p_gatemax) goto out_of_gate;
	  if(dEmcGeaHit[istaf].deltae < r_e_cutoff) goto out_of_gate;
	}

      i_key[0] = 100000 * (i1 + 1) + 1000 * i_index[0] + i_index[1];
      if( i_key[1] == -1) i_key[1] = i_key[0];

      /* Markus's correction: check if next tower will
	 be the same
      if( i_key[0] == i_key[1])
      */
      if((i_key[0] == i_key[1])&&(next_tower == i_key[0])&&(l_last_hit==0))
	{
	  if( dele <= r_dele_small )
	    {
	      if(dele_sum <= r_dele_max)
		{
		  dele_sum = dele_sum + dele;
		  if(l_found != 0 && l_last_hit == 0) goto out_of_gate;
		}
	      else
		{
		  dele = dele + dele_sum;
		  dele_sum = 0.0;
		}
	    }
	  else   /* single step dele too big */
	    {
	      dele = dele + dele_sum;
	      dele_sum = 0.0;
	    }
	}
      else  /* new tower hit, not the same as before */
	{
	  dele = dele + dele_sum;
	  dele_sum = 0.0;
	  /* Follow Markus's logic, Dec. 17, 2000 G. David 
	  i_key[1] = i_key[0];
	  */
	  i_key[1] = next_tower;
	}

      /*      if( i1 > 5) goto lead_glass; */ /* Different response */

      /* Previous track, subevt, file doesn't match current */
      /*
      if( l_found == 0)  
      {

	ntrack = itrack;
	isubevent = isubevt;

	error = -1;
	true_track = dio_truetrack(&ntrack, &isubevent, &nfile, &error);
	if( error == 0)
	  {
	    l_found = TRUE;

	    itrack_prev = itrack;
	    isubevt_prev = isubevt;
	    nfile_prev = nfile;
	    newxyz[0] = pos_x;
	    newxyz[1] = pos_y;
	    newxyz[2] = pos_z;
	  }
      }              
      */
      if( l_found == 0)  
      {
	l_found = TRUE;
	true_track = true_track_current;
	true_track_prev = true_track_current;
	newxyz[0] = pos_x;
	newxyz[1] = pos_y;
	newxyz[2] = pos_z;
      }

      /* End if l_found = 0 */

      if( l_found == 0)
      {
	 printf ("Error in mEmcGeaMakeRaw: True track not found \n");
      }

      if(i_gtrto_id == 0)
      {
	 i_gtrto_id = 1;
	 i_last_trkno = true_track;
      }

      if(istaf == dEmcGeaHit_h->nok - 1 ) l_last_hit =  TRUE ;

      /*  **************************************************************************
	  Process hit information (taken from emc_digi.f in PISA as of 8/15/96)

	  Get the distance from the readout device.  This is
	  the difference (RPOS + LSIZ) - RPROJ, where LSIZ is
	  the (full) longitudinal size of the cell (PbGl, crystal,
	  whatever), RPOS is the distance of the center of the
	  front face of the detector, RPROJ is the distance of the
	  hit PROJected on the vector pointing to the center of
	  the detector
	  ********************************************* ******************************  */

      if(dele >= r_dele_small)
	{

      /*  ********************************************************************
	  Recalculate energy and time of flight: correct time of flight
	  with light propagation speed (TOF_STAR), correct energy
	  with light attenuation over the distance between the current
	  position (where energy was deposited) and the end of the
	  module, where light is read out.
		*************************************** ******************************  */

	  /* If PbGl and response parametrization is chosen, do NOT
	     attenuate */

	  if( i1 > 5 && pbgl_response == 2)
	    {
	      dele_star = dele;
	      tof_star = tof;
	    }
	  /* If Cherenkov photons were generated, do NOT attenuate! */
	  else
	    if( i1 > 5 && pbgl_response == 3)
	      {
		dele_star = dele;
		tof_star = tof;
	      }
	    else
	    {

	      d_work1 = pos_x * ra_det[80][i1];
	      d_work2 = pos_y * ra_det[81][i1];
	      d_work = fabs(d_work1) + fabs(d_work2);
	      rproj = d_work;

	      /* Failed to account for global translations in the
		 geometry; this is still not quite clean
		 but works   Aug. 14, 2000  GD 
	      */
	      r_distance = 
		max(0.0, ra_det[5][i1] + ra_det[8][i1] + 
		    fabs(ra_det[14][i1]) - rproj); 
	      r_work = ( - r_distance / ra_det[82][i1] );
	      dele_star = dele * exp (r_work);
	      tof_star = tof + r_distance / ra_det[83][i1];
	    }

	}
      else
	{
	  dele_star = dele;
	  tof_star = 0.0;
	}

      /*  *************************************************************************

	  Figure out indices (in z and y direction) in the corresponding
	  subdetector arrays. This is trivial for PbGl, more complicated for
	  Shish-Kebab, particularly when crystals are in the middle

	  ccfm        i_index(1), i_index(2), and numed are retrieved during unpacking

	  i_index[1] = nubv[1][i];      ! Volume numbers
	  i_index[2] = nubv[2][i];
	  x_d[1] = 0.0;
	  numed = 0;                 ! Reset medium before GMEDIA call
	  CALL GMEDIA(x_m,numed);    ! Get medium where hit is registered

	  Lead glass: the returned volume indices are the indices
	  in the energy deposition array itself

	  Unfold the volume indices (cell, supermodule) and map
	  them to a simple (geometric) array / wall.  In the
	  subsequent analysis you want to make sure that adjacent
	  cells have adjacent indices in the energy and TOF array.
	  Get the relative "position" (i.e. integer index, in which
	  cell it is) within a supermodule.

	  i_index(1) is the number of supermodule within the octant
	  recall, octants are built up vertically, and from -z
	  so the supermodule number map of a wall is this

	  18   15   12    9     6    3
	  17   14   11    8     5    2
	  18   13   10    7     4    1  (-200.cm in z)

	  i_index(2) is the number of cell within the supermodule
	  recall: supermodules are built up vertically, starting at -z
	  so the cell numbers are

	  144 132 120 108  96  84  72  60  48  36  24  12
	  .............
	  133 121 109  97  85  73  61  49  37  25  13   1 (-z, smallest y)

	  Get the cell array index offset due to supermodules before the
	  current one ("before": see numbering scheme above)

	  **************************************************************************  */

      /*	correct if you feel index is not calculated correctaly By phool chand	*/
      /* No, it wasn't quite right... GD */

      switch(itype)
	{

	  /* This guy is lead scintillator */
	case 1:

	  iz_off = ( (i_index[0] - 1) / 
		     ( int ) ra_det[13][i1] ) * ( int ) ra_det[10][i1];
	  iy_off = ( (i_index[0]-1) % 
		     ( int ) ra_det[13][i1] ) * ( int ) ra_det[11][i1];

	 /*	Get the cell array indices within the supermodule	*/
	  iz_rel = 1 + (i_index[1] - 1) /  ra_det[11][i1];
	  iy_rel = i_index[1] - (iz_rel - 1) * ( int ) (ra_det[11][i1]);

	  iz = iz_rel + iz_off	-1;
	  iy = iy_rel + iy_off	-1;

	 /* Numbering in iz goes the other way (from positive z to negative)
	    in the West Arm */
	  if(i1 < 4 && iz <= 71) iz = 71 - iz;

	  if(iz <= -1 || iy <= -1) printf ("wrong indices %d %d \n",iz,iy );
	  if(itype == 1  &&  (iz > 71 || iy > 35))  printf ("wrong indices %d %d \n",iz,iy );

	  break;

	  /* This guy is lead glass */
	case 2:
	  iz_off = ( (i_index[0] - 1) / 
		     ( int ) ra_det[13][i1] ) * ( int ) ra_det[10][i1];
	  iy_off = ( (i_index[0]-1) % 
		     ( int ) ra_det[13][i1] ) * ( int ) ra_det[11][i1];

	 /*	Get the cell array indices within the supermodule	*/
	  iy_rel = 1 + (i_index[1] - 1) /  ra_det[10][i1];
	  iz_rel = i_index[1] - (iy_rel - 1) *  ra_det[10][i1];

	  iz = iz_rel + iz_off	-1;
	  iy = iy_rel + iy_off	-1;

	 /* Numbering in iz goes the other way (from positive z to negative)
	    in the West Arm */
	  if(i1 < 4 && iz <= 71) iz = 71 - iz;

	  if(iz <= -1 || iy <= -1) printf ("wrong indices %d %d \n",iz,iy );
	  if(itype == 1  &&  (iz > 71 || iy > 35))  printf ("wrong indices %d %d \n",iz,iy );

	  break;

	}  /* End of different treatment to get array indices for PbGl, PbSc */

      /*	 Diagnostic plots	*/

      if(dele > 0.0)
      {

	 /* ************************************************************************** 
	    Charlie is right: position uniformity correction should come
	    before the pulse shape reconstruction

	    If position nonuniformity correction is requested,
	    do it now (for Shish-Kebab only)

	    IF we include this in here, we need dEmcGeometry
	    ********************************************* ******************************  */

	 if(l_correct_unif  &&  itype == 1)
	 {
	    r_centx = (pos_x - emc_geom[i1][iy][iz][0]);

	    if(ra_det[80][i1] != 0.0)
	    {
	       r_centy = emc_geom[i1][iy][iz][1] + 
		 ra_det[81][i1] * r_centx / ra_det[80][i1];
	    }
	    else
	    {
	       r_centy = emc_geom[i1][iy][iz][1];
	    }

	    r_centz = emc_geom[i1][iy][iz][2];
	    r_disty = pos_y - r_centy;
	    r_distz = pos_z - r_centz;

	    j = 12 + r_disty * 24.0 / r_cellsize;
	    k = 12 + r_distz * 24.0 / r_cellsize;

	    j = max(1,min(j,24))	-1;
	    k = max(1,min(k,24))	-1;

	    dele_star = dele_star * emc_pos_unif[j][k];
	 }

	 /*  *********************************************************************
	     Work on track-to-tower and tower-to-track table
	     dele_star is final now, it's value isn't changed from now on
	     *************************************** ******************************  */

	 /* Restore dele_star to dele in case of PbGl and leave the
	    response to Muenster */
	 if ( i1 > 5) dele_star = dele;

	 if(i1 < 4)
	   {
	     i_twrkey = 10000 * i1 + 100 * iy + iz;
	   }
	 else
	   {
	     i_twrkey = 100000 + 10000 * (7 - i1) + 100 * iy + iz;
	   }

	 /* Code below this line has been rearranged by PhoolChand	*/

	 while (  l_last_hit || i_last_trkno != true_track  )
	 {				
	    /*  ************************************************** 
		! New track.  Save current 
		! data in dEmcGeaTrackTower 
		! and start cumulating data
		! for the next track
		****************************************************  */

	    /*  At first sort this stupid thing */

	    l_continue = TRUE;
	    is1 = 0;
	    while( l_continue && is1 < p_maxtowerhit)
	    {
	       l_continue = FALSE;
	       is1 = is1 + 1;
	       for(is2 = 0; is2 < p_maxtowerhit-2; is2++)
	       {
		  if( ia_twrkey[is2] > 0 )
		  {
		     if( ra_edep[is2] < ra_edep[is2+1] )
		     {
			l_continue = TRUE;
			i_work = ia_twrkey[is2];
			r_work1 = ra_edep[is2];
			ia_twrkey[is2] = ia_twrkey[is2+1];
			ra_edep[is2] = ra_edep[is2+1];
			ia_twrkey[is2+1] = i_work;
			ra_edep[is2+1] = r_work1;
		     }
		  }
	       }
	    }

	    k = 0;
	    while(k < p_maxtowerhit && ia_twrkey[k] > 0 )
	    {

	       /*	Write a new entry into dEmcGeaTrackTower table	*/
	       dEmcGeaTrackTower[i_gtrto_id-1].id = i_gtrto_id;
	       dEmcGeaTrackTower[i_gtrto_id-1].trkno = i_last_trkno;
	       dEmcGeaTrackTower[i_gtrto_id-1].input = 1;		/*	 For now	*/

	       for ( k1 = 0; k1 < 3; k1++ )
	       {
		  dEmcGeaTrackTower[i_gtrto_id-1].xyz[k1] = xyz[k1];

		  dEmcGeaTrackTower[i_gtrto_id-1].twrkey[k1] = ia_twrkey[k+k1];

		  /*		  
		     dEmcGeaTrackTower[i_gtrto_id-1].edep[k1] =ra_edep[k+k1];
		     Need to rescale it, too...  It worked until now because
		     the rescale factor was essentially 1
		     Dec. 21, 2000, G. David
		  */
		  dEmcGeaTrackTower[i_gtrto_id-1].edep[k1] = 
		    ra_edep[k+k1] * r_e_rescale;
	       }

	       k = k + 3;
	       dEmcGeaTrackTower[i_gtrto_id-1].nextid = 0;   /*	 Assume there is no more	*/

	       if(k < p_maxtowerhit  &&  ia_twrkey[k] > 0) 
	       {
		  dEmcGeaTrackTower[i_gtrto_id-1].nextid = i_gtrto_id + 1;
	       }

	       i_gtrto_id = i_gtrto_id + 1;
	    }       /*	 Loop over k, max. number of track-to-tower entries	*/

	    /*	! or this is the last hit	*/

	    for(k = 0; k < 3; k++) xyz[k] = newxyz[k];

	    for (ll = 0; ll < sizeof(ia_twrkey)/sizeof(ia_twrkey[0]); ll++)
	      {
		*(ia_twrkey + ll) = 0;
	      }
	    for (ll = 0; ll < sizeof(ra_edep)/sizeof(ra_edep[0]); ll++)
	      {
		*(ra_edep + ll) = 0.0;
	      }

	    if( ! l_last_hit )
	    {
	       i_last_trkno = true_track;
	    }
	    else
	    {
	       l_last_hit = FALSE;
	    }

	 }        	/*	  i_last_trkno != itrack (new track started)	*/

	 /*  ************************************************** 
	     ! Still add to the current
	     ! track-to-tower table
	     ****************************************************  */

	 j = 0;

	 /*  ************************************************** 
	     ! Find a match in the ia_twrkey array (it means
	     ! that this is NOT the first energy deposition
	     ! from this track into this tower)
	     ! or find the first UNUSED index in ia_twrkey
	     ! for a new tower
	     ********************* ******************************  */

	 while(j < p_maxtowerhit && ia_twrkey[j] > 0  &&  ia_twrkey[j] != i_twrkey)
	 {
	    j++;
	 }

	 /* Code above this line has been rearranged by phool Chand	*/

	 /*  ************************************************** 
	     ! Therefore, you ad to write out data from
	     ! previous track into dEmcGeaTrackTower
	     ! and clear it.

	     ! Now it is time to increment contents of
	     ! track cumulating arrays with the
	     ! current energy deposit
	     ********************* ******************************  */

	 if(j >= p_maxtowerhit)
	 {
	 }
	 else
	 {
	    /*	dele_star is before sampling fraction correction	*/
	    /*	therefore...	*/

	    ra_edep[j] = ra_edep[j] + dele_star / ra_det[84][i1];
	    if(ia_twrkey[j] == 0) ia_twrkey[j] = i_twrkey;
	 }

	 /*  ************************************************** 
	     ! Now take care of the other direction:
	     ! which track deposited into a particular tower?
	     ********************* ******************************  */

	 k1 = -1;
	 for ( j = 0; j < 4; j++ )
	 {
	    if(k1 == -1  &&  emc_parent[i1][iy][iz][j][1] == 0.0)
	    {
	       k1 = j;
	       emc_parent[i1][iy][iz][j][1] =  true_track;
	    }
	    if(emc_parent[i1][iy][iz][j][1] ==  true_track) k1 = j;
	 }

	 if(k1 >= 0)
	 {
	   j = k1;

	   /* Correcto for sampling fraction */
	    emc_parent[i1][iy][iz][j][0] = 
	      emc_parent[i1][iy][iz][j][0] + dele_star / ra_det[84][i1];

	    /* Carry on time of earliest GEANT hit */
	    if(emc_parent[i1][iy][iz][j][2] == 0.0)
	      emc_parent[i1][iy][iz][j][2] = tof;
	    if(emc_parent[i1][iy][iz][j][2] > tof)
	      emc_parent[i1][iy][iz][j][2] = tof;
	 }

	 /*  
	  *  New arrays for later use in time pulse shape
	  *  reconstruction Note that PbScint dele_star is modifed
	  *  below with emc_pos_unif factor, but this modified value
	  *  was not used in timing reconstruction??
	  */

	 i_tofindex = max(1, 
			  min(p_maxtimebin,( int ) 
			      ((tof_star-r_timeoffset)/r_timebin))) -1;
	 ra_pulse[i1][iy][iz][i_tofindex] = 
	   ra_pulse[i1][iy][iz][i_tofindex] + dele_star / ra_det[84][i1];
      } /*	 check that DELE > 0	*/

      /*	Deposit to 'i1' (This may be any of the subdetectors)	*/

      emc_dele_cal[i1][iy][iz] = emc_dele_cal[i1][iy][iz] + dele_star;	/*	 Modified dE	*/

      /*  *************************************************************** 
	  Time of flight is now the earliest time in the module
	  (this is NOT the real, measured TOF, but it will serve
	  as a starting point when we build up the pulse-shape to
	  get a good simulation of timing)
	  ********************************** ******************************  */

      if(tof_star > 0.0) 
      {
	 if(emc_tof_first[i1][iy][iz] == 0.0)
	 {
	    emc_tof_first[i1][iy][iz] = tof;
	 }
	 else
	 {
	    if(emc_tof_first[i1][iy][iz] > tof_star) 
	      emc_tof_first[i1][iy][iz] = tof;
	 }
      }


   out_of_gate:
      continue;

   }/*	 Loop over istaf = 1, dEmcGeaHit->nok	*/

   /* Sort emc_parent for decreasing energy; you will need the
      particle with highest deposited energy - the "dominant contributor"
      to make the empirical correction to the PbGl response */

   for ( i1 = 0; i1 < 8; i1++ )
   {
     for ( iy = 0; iy <  ra_det[11][i1]*ra_det[13][i1]; iy++ )    
     {
       for ( iz = 0; iz <  ra_det[10][i1]*ra_det[12][i1]; iz++  )
	 {
	   if(emc_parent[i1][iy][iz][1][0] > 0.0)  
	     /* You got more than one contributor */
	     {
	       for( i = 0; i < 3; i++)
		 {
		   for( j = 0; j < 3; j++)
		     {
		       if(emc_parent[i1][iy][iz][j+1][0] >
			  emc_parent[i1][iy][iz][j][0])
			 {
			   for( k = 0; k < 3; k++)
			     {
			       r_work1 = emc_parent[i1][iy][iz][j][k];
			       emc_parent[i1][iy][iz][j][k] =
				 emc_parent[i1][iy][iz][j+1][k];
			       emc_parent[i1][iy][iz][j+1][k] = r_work1;
			     }
			 }
		     }
		 }
	     }
	 } /* End loop over iz, modules in z direction */
     } /* End loop over iy, modules in y direction */
   } /* End loop over i1, sectors */

   /*  ****************************************************************************** 
       Make different corrections to the (total) deposited
       energy.  At this point energy is still strictly non-negative
       ************************************************* ******************************  */

   /* First deal with PbSc response */

   /*   for ( i1 = 0; i1 < i_detmax; i1++ ) */
   for ( i1 = 0; i1 < 6; i1++ )

   {
     for ( iy = 0; iy <  ra_det[11][i1]*ra_det[13][i1]; iy++ )    
/*	 Max. no. of modules, y dir.	*/ 
     {
       for ( iz = 0; iz <  ra_det[10][i1]*ra_det[12][i1]; iz++  )      
/*	 Max. no. of modules, z dir	*/

       {
	 if(emc_dele_cal[i1][iy][iz] > 0.0)
	   {

	       emc_dele_cal[i1][iy][iz] = 
		 emc_dele_cal[i1][iy][iz] * r_e_rescale;

	       /*	Multiply by inverse of sampling fraction	*/

	       if(ra_det[84][i1] > 0.0) emc_dele_cal[i1][iy][iz] = 
			       emc_dele_cal[i1][iy][iz] / ra_det[84][i1];

	       /*  ************************************************************ 
		   If number of photons/GeV is specified, we can generate
		   the statistical term in the energy resolution
		   ******************************* ******************************  */

	       if(ra_det[85][i1] > 0.0)
	       {
		  r_work2 = emc_dele_cal[i1][iy][iz] * ra_det[85][i1];
/*		  r_work3 = sqrt(r_work2);     */
		  d_work1 = r_work2;
		  d_work = sqrt(d_work1);
		  r_work3 = d_work;

		  norran_(&r_work1);
		  r_work2 = r_work2 + r_work3 * r_work1;  /*	 Mean + ran*sigma	*/
		  emc_dele_cal[i1][iy][iz] = r_work2 / ra_det[85][i1];
	       }

	       /*  ****************************************************************** 
		   If noise is specified, put uncorrelated noise on top of
		   the signal in every channel.  Channels with no signal are
		   deliberately omitted (left as 0.0).  If you really want to
		   have noise in every channel (no matter if it is hit or not),
		   change the code, the zeroing of the array EMC_DELE at the
		   beginning of this routine.  Who cares?  Its your CPU time.
		   ************************************* ******************************  */

	       norran_(&r_work1);
	       //	       emc_dele_cal[i1][iy][iz] = 
	       //		 emc_dele_cal[i1][iy][iz] + r_work1 * ra_det[86][i1];
	       emc_dele_cal[i1][iy][iz] = 
		 emc_dele_cal[i1][iy][iz] + r_work1 * pbsc_noise;

	       /*	DO pulse reconstruction		*/

	       switch (sim_timing)
		 {
		 case 0:

		   if(emc_dele_cal[i1][iy][iz] > r_timethresh)
		     {
		       for (ll = 0; ll < sizeof(ra_pulserecon) /
			      sizeof(ra_pulserecon[0]); ll++)
			 {
			   *(ra_pulserecon + ll) = 0.0;
			 }

		       r_tdecay = ra_det[89][i1];	/*	 Pulse decay time	*/
		       r_tdecay = 0.5;           /*	 Hardwire for now (Sep. 24, 97)	*/
		       r_ttune = ra_det[90][i1];
		       i_tdelay =  ra_det[91][i1] / r_timebin;
		       r_tfac = ra_det[92][i1];	
		       r_thresh = ra_det[93][i1];	/*	 TOF threshold	*/

		       r_talpha = r_tfac ;

		  /*		  
		  r_tmaxval = powf( (r_talpha / r_tdecay), r_talpha ) * exp( - r_talpha);
		  */

		       d_work4 = r_talpha;
		       d_work3 = exp(-d_work4);
		       d_work2 = r_talpha / r_tdecay;
		       d_work1 = pow(d_work2,d_work4);
		       d_work= d_work1 * d_work3;
		       r_tmaxval = d_work;

		  /*  ********************************************************************* 
		      The "time history" of the energy deposition (propagated to
		      the PMT's and attenuated) is in ra_pulse(index,i,j,i1)
		      Generate this "partial pulse" in the remaining time-bins
		      **************************************** ******************************  */

		       r_earliest_valid_tof = 0.0;

		       for ( i_tofindex = 0; i_tofindex < p_maxtimebin; i_tofindex++ )
			 {

		     /*  ****************************************************************************** 

			 Here you are reconstructing the pulse itself, then compare
			 it with the timing threshold (ra_det[94][i1])
			 This takes lots of time; we shortcut the pulse reconstruction
			 FROM A SPECIFIC E DEPOSIT AT A SPECIFIC TIME, 
			 if the pulse already passes the timing threshold.
			 (Watch out: you cannot just stop and say: that's my time, because
			 the time can still be pushed EARLIER, particularly for very
			 low energies with large slewing.  But it definitely cannot be
			 pushed LATER; therefore, one you have a valid threshold crossing,
			 you can stop reconstructing the rest of the pulse.)
			 NOTE THAT THIS WOULD NOT WORK WITH A CFD OR A TRAILING EDGE
			 TIMING  --  AS IS THE CASE FOR LEAD GLASS!

			 ! You already had a pulse crossing
			 ! the threshold, and now waited
			 ! 5 ns, more than the maximum slewing
			 ! no pulse, however big, that comes
			 ! later could influence your measurement
			 ! and result in earlier times

			 ************************************************* ******************************  */

			   if(ra_pulse[i1][iy][iz][i_tofindex] > ra_det[86][i1])	/*	 A.P.	*/
			     {

			/*	Do not generate pulse from noise!	*/

			       for ( k = i_tofindex; k < 2*p_maxtimebin-1; k++ )
				 {
				   r_t =  (k+1-i_tofindex) * r_timebin;

				   if((r_t * r_tdecay) <= 75.0)
				     {
			     /*
			       ra_pulserecon[k] = ra_pulserecon[k] +
			       powf (r_t, r_talpha) * exp( - r_t * r_tdecay)
			       * ra_pulse[i_tofindex][i][j][i1] / r_tmaxval;
			       */

				       d_work4 = ra_pulse[i1][iy][iz][i_tofindex] / r_tmaxval;
				       d_work3 = r_tdecay;
				       d_work2 = r_talpha;
				       d_work1 = r_t;
				       ra_pulserecon[k] = ra_pulserecon[k] +
					 pow(d_work1,d_work2) * exp(- d_work1*d_work3)
					 * d_work4;

				     }  /*	 floating point protection for Alpha	*/

				   if(ra_pulserecon[k] > 1.3*ra_det[92][i1])  /* check is it 92 or 93 */
				     {

				       if(r_earliest_valid_tof == 0.0) 
					 {
					   r_earliest_valid_tof = (i_tofindex) * 
					     r_timebin + r_timeoffset;
					 }
				     }
				 }              /*	 Loop over k	*/
			     }
			 }                /*	 Loop over i_tofindex	*/

		       i_tofindex = 0;

		       for ( k = 1;  k < 2 * p_maxtimebin - 1; k++  )
			 {
			   /* if(i_tofindex == 0 && ra_pulserecon[k] > ra_det[93][i1]) */
			   if(i_tofindex == 0 && ra_pulserecon[k] > r_timethresh)
			     {
			       i_tofindex = k;
			       emc_tof[i1][iy][iz] =  (i_tofindex +1) 
				 * r_timebin + r_timeoffset;
			     }
			 }           /*	 Loop over k in ra_pulserecon	*/

		       if(i_tofindex == 0) 		/*	 Missed something, put high val.	*/
			 {
			   emc_tof[i1][iy][iz] = - 99.0;
			 }
		     }        /*	 Total Es exceed TOF threshold	*/
		   break;     /*  End of full pulseshape reconstruction */

		 case 1:
		  /* Skip full pulseshape reconstruction */
		  /* Put in first arrival */

		   emc_tof[i1][iy][iz] = emc_tof_first[i1][i][j];
		   break;

		 case 2:
		  /* Do "constant fraction" a la PHENIX */
		  /* First implementation: just look for the first
		     peak of the signal (true story: the signal
		     is differentiated and the zero crossing of the
		     derivative is measured) */

		   if(emc_dele_cal[i1][iy][iz] > r_timethresh)
		     {
		       for (ll = 0; ll < sizeof(ra_pulserecon) /
			      sizeof(ra_pulserecon[0]); ll++)
			 {
			   *(ra_pulserecon + ll) = 0.0;
			 }

		       r_tdecay = ra_det[89][i1];	/*	 Pulse decay time	*/
		       r_tdecay = 0.5;           /*	 Hardwire for now (Sep. 24, 97)	*/
		       r_ttune = ra_det[90][i1];
		       i_tdelay =  ra_det[91][i1] / r_timebin;
		       r_tfac = ra_det[92][i1];	
		       r_thresh = ra_det[93][i1];	/*	 TOF threshold	*/

		       r_talpha = r_tfac ;

		       d_work4 = r_talpha;
		       d_work3 = exp(-d_work4);
		       d_work2 = r_talpha / r_tdecay;
		       d_work1 = pow(d_work2,d_work4);
		       d_work= d_work1 * d_work3;
		       r_tmaxval = d_work;

		  /*  ********************************************************************* 
		      The "time history" of the energy deposition (propagated to
		      the PMT's and attenuated) is in ra_pulse(index,i,j,i1)
		      Generate this "partial pulse" in the remaining time-bins
		      **************************************** ******************************  */

		       r_earliest_valid_tof = 0.0;

		       for ( i_tofindex = 0; i_tofindex < p_maxtimebin; i_tofindex++ )
			 {

		     /*  
		      *  Here you are reconstructing the pulse itself
		      */

			   /* ra_det[86][i1] is the noise level, set in mEmcGeaParams.c  */

			   if(ra_pulse[i1][iy][iz][i_tofindex] > ra_det[86][i1])
			     {

			/*	Do not generate pulse from noise!	*/

			       for ( k = i_tofindex; k < 2*p_maxtimebin-1; k++ )
				 {
				   r_t = (float) (k+1-i_tofindex) * r_timebin;

				   if((r_t * r_tdecay) <= 75.0)
				     {

				       d_work4 = ra_pulse[i1][iy][iz][i_tofindex] / r_tmaxval;
				       d_work3 = r_tdecay;
				       d_work2 = r_talpha;
				       d_work1 = r_t;
				       ra_pulserecon[k] = ra_pulserecon[k] +
					 pow(d_work1,d_work2) * exp(- d_work1*d_work3)
					 * d_work4;

				     }  /*	 floating point protection for Alpha	*/

				 }              /*	 Loop over k	*/
			     }
			 }                /*	 Loop over i_tofindex	*/

		       i_tofindex = 0;

		       /* Here is where you look for the peak of the pulse 
			  */

		       for ( k = 1;  k < 2 * p_maxtimebin - 2; k++  )
			 {

			   /* You still want to keep r_timethresh to avoid timing on channels
			      with too low energy */
			   if(i_tofindex == 0 && ra_pulserecon[k] > r_timethresh)
			     {
			       if(ra_pulserecon[k] > ra_pulserecon[k+1]
				  && ra_pulserecon[k] > ra_pulserecon[k+2])
				 {

				   i_tofindex = k;
				   emc_tof[i1][iy][iz] = (float) (i_tofindex +1) 
				     * r_timebin + r_timeoffset;
				 }

			     }
			 }           /*	 Loop over k in ra_pulserecon	*/

		       if(i_tofindex == 0) 		/*	 Missed something, put high val.	*/
			 {
			   emc_tof[i1][iy][iz] = - 99.0;
			 }
		     }        /*	 Total Es exceed TOF threshold	*/

		   break;           /* case 2, constant fraction a la PHENIX */

		 }

	    }                       /*	 EMC_DELE_CAL(i1,i,j) > 0.0	*/

	 }            /*	 Loop over iz, modules, z dir.	*/
      }               /*	 Loop over iy, modules, y direction	*/
   }  /*	 loop over i1, PbSc sectors 	*/

   /* Now deal with PbGl response 
      Individual hits have already been propagated and attenuated 
      exponentially */

   for(i1 = 6; i1 < 8; i1++)
     {
       for ( iy = 0; iy < ( int ) (ra_det[11][i1]*ra_det[13][i1]); iy++ )    
/*	 Max. no. of modules, y dir.	*/ 
	 {
	   for ( iz = 0; iz < ( int ) (ra_det[10][i1]*ra_det[12][i1]); iz++  )      
/*	 Max. no. of modules, z dir	*/

	     {
	       if(emc_dele_cal[i1][iy][iz] > 0.0)

		 {

		   switch(pbgl_response)
		     {
		     case 0:     
		       /* Leave everything unchanged; just exp. attenuation */
		       break;

		     case 1:    
		       /* Just smear for photoelectrons, resolution, noise */

		       if(ra_det[85][i1] > 0.0)  /* Photoelectrons */
			 {

			   r_work2 = emc_dele_cal[i1][iy][iz] * ra_det[85][i1];
			   r_work3 = sqrtf(r_work2);
			   norran_(&r_work1);
			   r_work2 = r_work2 + r_work3 * r_work1;  /*	 Mean + ran*sigma	*/
			   emc_dele_cal[i1][iy][iz] = r_work2 / ra_det[85][i1];
			 }

		       /* Overall resolution */
		       norran_(&r_work1);
		       r_work2 = sqrtf(emc_dele_cal[i1][iy][iz]);
		       emc_dele_cal[i1][iy][iz] = emc_dele_cal[i1][iy][iz] +
			 r_work2 * r_work1 * pbgl_resolution;

		       /* Uncorrelated noise */
		       norran_(&r_work1);
		       emc_dele_cal[i1][iy][iz] = emc_dele_cal[i1][iy][iz] 
			 + r_work1 * ra_det[86][i1];

		       /* Rescale to get photons right
			  February 15, 2000  G. David */
		       emc_dele_cal[i1][iy][iz] = 
			 emc_dele_cal[i1][iy][iz] * pbgl_rescale;

		       /* Take care of TOF */
		       emc_tof[i1][iy][iz] = emc_tof_first[i1][iy][iz]
			 + pbgl_tofoffset;
		       norran_(&r_work1);
		       emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] +
			 r_work2 * r_work1 * pbgl_tofresolution;

		       break;

		     case 2:
		       /* GEANT energies have been added.
			  Now it is time for corrections, a la Henner 
			  First you have to figure out particle ID */
		       true_track = (int) emc_parent[i1][iy][iz][0][1];

		       /* The following call IS necessary because
			  Henner needs the total momentum of incident part */ 
		       status = dio_ptrkstack(&true_track, &nfile, &error, 
                              &ptot, &ptheta, &pphi,
		              &r_vertex, &z_vertex, 
                              &theta_vertex, &phi_vertex,
                              &itparent, &idparent, &idpart);
		       assert(status == 0);

		       r_eexp = emc_dele_cal[i1][iy][iz];
		       r_porig = ptot;
		       r_etracking = r_eexp;    /* Default value */

		       if( idpart == 1)      /* Photons */
			 {
			   r_etracking =
			     r_eexp * (1.003 -
				       0.0531*exp(0.2905*r_porig));
			   /* Fudge factor */
			   r_etracking = r_etracking / 0.88;
			 }

		       if( idpart == 2 || idpart == 3)      /* Electrons */
			 {
			   r_etracking =
			     r_eexp * (0.9777 -
				       0.0469*exp(0.4567*r_porig));
			   /* Fudge factor */
			   r_etracking = r_etracking / 0.88;
			 }

		       if( idpart >= 5 && idpart <= 9)      /* Muons, pions */
			 {
			   if(r_porig > 0.5)
			     { if(r_eexp < 0.35) 
			       /* May be min.ion. */
			       {
				 /* r_etracking = r_eexp * 1.663; */
				 r_etracking = r_eexp * 1.85;
			       }
			     else
			       {
				 /* Fudge factor */
				 r_etracking = r_etracking * 0.82;
			       }
			     }
			 }

		       if( idpart >= 11 && idpart <= 16)   /* kaon, proton */
			 {
			   if(r_porig > 0.9)
			     { if(r_eexp < 0.4)
			       {
				 /* May be min.ion. */
				 r_etracking = r_eexp * 
				   (2.055 - 0.03 * r_porig);
			       }
			     else
			       {
				 /* Fudge factor */
				 r_etracking = r_etracking * 0.9;
			       }
			     }
			 }

		       /* Put in array */
		       emc_dele_cal[i1][iy][iz] = r_etracking;

		       /* Take care of TOF */
			 /* + 0.3 * sqrt(abs(r_etracking)) */
		       emc_tof[i1][iy][iz] = emc_tof_first[i1][iy][iz]
			 + pbgl_tofoffset;
		       norran_(&r_work1);
		       emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] +
			 r_work2 * r_work1 * pbgl_tofresolution;

		       break;
		     case 3:     
		       /* Cherenkov photons were generated; leave it alone */

		       /* Fudge factor added when Markus introduced
			  new parametrization: should be double-checked */

		       emc_dele_cal[i1][iy][iz] = 
			 emc_dele_cal[i1][iy][iz] * pbgl_ctrk_rescale;

		       /* Take care of TOF */

		       emc_tof[i1][iy][iz] = emc_tof_first[i1][iy][iz]
			 + pbgl_tofoffset;
		       norran_(&r_work1);
		       emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] +
			 r_work2 * r_work1 * pbgl_tofresolution;

		       break;

		     }

		 }  /* Module has non-zero energy */

	     }  /* End loop over iz, modules in z direction, PbGl sectors */

	 }  /* End loop over iy, modules in y direction, PbGl sectors */

     }   /* End loop over i1, lead glass sectors */

   /*	Fake transformation to "raw data"	*/

   iout_ok = 0;
   for ( iz = 0; iz < max_chanz; iz++ )
   {
      for ( iy = 0; iy < max_chany; iy++ )
      {
	 for ( i1 = 0; i1 < i_detmax; i1++ )
	 { 
	   //  Change to include PbGl noise G. David, Dec 2, 2000
	   //	    if(emc_dele_cal[i1][iy][iz] > 0.001)
	   //	    if((emc_dele_cal[i1][iy][iz] > 0.001)||i1>=6)
	   //  But only for channels where there is some energy already!
	   // Otherwise it just blows up the output  G. David Dec. 2, 2000

	   if(emc_dele_cal[i1][iy][iz] > 0.001)

	    {
	       iarm = 1;
	       if(i1 < 6)
	       {
		  itype = 1;
		  if(i1 < 4) iarm = 0;
	       }
	       else
	       {
		  itype = 2;
	       }

	       i_lopre = i_low_ped;
	       i_hipre = i_high_ped;
	       i_lopost = i_lopre - 
		 ( int ) (emc_dele_cal[i1][iy][iz] / r_lowgain_convfac);
	       i_hipost = i_hipre - 
		 ( int ) (emc_dele_cal[i1][iy][iz] / r_highgain_convfac);

	       // If full pulseshape reconstruction and LED timing, subtract
	       // offset due to delay and risetime in order to make simulated
	       // and real data directly comparable
	       // Also, smear resolution to the observed one:

	       if(sim_timing==0&&itype==1) {
		 emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] - 2.2;
		 // Add slewing correction for Year-2, because real data now
		 // come out as in principle slewing corrected
		 // the last term is not really slewing: it corrects for shifts
		 // connected to increasing shower depths - not tested beyond 10 GeV!
		 r_slew_e = emc_dele_cal[i1][iy][iz];
		 r_meas_t = emc_tof[i1][iy][iz];
		 r_slew_t = r_meas_t - 0.35 / pow(r_slew_e-0.015,0.75) - r_slew_e / 15.0;
		 emc_tof[i1][iy][iz] = r_slew_t;

		 norran_(&noise);
		 /*
		 // Year-2 sector dependent resolution
		 if(i1==0) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.375*noise;
		 if(i1==1) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.400*noise;
		 if(i1==2) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.430*noise;
		 if(i1==3) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.540*noise;
		 if(i1==4) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.360*noise;
		 if(i1==5) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.400*noise;
		 */
		 /*  Take this out for pp; also, have to find a more general solution
		 // Correction for Year-4 62 GeV effective resolution
		 if(i1>=0&&i1<=5) 
		   emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.280 + 0.150*noise;


		 norran_(&noise);
		 */
		 /*
		 // Upper 2 rows in each FEM have slightly worse resolution
		 if(iy==10 || iy==11 || iy==22 || iy==23 || iy==34 || iy==35) {
		   emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.2*noise;
		 }
		 */
	       }

	       if(sim_timing==0&&itype==2) {
		 emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] - 3.03;
		 norran_(&noise);
		 emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] - 0.6*noise;
	       }

	       

	       i_tof = ( int ) (emc_tof[i1][iy][iz] / r_tdc_convfac);

	       // Insert Markus's suggestion for PbGl noise
	       // G. David, Dec. 2, 2000
	       if(i1>=6)
		 {
		   norran_(&noise);
		   i_lopost = i_lopost - (int)(8.3*noise);
		   i_hipost = i_hipost - (int)(1.04*noise);
		 }

	       // End insert

	       if(i_lopost < i_minvalue) i_lopost = i_minvalue;
	       if(i_hipost < i_minvalue) i_hipost = i_minvalue;

	       /* Allow only 12 bits for data */
	       i_lopost = i_lopost & 0x00000FFF;
	       i_hipost = i_hipost & 0x00000FFF;
	       i_tof    = i_tof    & 0x00000FFF;

	       /*  ************************************************** 
		   c
		   c		Write out a record
		   c
		   ********************* ******************************  */

	       dEmcRawData[iout_ok].id = iout_ok +1;
	       dEmcRawData[iout_ok].evno = header[0].event;

	       /* Build hardware key		*/
	       switch(itype)
		 {
		 case 1:
		   /* Lead scintillator */
		   ismodz = iz / 12;
		   ismody = iy / 12;
		   irelz = iz % 12;
		   irely = iy % 12;
		   ismoffset = 432 * ismodz + 144 * ismody;
		   itower = (ismoffset + 12 * irely + irelz) & 0x1FFF;
		   break;

		 case 2:
		   ismodz = iz / 6;
		   ismody = iy / 4;
		   irelz = iz % 6;
		   irely = iy % 4;
		   ismoffset = 288 * ismodz + 24 * ismody;
		   itower = (ismoffset + 4 * irely + irelz) & 0x1FFF;
		   break;

		 default:
		   cout << PHWHERE << "invalid itype " << itype << " exiting" << endl;
		   exit(1);
		 }

	       isector = i1 * 0x2000;  /* Shift 13 bits */
	       ihwkey = isector + itower;

	       dEmcRawData[iout_ok].hwkey = ihwkey;
	       dEmcRawData[iout_ok].type  = itype;

	       if(i1 < 4)
		 {
		   i_twrkey = 10000 * i1 + 100 * iy + iz;
		 }
	       else
		 {
		   i_twrkey = 100000 + 10000 * (7 - i1) + 100 * iy + iz;
		 }

	       dEmcRawData[iout_ok].swkey = i_twrkey;
	       dEmcRawData[iout_ok].adclopre = i_lopre;
	       dEmcRawData[iout_ok].adchipre = i_hipre;
	       dEmcRawData[iout_ok].adclopost = i_lopost;
	       dEmcRawData[iout_ok].adchipost = i_hipost;
	       dEmcRawData[iout_ok].tdc = i_tof;

	       iout_ok = iout_ok + 1;
	    }
	 }
      }
   }

   dEmcRawData_h->nok = iout_ok;
   dEmcGeaTrackTower_h->nok = i_gtrto_id - 1;

   i_gtotr_id = 0;

   for ( i1 = 0; i1 < i_detmax; i1++ )
   {
      for ( iy = 0; iy < max_chany; iy++ )
      {
	 for ( iz = 0; iz < max_chanz; iz++ )
	 {
	    if(emc_parent[i1][iy][iz][0][0] > 0.0)
	    {
	       if(i1 < 4)
		 {
		   i_twrkey = 10000 * i1 + 100 * iy + iz;
		 }
	       else
		 {
		   i_twrkey = 100000 + 10000 * (7 - i1) + 100 * iy + iz;
		 }

	       dEmcGeaTowerTrack[i_gtotr_id].id = i_gtotr_id +1;
	       dEmcGeaTowerTrack[i_gtotr_id].twrkey = i_twrkey;
	       dEmcGeaTowerTrack[i_gtotr_id].input = 1;

	       for ( j = 0; j < 3; j++ )
	       {
		  dEmcGeaTowerTrack[i_gtotr_id].trkno[j] = 
		    emc_parent[i1][iy][iz][j][1];

		  /* Have to rescale it in order to account
		     for decreased attenuation length
		     Dec. 21, G. David
		  dEmcGeaTowerTrack[i_gtotr_id].edep[j] = 
		    emc_parent[i1][iy][iz][j][0];
		  */

		  dEmcGeaTowerTrack[i_gtotr_id].edep[j] = 
		    emc_parent[i1][iy][iz][j][0] * r_e_rescale;

		  dEmcGeaTowerTrack[i_gtotr_id].toffirst[j] = 
		    emc_parent[i1][iy][iz][j][2];
	       }

	       i_gtotr_id = i_gtotr_id + 1;
	    }
	 }
      }
   }

   dEmcGeaTowerTrack_h->nok = i_gtotr_id;

   return ( STAFCV_OK );
}
