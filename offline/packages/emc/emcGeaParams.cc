//////////////////////////////////////////////////////////////////////////////////
//
//  class to replace EmcGeaParams staff table.
//
//
//  for the glory history here is the mapping between
//  parameter names, their position in the orginal array
//  (fortran indexing convention, indices start with 1), and
//  the original meaning.
//  
//  - the original description was in the fortran code [emc.f]
//  - which was converted into c code (with staff tables [mEmcGeaParams.c])
//  - which was converted into c++ code (using emc reco interface [mEmcGeaParamsWrapper.C])
//  - which was converted into another c++ code (using phool interface [EmcGeaParams.h])
//
//  of course no typo or any other kind of error occured.. :)
//
//
//  c          udetpar(1) = float(emc_walls)     ! number of walls from PHNX.PAR
//  c          udetpar(2) = float(emc_opt)       ! EMCal option from PHNX.PAR
//  c          udetpar(3) = float(iwall)        ! wall number (1-4)
//  c          udetpar(4) = float(itype)        ! detector type (Sh-K,PbGl)
//  c          udetpar(5) = angle               ! phi angle of wall center
//  c          udetpar(6) = rpos                ! radial position of wall center
//  c          udetpar(7) = zc_start             ! center of first cell, z coor.
//  c          udetpar(8) = yc_start             ! center of first cell, y coor.
//  c          udetpar(9) = lsiz                ! long. size of a cell
//  c          udetpar(10) = tsiz               ! transverse size of a cell
//  c          udetpar(11) = FLOAT(no_modz)     ! No. of cells in z in a supermod.
//  c          udetpar(12) = FLOAT(no_mody)     ! No. of cells in y in a supermod.
//  c          udetpar(13) = FLOAT(no_smodz)    ! No. of supermods. in z / wall
//  c          udetpar(14) = FLOAT(no_smody)    ! No. of supermods. in y / wall
//  c          udetpar(15) = 0.0                ! 
//  c          udetpar(16) = 0.0                !
//  c          udetpar(17) = 0.0                !
//  c          udetpar(18) = FLOAT(scint_emc_med)  ! Shish-Kebab scint. medium
//  c          udetpar(19) = 0.0                !
//  c          udetpar(20) = 0.0                !
//  c          udetpar(21) = 0.0
//  c          udetpar(22) = FLOAT(emc_debug)
//  c          udetpar(23) = gcuts(1)
//  c          udetpar(24) = gcuts(2)
//  c          udetpar(25) = gcuts(3)
//  c          udetpar(26) = gcuts(4)
//  c          udetpar(27) = gcuts(5)
//  c          udetpar(28) = 0.0     !
//  c          udetpar(29) = 0.0     !
//  c          udetpar(30) = emc_r_min_sc ! bitp lower limit, PbSc
//  c          udetpar(31) = emc_r_max_sc ! bitp upper limit, PbSc
//  c          udetpar(32) = emc_r_step ! bitp stepsize, PbSc
//  c          udetpar(33) = emc_z_min ! bitp lower limit
//  c          udetpar(34) = emc_z_max ! bitp upper limit
//  c          udetpar(35) = emc_z_step ! bitp stepsize
//  c          udetpar(36) = emc_x_min_sc ! bitp lower limit, PbSc
//  c          udetpar(37) = emc_x_max_sc ! bitp upper limit, PbSc
//  c          udetpar(38) = emc_x_step ! bitp stepsize, PbSc
//  c          udetpar(39) = 0.0     !
//  c          udetpar(40) = emc_dele_max_sc ! bitp dE upper limit, PbSc
//  c          udetpar(41) = emc_dele_step_sc ! bitp dE upper limit, PbSc
//  c          udetpar(42) = emc_tof_min ! bitp lower limit
//  c          udetpar(43) = emc_tof_max ! bitp upper limit
//  c          udetpar(44) = emc_tof_step ! bitp stepsize
//  c          udetpar(45) = 0.0     !
//  c          udetpar(46) = 0.0     !
//  c          udetpar(47) = 0.0     !
//  c          udetpar(48) = 0.0     !
//  c          udetpar(49) = 0.0     !
//  c          udetpar(50) = FLOAT(emc_ind1_max_sc) ! bitp tower ind. 
//  c          udetpar(51) = FLOAT(emc_ind2_max_sc) ! bitp tower ind. 
//  c          udetpar(52) = FLOAT(emc_iwall_max) ! 
//  c          udetpar(53) = FLOAT(emc_itype_max) ! 
//  c          udetpar(54) = FLOAT(emc_i1_max) ! 
//  c          udetpar(55) = 0.0     !
//  c          udetpar(56) = 0.0     !
//  c          udetpar(57) = 0.0     !
//  c          udetpar(58) = 0.0     !
//  c          udetpar(59) = 0.0     !
//  c          udetpar(60) = FLOAT(emc_itrack_max) ! 
//  c          udetpar(61) = FLOAT(emc_spart_max) ! 
//  c          udetpar(62) = FLOAT(emc_ncycle_max) ! 
//  c          udetpar(63) = 0.0     !
//  c          udetpar(64) = 0.0     !
//  c          udetpar(65) = emc_cutgam ! 
//  c          udetpar(66) = emc_cutele ! 
//  c          udetpar(67) = emc_cutneu ! 
//  c          udetpar(68) = emc_cuthad ! 
//  c          udetpar(69) = emc_cutmuo ! 
//  c          udetpar(70) = 0.0     !
//  c          udetpar(71) = 0.0     !
//  c          udetpar(72) = 0.0     !
//  c          udetpar(73) = 0.0     !
//  c          udetpar(74) = 0.0     !
//  c          udetpar(75) = 0.0     !
//  c          udetpar(76) = 0.0     !
//  c          udetpar(77) = 0.0     !
//  c          udetpar(78) = 0.0     !
//  c          udetpar(79) = 0.0     !
//  c          udetpar(80) = 0.0     !
//  c
//  c     Field added to ra_det
//  c     (eventually to be overwritten by user parameters
//  c
//  c          ra_det(81,j) = COSD(phi angle)
//  c          ra_det(82,j) = SIND(phi angle)
//  c          ra_det(83,j) = Attenuation length
//  c          ra_det(84,j) = Speed of light (cm/ns)
//  c
//
//
//////////////////////////////////////////////////////////////////////////////////

#include <cmath>

#include <phool.h>
#include <PHCompositeNode.h>
#include <EmcPISAPara.h>

#include <emcGeaParams.h>

ClassImp(emc_gea_params_t);
ClassImp(emcGeaParams);




namespace {


  //
  // New threshold setting 'PPME' introduced to accomodate
  // high pt photons in Pythia events - G. David, Feb. 11, 2000
  //
  float make_sampfrac_auto_pbsc(float val1, float val2){
    float ra_newcalib[4][4] = {
      { 0.2285, 0.1891, 0.1715, 0.1428, },
      { 0.2285, 0.1891, 0.1715, 0.1428, }, 
      { 0.1872, 0.1658, 0.1715, 0.1387, },
      { 0.1382, 0.1593, 0.1715, 0.1295, },
    };

    int ind1, ind2;

    float lim1[] = { 0.0001, 0.005, 0.02, 0.1 };
    for(ind1 = 0; ind1 < 4; ind1++) if( val1 <= lim1[ind1] ) break; 
    if(ind1 == 4) ind1 = 0; // TODO: why??

    float lim2[] = { 0.0, 0.0029, 0.0049, 0.0099 };
    for(ind2 = 3; ind2 != 0; ind2--) if( val2 >= lim2[ind2] ) break; 

    printf("make_sampfrac_auto_pbsc : %f %f -> %f\n", val1, val2, ra_newcalib[ind1][ind2]);
    return ra_newcalib[ind1][ind2];
  }

  float make_sampfrac_auto_pbgl(float val1, float val2){
    return 0.2285;
  }



  /*  Modified sampling fractions according to recalibration
      August 1999, G. David (modifications entered Jan. 17, 2000)
      Modified values: .0010  (AUAU)
                       .0030  (PPLO)
		       .0100  (PPHI)
  */ 
  float make_sampfrac_noauto(float cutgam){
    float sampfrac[23][2]= {
      { .0001, .2443, }, { .0002, .2432, }, { .0005, .2313, }, { .0008, .2204, },
      { .0010, .2251, }, { .0012, .1981, }, { .0015, .1889, }, { .0018, .1809, },
      { .0020, .1760, }, { .0022, .1717, }, { .0025, .1650, }, { .0028, .1607, },
      { .0030, .1872, }, { .0040, .1429, }, { .0050, .1307, }, { .0060, .1214, },
      { .0070, .1130, }, { .0080, .1058, }, { .0090, .0993, }, { .0100, .1426, },
      { .0120, .0824, }, { .0150, .0710, }, { .0200, .0576, },
    };
    
    int i; for(i = 0; i < 23; i++) if(cutgam <= sampfrac[i][0]) break;
    
    // underflow
    if( i == 0 ){printf("make_sampfrac_noauto: %f -> underflow\n", cutgam); return sampfrac[0][1];}

    // overflow
    if( i == 23 ){ printf("make_sampfrac_noauto: %f -> overflow\n", cutgam); return sampfrac[22][1]; }

    // interpolate
    double x = cutgam;
    double x1 = sampfrac[i-1][0], x2 = sampfrac[i][0];
    double y1 = sampfrac[i-1][1], y2 = sampfrac[i][1];

    printf("make_sampfrac_noauto: %f -> %f\n", cutgam, y1 + (y2-y1)/(x2-x1)*(x-x1));
    return  y1 + (y2-y1)/(x2-x1)*(x-x1);
  }

}





emcGeaParams::emcGeaParams(){}




int emcGeaParams::init(PHCompositeNode * root){
  int nrows = EmcPISAPara::GetEmcParaCount();
  resize(nrows);

  EmcPISAPara * parevent = EmcPISAPara::GetEmcParaEvt();
  for(int i = 0; i < nrows; i++){
    float upar[80];
    parevent[i].GetEmcPar(upar);

    emc_gea_params_t * parm = &(*this)[i];
    
    parm->nwall            =  upar[0]; /* number of walls from PHNX.PAR */
    parm->emc_opt          =  upar[1]; /* EMCal option from PHNX.PAR */
    parm->wall             =  upar[2]; /* wall number (1-4) */
    parm->type             =  upar[3]; /* detector type (Sh-K,PbGl */
    parm->angle            =  upar[4]; /* phi angle of wall center */
    parm->rpos             =  upar[5]; /* radial position of wall center */
    parm->zc_start         =  upar[6]; /* center of first cell, z coor. */
    parm->yc_start         =  upar[7]; /* center of first cell, y coor. */
    parm->lsize            =  upar[8]; /* long. size of a cell */
    parm->tsize            =  upar[9]; /* transverse size of a cell */
    parm->nmodz            = upar[10]; /* No. of cells in z in a supermod. */
    parm->nmody            = upar[11]; /* No. of cells in y in a supermod. */
    parm->nsmodz           = upar[12]; /* No. of supermods. in z / wall */
    parm->nsmody           = upar[13]; /* No. of supermods. in y / wall */
    
    parm->translate[0]     = upar[14];
    parm->translate[1]     = upar[15];
    parm->translate[2]     = upar[16];
    
    parm->scint_emc_med    = upar[17]; /* Shish-Kebab scint. medium */
    
    parm->debug            = upar[21]; /* */
    parm->gcuts[0]         = upar[22]; /* */
    parm->gcuts[1]         = upar[23]; /* */
    parm->gcuts[2]         = upar[24]; /* */
    parm->gcuts[3]         = upar[25]; /* */
    parm->gcuts[4]         = upar[26]; /* */
    
    parm->r_min_sc         = upar[29]; /* bitp lower limit, PbSc */
    parm->r_max_sc         = upar[30]; /* bitp upper limit, PbSc */
    parm->r_step           = upar[31]; /* bitp stepsize, PbSc */
    parm->z_min            = upar[32]; /* bitp lower limit */
    parm->z_max            = upar[33]; /* bitp upper limit */
    parm->z_step           = upar[34]; /* bitp stepsize */
    parm->x_min_sc         = upar[35]; /* bitp lower limit, PbSc */
    parm->x_max_sc         = upar[36]; /* bitp upper limit, PbSc */
    parm->x_step           = upar[37]; /* bitp stepsize, PbSc */
    
    parm->dele_max_sc      = upar[39]; /* bitp dE upper limit, PbSc */
    parm->dele_step_sc     = upar[40]; /* bitp dE upper limit, PbSc */
    parm->tof_min          = upar[41]; /* bitp lower limit */
    parm->tof_max          = upar[42]; /* bitp upper limit */
    parm->tof_step         = upar[43]; /* bitp stepsize */
    
    parm->ind1_max_sc      = upar[49]; /* bitp tower ind. */
    parm->ind2_max_sc      = upar[50]; /* bitp tower ind. */
    parm->wall_max         = upar[51]; /* */
    parm->type_max         = upar[52]; /* */
    parm->i1_max           = upar[53]; /* */
    
    parm->track_max        = upar[59]; /* */
    parm->spart_max        = upar[60]; /* */
    parm->ncycle_max       = upar[61]; /* */
    
    parm->cutgam           = upar[64]; /* */
    parm->cutele           = upar[65]; /* */
    parm->cutneu           = upar[66]; /* */
    parm->cuthad           = upar[67]; /* */
    parm->cutmuo           = upar[68]; /* */
    
    parm->array[3]         = upar[18];
    parm->array[4]         = upar[19];
    parm->array[5]         = upar[20];
    
    parm->array[6]         = upar[27];
    parm->array[7]         = upar[28];
    
    parm->array[8]         = upar[38];
    
    parm->array[9]         = upar[44];
    parm->array[10]        = upar[45];
    parm->array[11]        = upar[46];
    parm->array[12]        = upar[47];
    
    parm->array[13]        = upar[48];
    
    parm->array[14]        = upar[54];
    parm->array[15]        = upar[55];
    parm->array[16]        = upar[56];
    parm->array[17]        = upar[57];
    parm->array[18]        = upar[58];
    
    parm->array[19]        = upar[62];
    parm->array[20]        = upar[63];
    
    parm->array[21]        = upar[69];
    parm->dunno            = upar[70];
    parm->array[23]        = upar[71];
    parm->array[24]        = upar[72];
    parm->array[25]        = upar[73];
    parm->array[26]        = upar[74];
    parm->array[27]        = upar[75];
    parm->array[28]        = upar[76];
    parm->array[29]        = upar[77];
    parm->array[30]        = upar[78];
    parm->emc_response_option = upar[79];


    //
    // corrections ( shall i call them afterburners? )
    //

    //-----------------------------------------------------------------------------------------------------------
    

    // Correction following Maxim Volkov's suggestion   G. David, Nov. 21, 2000
    if(i > 5) parm->rpos = 543.2;


    //-----------------------------------------------------------------------------------------------------------


    /* Maxim Volkov discovered a discrepancy between actual size in
       PbGl tower size in PISA and the size passed in GeaParams.  
       True sizes are in z direction 4.104 cm and in y direction 4.1105 cm.
       Since the towers are everywhere assumed to be a square and
       using the z dimension in y only leaves max. 3 mm discrepancy,
       we use the z true z dimension for both z and y now.
       This has to disappear anyway a.s.a.p. when we introduce the
       true (survey) geometry and propagate it back to PISA.
       G. David, Nov. 21, 2000
    */
    if(i > 5) parm->tsize = 4.104;


    //-----------------------------------------------------------------------------------------------------------


    parm->cosangle = cos( ToRadian * parm->angle );
    parm->sinangle = sin( ToRadian * parm->angle );


    //-----------------------------------------------------------------------------------------------------------
    

    // Changed at Sasha Bazilevsky's request, Dec. 5,2000 G. David
    parm->att_length = 120.0;          /* Attenuation length */
    parm->speed_of_light = 17.0;       /* Speed of light in medium */
    parm->noise = 0.0;                 /* Noise */
    parm->adcconv = 1.0;               /* MeV/ADC (FAKE VALUE!!!) */
    parm->decaytime = 10.0;            /* Pulse decay time (ns) */
    parm->edependence = 10.0;          /* Tune for energy dependence */
    parm->delay = 3.0;                 /* Delay (?) */
    parm->tfac = 3.0;                  /* tfac (?) */
    parm->tofthresh = 0.04;            /* TOF thresh */
    parm->ttomax = 3.0;                /* Time to reach max at 1 GeV */
    

    //-----------------------------------------------------------------------------------------------------------


    if ( operator[](0).dunno ){
      if(i < 6) parm->sampfrac = make_sampfrac_auto_pbsc(parm->gcuts[0], parm->dunno);
      else      parm->sampfrac = make_sampfrac_auto_pbgl(parm->gcuts[0], parm->dunno);
    } else {
      parm->sampfrac = make_sampfrac_noauto( parm->gcuts[0] );
    }      

    if(i < 6){
      parm->sampfrac *= 0.87;          /* To correct for other losses */
      parm->photoe_per_gev = 1500.0;   /* Photoelectrons per GeV */
    } else {
      parm->sampfrac = 1.0;            /* To correct for other losses */
      parm->photoe_per_gev = 600.0;    /* Photoelectrons per GeV */
    }
    
    if(parm->sampfrac == 0.0) parm->sampfrac = 0.2256; // unreachable code: sampfrac is newer 0...

    //-----------------------------------------------------------------------------------------------------------


    if( parm->edependence > 0.0){
      float tmp = parm->edependence * log(2.0);
      parm->tfac = parm->ttomax * tmp / (1.0 + tmp);
    }
      

    //-----------------------------------------------------------------------------------------------------------
  }




  return 0;
}





