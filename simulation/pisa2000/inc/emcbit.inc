c
c	file emcbit.inc
c
c	Radii (min, max)
c
	REAL*4	emc_r_min_sc,emc_r_max_sc,emc_r_step
	REAL*4	emc_r_min_gl,emc_r_max_gl
c
c	z limits (min, max)
c
	REAL*4	emc_z_min,emc_z_max,emc_z_step
c
c	x limits (min, max)
c
	REAL*4	emc_x_min_sc,emc_x_max_sc,emc_x_step
	REAL*4	emc_x_min_gl,emc_x_max_gl
c
c	Delta E (max)
c
	REAL*4	emc_dele_max_sc,emc_dele_step_sc
	REAL*4	emc_dele_max_gl,emc_dele_step_gl
c
c	TOF (min, max)
c
	REAL*4	emc_tof_min,emc_tof_max,emc_tof_step
c
c	Sectors, detector types
c
	INTEGER*4 emc_iwall_max,emc_itype_max,emc_i1_max
c
c	Indices within supermodule
c
	INTEGER*4 emc_ind1_max_sc,emc_ind2_max_sc
	INTEGER*4 emc_ind1_max_gl,emc_ind2_max_gl
c
c	Misc
c
	INTEGER*4 emc_itrack_max,emc_spart_max,emc_ncycle_max
c
	COMMON/EMCBIT/
     1		emc_r_min_sc,emc_r_max_sc,emc_r_step,
     1		emc_r_min_gl,emc_r_max_gl,
     1		emc_z_min,emc_z_max,emc_z_step,
     1		emc_x_min_sc,emc_x_max_sc,emc_x_step,
     1		emc_x_min_gl,emc_x_max_gl,
     1		emc_dele_max_sc,emc_dele_step_sc,
     1		emc_dele_max_gl,emc_dele_step_gl,
     1		emc_tof_min,emc_tof_max,emc_tof_step,
     1		emc_iwall_max,emc_itype_max,emc_i1_max,
     1		emc_ind1_max_sc,emc_ind2_max_sc,
     1		emc_ind1_max_gl,emc_ind2_max_gl,
     1		emc_itrack_max,emc_spart_max,emc_ncycle_max
