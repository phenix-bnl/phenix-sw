/*:>--------------------------------------------------------------------
**: FILE:       mPadRec.c.template
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.2 1998/02/03 22:21:05 dave Exp  
**:<------------------------------------------------------------------*/

/*:>--------------------------------------------------------------------
**: ROUTINE:     mPadRec.c
**: DESCRIPTION: Physics Analysis Module ANSI C template.
**:              Contains pixel cluster reconstruction functions.
**:              Originally developed by Tom Svensson (Lund Group).
**:              Updated and adapted to STAF by Paul Nilsson.
**:
**: AUTHOR:      Paul Nilsson, Lund group, paul@kosufy.lu.se
**: ARGUMENTS:
**:        IN:
**:         dPadRecPar       - Input parameters for the pcRec module
**:         dPadRecPar_h     - header Structure for dPadRecPar
**:         dPadGeom         - PC general geometry parameters
**:         dPadGeom_h       - header Structure for dPadGeom
**:         dPadXRaw         - Raw PC data
**:         dPadXRaw_h       - header Structure for dPadXRaw
**:         pc23par        - PC2/3 geometry updating date (Z=0 split).
**:         pc23par_h       - header Structure for pc23par 
**:        OUT:
**:         dPadXCluster     - Information on each reconstructed cluster
**:         dPadXCluster_h   - header Structure for dPadXCluster
**:         dPadXRawClus     - Relates pcRaw to pcCluster
**:         dPadXRawClus_h   - header Structure for dPadXRawClus
**:   RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/


#include <mPadRec.h>
#include <emlLib.h>

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/* Undefine the MIN and MAX functions if they are predefined */
#ifdef MIN
#undef MIN
#endif
#ifdef MAX
#undef MAX
#endif

#define MAX(a,b) ( ((a) > (b)) ? (a) : (b) )
#define MIN(a,b) ( ((a) < (b)) ? (a) : (b) )
#define truncff(a) ceil( (a + 0.000001) ) - 1.0
#define nearf(a) ceil( (a + 0.500001) ) - 1.0
#define fpad(w,c,t) w + 1 - (w - c + t + 1000)%3;
#define ftype(w,c) (c - w + 999)%3;

typedef struct
{
  int nr_part;
  int maxx;
  int maxz;
  int minx;
  int minz;
  int nr_cells;
  int cellx[1000];
  int cellz[1000];
  float rect[4][4];
} pcpix_clust;

typedef struct
{
  int padx[1000];
  int padz[1000];
  int type[1000];
} sector;

typedef int counter;

static sector sect[16];
static int search_mode = 2;
static int flag[1000],celltype[60];
static int iprintrec = 0;
static int clusters;
static short int pcrawid[17][121][217];
static short int padxx[16][10000],padzz[16][10000];
static long newPISAdate1 = 19980215;  /* PISA updating date, corresponding to 
					 pc23par.idatePC23 */
static long newPISAdate2 = 19981225;
static int split;
static int irawold = 0;
/* FILE *fp; */

/* Define internal pointers for use throughout the file */
TABLE_HEAD_ST *dPadXClusterRec_h;
TABLE_HEAD_ST *dPadGeomRec_h;
TABLE_HEAD_ST *dPadRecParRec_h;
TABLE_HEAD_ST *dPadXRawRec_h;
TABLE_HEAD_ST *dPadXRawClusRec_h;
TABLE_HEAD_ST *pc23parRec_h ;
DPADCLUSTER_ST *dPadXClusterRec;
DPADGEOM_ST *dPadGeomRec;
DPADRECPAR_ST *dPadRecParRec;
DPADRAW_ST *dPadXRawRec;
DPADRAWCLUS_ST *dPadXRawClusRec;
DPAD23PAR_ST *pc23parRec;

extern void utiRPhitoXY(float *lr, float *lphi, float *lx, float *ly);
extern void utiXYtoRPhi(float *kx, float *ky, float *kr, float *kphi);
extern void utiLocaltoGlobal(float lXYZloc[], float lXYZtran[], 
			     float lTheta, float *lXglo,
			     float *lYglo, float *lZglo);

/************************************************************************
NumbPadtoPixel change numbering convention from pad space to pixel space
Should be removed after the chain is being optimised.
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    by     W. Xie,           10/13/1998.
************************************************************************/

void NumbPadtoPixel(int ich, int *ipx, int *ipz)
{
  float tempx, tempz;
/*fprintf(fp,"padx =%d, padz = %d\n", *ipx, *ipz);*/

  tempx = 3*(*ipx)+(*ipz+4-1)%3 -1;

  if(*ipz>=dPadGeomRec[0].npdz[ich-1]/2) tempz = *ipz - 2 ;
  else          tempz = *ipz;

  if((ich == 2 || ich == 3)&&*ipx>=dPadGeomRec[0].npdx[ich-1]/2)
    tempx -=2;

  *ipx = tempx;
  *ipz = tempz-1;
/*fprintf(fp,"-----------------------------------------------\n");
fprintf(fp,"padx =%d, padz = %d\n", *ipx, *ipz);
fprintf(fp,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n");
*/

}      
/***************************************************************************
 ***************************************************************************

 PCPIX_PADS2CELLS
 ----------------

 DESCRIPTION: Reconstruction of fired cells from fired pads.

 AUTHOR/CONTACT: P. Nilsson, Lund University

 REVISIONS:
	Date		Author		Description

	6/19/98         P. Nilsson      Added the cell type
	4/1/98          P. Nilsson      Original

 INPUT VARIABLES:	ich = chamber number (0,1, or 2)
                        isect = sector number

 OUTPUT VARIABLES:	sect[isect].padx[++j] (wire)
                        sect[isect].padz[j]   (cell)
		        sect[isect].type[j]   (cell type)

 DATA STRUCTURES USED:	dPadGeomRec, padxx, padzz

 ***************************************************************************
 ***************************************************************************/

void pcpix_pads2cells(int ich,int isect)
{
  int a,b,c,xa,xb,xc,za,zb,zc,w,wire,cell,nwires,ncells,ctype;
  counter i,j=-1,k=-1;

  /* Number of wires and cells */
  nwires = dPadGeomRec[0].npdwr[ich];
  ncells = dPadGeomRec[0].npdz[ich];

  while (padxx[isect][++k] != -2)
    {
      /* Pick a fired pad from the list and check its type */
      wire = padxx[isect][k];
      cell = padzz[isect][k];
      ctype = ftype(wire,cell); /* 0,1,2 configuration types */

      /* Check the upper, middle, and lower cells of the fired pad */
      /* individually */
      /* Each cell is in turn divided into 3 pixels a,b, and c */
      for (i=-1; i<=1; i++)
	{
	  w = wire + i;

	  /* Pixel a */
	  za = cell - 1;
	  xa = fpad(w,za,ctype);
	  a = pcrawid[isect][xa][za];

	  /* Pixel b */
	  zb = cell;
	  xb = fpad(w,zb,ctype);
	  b = pcrawid[isect][xb][zb];

	  /* Pixel c */
	  zc = cell + 1;
	  xc = fpad(w,zc,ctype);
	  c = pcrawid[isect][xc][zc];

	  if ((a >= 0) && (b >= 0) && (c >= 0))
	    if ((w > 0) && (w < nwires) && (cell > 0) && (cell < ncells))
	      {
		/* Store the coordinates */
		sect[isect].padx[++j] = w;
		sect[isect].padz[j] = cell;
		sect[isect].type[j] = ctype;
	      } /* else printf("Edge hit!\n"); */
	}
    }

  
  /* printf("Number of fired cells:%d\n",j);
  //l = -1;
  //if (j > 0)
  //  while (sect[isect].padx[++l] > 0)
  //    printf("--> hits at cell with index padx = %d, padz = %d, l = %d, sector = %d, type = %d\n",sect[isect].padx[l],sect[isect].padz[l],l,isect,sect[isect].type[l]);
  */

}   /* end pads2cells */


/***************************************************************************
 ***************************************************************************
 LUX_PCPIX_OUT2
 --------------

 DESCRIPTION: 

 AUTHOR/CONTACT: K. Filimonov, McGill University

 REVISIONS:
       Date            Author          Description

       2/17/96         P. Nilsson      Modified for Lund version
       4/2/96          J. Mitchell     Converted to C for LUXOR
       29/1/96         K. Filimonov    Modified for CVS
       11/8/95         K. Filimonov    Modified for pixel option
       4/06/95         J. Mitchell     Original

 INPUT VARIABLES:   ich = pad chamber number (0, 1, or 2)
                    is = sector number
		    iwire = wire number
		    icell = pad number on the wire
		    xyz = local coordinates of cluster

 OUTPUT VARIABLES:

 ***************************************************************************
 ***************************************************************************/

void lux_pcpix_out2(int ich,int is,int iwire,int icell,float xyz[],int pid,int mult)
{
  int iclus,igcl,ArmSect,arm;
  float xglo,yglo,zglo;
  float Rtranslate,Phitranslate,Theta,DeltaPhi;
  float XYZtranslate[3],XYZglobal[3];
  float x,y,r,phi;
    
  /* Use utility to go from local to global coordinates */
  arm = 0;
  if (is >= dPadGeomRec[0].sectperarm[ich]) arm = 1;
  Rtranslate = dPadGeomRec[0].inradius[ich];

  /* CFM November 29, safety fix for Theta possibly not being initialized */
  Theta = 90.0;

  /* CFM November 29, safety check on arm having a bad value */

  if(arm !=0 && arm !=1) {
    printf("\n mPadRec error, illegal arm value %d\n", arm);
  }

  if (arm == 0) 
    {
      DeltaPhi = fabs(dPadGeomRec[0].phitopw - dPadGeomRec[0].phibotw)/
	dPadGeomRec[0].sectperarm[ich];
      ArmSect = is;
      Phitranslate = dPadGeomRec[0].phibotw + (ArmSect+0.5)*DeltaPhi;
      Theta = 90.0 - Phitranslate;
    }

  if (arm == 1) 
    {
      DeltaPhi = fabs(dPadGeomRec[0].phitope-dPadGeomRec[0].phibote)/
	dPadGeomRec[0].sectperarm[ich];
      ArmSect = is - dPadGeomRec[0].sectperarm[ich];
      Phitranslate = dPadGeomRec[0].phibote - (ArmSect+0.5)*DeltaPhi;
      Theta = 450.0 - Phitranslate;
      if (Theta > 360.0) Theta -= 360.0;
    }

  utiRPhitoXY(&Rtranslate,&Phitranslate,&XYZtranslate[0],&XYZtranslate[1]);

  XYZtranslate[2] = 0.0;
  utiLocaltoGlobal(xyz,XYZtranslate,Theta,&xglo,&yglo,&zglo);

  XYZglobal[0] = xglo;
  XYZglobal[1] = yglo;
  XYZglobal[2] = zglo;

  iclus = dPadXClusterRec_h->nok;
  dPadXClusterRec[iclus].id = iclus;
  dPadXClusterRec[iclus].arm = 0;
  dPadXClusterRec[iclus].sector = is;

  if (is>=dPadGeomRec[0].sectperarm[ich]) 
    {
      dPadXClusterRec[iclus].arm = 1;
      dPadXClusterRec[iclus].sector = is - dPadGeomRec[0].sectperarm[ich];
    }

  dPadXClusterRec[iclus].wire = iwire;
  dPadXClusterRec[iclus].cell = icell;
  dPadXClusterRec[iclus].xyz[0] = XYZglobal[0];
  dPadXClusterRec[iclus].xyz[1] = XYZglobal[1];
  dPadXClusterRec[iclus].xyz[2] = XYZglobal[2];//xyz[2];
  /* Geometry correction for PC1 - shift of 0.625 degrees in phi. 
     Added by JTM on 9/23/98 */
  if (dPadRecParRec[0].pcnumber == 0)
    {
      if (pc23parRec[0].idatePC23>=newPISAdate1 &&
	  pc23parRec[0].idatePC23<newPISAdate2)
	{
	  x = dPadXClusterRec[iclus].xyz[0];
	  y = dPadXClusterRec[iclus].xyz[1];
	  utiXYtoRPhi(&x,&y,&r,&phi);
	  phi -= 0.625;
	  utiRPhitoXY(&r,&phi,&x,&y);
	  dPadXClusterRec[iclus].xyz[0] = x;
	  dPadXClusterRec[iclus].xyz[1] = y;
	}   /* pc23par.idatePC23>=newPISAdate1 */
    }   /* dPadRecPar.pcnumber==0 */
  /* END JTM 9/23/98 modifications */
  dPadXClusterRec[iclus].dxyz[0] = xyz[0]; //.4; /* PRELIMINARY */
  dPadXClusterRec[iclus].dxyz[1] = xyz[1]; //.4; /* PRELIMINARY */
  dPadXClusterRec[iclus].dxyz[2] = xyz[2]; //.4; /* PRELIMINARY */
  dPadXClusterRec[iclus].type = mult;  /* nr of cells in cluster */
  dPadXClusterRec_h->nok++;

  /* Fill relational data structures */
  igcl = dPadXRawClusRec_h->nok;
  dPadXRawClusRec[igcl].rawid = pid;
  dPadXRawClusRec[igcl].clusid = igcl;
  dPadXRawClusRec_h->nok++;

}   /* end lux_pcpix_out2 */


/***************************************************************************
 ***************************************************************************

 PCPIX_ADJACENT
 --------------

 DESCRIPTION: Returns 1 or 0 whether two cells are adjacent neighbours or not

 AUTHOR/CONTACT: P. Nilsson, T. Svensson, Lund University

 REVISIONS:
	Date		Author		Description

        13/02/98        P. Nilsson      Converted to LUXOR
        12/02/96        T. Svensson     Original

 INPUT VARIABLES: w1 = wire 1, p1 = pad 1, w2 = wire 2, p2 = pad 2


 OUTPUT VARIABLES: neighbour (= 1 if two cells are adjacent)


 DATA STRUCTURES USED:	

 ***************************************************************************
 ***************************************************************************/


int pcpix_adjacent(int w1, int p1, int w2, int p2)
{
  int neighbourgh = 0;

  switch (search_mode) /* 2 fired adjacent cells = 1 cluster */
    {
    case 1: /* Allow diagonally positioned cells to be "adjacent" */ 
      if ((abs(w1-w2) <= 1) && (abs(p1-p2) <= 1))
	neighbourgh = 1;
      break;
    case 2: /* Don't allow diagonally positioned cells to be "adjacent" */ 
      if (((abs(w1-w2) <= 1) && (abs(p1-p2) <= 1)) &&
	 !((abs(w1-w2) == 1) && (abs(p1-p2) == 1)))
	neighbourgh = 1;
      break;
    default:
      printf(" ERROR: search mode (%d) is not correctly defined \n",search_mode);
      break;
    }

  return (neighbourgh);

}   /* end pcpix_adjacent */


/***************************************************************************
 ***************************************************************************

 PCPIX_REMOVE_FLAGGED
 --------------------

 DESCRIPTION: In order to prevent cells belonging to different clusters
              from being counted twice, they have to be removed from the 
              stream. This is done by simply skipping them when the
              remaining cells are sorted...

 AUTHOR/CONTACT: P. Nilsson, T. Svensson, Lund University

 REVISIONS:
	Date		Author		Description

        13/02/98        P. Nilsson      Converted to LUXOR
        12/02/96        T. Svensson     Original

 INPUT VARIABLES: nr_in_array = number of flagged cells to be removed,
                  isect = sector number
                  flag[] = flag vector, i.e. which cells have been used?

 OUTPUT VARIABLES:

 DATA STRUCTURES USED: sect = padx, padz (coordinates of fired cells)

 ***************************************************************************
 ***************************************************************************/


void pcpix_remove_flagged(int nr_in_array, int isect)
{
  counter i,k=0;

  for (i=0; i <= nr_in_array; ++i) 
    {
      if (flag[i] == 0) /* Skip all flagged cells */
	{
	  sect[isect].padx[k] = sect[isect].padx[i];
	  sect[isect].padz[k] = sect[isect].padz[i];
	  sect[isect].type[k++] = sect[isect].type[i];
	}
      else
	flag[i] = 0; /* Clear the flag vector */
    }

}   /* end remove_flagged */


/***************************************************************************
 ***************************************************************************

 PCPIX_NR_PART_IN_CLUSTER
 ------------------------

 DESCRIPTION: In this function a hypothetical box is put around the found
              cluster to estimate the number of particles that created it.

	      NOTE: THIS FUNCTION WILL BE OPTIMIZED AS SOON AS WE
	            HAVE SOME REAL DATA TO COMPARE WITH

 AUTHOR/CONTACT: P. Nilsson, T. Svensson, Lund University

 REVISIONS:
	Date		Author		Description

        13/02/98        P. Nilsson      Converted to LUXOR
        12/02/96        T. Svensson     Original

 INPUT VARIABLES: clst_ptr (pointer to clst)

 OUTPUT VARIABLES: clst_ptr->nr_part (number of particles in a given
                                      cluster)

 DATA STRUCTURES USED: clst = e.g. cellx, cellz, nr_cells, nr_part...

 ***************************************************************************
 ***************************************************************************/


void pcpix_nr_part_in_cluster(pcpix_clust *clst_ptr)
{
  /*
  static part1x = 1;
  static part1z = 0;
  static part2x = 2;
  static part2z = 0;
  static part3x = 3;
  static part3z = 0;
  static part4x = 1;
  static part4z = 1;
  static part5x = 2;
  static part5z = 1;
  static part6x = 3;
  static part6z = 1;
  static part7x = 1;
  static part7z = 2;
  static part8x = 2;
  static part8z = 2;
  static part9x = 3;
  static part9z = 2;
  */

  static int part1x = 2;
  static int part1z = 2;
  static int part2x = 5;
  static int part2z = 5;
  static int part3x = 7;
  static int part3z = 7;

  counter k;
  int x_len;
  int z_len;

  clst_ptr->maxx = 0;
  clst_ptr->maxz = 0;
  clst_ptr->minx = 1000;
  clst_ptr->minz = 1000;

  for (k = 0; k <= clst_ptr->nr_cells; ++k)
    {
      clst_ptr->maxx = MAX(clst_ptr->cellx[k],clst_ptr->maxx);
      clst_ptr->maxz = MAX(clst_ptr->cellz[k],clst_ptr->maxz);
      clst_ptr->minx = MIN(clst_ptr->cellx[k],clst_ptr->minx);
      clst_ptr->minz = MIN(clst_ptr->cellz[k],clst_ptr->minz);
    }

  /* Store the width and height of the cluster */
  x_len = clst_ptr->maxx - clst_ptr->minx;
  z_len = clst_ptr->maxz - clst_ptr->minz;

  /* Determine how many particles a certain cluster contains 
     depending on the size of the cluster */

  /*
  if ((x_len <= part3x) && ( z_len <= part3z))
    if (clst_ptr->nr_cells <= 2)
      clst_ptr->nr_part = 1;
    else
      clst_ptr->nr_part = 2;
  else if ((x_len <= part4x) && ( z_len <= part4z))
    if (clst_ptr->nr_cells <= 2)
      clst_ptr->nr_part = 1;
    else
      clst_ptr->nr_part = 2;
  else if ((x_len <= part5x) && ( z_len <= part5z))
    if (clst_ptr->nr_cells <= 3)
      clst_ptr->nr_part = 1;
    else
      clst_ptr->nr_part = 2; 
  else if ((x_len <= part6x) && ( z_len <= part6z))
    if (clst_ptr->nr_cells <= 5)
      clst_ptr->nr_part = 2;
    else
      clst_ptr->nr_part = 3;
  else if ((x_len <= part7x) && ( z_len <= part7z))
    if (clst_ptr->nr_cells <= 4)
      clst_ptr->nr_part = 2;
    else
      clst_ptr->nr_part = 3;
  else if ((x_len <= part8x) && ( z_len <= part8z))
    if (clst_ptr->nr_cells <= 6)
      clst_ptr->nr_part = 2;
    else
      clst_ptr->nr_part = 3; 
  else
    clst_ptr->nr_part = 3;
  */

  if ((x_len <= part1x) && ( z_len <= part1z))
    if (clst_ptr->nr_cells <= 7) /* 5,6 */
      clst_ptr->nr_part = 1;
    else
      clst_ptr->nr_part = 2;
  else if ((x_len <= part2x) && ( z_len <= part2z))
    if (clst_ptr->nr_cells <= 16) /* 10,12,14 */
      clst_ptr->nr_part = 2;
    else
      clst_ptr->nr_part = 3; 
  else if ((x_len <= part3x) && ( z_len <= part3z))
    if (clst_ptr->nr_cells <= 24) /* 17,19,22 */
      clst_ptr->nr_part = 3;
    else
      clst_ptr->nr_part = 4; 
  else
    clst_ptr->nr_part = 4;

}   /* end pcpix_nr_part_in_cluster */


/***************************************************************************
 ***************************************************************************

 PCPIX_SPLIT_CLUSTER
 -------------------

 DESCRIPTION: This guy tries to split the cluster depending on the cluster
              size and the number of particles that generated it.

	      NOTE: THIS FUNCTION WILL BE OPTIMIZED AS SOON AS WE
	            HAVE SOME REAL DATA TO COMPARE WITH

 AUTHOR/CONTACT: P. Nilsson, T. Svensson, Lund University

 REVISIONS:
	Date		Author		Description

        13/02/98        P. Nilsson      Converted to LUXOR
        12/02/96        T. Svensson     Original

 INPUT VARIABLES: clst_ptr (pointer to clst)

 OUTPUT VARIABLES: clst_ptr->rect[][] (local coordinates for the rectangle
                                       surrounding the cluster)

 DATA STRUCTURES USED: clst = e.g. cellx, cellz, nr_cells, nr_part...

 ***************************************************************************
 ***************************************************************************/


void pcpix_split_cluster(pcpix_clust *clst_ptr)
{
  counter k;
  int x_len;
  int z_len;
  float side;

  x_len = clst_ptr->maxx - clst_ptr->minx;
  z_len = clst_ptr->maxz - clst_ptr->minz;

  if (clst_ptr->nr_part == 1)
    {
      clst_ptr->rect[0][0] = clst_ptr->minx;
      clst_ptr->rect[0][1] = clst_ptr->minz;
      clst_ptr->rect[0][2] = clst_ptr->maxx;
      clst_ptr->rect[0][3] = clst_ptr->maxz;
    }

  /* Split the cluster "horizontally" */
  else if (x_len >= z_len)
    {
      side = (float)(x_len+1)/(float)clst_ptr->nr_part;
      for (k = 0; k <= clst_ptr->nr_part-1; ++k)
	{
	  clst_ptr->rect[k][0] = clst_ptr->minx + k * side;
	  clst_ptr->rect[k][1] = clst_ptr->minz;
	  clst_ptr->rect[k][2] = clst_ptr->minx + ( k + 1 ) * side - 1;
	  clst_ptr->rect[k][3] = clst_ptr->maxz;	  
	}
    }

  /* Split the cluster "vertically" */
  else
    {
      side = (float)(z_len+1)/(float)clst_ptr->nr_part;
      for (k = 0; k <= clst_ptr->nr_part-1; ++k)
	{
	  clst_ptr->rect[k][0] = clst_ptr->minx;
	  clst_ptr->rect[k][1] = clst_ptr->minz + k * side;
	  clst_ptr->rect[k][2] = clst_ptr->maxx;
	  clst_ptr->rect[k][3] = clst_ptr->minz + ( k + 1 ) * side - 1;	  
	}
    }

  return;

}   /* end pcpix_split_cluster */


/***************************************************************************
 ***************************************************************************

 PCPIX_CALCULATE_POS
 -------------------

 DESCRIPTION: The function looks for fired cells within each cluster (if the
              original cluster was split into several clusters in the
              previous function) and weighs the content to estimate the x
              and z coordinate of the traversing particle(s) that generated
              the cluster(s) in the first place.

	      NOTE: THIS FUNCTION WILL BE TOTALLY REWRITTEN (->FASTER)

 AUTHOR/CONTACT: P. Nilsson, T. Svensson, Lund University

 REVISIONS:
	Date		Author		Description

        13/02/98        P. Nilsson      Converted to LUXOR
        12/02/96        T. Svensson     Original

 INPUT VARIABLES: clst_ptr (pointer to clst), ich (chamber number),
                  isect (sector number)

 OUTPUT VARIABLES: ich (chamber number), isect (sector number),
                   icellx (cell coordinate for the hit),
                   icellz (cell coordinate for the hit),
                   xyz[] (local coordinates for the cluster),
                   nttk[] (id's for the three pads that makes the central
                           cell)

 DATA STRUCTURES USED: clst = e.g. cellx, cellz, nr_cells, nr_part...

 ***************************************************************************
 ***************************************************************************/

void pcpix_calculate_pos(pcpix_clust *clst_ptr, int ich, int isect)
{
  counter k,l,nn,n;
  float xw[200], zw[200], xyz[3];
  float sumx, sumz, nsumx, nsumz, aasep, pxlen, pxsep;
  float wside, wcent, zgap, pdxoff, pdzoff;
  int nr_x_cells, nr_z_cells, xtmp[200], ztmp[200], xfreq[200],zfreq[200];
  int icellx, icellz, ipadx, ipadz, pid, ipos, type;


  for (k = 0; k <= clst_ptr->nr_part-1; ++k)
    {
      nr_x_cells = (int)(ceil(clst_ptr->rect[k][2]) - clst_ptr->rect[k][0]+1);
      nr_z_cells = (int)(ceil(clst_ptr->rect[k][3]) - clst_ptr->rect[k][1]+1);

      for (l = 0; l <= nr_x_cells-1; ++l)
	if (l == 0)
	  {
	    xw[l] = clst_ptr->rect[k][0]-truncff(clst_ptr->rect[k][0]);
	    if( xw[l] < 0.01 )
	      xw[l] = 1;
	  }
        else if (l == nr_x_cells - 1)
	  {
	  xw[l] = clst_ptr->rect[k][2]-truncff(clst_ptr->rect[k][2]);
	    if( xw[l] < 0.01 )
	      xw[l] = 1;
	  }
        else 
	  xw[l] = 1;

      for (l = 0; l <= nr_z_cells-1; ++l)
	if (l == 0)
	  {
	    zw[l] = clst_ptr->rect[k][1]-truncff(clst_ptr->rect[k][1]);
	    if( zw[l] < 0.01 )
	      zw[l] = 1;
	  }
        else if (l == nr_z_cells - 1)
	  {
	    zw[l] = clst_ptr->rect[k][3]-truncff(clst_ptr->rect[k][3]);
	    if (zw[l] < 0.01)
	      zw[l] = 1;
	  }
        else 
	  zw[l] = 1;

      xtmp[0] = nearf(clst_ptr->rect[k][0]-0.1);
      xfreq[0] = 0;
      for (l = 1; l <= nr_x_cells-1; ++l)
        {
          xtmp[l] = xtmp[l-1] + 1;
          xfreq[l] = 0;
        }

      ztmp[0] = nearf(clst_ptr->rect[k][1]-0.1);
      zfreq[0] = 0;
      for (l = 1; l <= nr_z_cells-1; ++l)
        {
          ztmp[l] = ztmp[l-1] + 1;
          zfreq[l] = 0;
        }

      for (l = 0; l <= clst_ptr->nr_cells; ++l)
        {
          if ((clst_ptr->cellx[l] >= nearf(clst_ptr->rect[k][0]-0.1)) &&
	     (clst_ptr->cellx[l] <= nearf(clst_ptr->rect[k][2]-0.1) ) )
	    if ((clst_ptr->cellz[l] >= nearf(clst_ptr->rect[k][1])-0.1) &&
	       (clst_ptr->cellz[l] <= nearf(clst_ptr->rect[k][3]-0.1) ) )
	      {
	        for (n = 0; n <= nr_x_cells-1; ++n)
	          if(clst_ptr->cellx[l] == xtmp[n])
	              ++xfreq[n];
	        for (n = 0; n <= nr_z_cells-1; ++n)
	          if (clst_ptr->cellz[l] == ztmp[n])
		    ++zfreq[n];	    
	      }
        }

      /* Sum up the contribution along the x-axis */
      for (l = 0, sumx = 0, nsumx = 0 ; l <= nr_x_cells-1 ; ++l)
        {
          sumx += ((float)xtmp[l] * (float)xfreq[l]) * xw[l];
          nsumx += ((float)xfreq[l] * xw[l]);
        }

      /* Sum up the contribution along the z-axis */
      for (l = 0, sumz = 0, nsumz = 0; l <= nr_z_cells-1 ; ++l)
        {
          sumz += (float)ztmp[l] * (float)zfreq[l] * zw[l];
          nsumz += ( (float)zfreq[l] * zw[l] );
        }

      /* Calculate the weighed center for the hit */
      aasep = dPadGeomRec[0].aasep[ich];
      pxlen = dPadGeomRec[0].pxlen[ich];
      pxsep = dPadGeomRec[0].pxsep[ich];
      wside = dPadGeomRec[0].wside[ich];
      wcent = dPadGeomRec[0].wcent[ich];

      /* Restore the gap in PC2/3 */
      /* zgap = 0.; // No gap in the present PISA files!!! */
      if ((pc23parRec[0].idatePC23 >= newPISAdate1) && (sumz/nsumz >= 106.))
        zgap = dPadGeomRec[0].zgap[ich];
      else
        zgap = 0.;

      /* Get the cell coordinates of the reconstructed hit */
      icellx = (int) nearf(sumx/nsumx);
      icellz = (int) nearf(sumz/nsumz);

      /* Offset variables to the local coordinate system */
      pdxoff = dPadGeomRec[0].pdxoff[ich];
      pdzoff = dPadGeomRec[0].pdzoff[ich] ;

      /* Transformation from cell coordinates to local coordinates */
      xyz[0] = (sumx/nsumx)*aasep + wside + 0.5*wcent + pxsep + pdxoff;
      xyz[1] = 0;
      xyz[2] = (sumz/nsumz)*(pxlen + pxsep) + 0.5*pxlen + zgap + pdzoff;

      if (iprintrec > 0)
	{
	  //	  printf("Center is at xyz[0]:%f xyz[2]:%f\n",xyz[0],xyz[2]);
	  //	  printf("which corresponds to icellx:%d icellz:%d ipos:%d\n",icellx,icellz,ipos);
	}

      /* Which pixel in the cell was hit? */
      ipos = (int) nearf(3*sumx/nsumx) - 3*icellx;

      /* Retrieve the cell type */
      nn = -1;
      while (++nn && nn < 1000)
	if ((icellx == clst_ptr->cellx[nn]) && (icellz == clst_ptr->cellz[nn])) break;
      type = celltype[nn];

      /* Which pad does the pixel belong to? */
      /* WARNING: icellz -/+ ipos = Xie's/Jeff's numbering convention */
      ipadz = icellz - ipos;
      ipadx = fpad(icellx,ipadz,type);
      pid = pcrawid[isect][ipadx][ipadz];

      /*
      split++;
      */

      /* Fill out the data structure: */
      lux_pcpix_out2(ich,isect,icellx,icellz,xyz,pid,clst_ptr->nr_cells+1);

    }

}   /* end pcpix_calculate_pos */


/***************************************************************************
 ***************************************************************************

 PCPIX_FIND_CLUSTER
 ---------------

 DESCRIPTION: Identification of the clusters, one at the time...

 AUTHOR/CONTACT: P. Nilsson, T. Svensson, Lund University

 REVISIONS:
	Date		Author		Description

        13/02/98        P. Nilsson      Converted to LUXOR
        12/02/96        T. Svensson     Original

 INPUT VARIABLES: ich = chamber number, isect = sector number

 OUTPUT VARIABLES:

 DATA STRUCTURES USED: sect = padx, padz (coordinates of fired cells)
                       clst = e.g. cellx, cellz, nr_cells, nr_part...

 ***************************************************************************
 ***************************************************************************/


void pcpix_find_cluster(int ich, int isect)
{
  counter j,k,l;
  int cellmax;
  pcpix_clust clst;
  pcpix_clust *clst_ptr = &clst;

  cellmax = 60;

  /* Reset the celltype vector */
  /* This vector will contain the cell types calculated with the ftype */
  /* function. All cells in a cluster have the same types except   */
  /* when the cluster lies on a gap in the pixel plane */
  for (l = 0; l < cellmax; l++) celltype[l] = -1;
  /*  printf("(find_cluster) search_mode = %d\n",search_mode); */

  while (sect[isect].padx[0] != 0)
    {
      l = k = 0;
      /* Grab the coordinates of the first cell in the cluster */
      clst_ptr->cellx[0] = sect[isect].padx[0];
      clst_ptr->cellz[0] = sect[isect].padz[0];

      /* Save the type of the first cell in the cluster for later use */
      celltype[0] = sect[isect].type[0];

      /* Mark the first cell to prevent double counting */
      flag[0] = 1;

      /* Sort out all neighbouring cells to the first cell */
      /* i.e. identify a cluster */
      while (l <= k)
	{
	  if(k == 0)
	    j = 1; /* When we are looking at the first cell */
	  else
	    j = 0;
	  while (sect[isect].padx[j] != 0 )
	    {
	      /* Look for unflagged neighbouring cells to the first cell */
	      if ((pcpix_adjacent(clst_ptr->cellx[l],clst_ptr->cellz[l],
		   sect[isect].padx[j],sect[isect].padz[j]) == 1) 
		 && (flag[j] == 0))
		{
		  /* Store the cell coordinates and the cell types */
		  clst_ptr->cellx[++k] = sect[isect].padx[j];
		  clst_ptr->cellz[k] = sect[isect].padz[j];
		  if (k < cellmax) celltype[k] = sect[isect].type[j];
		  /* tag the found cell for later removal */
	          flag[j] = 1;
		}
	      ++j;
	    }
	  ++l;
	}

        /* Increase the number of found clusters */
      /*
        clusters++;
      */
        /* Store the number of cells in the found cluster */
        clst_ptr->nr_cells = k;
        /* Remove all used cells to prevent double counting */
        pcpix_remove_flagged(j,isect);
        /* Estimate the number of particles that generated the cluster(s) */
        pcpix_nr_part_in_cluster(clst_ptr);
        /* Split a large enough cluster into subclusters */
        pcpix_split_cluster(clst_ptr);
        /* Calculate the weighed positions of the hits*/
	pcpix_calculate_pos(clst_ptr,ich,isect);
    }

  return;

}   /* end pcpix_find_cluster */


/**********************************************************************/

long mPadRec_(
  TABLE_HEAD_ST     *dPadRecPar_h,     DPADRECPAR_ST       *dPadRecPar ,
  TABLE_HEAD_ST       *dPadGeom_h,       DPADGEOM_ST         *dPadGeom ,
  TABLE_HEAD_ST       *dPadXRaw_h,        DPADRAW_ST         *dPadXRaw ,
  TABLE_HEAD_ST   *dPadXCluster_h,    DPADCLUSTER_ST     *dPadXCluster ,
  TABLE_HEAD_ST   *dPadXRawClus_h,    DPADRAWCLUS_ST     *dPadXRawClus ,
  TABLE_HEAD_ST      *pc23par_h,        DPAD23PAR_ST          *pc23par ) 

/***************************************************************************
 ***************************************************************************

 mPadRec
 -----------

 DESCRIPTION: Main pad reconstruction routine for PC (Lund version)

 AUTHOR/CONTACT: P. Nilsson, Lund University

 REVISIONS:
	Date		Author		Description

        13/2/98         P. Nilsson      Modified for Lund clustering
	9/9/97          J. Mitchell     Converted to STAF
	4/1/96          J. Mitchell     Converted to C for LUXOR
	1/25/96         K. Filimonov    Modified for CVS
	10/12/95        K. Filimonov    Modified for pixel option 
	4/06/95		J. Mitchell	Modified for tracking shell
 	1/12/95		M. Rosati	Original

 ***************************************************************************
 ***************************************************************************/

{

  int ich,is,ipx,ipz,iraw,iarm,ii[16],zoff;
  /* search_mode = 1: 2 fired diagonally adjacent cells = 1 cluster  */
  /* search_mode = 2: 2 fired diagonally adjacent cells = 2 clusters */

  /* Set up internal pointers */
  dPadXClusterRec_h = dPadXCluster_h;
  dPadRecParRec_h = dPadRecPar_h;
  dPadGeomRec_h = dPadGeom_h;
  dPadXRawRec_h = dPadXRaw_h;
  dPadXRawClusRec_h = dPadXRawClus_h;
  dPadXClusterRec = dPadXCluster;
  dPadGeomRec = dPadGeom;
  dPadRecParRec = dPadRecPar;
  dPadXRawRec = dPadXRaw;
  dPadXRawClusRec = dPadXRawClus;
  pc23parRec_h = pc23par_h;
  pc23parRec = pc23par;

  clusters = 0;
  split = 0;
/*??????????????????????
   fp = fopen("rec.dat","w");
??????????????????????*/

  /* iprintrec = 0; */
  if (dPadRecParRec[0].verbose != 0) iprintrec = 1;
  if (iprintrec > 0)
  {
      printf("\n");
      printf("*********************************************** \n");
      printf("*  E N T E R  PC  R E C O N S T R U C T I O N * \n");
      printf("*  Lund version 2.0c July 29th 1998 P.Nilsson * \n");
      printf("*********************************************** \n");
      printf("\n");
  }

  ich = dPadRecParRec[0].pcnumber;

  /* initialize the id map */

  /* loop over sectors */
  for (is = 0; is < dPadGeomRec[0].npdsec[ich]; is++) 
    {
      ii[is] = 0;
      for (iraw = 0; iraw<=10000; iraw++)
	{
	  padxx[is][iraw] = -2;
	  padzz[is][iraw] = -2;
	}
      /* loop over wires */
      for (ipx = 0; ipx <= dPadGeomRec[0].npdwr[ich]; ipx++) 
	{
	  sect[is].padx[ipx] = 0;
	  /* loop over pads along wires */
	  for (ipz = 0; ipz <= dPadGeomRec[0].npdz[ich]; ipz++) 
	    {
	      sect[is].padz[ipz] = 0;
	      pcrawid[is][ipx][ipz] = -1;
	    }
	}
    }

  /* set the fired pad map flag */
  for (iraw = 0; iraw < dPadXRawRec_h->nok; iraw++)
    {
      iarm = dPadXRawRec[iraw].arm;
      is = dPadXRawRec[iraw].sector;

      /* A unique sector number is used for both arms */
      if (iarm == 1) is += dPadGeomRec[0].sectperarm[ich];

      /* Restore the discontinuity in z */
      if (dPadXRawRec[iraw].side == 0)
	/* South side */
         zoff = 0;
      else
	/* North side */
	/* ???zoff = (dPadGeomRec[0].npdz[ich]-4)/2 + 1; */
         zoff = dPadGeomRec[0].npdz[ich]/2 + 1;

      ipx = dPadXRawRec[iraw].padx;
      ipz = dPadXRawRec[iraw].padz + zoff;
/*--  Now change numbering convention from pad system to pixel system */
/*-- by W. Xie  10/13/98  --*/
/*-- ??? */
      NumbPadtoPixel(ich+1,&ipx,&ipz);
/*--------------------------------------------------------------------*/
      padxx[is][ii[is]] = ipx;
      padzz[is][ii[is]++] = ipz;
      pcrawid[is][ipx][ipz] = dPadXRawRec[iraw].id;
      
      /*      printf("map: is = %d ipx = %d ipz = %d pcrawid = %d\n",is,ipx,ipz,pcrawid[is][ipx][ipz]); */
    }

  irawold = dPadXRawRec_h->nok;

  /* Main reconstruction loop */
  for (is = 0; is < dPadGeomRec[0].npdsec[ich]; is++) 
    {
      /* Reconstruct the fired cells using a pre-set value 
	 of threshold */
	  pcpix_pads2cells(ich,is);

      /* Search for clusters, assign the positions and fill out 
	 the data structures */
	  pcpix_find_cluster(ich,is);
    }

  /* Check counters on the output tables */
  if ((dPadXCluster_h->maxlen <= 0) ||
      (dPadXCluster_h->nok > dPadXCluster_h->maxlen))
    {
      printf("pcRec-F: Too many dPadXCluster rows: %ld\n",
	     dPadXCluster_h->nok);
      return STAFCV_BAD;
    }
  if ((dPadXRawClus_h->maxlen <= 0) ||
      (dPadXRawClus_h->nok > dPadXRawClus_h->maxlen))
    {
      printf("pcRec-F: Too many dPadXRawClus rows: %ld\n",
	     dPadXRawClus_h->nok);

/* Unsuccessful completion of analysis module... */
      return STAFCV_BAD;
    }

  /*
    printf("Chamber                       : PC%d\n",ich+1);
    printf("Number of unsplit clusters    : %d\n",clusters);
    printf("Number of split clusters      : %d\n",split);
    printf("Total number of split clusters: %d\n",dPadXClusterRec_h->nok);
  */

/* Successful completion of analysis module... */
    return STAFCV_OK;

}   /* end mPadRec module */
