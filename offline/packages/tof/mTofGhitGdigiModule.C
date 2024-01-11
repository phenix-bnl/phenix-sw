/****************************************************************************
 *        Convert from mTofGhitGdigi.c to mTofGhitGdigiModule.C
 *                      [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofGhitGdigi.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    10/22/97 A.Kiyomichi  First Version
 *:             11/20/97 A.Kiyomichi  Add fkin, geo, ghitgdigi table
 *:             12/31/97 A.Kiyomichi  Change from itrack to mctrack
 *:             04/09/98 A.Kiyomichi  Modify using dio-function
 *:             04/10/98 A.Kiyomichi  Add perfpar table
 *:             10/28/98 A.Kiyomichi  Add path length calculation
 *:             10/28/99 A.Kiyomichi  Modify the tof_pathlength()
 *:             06/10/00 A.Kiyomichi  create pure PHOOL module
 *:             12/21/00 A.Kiyomichi  set evaNode (dTofGdigi dTofGhitGdigi)
 *:             11/30/01 A.Kiyomichi  delete dTofGeo
 *:             05/24/02 A.Kiyomichi  Hit selection: average of all steps
 *:<------------------------------------------------------------------*/

#include "Tof.hh"
#include "mTofGhitGdigiModule.h"
#include "tofghitWrapper.h"
#include "dTofGdigiWrapper.h"
#include "dTofGhitGdigiWrapper.h"
#include "dio_trk.hh"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include "gsl/gsl_math.h"

#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

// forward declarations of static functions
static float tof_pathlength(short *partl, float *tof, float *ptot);

static short trkcheck(int &mctrack1, int &mctrack2);

/* forward declarations of TOF functions */
extern "C" short tofghit_slatid(TOFGHIT_ST             *ghit );

PHBoolean
mTofGhitGdigiModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  //PHTable *w;
  PHCompositeNode *parNode, *tofNode, *geaNode, *evaNode, *outNode;

  parNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    parNode = new PHCompositeNode("PAR");
    root->addNode(parNode);
  }

  tofNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "TOF"));
  if (!tofNode) {
    tofNode = new PHCompositeNode("TOF");
    root->addNode(tofNode);
  }
  
  geaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode) {
    geaNode = new PHCompositeNode("GEA");
    root->addNode(geaNode);
  }

  evaNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "EVA"));
  if (!evaNode) {
    evaNode = new PHCompositeNode("EVA");
    root->addNode(evaNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the tofghit
  tofghitWrapper *tofghitWrap;
  outNode = geaNode;
  n = iii.findFirst("PHIODataNode", "tofghit");
  if (!n) {
    cout << "ERROR:  'in' parameter tofghit not found" << endl;
     tofghitWrap = new tofghitWrapper("tofghit", 3000);
     if (!tofghitWrap) {
       return 1;
     }
     n = new TableNode_t(tofghitWrap,"tofghit");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","tofghit"));
  if (!d) {
    cerr << "  Error  in tofghit"<< endl; return 1;
  } else {
    tofghitWrap = static_cast<tofghitWrapper*>(d->getData());
    if (!tofghitWrap) {cerr<<" Error in tofghit"<< endl; return 1;}
  }
  delete jjj;
  TOFGHIT_ST *ghit = tofghitWrap->TableData();

  // Extract the data from the dTofGdigi
  dTofGdigiWrapper *TofGdigiWrapper;
  outNode = evaNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGdigi")))) {
     TofGdigiWrapper = new dTofGdigiWrapper("dTofGdigi", 300);
     if (!TofGdigiWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGdigiWrapper,"dTofGdigi");
     outNode->addNode(d);
  } else {
    TofGdigiWrapper = static_cast<dTofGdigiWrapper*>(d->getData());
    if (!TofGdigiWrapper){ cerr<<" Error in dTofGdigi"<< endl; exit(1);}
    TofGdigiWrapper->SetMaxRowCount(300);
  }
  delete jjj;
  nodes.append(d);
  DTOFGDIGI_ST *digi = TofGdigiWrapper->TableData();

  // Extract the data from the dTofGhitGdigi
  dTofGhitGdigiWrapper *TofGhitGdigiWrapper;
  outNode = evaNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGhitGdigi")))) {
     TofGhitGdigiWrapper = new dTofGhitGdigiWrapper("dTofGhitGdigi", 3000);
     if (!TofGhitGdigiWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGhitGdigiWrapper,"dTofGhitGdigi");
     outNode->addNode(d);
  } else {
    TofGhitGdigiWrapper = static_cast<dTofGhitGdigiWrapper*>(d->getData());
    if (!TofGhitGdigiWrapper){ cerr<<" Error in dTofGhitGdigi"<< endl; exit(1);}
    TofGhitGdigiWrapper->SetMaxRowCount(3000);
  }
  delete jjj;
  nodes.append(d);
  DTOFGHITGDIGI_ST *ghitgdigi = TofGhitGdigiWrapper->TableData();

  //=======================================================================
  // Convert from mTofGhitGdigi.c
  //=======================================================================
  /* Locals */
  long   i,j,k;
  long   nghit_total;  /* Total number of rows in ghit table */
  long   ihit;         /* Loop counter:  current row of ghit table */

  long   ndigi_total = 0;
  long   idigi;
  short  nslathit_num[TOF_NSLAT];
  short  nslathit;
  short  hits_seq;

//short  n_subvolume;
  short  n_panel;
  short  n_column;
  short  n_pslat;
  short  nslat_seq;
  short  slatid;
  short  partl;
  int    mctrack;
  double pos_m[3];
  float  pos_hit_slat;
  float  p_m[3];
  float  tof;
  float  dele;
  //float  eloss = 0.;
  float  theta;
  float  phi;
  /* Add Oct-16-98  --AK */
  float  path = 0;
  //float  r_vert = 0;

  /* Use in dio_ptrkstack & dio_ptrkorigin */
  int   true_track;             /* Globally unique track ID */
  int   nfile;
  int   error;
  float ptot;                   /* output momentum */
  float ptheta;                 /* output theta direction */
  float pphi;                   /* output phi direction */
  float r_vertex;               /* output vertex r position */
  float z_vertex;               /* output vertex z position */
  float theta_vertex;           /* output vertex theta position */
  float phi_vertex;             /* output vertex phi position */
  //int   itparent;               /* output parent track number */
  //int   idparent;               /* output parent ID number */
  int   itorigin;               /* output origin track number */
  int   idorigin;               /* output origin ID number */
  int   idpart;                 /* output particle ID number */
  int   itdigiorigin[1000];

  // # of steps in one track May-24-02  --AK
  float nstep;
  int   nstep_track[1000];
  memset(nstep_track,0,sizeof(nstep_track));

  nghit_total = tofghitWrap->RowCount();


  if (tofghitWrap->RowCount() > TofGhitGdigiWrapper->MaxRowCount()) {
    cout<<"tofghitWrap->RowCount() "              <<tofghitWrap->RowCount()
        <<" > TofGhitGdigiWrapper->MaxRowCount() "<<TofGhitGdigiWrapper->MaxRowCount()
        <<endl;
    return False;
  }

  for(i=0; i<TOF_NSLAT; i++){
    /* Initialize */
    nslathit_num[i] = 0;
  }

  for(ihit=0; ihit<nghit_total; ihit++){
//  n_subvolume    = ghit[ihit].subvol;
    n_panel        = ghit[ihit].panel;
    n_column       = ghit[ihit].column;
    n_pslat        = ghit[ihit].pslat;
    nslat_seq      = ghit[ihit].slat_seq;
    partl          = ghit[ihit].partl;
    mctrack        = ghit[ihit].mctrack;
    pos_m[0]       = ghit[ihit].pos_m[0];
    pos_m[1]       = ghit[ihit].pos_m[1];
    pos_m[2]       = ghit[ihit].pos_m[2];
    pos_hit_slat   = ghit[ihit].pos_hit_slat;
    theta          = (float)acos(pos_m[2]/sqrt(pos_m[0]*pos_m[0]
		    + pos_m[1]*pos_m[1] + pos_m[2]*pos_m[2]))*180.0/ M_PI;
    phi            = (float)atan2(pos_m[1],pos_m[0])*180.0/ M_PI;
    if(phi<0.)phi += 360.;
    p_m[0]         = ghit[ihit].p_m[0];
    p_m[1]         = ghit[ihit].p_m[1];
    p_m[2]         = ghit[ihit].p_m[2];
    tof            = ghit[ihit].tof;
    dele           = ghit[ihit].dele;
    nslathit       = 1;
    hits_seq       = 1;
    slatid         = tofghit_slatid(ghit+ihit);      /* add 17/Nov/97 */

    if((partl <= 0)||(mctrack <= 0)){
      /* No data in tofghit */
      ghitgdigi[ihit].ghitid  = ihit;
      ghitgdigi[ihit].slatid  = -1;
      ghitgdigi[ihit].gdigiid = -1;
    } else {
      /* Relational table between tofghit and dTofGdigi */
      ghitgdigi[ihit].ghitid   = ihit;
      ghitgdigi[ihit].slatid   = slatid;

      true_track = mctrack;
      /* add 9/Apr/98 */
      dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		     &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		     &itorigin, &idorigin, &idpart);
      idigi = -1;
      for(i = 0; i < ndigi_total; i++){
	/* Search new hit */
	if(itdigiorigin[i] == itorigin){
	  if(trkcheck(digi[i].mctrack, mctrack) == 1 ){ 
	    idigi = i;
	    break;
	  }
	}
	/* 
	if(digi[i].slat_seq == nslat_seq){
	} 
	*/ 
	/* Need more consideration  31-Dec-97 AK */
      }

      if(idigi >= 0){
	nstep_track[idigi]++;
	nstep = nstep_track[idigi] + 1;

	ghitgdigi[ihit].gdigiid  = digi[idigi].id;
	digi[idigi].eloss       += dele;

	digi[idigi].tof = 
	  (digi[idigi].tof*nstep_track[idigi] + tof)/nstep;
	digi[idigi].pos_m[0] = 
	  (digi[idigi].pos_m[0]*nstep_track[idigi] + pos_m[0])/nstep;
	digi[idigi].pos_m[1] = 
	  (digi[idigi].pos_m[1]*nstep_track[idigi] + pos_m[1])/nstep;
	digi[idigi].pos_m[2] = 
	  (digi[idigi].pos_m[2]*nstep_track[idigi] + pos_m[2])/nstep;
	digi[idigi].pos_hit_slat = 
	  (digi[idigi].pos_hit_slat*nstep_track[idigi] + pos_hit_slat)/nstep;
	digi[idigi].theta = 
	  (digi[idigi].theta*nstep_track[idigi] + theta)/nstep;
	digi[idigi].phi = 
	  (digi[idigi].phi*nstep_track[idigi] + phi)/nstep;
	ptot = sqrt(digi[idigi].p_m[0]*digi[idigi].p_m[0] + 
		    digi[idigi].p_m[1]*digi[idigi].p_m[1] + 
		    digi[idigi].p_m[2]*digi[idigi].p_m[2]);
	path = tof_pathlength(&partl, &tof, &ptot);
	digi[idigi].path = 
	  (digi[idigi].path*nstep_track[idigi] + path)/nstep;

      } else {
	ghitgdigi[ihit].gdigiid        = ndigi_total;
	digi[ndigi_total].id           = ndigi_total;
	digi[ndigi_total].slatid       = slatid;
	digi[ndigi_total].panel        = n_panel;
	digi[ndigi_total].column       = n_column;
	digi[ndigi_total].pslat        = n_pslat;
	digi[ndigi_total].slat_seq     = nslat_seq;
	digi[ndigi_total].mctrack      = mctrack;
	digi[ndigi_total].partl        = partl;
	digi[ndigi_total].tof          = tof;
	digi[ndigi_total].eloss        = dele;
	digi[ndigi_total].pos_m[0]     = pos_m[0];
	digi[ndigi_total].pos_m[1]     = pos_m[1];
	digi[ndigi_total].pos_m[2]     = pos_m[2];
	digi[ndigi_total].pos_hit_slat = pos_hit_slat;
	digi[ndigi_total].theta        = theta;
	digi[ndigi_total].phi          = phi;
	digi[ndigi_total].p_m[0]       = p_m[0];
	digi[ndigi_total].p_m[1]       = p_m[1];
	digi[ndigi_total].p_m[2]       = p_m[2];
	ptot = sqrt(p_m[0]*p_m[0] + p_m[1]*p_m[1] + p_m[2]*p_m[2]);
	digi[ndigi_total].path         = tof_pathlength(&partl, &tof, &ptot);
	digi[ndigi_total].nslathit     = nslathit;
	digi[ndigi_total].hits_seq     = hits_seq;
	itdigiorigin[ndigi_total]      = itorigin;

	ndigi_total++;

	if (ndigi_total > (int) TofGdigiWrapper->MaxRowCount()) {
	  cout<<"ndigi_total "<<ndigi_total<<" > TofGdigiWrapper->MaxRowCount() "<<TofGdigiWrapper->MaxRowCount()<<endl;
	  return False;
	}

      } /* if(idigi >= 0){ -- } else { */
    } /* if((partl <= 0)||(mctrack <= 0)){ -- } else { */
  }
  /* Search multiple hits in one slat */
  for(i = 0; i < ndigi_total ; i++){
    k = digi[i].slatid;
    nslathit_num[k]++;
  }
  for(i = 0; i < ndigi_total ; i++){
    k = digi[i].slatid;
    digi[i].nslathit  = nslathit_num[k];
  }
  /* Search multiple hits sequency */
  for(i = 0; i < ndigi_total - 1; i++){
    for(j = i + 1; j < ndigi_total; j++){
      if(digi[i].slatid == digi[j].slatid){
	if(digi[i].tof > digi[j].tof){
	  digi[i].hits_seq++;
	} else {
	  digi[j].hits_seq++;
	}
      }
    }
  }
  TofGhitGdigiWrapper->SetRowCount(nghit_total);
  TofGdigiWrapper->SetRowCount(ndigi_total);

/*:>--------------------------------------------------------------------
  cout<<"Number of rows in         dTofGdigi = "<<ndigi_total<<endl;
  for(i = 0; i < ndigi_total; i++){
    cout<<i<<"\t: slat= "<<digi[i].slat_seq
        <<"\t mctrack= "<<digi[i].mctrack<<"\t y= "<<digi[i].pos_m[1]
        <<"\tpid= "     <<digi[i].partl  <<"\t";
    cout<<"tof= "       <<digi[i].tof    <<"\t eloss= "  <<digi[i].eloss
        <<"  nslathit= "<<nslathit       <<"  hits_seq= "<<digi[i].hits_seq
        <<endl;
  }
**:>------------------------------------------------------------------*/

  return True;
}

/* PRIVATE functions (implementation) */
static 
short 
trkcheck(int &mctrack1 ,
	 int &mctrack2)
{
  /* Check the two hits are same track or not  */
  /*--------------------------------------------------------*
   *     IN: mctrack1  - Monte Carlo (GEANT) track number 1 *
   *         mctrack2  - Monte Carlo (GEANT) track number 2 *
   *    OUT: this function - "-1" is not, "1" is same track *
   *--------------------------------------------------------*/

  //long  i, j, k;
  //short flag1,flag2;

  int   true_track1;            /* Globally unique track ID [1] */
  int   true_track2;            /* Globally unique track ID [2] */
  int   nfile;
  int   error;
  float ptot;                   /* output momentum */
  float ptheta;                 /* output theta direction */
  float pphi;                   /* output phi direction */
  float r_vertex;               /* output vertex r position */
  float z_vertex;               /* output vertex z position */
  float theta_vertex;           /* output vertex theta position */
  float phi_vertex;             /* output vertex phi position */
  int   itparent;               /* output parent track number */
  int   idparent;               /* output parent ID number */
  int   idpart;                 /* output particle ID number */

  true_track1 = mctrack1;
  true_track2 = mctrack2;

  if(true_track1 == true_track2){
    return(1);
  } else {
    do{
      dio_ptrkstack(&true_track1, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		    &itparent, &idparent, &idpart);
      if(r_vertex < 503.){
	break;
      } else {
	true_track1 = itparent;
      }
    }
    while(itparent > 0);
    do{
      dio_ptrkstack(&true_track2, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		    &itparent, &idparent, &idpart);
      if(r_vertex < 503.){
	break;
      } else {
	true_track2 = itparent;
      }
    }
    while(itparent > 0);
    if(true_track1 == true_track2){
      mctrack1 = true_track1;
      mctrack2 = true_track2;
      return(1);
    } else {
      return(-1);
    }
  }
  return(-1);
}

static float tof_pathlength(short *partl, float *tof, float *ptot)
{
  /* Calculation of flight path length */
  /*--------------------------------------------------------*
   *     IN:   partl  - Particle ID                         *
   *             tof  - Time of flight                      *
   *            ptot  - Total momentum                      *
   *    OUT: [itself] - Flight path length                  *
   *--------------------------------------------------------*/ 
  float  path;
  double m2, p2;
  float  math = -1;
  float  beta;
  float  ftof;
  float  p;
  short  pid;
  const  float clight = 29.9792458; /* [cm/ns] */

  pid  = *partl;
  ftof = *tof;
  p    = *ptot;

  if(pid ==  1) math = 0.0000e+0;   /*  GAMMA  */
  if(pid ==  2) math = 0.5110e-3;   /*  POSITORON  */
  if(pid ==  3) math = 0.5110e-3;   /*  ELECTRON  */
  if(pid ==  4) math = 0.0000e+0;   /*  NEWTRINO  */
  if(pid ==  5) math = 0.1057e+0;   /*  MUON +  */
  if(pid ==  6) math = 0.1057e+0;   /*  MUON -  */
  if(pid ==  7) math = 0.1350e+0;   /*  PION 0  */
  if(pid ==  8) math = 0.1396e+0;   /*  PION +  */
  if(pid ==  9) math = 0.1396e+0;   /*  PION -  */
  if(pid == 10) math = 0.4977e+0;   /*  KAON 0 LONG  */
  if(pid == 11) math = 0.4936e+0;   /*  KAON +  */
  if(pid == 12) math = 0.4936e+0;   /*  KAON -  */
  if(pid == 13) math = 0.9396e+0;   /*  NEWTRON  */
  if(pid == 14) math = 0.9383e+0;   /*  PROTON  */
  if(pid == 15) math = 0.9383e+0;   /*  ANTIPROTON  */
  if(pid == 16) math = 0.4977e+0;   /*  KAON 0 SHORT  */
  if(pid == 17) math = 0.5488e+0;   /*  ETA  */
  if(pid == 18) math = 0.1116e+1;   /*  LAMBDA  */
  if(pid == 19) math = 0.1189e+1;   /*  SIGMA +  */
  if(pid == 20) math = 0.1193e+1;   /*  SIGMA 0  */
  if(pid == 21) math = 0.1197e+1;   /*  SIGMA -  */
  if(pid == 22) math = 0.1315e+1;   /*  XI 0  */
  if(pid == 23) math = 0.1321e+1;   /*  XI -  */
  if(pid == 24) math = 0.1672e+1;   /*  OMEGA -  */
  if(pid == 25) math = 0.9396e+0;   /*  ANTINEUTRON  */
  if(pid == 26) math = 0.1116e+1;   /*  ANTILAMBDA  */
  if(pid == 27) math = 0.1189e+1;   /*  ANTISIGMA -  */
  if(pid == 28) math = 0.1193e+1;   /*  ANTISIGMA 0  */
  if(pid == 29) math = 0.1197e+1;   /*  ANTISIGMA +  */
  if(pid == 30) math = 0.1315e+1;   /*  ANTIXI 0  */
  if(pid == 31) math = 0.1321e+1;   /*  ANTIXI +  */
  if(pid == 32) math = 0.1671e+1;   /*  ANTIOMEGA +  */
  if(pid > 32)  math = 0.1396e+0;   /*  PION +  */
     
  m2 = math*math;
  p2 = p*p;
  beta = sqrt(p2/(p2 + m2));
  path = ftof*clight*beta;

  return(path);
}
