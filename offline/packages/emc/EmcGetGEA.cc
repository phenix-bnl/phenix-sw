#include "EmcPISAHit.h"
#include "EmcPISAPara.h"
#include "dEmcGeaHitWrapper.h"
#include "emcparWrapper.h"
#include "root_ptrk.h"

#include "PHIODataNode.h"

#include <cstdlib>
#include <cassert>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

long
EmcGetGEA(PHCompositeNode* topNode)
{

  int true_track;
  int nfile;
  int error;
  float ptot;
  float ptheta;
  float pphi;
  float r_vertex;
  float z_vertex;
  float theta_vertex;
  float phi_vertex;
  int itparent;
  int idparent;
  int idpart;


  static Int_t iEMC = 0;
  PHNodeIterator iter(topNode), *j;
  PHNodeIterator *jpar;
  PHCompositeNode* geaNode;
  PHCompositeNode* parNode;
  PHNode *n2;                   // EmcGetDCM also defined n1 pointer
  TableNode_t *d;

  // Find the GEA "directory" if it exists; otherwise exit on error
  n2 = iter.findFirst("PHCompositeNode", "GEA");
  if (!n2) {
    cerr << "\n EmcGetGEA <E> unable to find GEA node; program exiting " << endl;
    exit(1);
  }
  else {
    geaNode = static_cast<PHCompositeNode*>(n2);
  }
  
  if(iEMC == 0) {
    iEMC = 1;

    PHNode *n3;
    // Find the PAR "directory" if it exists; otherwise exit on error.
    n3 = iter.findFirst("PHCompositeNode", "PAR");
    if (!n3) {
      cerr << "\n EmcGetGEA <E> unable to find PAR node; program exiting " << endl;
      exit(1);
    }
    else {
      parNode = static_cast<PHCompositeNode*>(n3);
    }
    
    EmcPISAPara *parevent = EmcPISAPara::GetEmcParaEvt();
    Int_t emcParaRows = EmcPISAPara::GetEmcParaCount();    // should be 8 walls

    // Instantiate the PAR table for this subsystem, and attach it to
    // the PAR "directory" node.
    emcparWrapper* wpar;
    jpar = new PHNodeIterator(parNode);

    d = static_cast<TableNode_t*>(jpar->findFirst("PHIODataNode","emcpar"));
    if (!d) {
      cerr << "\n EmcGetGEA<E>: unable to find emcpar STAF Table; " ;
      cerr << " program is exiting at this point " << endl;
      exit(1);
    }
    else {
      wpar = static_cast<emcparWrapper*>(d->getData());
      if (!wpar) {
	cerr << "\n EmcGetGEA<E>: unable to find emcparWrapper pointer; " ;
	cerr << " program is exiting at this point " << endl;
	exit(1);
      }
      wpar->SetMaxRowCount(emcParaRows);
    }

    EMCPAR_ST* emcpar = wpar->TableData();
    Float_t upar[80];
    for(int i=0; i<emcParaRows; i++) {

      parevent[i].GetEmcPar(upar);

      emcpar[i].emc_walls        = upar[0]; /* number of walls from PHNX.PAR */
      emcpar[i].emc_opt          = upar[1]; /* EMCal option from PHNX.PAR */
      emcpar[i].iwall            = upar[2]; /* wall number (1-4) */
      emcpar[i].itype            = upar[3]; /* detector type (Sh-K,PbGl */
      emcpar[i].angle            = upar[4]; /* phi angle of wall center */
      emcpar[i].rpos             = upar[5]; /* radial position of wall center */
      emcpar[i].zc_start         = upar[6]; /* center of first cell, z coor. */
      emcpar[i].yc_start         = upar[7]; /* center of first cell, y coor. */
      emcpar[i].lsiz             = upar[8]; /* long. size of a cell */
      emcpar[i].tsiz             = upar[9]; /* transverse size of a cell */
      emcpar[i].no_modz          = upar[10]; /* No. of cells in z in a supermod. */
      emcpar[i].no_mody          = upar[11]; /* No. of cells in y in a supermod. */
      emcpar[i].no_smodz         = upar[12]; /* No. of supermods. in z / wall */
      emcpar[i].no_smody         = upar[13]; /* No. of supermods. in y / wall */
			       
      emcpar[i].scint_emc_med    = upar[17]; /* Shish-Kebab scint. medium */
			       
      emcpar[i].emc_debug        = upar[21]; /* */
      emcpar[i].gcuts[0]         = upar[22]; /* */
      emcpar[i].gcuts[1]         = upar[23]; /* */
      emcpar[i].gcuts[2]         = upar[24]; /* */
      emcpar[i].gcuts[3]         = upar[25]; /* */
      emcpar[i].gcuts[4]         = upar[26]; /* */
			       
      emcpar[i].emc_r_min_sc     = upar[29]; /* bitp lower limit, PbSc */
      emcpar[i].emc_r_max_sc     = upar[30]; /* bitp upper limit, PbSc */
      emcpar[i].emc_r_step       = upar[31]; /* bitp stepsize, PbSc */
      emcpar[i].emc_z_min        = upar[32]; /* bitp lower limit */
      emcpar[i].emc_z_max        = upar[33]; /* bitp upper limit */
      emcpar[i].emc_z_step       = upar[34]; /* bitp stepsize */
      emcpar[i].emc_x_min_sc     = upar[35]; /* bitp lower limit, PbSc */
      emcpar[i].emc_x_max_sc     = upar[36]; /* bitp upper limit, PbSc */
      emcpar[i].emc_x_step       = upar[37]; /* bitp stepsize, PbSc */

      emcpar[i].emc_dele_max_sc  = upar[39]; /* bitp dE upper limit, PbSc */
      emcpar[i].emc_dele_step_sc = upar[40]; /* bitp dE upper limit, PbSc */
      emcpar[i].emc_tof_min      = upar[41]; /* bitp lower limit */
      emcpar[i].emc_tof_max      = upar[42]; /* bitp upper limit */
      emcpar[i].emc_tof_step     = upar[43]; /* bitp stepsize */

      emcpar[i].emc_ind1_max_sc  = upar[49]; /* bitp tower ind. */
      emcpar[i].emc_ind2_max_sc  = upar[50]; /* bitp tower ind. */
      emcpar[i].emc_iwall_max    = upar[51]; /* */
      emcpar[i].emc_itype_max    = upar[52]; /* */
      emcpar[i].emc_i1_max       = upar[53]; /* */

      emcpar[i].emc_itrack_max   = upar[59]; /* */
      emcpar[i].emc_spart_max    = upar[60]; /* */
      emcpar[i].emc_ncycle_max   = upar[61]; /* */

      emcpar[i].emc_cutgam       = upar[64]; /* */
      emcpar[i].emc_cutele       = upar[65]; /* */
      emcpar[i].emc_cutneu       = upar[66]; /* */
      emcpar[i].emc_cuthad       = upar[67]; /* */
      emcpar[i].emc_cutmuo       = upar[68]; /* */

      emcpar[i].array[0]         = upar[14];
      emcpar[i].array[1]         = upar[15];
      emcpar[i].array[2]         = upar[16];

      emcpar[i].array[3]         = upar[18];
      emcpar[i].array[4]         = upar[19];
      emcpar[i].array[5]         = upar[20];

      emcpar[i].array[6]         = upar[27];
      emcpar[i].array[7]         = upar[28];

      emcpar[i].array[8]         = upar[38];

      emcpar[i].array[9]         = upar[44];
      emcpar[i].array[10]        = upar[45];
      emcpar[i].array[11]        = upar[46];
      emcpar[i].array[12]        = upar[47];

      emcpar[i].array[13]        = upar[48];

      emcpar[i].array[14]        = upar[54];
      emcpar[i].array[15]        = upar[55];
      emcpar[i].array[16]        = upar[56];
      emcpar[i].array[17]        = upar[57];
      emcpar[i].array[18]        = upar[58];

      emcpar[i].array[19]        = upar[62];
      emcpar[i].array[20]        = upar[63];

      emcpar[i].array[21]        = upar[69];
      emcpar[i].array[22]        = upar[70];
      emcpar[i].array[23]        = upar[71];
      emcpar[i].array[24]        = upar[72];
      emcpar[i].array[25]        = upar[73];
      emcpar[i].array[26]        = upar[74];
      emcpar[i].array[27]        = upar[75];
      emcpar[i].array[28]        = upar[76];
      emcpar[i].array[29]        = upar[77];
      emcpar[i].array[30]        = upar[78];
      emcpar[i].array[31]        = upar[79];

    }
    wpar->SetRowCount(emcParaRows);

    delete jpar;
  }

  EmcPISAHit *event = EmcPISAHit::GetEmcHitEvt();
  Int_t emcRows = EmcPISAHit::GetEmcCount();    // variable number of rows

  // Instantiate the GEA table for this subsystem, and attach it to
  // the GEA "directory" node.
  dEmcGeaHitWrapper* w;
  j = new PHNodeIterator(geaNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","dEmcGeaHit"));
  if (!d) {
    cerr << "\n EmcGetGEA<E>: unable to find dEmcGeaHit STAF Table; " ;
    cerr << " program is exiting at this point " << endl;
    exit(1);
  }
  else {
    w = static_cast<dEmcGeaHitWrapper*>(d->getData());
    if (!w) {
      cerr << "\n EmcGetGEA<E>: unable to find dEmcGeaHitWrapper pointer; " ;
      cerr << " program is exiting at this point " << endl;
      exit(1);
    }
    w->SetMaxRowCount(emcRows);
  }
  delete j;

  DEMCGEAHIT_ST* dEmcGeaHit = w->TableData();

  Int_t nPbGl = 0;

  for(int i=0; i<emcRows; i++) {
    dEmcGeaHit[i].id         = i;                      // serial ID number
    dEmcGeaHit[i].type       = event[i].GetItype();   
    dEmcGeaHit[i].sector     = event[i].GetWall() ; 
    if(dEmcGeaHit[i].type != 1 && dEmcGeaHit[i].type != 2) {
       cerr <<"\n Bad type " << dEmcGeaHit[i].type << " i = " << i <<"\n";
       exit(1);
    }
    if(dEmcGeaHit[i].sector < 7) {
      dEmcGeaHit[i].smodind    = event[i].GetIndex1();
      dEmcGeaHit[i].towerind   = event[i].GetIndex2();
    }
    if(dEmcGeaHit[i].sector > 6) {
      nPbGl++;
      dEmcGeaHit[i].smodind    = event[i].GetIndex1();
      dEmcGeaHit[i].towerind   = event[i].GetIndex2();
    }
    dEmcGeaHit[i].deltae     = event[i].GetDele();  
    dEmcGeaHit[i].xyz[0]     = event[i].GetPosx();
    dEmcGeaHit[i].xyz[1]     = event[i].GetPosy();
    dEmcGeaHit[i].xyz[2]     = event[i].GetPosz();
    dEmcGeaHit[i].tof        = event[i].GetTof();
    dEmcGeaHit[i].numed      = event[i].GetNumed();
    dEmcGeaHit[i].itrack     = event[i].GetNtrack();
    dEmcGeaHit[i].isubevt    = event[i].GetIsubevent();   
    dEmcGeaHit[i].nfile      = event[i].GetNfile();

    Int_t mctrack = event[i].GetMctrack();
    true_track = mctrack;
    int status = dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
			       &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
			       &itparent, &idparent, &idpart);
    assert( status == 0 );
    dEmcGeaHit[i].partid     = idpart;
    dEmcGeaHit[i].true_track = true_track;
  

  }  // loop over emcRows

  w->SetRowCount(emcRows);

 // cout << "\n Exiting EmcGetGEA: emcRows = " << emcRows << ", nPbGl = " << nPbGl << endl;

 return 0;
}

