/****************************************************************************
 *         Convert from mTofSetGeo.c to mTofSetGeoModule.C
 *                    [ STAF wrapper => pure PHOOL module ]
 **************************************************************************** 
 *:>--------------------------------------------------------------------
 *: FILE:       mTofSetGeo.c
 *: AUTHOR:     Akio Kiyomichi (Univ. of Tsukuba)
 *: HISTORY:    11/14/97  First Version
 *:             04/09/97  Change parameter name (par -> geopar)
 *:             06/11/00 A.Kiyomichi create pure PHOOL module
 *:<------------------------------------------------------------------*/

#include "utiPrototype.hh"
#include "Tof.hh"
#include "mTofSetGeoModule.h"
#include "dTofGeoParWrapper.h"
#include "dTofGeoWrapper.h"

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

PHBoolean
mTofSetGeoModule::event(PHCompositeNode *root) {
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root), *jjj;
  PHNode *n;
  TableNode_t *d;
  PHCompositeNode *parNode, *outNode;

  parNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    parNode = new PHCompositeNode("PAR");
    root->addNode(parNode);
  }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  // Extract the data from the dTofCalPar
  dTofGeoParWrapper *TofGeoParWrapper;
  outNode = parNode;
  n = iii.findFirst("PHIODataNode", "dTofGeoPar");
  if (!n) {
    cout << "ERROR:  'in' parameter dTofGeoPar not found" << endl;
     TofGeoParWrapper= new dTofGeoParWrapper("dTofGeoPar", 1);
     if (!TofGeoParWrapper) {
       return 1;
     }
     n = new TableNode_t(TofGeoParWrapper,"dTofGeoPar");
     outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGeoPar"));
  if (!d) {
    cerr << "  Error "<< endl; return 1;
  } else {
    TofGeoParWrapper = static_cast<dTofGeoParWrapper*>(d->getData());
    if (!TofGeoParWrapper) {cerr<<" Error"<< endl; return 1;}
  }
  delete jjj;
  DTOFGEOPAR_ST *geopar = TofGeoParWrapper->TableData();

  // Extract the data from the dTofGeo
  dTofGeoWrapper *TofGeoWrapper;
  outNode = parNode;
  jjj = new PHNodeIterator(outNode);
  if (!(d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofGeo")))) {
     TofGeoWrapper = new dTofGeoWrapper("dTofGeo", 1100);
     if (!TofGeoWrapper) {
       return 1;
     }
     d = new TableNode_t(TofGeoWrapper,"dTofGeo");
     outNode->addNode(d);
  } else {
    TofGeoWrapper = static_cast<dTofGeoWrapper*>(d->getData());
    if (!TofGeoWrapper){ cerr<<" Error"<< endl; exit(1);}
    TofGeoWrapper->SetMaxRowCount(1100);
  }
  delete jjj;
  nodes.append(d);
  DTOFGEO_ST *geo = TofGeoWrapper->TableData();

  //=======================================================================
  // Convert from mTofSetGeo.c
  //=======================================================================

  short i;
  short n_panel;      /* Panel in PISA */
  short sector;       /* Sector (0-1) */
  short side;         /* South=0,North=1 */
  short panel;        /* Panel  (0-3) */
  short slat;         /* Slat   (0-95) */
  float pos[3];       /* TOF slat position */
  float r;            /* TOF slat   r-position */
  float phi;          /* TOF slat phi-position */
  float pphi;
  
  float rslat;        /* Slat R-position from panel front surface */
  float slat_width;   /* Slat width */
  float scintz;       /* Slat local-Z position */
  float panel_r;      /* TOF Panel   R-position */
  float panel_phi;    /* TOF Panel phi-position */
  float panel_z;      /* TOF Panel   Z-position */
  rslat      = geopar->rslat;
  slat_width = geopar->slat_width;

  for(i=0;i<TOF_NSLAT;i++) {
    n_panel = i/TOF_NSLAT_PANEL;
    slat    = i%TOF_NSLAT_PANEL;
    switch(n_panel)
      {
      case  0: sector  =  1;  side  =  0;  panel  =  3;  break;
      case  1: sector  =  1;  side  =  0;  panel  =  2;  break;
      case  2: sector  =  1;  side  =  0;  panel  =  1;  break;
      case  3: sector  =  1;  side  =  0;  panel  =  0;  break;
      case  4: sector  =  1;  side  =  1;  panel  =  0;  break;
      case  5: sector  =  1;  side  =  1;  panel  =  1;  break;
      case  6: sector  =  1;  side  =  1;  panel  =  2;  break;
      case  7: sector  =  1;  side  =  1;  panel  =  3;  break;
      case  8: sector  =  0;  side  =  0;  panel  =  0;  break;
      case  9: sector  =  0;  side  =  1;  panel  =  0;  break;
      case 10: sector  = -1;  side  = -1;  panel  = -1;  break;
      default: sector  = -1;  side  = -1;  panel  = -1;
      }
    panel_r   = geopar->rpos[n_panel];
    panel_phi = geopar->phi[n_panel];
    panel_z   = geopar->zpos[n_panel];
    
    if((0<=slat)&&(slat<=15)){
      scintz = geopar->scintz[0];
      pos[2] = panel_z + slat_width*( -16 + 1.5 + (slat%16)*2 );
    } else if((16<=slat)&&(slat<=31)){
      scintz = geopar->scintz[2];
      pos[2] = panel_z + slat_width*( -16 + 0.5 + (slat%16)*2 );
    } else if((32<=slat)&&(slat<=47)){
      scintz = geopar->scintz[1];
      pos[2] = panel_z + slat_width*( -16 + 1.5 + (slat%16)*2 );
    } else if((48<=slat)&&(slat<=63)){
      scintz = - geopar->scintz[1];
      pos[2] = panel_z + slat_width*( -16 + 0.5 + (slat%16)*2 );
    } else if((64<=slat)&&(slat<=79)){
      scintz = - geopar->scintz[2];
      pos[2] = panel_z + slat_width*( -16 + 1.5 + (slat%16)*2 );
    } else if((80<=slat)&&(slat<=95)){
      scintz = - geopar->scintz[0];
      pos[2] = panel_z + slat_width*( -16 + 0.5 + (slat%16)*2 );
    } else {
      scintz = 0.;
      pos[2] = 0.;
    }
    r = sqrt((panel_r + rslat)*(panel_r + rslat) + scintz*scintz);
    pphi = atan2((double)scintz,(double)(panel_r+rslat));
    phi = panel_phi + 180.0 / M_PI * pphi;
    utiRPhitoXY( &r, &phi, &pos[0], &pos[1] );
    
    (geo+i)->slatid    = i;
    (geo+i)->sector    = sector;
    (geo+i)->side      = side;
    (geo+i)->panel     = panel;
    (geo+i)->slat      = slat;
    (geo+i)->pos[0]    = pos[0];
    (geo+i)->pos[1]    = pos[1];
    (geo+i)->pos[2]    = pos[2];
    (geo+i)->r         = r;
    (geo+i)->phi       = phi;
  }
  TofGeoWrapper->SetRowCount(TOF_NSLAT);

  return True;
}
