//
// Original author: Charles F. Maguire
// Creation date: August 26, 2000
//
// Purpose: retrieve particle kinematics information at the CINT level
//

#include <PHTable.hh>
#include <root_ptrk.h>

long
GeaTrkStack(const Int_t& mcTrack, Int_t& idPart, Int_t& idParent,
	    Float_t& pTot, Float_t& rVertex, Float_t& zVertex,
            Float_t& pTheta, Float_t& pPhi, Int_t& nFile, Int_t& itParent,
            Int_t& itOrigin, Int_t& idOrigin)
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

  int itorigin;
  int idorigin;

  true_track = mcTrack;
  dio_ptrkorigin(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
                &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
                &itorigin, &idorigin, &idpart);

  dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
                &r_vertex, &z_vertex, &theta_vertex, &phi_vertex,
                &itparent, &idparent, &idpart);

  idPart = idpart;
  idParent = idparent;
  
  pTot = ptot;
  rVertex = r_vertex;
  zVertex = z_vertex;
  pTheta = ptheta;
  pPhi = pphi;

  nFile = nfile;
  itParent = itparent;
  itOrigin = itorigin;
  idOrigin = idorigin;

 return 0;

}
