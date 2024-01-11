// Original author: Jeffery T. Mitchell

// Description: Transforms DST hits and tracks from one frame to
// another.

//INCLUDECHECKER: Removed this line: #include "phool.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHTable.hh"
//INCLUDECHECKER: Removed this line: #include "PHGeometry.h"
#include "cglTransformDST.hh"
#include "cglDetectorGeo.hh"
#include "dPadClusterWrapper.h"
#include "dDchHitWrapper.h"
#include "dDchTracksWrapper.h"
#include "TecOutV1.hh"
#include "dTofReconstructedWrapper.h"
#include "dEmcClusterLocalWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "BbcOut.h"
#include "VtxOut.h"
#include "ZdcOut.h"
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
#include "getClass.h"

#include <iostream>
//INCLUDECHECKER: Removed this line: #include <cmath>

template <class T>
inline T
sqr(const T& x)
{
  return x * x;
}

using namespace std;
using namespace PHGeometry;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<TObject> ObjectNode_t;
typedef PHIODataNode<BbcOut>  BbcOutNode_t;
typedef PHIODataNode<VtxOut>  VtxOutNode_t;
typedef PHIODataNode<ZdcOut>  ZdcOutNode_t;
typedef PHIODataNode<TecOutV1> TecOutNode_t;

// Constructors for cglTransformDST
cglTransformDST::cglTransformDST()
{
  Verbose = 0;
}

// Event transform method for cglTransformDST
void
cglTransformDST::transform(PHCompositeNode* topNode,
                           PHFrame& f1, PHFrame& f2,
                           short arm, short sector)
{
  PHNodeIterator topIter(topNode);

  TableNode_t *dEmcClusterLocalNode;
  dEmcClusterLocalWrapper *dEmcClusterLocal;
  TableNode_t *dEmcClusterLocalExtNode;
  dEmcClusterLocalExtWrapper *dEmcClusterLocalExt;

  // New output classes
  PHTypedNodeIterator<BbcOut> bbciter(topNode);
  BbcOutNode_t *BbcOutNode = bbciter.find("BbcOut");
  BbcOut* bbcout = 0;
  if (BbcOutNode)
    {
      bbcout = BbcOutNode->getData();
    }

  PHTypedNodeIterator<VtxOut> vtxiter(topNode);
  VtxOutNode_t *VtxOutNode = vtxiter.find("VtxOut");
  VtxOut* vtxout = 0;
  if (VtxOutNode)
    {
      vtxout = VtxOutNode->getData();
    }

  PHTypedNodeIterator<ZdcOut> zdciter(topNode);
  ZdcOutNode_t *ZdcOutNode = zdciter.find("ZdcOut");
  ZdcOut* zdcout = 0;
  if (ZdcOutNode)
    {
      zdcout = ZdcOutNode->getData();
    }

  dEmcClusterLocalNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode", "dEmcClusterLocal"));
  if (!dEmcClusterLocalNode)
    {
      cout << "cglTransformDST::transform (event)-F: dEmcClusterLocal not found.\n";
      return ;
    }
  dEmcClusterLocal = static_cast<dEmcClusterLocalWrapper*>(dEmcClusterLocalNode->getData());

  dEmcClusterLocalExtNode = static_cast<TableNode_t*>(topIter.findFirst("PHIODataNode", "dEmcClusterLocalExt"));
  if (!dEmcClusterLocalExtNode)
    {
      cout << "cglTransformDST::transform (event)-F: dEmcClusterLocalExt not found.\n";
      return ;
    }
  dEmcClusterLocalExt = static_cast<dEmcClusterLocalExtWrapper*>(dEmcClusterLocalExtNode->getData());

  // Data retrieval section complete

  transform(bbcout, f1, f2);
  transform(zdcout, f1, f2);
  transform(vtxout, f1, f2);
  transform(arm, sector, dEmcClusterLocal, f1, f2);
  transform(arm, sector, dEmcClusterLocalExt, f1, f2);
}

void
cglTransformDST::transformPhenix(PHCompositeNode* topNode)
{

  cglDetectorGeo *cglDetGeo = findNode::getClass<cglDetectorGeo>(topNode,"cglDetectorGeo");
  if (!cglDetGeo)
    {
      cerr << "cglHitAssociate::event-F: Detector geometries not found.\n";
      return ;
    }

  PHFrame newFrame;
  // Information gathered...
  PHPoint refPoint(0.0, 0.0, 0.0);
  PHVector refX(1.0, 0.0, 0.0);
  PHVector refY(0.0, 1.0, 0.0);
  PHFrame refFrame(refPoint, refX, refY);
  newFrame = cglDetGeo->get_phenixFrame();
  transform(topNode, newFrame, refFrame, 2, -1);
}

void
cglTransformDST::transformEast(PHCompositeNode* topNode)
{

  cglDetectorGeo *cglDetGeo = findNode::getClass<cglDetectorGeo>(topNode,"cglDetectorGeo");
  if (!cglDetGeo)
    {
      cerr << "cglHitAssociate::event-F: Detector geometries not found.\n";
      return ;
    }

  PHFrame oldFrame, newFrame;
  oldFrame = cglDetGeo->get_phenixFrame();
  newFrame = cglDetGeo->get_eastFrame();
  transform(topNode, newFrame, oldFrame, 0, -1);
}

void
cglTransformDST::transformWest(PHCompositeNode* topNode)
{


  cglDetectorGeo *cglDetGeo = findNode::getClass<cglDetectorGeo>(topNode,"cglDetectorGeo");
  if (!cglDetGeo)
    {
      cerr << "cglHitAssociate::event-F: Detector geometries not found.\n";
      return ;
    }

  PHFrame oldFrame, newFrame;
  oldFrame = cglDetGeo->get_phenixFrame();
  newFrame = cglDetGeo->get_westFrame();
  transform(topNode, newFrame, oldFrame, 1, -1);
}

void
cglTransformDST::transformRetracted(PHCompositeNode* topNode)
{

  // Transform both arms
  transformEast(topNode);
  transformWest(topNode);
}

void 
cglTransformDST::transform(long index, ZdcOut *zdcout,
				PHMatrix& m, PHVector& v) 
{

  double x,y,z;
  float ZdcVtxError, ZdcT0, ZdcT0Error;
  PHPoint newZdcPoint, oldZdcPoint;
  // Fetch the coordinates of the hit
  x = 0.0;
  y = 0.0;
  z = zdcout->get_Zvertex();
  ZdcVtxError = zdcout->get_ZvertexError();
  ZdcT0 = zdcout->get_TimeZero();
  ZdcT0Error = zdcout->get_TimeZeroError();

  // Place it into a PHPoint
  oldZdcPoint.setX(x);
  oldZdcPoint.setY(y);
  oldZdcPoint.setZ(z);

  // Transform the puppy
  newZdcPoint = transformPoint(m,v,oldZdcPoint);

  // Feed the results back into the DST
  x = newZdcPoint.getX();
  y = newZdcPoint.getY();
  z = newZdcPoint.getZ();
  zdcout->set_TimeVertex(ZdcT0,ZdcT0Error,z,ZdcVtxError);
}

void 
cglTransformDST::transform(long index, BbcOut *bbcout,
				PHMatrix& m, PHVector& v) 
{
  double x,y,z;
  float BbcVtxError, BbcT0, BbcT0Error;
  PHPoint newBbcPoint, oldBbcPoint;
  // Fetch the coordinates of the hit
  x = 0.0;
  y = 0.0;
  z = bbcout->get_VertexPoint();
  BbcVtxError = bbcout->get_dVertexPoint();
  BbcT0 = bbcout->get_TimeZero();
  BbcT0Error = bbcout->get_dTimeZero();

  // Place it into a PHPoint
  oldBbcPoint.setX(x);
  oldBbcPoint.setY(y);
  oldBbcPoint.setZ(z);

  // Transform the puppy
  newBbcPoint = transformPoint(m,v,oldBbcPoint);

  // Feed the results back into the DST
  x = newBbcPoint.getX();
  y = newBbcPoint.getY();
  z = newBbcPoint.getZ();
  bbcout->set_TimeVertex(BbcT0,BbcT0Error,z,BbcVtxError);
}

void 
cglTransformDST::transform(long index, VtxOut *vtxout,
			   PHMatrix& m, PHVector& v) 
{
  double x,y,z;

  if (vtxout->isPadVtx()) // check if pad vtx exists
    {
      float PadVtxError;
      PHPoint oldPadPoint, newPadPoint;
      // Fetch the coordinates of the hit
      x = 0.0;
      y = 0.0;
      z = vtxout->get_PadVertex();
      PadVtxError = vtxout->get_PadVertexError();
      // Place it into a PHPoint
      oldPadPoint.setX(x);
      oldPadPoint.setY(y);
      oldPadPoint.setZ(z);

      // Transform the puppy
      newPadPoint = transformPoint(m,v,oldPadPoint);

      // Feed the results back into the DST
      x = newPadPoint.getX();
      y = newPadPoint.getY();
      z = newPadPoint.getZ();
      vtxout->set_PadVtx(z,PadVtxError);
    }

  if (vtxout->isBbcVtx()) // check if bbc vtx exists
    {
      float BbcVtxError;
      PHPoint oldBbcPoint, newBbcPoint;
      // Fetch the coordinates of the hit
      x = 0.0;
      y = 0.0;
      z = vtxout->get_BbcVertex();
      BbcVtxError = vtxout->get_BbcVertexError();
      // Place it into a PHPoint
      oldBbcPoint.setX(x);
      oldBbcPoint.setY(y);
      oldBbcPoint.setZ(z);

      // Transform the puppy
      newBbcPoint = transformPoint(m,v,oldBbcPoint);

      // Feed the results back into the DST
      x = newBbcPoint.getX();
      y = newBbcPoint.getY();
      z = newBbcPoint.getZ();
      vtxout->set_BbcVtx(z,BbcVtxError);
    }

  if (vtxout->isZdcVtx())  // check if zdc vtx exists
    {
      float ZdcVtxError;
      PHPoint newZdcPoint, oldZdcPoint;
      // Fetch the coordinates of the hit
      x = 0.0;
      y = 0.0;
      z = vtxout->get_ZdcVertex();
      ZdcVtxError = vtxout->get_ZdcVertexError();
      // Place it into a PHPoint
      oldZdcPoint.setX(x);
      oldZdcPoint.setY(y);
      oldZdcPoint.setZ(z);

      // Transform the puppy
      newZdcPoint = transformPoint(m,v,oldZdcPoint);

      // Feed the results back into the DST
      x = newZdcPoint.getX();
      y = newZdcPoint.getY();
      z = newZdcPoint.getZ();
      vtxout->set_ZdcVtx(z,ZdcVtxError);
    }
}

void
cglTransformDST::transform(long index,
                           dPadClusterWrapper* dPadCluster,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z;
  PHPoint oldPoint, newPoint;

  // Fetch the coordinates of the hit
  x = dPadCluster->get_xyz(0, index);
  y = dPadCluster->get_xyz(1, index);
  z = dPadCluster->get_xyz(2, index);

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);

  // Transform the puppy
  newPoint = transformPoint(m, v, oldPoint);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  z = newPoint.getZ();
  dPadCluster->set_xyz(0, index, x);
  dPadCluster->set_xyz(1, index, y);
  dPadCluster->set_xyz(2, index, z);
}

void
cglTransformDST::transform(long index, dDchHitWrapper* dDchHit,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z, ex, ey, ez, vx, vy, vz;
  PHPoint oldPoint, newPoint, oldError, newError;
  PHVector oldVector, newVector;

  // Fetch the coordinates of the hit
  x = dDchHit->get_xyz(0, index);
  y = dDchHit->get_xyz(1, index);
  z = dDchHit->get_xyz(2, index);
  ex = dDchHit->get_err(0, index);
  ey = dDchHit->get_err(1, index);
  ez = dDchHit->get_err(2, index);
  vx = dDchHit->get_vxyz(0, index);
  vy = dDchHit->get_vxyz(1, index);
  vz = dDchHit->get_vxyz(2, index);

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);
  oldError.setX(x);
  oldError.setY(y);
  oldError.setZ(z);
  oldVector.setX(vx);
  oldVector.setY(vy);
  oldVector.setZ(vz);

  // Transform the puppy
  newPoint = transformPoint(m, v, oldPoint);
  newError = transformPoint(m, v, oldError);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  z = newPoint.getZ();
  ex = newError.getX();
  ey = newError.getY();
  ez = newError.getZ();
  vx = newVector.getX();
  vy = newVector.getY();
  vz = newVector.getZ();
  dDchHit->set_xyz(0, index, x);
  dDchHit->set_xyz(1, index, y);
  dDchHit->set_xyz(2, index, z);
  dDchHit->set_err(0, index, ex);
  dDchHit->set_err(1, index, ey);
  dDchHit->set_err(2, index, ez);
  dDchHit->set_vxyz(0, index, vx);
  dDchHit->set_vxyz(1, index, vy);
  dDchHit->set_vxyz(2, index, vz);
}

void
cglTransformDST::transform(long index,
                           dDchTracksWrapper* dDchTracks,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z, vx, vy, vz, ex, ey, ez, evx, evy, evz;
  double phi, alpha, beta;
  PHLine oldLine, newLine;
  PHPoint oldPoint, newPoint, oldError, newError;
  PHVector oldVector, newVector, oldErrVect, newErrVect;

  // Fetch the parameters of the line
  x = dDchTracks->get_point(0, index);
  y = dDchTracks->get_point(1, index);
  z = dDchTracks->get_point(2, index);
  vx = dDchTracks->get_direction(0, index);
  vy = dDchTracks->get_direction(1, index);
  vz = dDchTracks->get_direction(2, index);
  ex = dDchTracks->get_err_point(0, index);
  ey = dDchTracks->get_err_point(1, index);
  ez = dDchTracks->get_err_point(2, index);
  evx = dDchTracks->get_err_direction(0, index);
  evy = dDchTracks->get_err_direction(1, index);
  evz = dDchTracks->get_err_direction(2, index);

  // Place it into a PHLine
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);
  oldVector.setX(vx);
  oldVector.setY(vy);
  oldVector.setZ(vz);
  oldError.setX(ex);
  oldError.setY(ey);
  oldError.setZ(ez);
  oldErrVect.setX(evx);
  oldErrVect.setY(evy);
  oldErrVect.setZ(evz);

  oldLine.setBasepoint(oldPoint);
  oldLine.setDirection(oldVector);

  // Transform the puppy
  newLine = transformLine(m, v, oldLine);
  newError = transformPoint(m, v, oldError);

  // Feed the results back into the DST
  newPoint = newLine.getBasepoint();
  newVector = newLine.getDirection();
  x = newPoint.getX();
  y = newPoint.getY();
  z = newPoint.getZ();
  vx = newVector.getX();
  vy = newVector.getY();
  vz = newVector.getZ();
  ex = newError.getX();
  ey = newError.getY();
  ez = newError.getZ();
  evx = newErrVect.getX();
  evy = newErrVect.getY();
  evz = newErrVect.getZ();
  dDchTracks->set_point(0, index, x);
  dDchTracks->set_point(1, index, y);
  dDchTracks->set_point(2, index, z);
  dDchTracks->set_err_point(0, index, ex);
  dDchTracks->set_err_point(1, index, ey);
  dDchTracks->set_err_point(2, index, ez);
  dDchTracks->set_direction(0, index, vx);
  dDchTracks->set_direction(1, index, vy);
  dDchTracks->set_direction(2, index, vz);
  dDchTracks->set_err_direction(0, index, evx);
  dDchTracks->set_err_direction(1, index, evy);
  dDchTracks->set_err_direction(2, index, evz);

  // Alpha, beta, etc.
  phi = 0;
  x = dDchTracks->get_point(0, index);
  if (x != 0.0)
    {
      phi = atan2(dDchTracks->get_point(1, index),
                  dDchTracks->get_point(0, index));
    }  // x check
  vx = dDchTracks->get_direction(0, index);
  alpha = 0.0;
  beta = 0.0;
  if (vx != 0.0)
    {
      alpha = phi - atan2(dDchTracks->get_direction(1, index),
                          dDchTracks->get_direction(0, index));
    }  // vx check
  vz = dDchTracks->get_direction(2, index);
  if (vz != 0.0)
    {
      beta = atan2((double)sqrt(sqr(dDchTracks->get_direction(1, index)) +
                        sqr(dDchTracks->get_direction(0, index))),
                   (double)dDchTracks->get_direction(2, index));
    }  // vz check
  dDchTracks->set_alpha(index, alpha);
  dDchTracks->set_beta(index, beta);
  dDchTracks->set_betaNoVertex(index, beta);
  dDchTracks->set_zed(index, beta);
}

void
cglTransformDST::transform(long index, TecOutV1* tecout,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z;
  PHPoint oldPoint, newPoint;

  // Fetch the coordinates of the inner hit
  x = tecout->getTrackXin(index);
  y = tecout->getTrackYin(index);
  z = 0.0;

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);

  // Transform the puppy
  newPoint = transformPoint(m, v, oldPoint);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  tecout->setTrackXin((int)index,(float)x);
  tecout->setTrackYin((int)index,(float)y);

  // Fetch the coordinates of the outer hit
  x = tecout->getTrackXout(index);
  y = tecout->getTrackYout(index);
  z = 0.0;

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  tecout->setTrackXout((int)index,(float)x);
  tecout->setTrackYout((int)index,(float)y);
}

void
cglTransformDST::transform(long index,
                           dTofReconstructedWrapper* dTofReconstructed,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z;
  PHPoint oldPoint, newPoint;

  // Fetch the coordinates of the hit
  x = dTofReconstructed->get_xtof(0, index);
  y = dTofReconstructed->get_xtof(1, index);
  z = dTofReconstructed->get_xtof(2, index);

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);

  // Transform the puppy
  newPoint = transformPoint(m, v, oldPoint);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  z = newPoint.getZ();
  dTofReconstructed->set_xtof(0, index, x);
  dTofReconstructed->set_xtof(1, index, y);
  dTofReconstructed->set_xtof(2, index, z);
}

void
cglTransformDST::transform(long index,
                           dEmcClusterLocalWrapper* dEmcCluster,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z;
  PHPoint oldPoint, newPoint;

  // Fetch the coordinates of the hit
  x = dEmcCluster->get_xyz(0, index);
  y = dEmcCluster->get_xyz(1, index);
  z = dEmcCluster->get_xyz(2, index);

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);

  // Transform the puppy
  newPoint = transformPoint(m, v, oldPoint);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  z = newPoint.getZ();
  dEmcCluster->set_xyz(0, index, x);
  dEmcCluster->set_xyz(1, index, y);
  dEmcCluster->set_xyz(2, index, z);
}

void
cglTransformDST::transform(long index,
                           dEmcClusterLocalExtWrapper* dEmcClusterExt,
                           PHMatrix& m, PHVector& v)
{
  double x, y, z;
  PHPoint oldPoint, newPoint;

  // Fetch the coordinates of the hit
  x = dEmcClusterExt->get_xyz(0, index);
  y = dEmcClusterExt->get_xyz(1, index);
  z = dEmcClusterExt->get_xyz(2, index);

  // Place it into a PHPoint
  oldPoint.setX(x);
  oldPoint.setY(y);
  oldPoint.setZ(z);

  // Transform the puppy
  newPoint = transformPoint(m, v, oldPoint);

  // Feed the results back into the DST
  x = newPoint.getX();
  y = newPoint.getY();
  z = newPoint.getZ();
  dEmcClusterExt->set_xyz(0, index, x);
  dEmcClusterExt->set_xyz(1, index, y);
  dEmcClusterExt->set_xyz(2, index, z);
}

void
cglTransformDST::transform(long index, BbcOut* BbcOut,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, BbcOut, m, v);
}

void
cglTransformDST::transform(long index, ZdcOut* ZdcOut,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, ZdcOut, m, v);
}

void
cglTransformDST::transform(long index, VtxOut* VtxOut,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, VtxOut, m, v);
}

void
cglTransformDST::transform(long index,
                           dPadClusterWrapper* dPadCluster,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, dPadCluster, m, v);
}

void
cglTransformDST::transform(long index, dDchHitWrapper* dDchHit,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, dDchHit, m, v);
}

void
cglTransformDST::transform(long index,
                           dDchTracksWrapper* dDchTracks,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, dDchTracks, m, v);
}

void
cglTransformDST::transform(long index, TecOutV1* tecout,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, tecout, m, v);
}

void
cglTransformDST::transform(long index,
                           dTofReconstructedWrapper* dTofReconstructed,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, dTofReconstructed, m, v);
}

void
cglTransformDST::transform(long index,
                           dEmcClusterLocalWrapper* dEmcCluster,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, dEmcCluster, m, v);
}

void
cglTransformDST::transform(long index,
                           dEmcClusterLocalExtWrapper* dEmcClusterExt,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  transform(index, dEmcClusterExt, m, v);
}

void 
cglTransformDST::transform(BbcOut* BbcOut,
			   PHFrame& f1, PHFrame& f2) 
{

  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1,f2,m,v);
  size_t i = 1;
  transform(i,BbcOut,m,v);
}

void 
cglTransformDST::transform(ZdcOut* ZdcOut,
			   PHFrame& f1, PHFrame& f2) 
{

  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1,f2,m,v);
  size_t i = 1;
  transform(i,ZdcOut,m,v);
}

void 
cglTransformDST::transform(VtxOut* VtxOut,
			   PHFrame& f1, PHFrame& f2) 
{

  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1,f2,m,v);
  size_t i = 1;
  transform(i,VtxOut,m,v);
}

void
cglTransformDST::transform(dPadClusterWrapper* dPadCluster,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dPadCluster->RowCount(); i++)
    {
      transform(i, dPadCluster, m, v);
    }
}

void
cglTransformDST::transform(dDchHitWrapper* dDchHit,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dDchHit->RowCount(); i++)
    {
      transform(i, dDchHit, m, v);
    }
}

void
cglTransformDST::transform(dDchTracksWrapper* dDchTracks,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dDchTracks->RowCount(); i++)
    {
      transform(i, dDchTracks, m, v);
    }
}

void
cglTransformDST::transform(TecOutV1* tecout,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (int i = 0; i < tecout->getNTracks(); i++)
    {
      transform(i, tecout, m, v);
    }
}

void
cglTransformDST::transform(dTofReconstructedWrapper* dTofReconstructed,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dTofReconstructed->RowCount(); i++)
    {
      transform(i, dTofReconstructed, m, v);
    }
}

void
cglTransformDST::transform(dEmcClusterLocalWrapper* dEmcCluster,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dEmcCluster->RowCount(); i++)
    {
      transform(i, dEmcCluster, m, v);
    }
}

void
cglTransformDST::transform(dEmcClusterLocalExtWrapper* dEmcClusterExt,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dEmcClusterExt->RowCount(); i++)
    {
      transform(i, dEmcClusterExt, m, v);
    }
}

void
cglTransformDST::transform(short arm, short sector,
                           dPadClusterWrapper* dPadCluster,
                           PHFrame& f1, PHFrame& f2)
{
  short iarm, isect;
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);

  for (size_t i = 0; i < dPadCluster->RowCount(); i++)
    {
      // Fetch the arm and sector number of the cluster
      iarm = (dPadCluster->get_xyz(0, i) > 0) ? 1 : 0;
      isect = dPadCluster->get_sector(i);
      if ((arm == iarm && sector == isect)
	  || (arm == iarm && sector < 0)
	  || (arm == 2 && sector < 0)
	  || (arm == 2 && sector == isect))
	{
	  transform(i, dPadCluster, m, v);
	}
    }
}

void
cglTransformDST::transform(short arm, dDchHitWrapper* dDchHit,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dDchHit->RowCount(); i++)
    {
      if ((arm == dDchHit->get_arm(i)) || (arm == 2))
        {
          transform(i, dDchHit, m, v);
        }
    }
}

void
cglTransformDST::transform(short arm,
                           dDchTracksWrapper* dDchTracks,
                           PHFrame& f1, PHFrame& f2)
{
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dDchTracks->RowCount(); i++)
    {
      if ((arm ==  dDchTracks->get_arm(i)) || (arm == 2))
	{
          transform(i, dDchTracks, m, v);
        }
    }
}

void
cglTransformDST::transform(short arm, short sector,
                           TecOutV1* tecout,
                           PHFrame& f1, PHFrame& f2)
{
  short iarm, isect;
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (int i = 0; i < tecout->getNTracks(); i++)
    {
      iarm = 0;
      // Until it's implemented - set sector to -1
      sector = -1; 
      cout << PHWHERE << " this piece of code does not work correctly " << endl;
      cout << "isect is not initialized and compared to sector in the following if" << endl;
      cout << "I have no idea how this is supposed to work so please" << endl;
      cout << "fix this and remove the exit or send mail to off-l" << endl;
	exit(1); 
      if ((arm == iarm && sector == isect)
	  || (arm == iarm && sector < 0)
	  || (arm == 2 && sector < 0))
        {
          transform(i, tecout, m, v);
        }
    }
}

void
cglTransformDST::transform(short arm, short sector,
                           dTofReconstructedWrapper* dTofReconstructed,
                           PHFrame& f1, PHFrame& f2)
{
  short iarm, isect;
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dTofReconstructed->RowCount(); i++)
    {
      iarm = (dTofReconstructed->get_xtof(0, i) > 0.0) ? 1 : 0;
      isect = dTofReconstructed->get_panel(i);
      if ((arm == iarm && sector == isect)
	  || (arm == iarm && sector < 0)
	  || (arm == 2 && sector < 0)
	  || (arm == 2 && sector == isect))
        {
          transform(i, dTofReconstructed, m, v);
        }
    }
}

void
cglTransformDST::transform(short arm, short sector,
                           dEmcClusterLocalWrapper* dEmcCluster,
                           PHFrame& f1, PHFrame& f2)
{
  short iarm, isect;
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dEmcCluster->RowCount(); i++)
    {
      iarm = (dEmcCluster->get_xyz(0, i) > 0.0) ? 1 : 0;
      isect = dEmcCluster->get_sector(i);
      if ((arm == iarm && sector == isect)
	  || (arm == iarm && sector < 0)
	  || (arm == 2 && sector < 0)
	  || (arm == 2 && sector == isect))
        {
          transform(i, dEmcCluster, m, v);
        }
    }
}

void
cglTransformDST::transform(short arm, short sector,
                           dEmcClusterLocalExtWrapper* dEmcClusterExt,
                           PHFrame& f1, PHFrame& f2)
{
  short iarm, isect;
  PHMatrix m;
  PHVector v;

  frames2MatrixAndVector(f1, f2, m, v);
  for (size_t i = 0; i < dEmcClusterExt->RowCount(); i++)
    {
      iarm = (dEmcClusterExt->get_xyz(0, i) > 0.0) ? 1 : 0;
      isect = dEmcClusterExt->get_sector(i);
      if ((arm == iarm && sector == isect)
	  || (arm == iarm && sector < 0)
	  || (arm == 2 && sector < 0)
	  || (arm == 2 && sector == isect))
        {
          transform(i, dEmcClusterExt, m, v);
        }
    }
}

void
cglTransformDST::set_Verbose(short setverb)
{
  Verbose = setverb;
}

short
cglTransformDST::get_Verbose()
{
  return Verbose;
}
