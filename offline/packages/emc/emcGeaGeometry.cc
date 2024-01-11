//////////////////////////////////////////////////////////////////////////////////
//
// emcal physical geometry description, that is embedded in the output of
// pisa. it is used ony internally by the emcal response code, and not meant
// to be written out to disc.
//
// reincarnation of the old dEmcGeomtery staf table. 
//
//////////////////////////////////////////////////////////////////////////////////

#include <cmath>
#include <cassert>

#include <phool.h>
#include <PHCompositeNode.h>
#include <emcGeaParams.h>
#include <emcGeaGeometry.h>


ClassImp(emc_geometry_t);
ClassImp(emcGeaGeometry);

const unsigned int emcGeaGeometry::wallmax;
const unsigned int emcGeaGeometry::ymax;
const unsigned int emcGeaGeometry::zmax;


emcGeaGeometry::emcGeaGeometry(){}



int emcGeaGeometry::init(emcGeaParams * params){
  assert(params->size() == wallmax);
  
 
  (*this).resize( params->size() );
  for(unsigned int wall = 0; wall < params->size(); wall++ ){
    emc_gea_params_t * param = &(*params)[wall];

    (*this)[wall].resize( (unsigned int)(param->nmody * param->nsmody) );
    for(unsigned int y = 0; y < param->nmody * param->nsmody; y++){

      (*this)[wall][y].resize( (unsigned int)(param->nmodz * param->nsmodz) );
      for(unsigned int z = 0; z < param->nmodz * param->nsmodz; z++){
	emc_geometry_t * geom = &(*this)[wall][y][z];

	// Apparent z position is the center of the first cell + ...
	float appz = param->zc_start + z  * param->tsize;

	// Apparent y position is the center of the first cell + ...
	// times the COSine of the wall inclination
	float appy = param->yc_start   +   y * param->tsize * fabs(param->cosangle);  

	// Apparent x position is RPOS * COSine of wall inclination
	// - YPOSisiton in the coo.system of the wall * SINe of wall inclination
	float appx = param->rpos * fabs(param->cosangle)  - 
	  ( - ( param->nmody * param->nsmody  - 1.0) * param->tsize / 2.0 + y * param->tsize) * param->sinangle;
	if(wall > 3) appx = -appx;

	// Add carriage translation
	appx += param->translate[0];

	geom->x = appx;
	geom->y = appy;
	geom->z = appz;

	
	// angles
	double work1 = sqrt(appx * appx + appy * appy);
	double work2 = appz;
	double work0 = appx;
	
	if(work1 > 0.0){
	  float measthe = ToDegree * atan2(work1, work2);
	  float measphi = ToDegree * acos(work0/work1);
	  if(appy < 0.0) measphi = 360.0 - measphi;

	  geom->theta = measthe;
	  geom->phi = measphi;
	}
	

	// distances
	geom->distance = sqrt( appx*appx + appy*appy + appz*appz );
	geom->flashtime = geom->distance / 30.0;


	// Write unit vector corresponding to the axis of the tower
	geom->ex = param->cosangle;
	geom->ey = param->sinangle;
	geom->ez = 0.0;




	// indices: this is always tricky...
	geom->arm = wall / 4;
	if(geom->arm == 0 ) geom->sector = wall;
	else                geom->sector = 7 - wall;  // Sector numbering from bottom up in East Arm
	
	
	/*  Numbering reversed in West Arm */
	if(geom->arm == 0) geom->iz = 71 - z;
	else               geom->iz = z;
	geom->iy = y;
	
	geom->swkey = 100000 * geom->arm  +  10000 * geom->sector  +
	  100 * geom->iy  +  geom->iz;
	
	
	/*      Get theta, phi within the sector (local) coordinate system) */
	/*      But buddy, this is c, indices are shifted by one */
	float distr = param->rpos;
	float distz = (z - param->nmodz * param->nsmodz / 2.0) * param->tsize;
	float disty = (y - param->nmody * param->nsmody / 2.0) * param->tsize;
	
	geom->sectheta = ToDegree * atan2(distz , distr);
	geom->secphi = ToDegree * atan2(disty, distr);
	

      }
    }
  }

  return 0;
}



