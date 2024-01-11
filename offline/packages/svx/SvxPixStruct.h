// ====================
// FILE: SvxPixStruct.h
// ====================
#ifndef __SVXPIXSTRUCT_HH_
#define __SVXPIXSTRUCT_HH_

#include <vector>
#include <algorithm>

#include <SvxPixStructure.h>
#include <SvxRawhit.h>

#include <cmath>
#include <cstdlib>

//class SvxRawHit;

/**
 * @brief  The implementation class (with C++ template) for a pixel structure.
 *
 * This class holds the parameters of (stri)pixel sensor, 
 * such as the size and the number of pixels in x/z directions per sensor.
 * The actual values are specified in SvxPixStruct.C.
 *
 * @date  Created by V. L. Rykov on 02-Mar-2004
 */
template <int TYPE>
class SvxPixStruct : public SvxPixStructure
{
 private:
  enum {SVXNFIREDPIXELS = 20};             //! Initial length of pixelList
  enum {SVXNRAWHITINIT  = 20};             //! Initial length of raw list

 public:
  // Constructor(s) & Destructor
  // """""""""""""""""""""""""""
  SvxPixStruct() {
    nFiredPixels = 0;
    pixelList  = new SvxFiredPixelList(SVXNFIREDPIXELS);

//     nRawptr = 0;
//     rawPtrLength = ( TYPE < 10 ) ? 1 :SVXNRAWHITINIT;
//     rawPtr       = new SvxRawhit *[rawPtrLength];
    int len = ( TYPE < 10 ) ? 1 :SVXNRAWHITINIT;
    _rawPtr.reserve(len);

    /*std::cout << "SvxPixStruct<" << TYPE
	      << "> object created: nFiredPixels = "
	      << nFiredPixels << std::endl;*/
  }

  virtual ~SvxPixStruct() {
    delete pixelList;
//     for ( unsigned int i = 0; i < rawPtrLength; i++ ) {
//       rawPtr[i] = 0;
//     }
//     delete [] rawPtr;
    for (unsigned int i=0; i<_rawPtr.size(); i++) delete _rawPtr[i];
    /*std::cout << "SvxPixStruct<" << TYPE <<
      "> object destroyed" << std::endl;*/
  }

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset   ()       {
    rawlistReset();
    nFiredPixels = 0;
    if ( pixelList->getListLength() > SVXNFIREDPIXELS )
      pixelList->setListLength(SVXNFIREDPIXELS);
  }

  virtual int isValid  () const { return ( nFiredPixels > 0 ) ? 1 : 0 ; }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: SvxPixStruct<" << TYPE << "> object: "
       << "nFiredPixels = " << nFiredPixels << std::endl;
  }

  // Handling parameters
  // """""""""""""""""""
  // Get parameters
  virtual int    get_nXpitch    () const { return nXpitch;    }
  virtual double get_xPitch     () const { return xPitch;     }
  virtual double get_xhalfWidth () const { return xhalfWidth; }
  virtual int    get_nZpitch    () const { return nZpitch;    }
  virtual double get_zPitch     () const { return zPitch;     }
  virtual double get_zhalfWidth () const { return zhalfWidth; }

  // Pixel internal local position
  virtual double getXpixel(const int ix)          const
    { return xPitch*(static_cast<double>(ix)+0.5) - xhalfWidth; }
  virtual double getZpixel(const int iz)          const
    { return zPitch*(static_cast<double>(iz)+0.5) - zhalfWidth; }

  // Print parameters
  virtual void printPar()  const
    {
      std::cout << "SvxPixStruct<" << TYPE << "> object" << std::endl;
      std::cout << "   nXpitch = " << nXpitch << std::endl;
      std::cout << "   xPitch  = " << xPitch << " cm" << std::endl;
      std::cout << "   xWidth  = " << 2.*xhalfWidth << " cm" << std::endl;
      std::cout << "   nZpitch = " << nZpitch << std::endl;
      std::cout << "   zPitch  = " << zPitch << " cm" << std::endl;
      std::cout << "   zWidth  = " << 2.*zhalfWidth << " cm" << std::endl;
    }

  // Manipulations of rawPtr list
  // """"""""""""""""""""""""""""
  //virtual unsigned int get_nRawptr      () const { return nRawptr       ;}
  virtual unsigned int get_nRawptr() const { return _rawPtr.size(); }
  //virtual unsigned int getRawListLength () const { return rawPtrLength; ;}
  virtual unsigned int getRawListLength() const { return _rawPtr.capacity(); ;}
  virtual SvxRawhit*   get_rawPtr       (const unsigned int ind) const {
    //return ( ind < rawPtrLength ) ? rawPtr[ind] : 0;
    try { return _rawPtr.at(ind); } catch (...) {}
    return 0;
  }

  virtual SvxRawhit* addRawhit(SvxRawhit* rawhit) {
    _rawPtr.push_back(rawhit);
    return rawhit;
//     if ( nRawptr >= rawPtrLength ) {
// 	unsigned int oldLength = rawPtrLength;
// 	rawPtrLength = nRawptr + 1 + (int) sqrt(3.*double(nRawptr+1));
// 	SvxRawhit **tmpList = rawPtr;
// 	rawPtr              = new SvxRawhit *[rawPtrLength];
// 	for ( unsigned int i = 0; i < oldLength; i++ ) {
// 	  rawPtr[i]  = tmpList[i];
// 	  tmpList[i] = 0;
// 	}
// 	delete [] tmpList;
//     }
//     return rawPtr[nRawptr++] = rawhit;
  }
  /**
   * Remove from rawPtr[] a pointer that is equal to "rawhit"
   * @param[in] rawhit  a SvxRawhit pointer to be removed.
   * @return    true if a pointer is removed.
   */
  virtual int removeRawhit(SvxRawhit* rawhit) {
    std::vector<SvxRawhit*>::iterator it = std::find(_rawPtr.begin(),_rawPtr.end(),rawhit);
    if ( it != _rawPtr.end() )
      {
	_rawPtr.erase(it);
	return 1;
      }
    return 0;
//     int bl_found = false;
//     for (unsigned int i = 0; i < nRawptr; i++) {
//       if (! bl_found && rawPtr[i] == rawhit) {
// 	bl_found = true;
// 	nRawptr--;
//       }
//       if (bl_found && i < nRawptr) rawPtr[i] = rawPtr[i+1];
//     }
//     return bl_found;
  }

  virtual void rawlistReset() {
    for(unsigned int i=0; i<_rawPtr.size(); i++) delete _rawPtr[i];
    _rawPtr.clear();
    _rawPtr.reserve(SVXNRAWHITINIT);
    return;
//     nRawptr = 0;
//     if ( rawPtrLength <= SVXNRAWHITINIT ) return;
    
//     SvxRawhit **tmpList = rawPtr;
//     for ( unsigned int i = 0; i < rawPtrLength; i++ ) {
//       tmpList[i] = 0;
//     }
//     delete [] tmpList;
//     rawPtrLength = SVXNRAWHITINIT;
//     rawPtr       = new SvxRawhit *[rawPtrLength];
  }

  // Handling pixelList
  // """"""""""""""""""
  // Get data

  virtual int get_xrow    (const unsigned int i) const
    { return ( i < nFiredPixels ) ? pixelList->getPixel(i)->ix     : -1 ;}
  virtual int get_zcolumn (const unsigned int j) const
    { return ( j < nFiredPixels ) ? pixelList->getPixel(j)->iz     : -1 ;}
  virtual int get_charge  (const unsigned int k) const
    { return ( k < nFiredPixels ) ? pixelList->getPixel(k)->charge : -1 ;}

  // Find fired pixel(s)
  // -------------------
  virtual int firePixels(double x, double z, double chrg)
    {
      nFiredPixels = 0;
      if ( fabs(x) >= xhalfWidth || fabs(z) >= zhalfWidth ) return 0;

      //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
      SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
      if ( (pixel->charge = (int) floor(siIonPot*chrg+0.5)) ) {
	pixel->ix = (int) floor((x+xhalfWidth)/xPitch);
	pixel->iz = (int) floor((z+zhalfWidth)/zPitch);
	++nFiredPixels;
      }
      return nFiredPixels;
    }

  //// 
  //// Charge sharing in both x and z directions (2009 Jan, by Manabu Togawa)
  //// 
  virtual int firePixelsAndShareChargeXZ(double xin,double zin,double xout,double zout,double chrg)
    {
      nFiredPixels = 0;
//      if ( fabs(xin) >= xhalfWidth || fabs(zin) >= zhalfWidth ) {
//         std::cout << "firePixelsAndShareCharge: coordinates outside bounds, " 
//                   << " xin = " << xin << " (half width = " << xhalfWidth 
//                   << "), zin = " << zin << " (half width = " << zhalfWidth 
//                   << ")" << std::endl; 
//         return 0;
//      }

      int xroparity = 0;
      int zroparity = 0;
      double x1 = xin +xhalfWidth;
      double x2 = xout+xhalfWidth;
      double z1 = zin +zhalfWidth;
      double z2 = zout+zhalfWidth;      

      int ix1 = (int) floor(x1/xPitch);
      int ix2 = (int) floor(x2/xPitch);
      int iz1 = (int) floor(z1/zPitch);
      int iz2 = (int) floor(z2/zPitch);

      if( (ix2-ix1) < 0 ) xroparity = 1;
      if( (iz2-iz1) < 0 ) zroparity = 1;

      if( (x2-x1) < 0 ){
	x1 = xout+xhalfWidth;
	x2 = xin +xhalfWidth;
	ix1 = (int) floor(x1/xPitch);
	ix2 = (int) floor(x2/xPitch);
      }
      if( (z2-z1) < 0 ){
	z1 = zout+zhalfWidth;
	z2 = zin +zhalfWidth; 
	iz1 = (int) floor(z1/zPitch);
	iz2 = (int) floor(z2/zPitch);
      }

      if (ix1 <  0      ) ix1 = 0;
      if (ix2 >= nXpitch) ix2 = nXpitch - 1;
      if (iz1 <  0      ) iz1 = 0;
      if (iz2 >= nZpitch) iz2 = nZpitch - 1;
      if (ix1 > ix2 || iz1 > iz2) return 0; // coordinates outside bounds


      int nfiredx = (ix2-ix1)+1;
      int nfiredz = (iz2-iz1)+1;

      //--printf("x1=%f, x2=%f, z1=%f, z2=%f xroparity = %d, ix1=%d, ix2=%d, iz1=%d, iz2=%d nx=%d nz=%d\n"
      //--         ,x1,x2,z1,z2,xroparity, ix1, ix2, iz1, iz2, nfiredx, nfiredz);
      //printf("   xwid: %f, zwid: %f\n", xhalfWidth, zhalfWidth);

      //      double totalcharge = siIonPot*chrg;
      double tracklength = sqrt( (x2-x1)*(x2-x1) + (z2-z1)*(z2-z1) );

//      std::cout << "firePixelsAndShareCharge: ============================= " 
//                << xin << " " << xout << " " << zin << " " << zout << " " 
//                << chrg << " " << tracklength << " " << siIonPot << std::endl;
//      std::cout << "firePixelsAndShareCharge: nfired = " 
//                << nfiredx << " " << nfiredz << std::endl;

      // z = slop * x + offset for track
      double slope  = 0;
      if( (x2-x1)>0 ) slope = ( z2-z1 ) / ( x2-x1 );
      double offset = ( (z2+z1) - slope * (x2+x1) ) / 2;

//       printf("nx = %d   nz = %d\n",nfiredx,nfiredz);
//       printf("ix1 = %d   iz1 = %d\n",ix1,iz1);
//       printf("tracklength= %f\n",tracklength);

      if( nfiredx>1 || nfiredz>1 ){  // for charge sharing
	for(int nx=0; nx<nfiredx; nx++){
	  for(int nz=0; nz<nfiredz; nz++){
	    
	    int EdgeHitL = 0; double xEdgeL = 0; double zEdgeL = 0; 
	    int EdgeHitR = 0; double xEdgeR = 0; double zEdgeR = 0; 
	    int EdgeHitT = 0; double xEdgeT = 0; double zEdgeT = 0; 
	    int EdgeHitB = 0; double xEdgeB = 0; double zEdgeB = 0; 
	    
	    // Left
	    zEdgeL = (iz1+nz) * zPitch;
	    if( slope>0 ) xEdgeL = ( zEdgeL - offset ) / slope;
	    // Right
	    zEdgeR = (iz1+nz+1 ) * zPitch;
	    if( slope>0 ) xEdgeR = ( zEdgeR - offset ) / slope;
	    // Top
	    xEdgeT = (ix1+nx+1 ) * xPitch;
	    zEdgeT = slope * xEdgeT + offset;
	    // Bottom
	    xEdgeB = (ix1+nx) * xPitch;
	    zEdgeB = slope * xEdgeB + offset;
	    
	    if( (ix1+nx)*xPitch <= xEdgeL && xEdgeL < (ix1+nx+1)*xPitch && x1<=xEdgeL ) EdgeHitL = 1;
	    if( (ix1+nx)*xPitch <= xEdgeR && xEdgeR < (ix1+nx+1)*xPitch && xEdgeR<=x2 ) EdgeHitR = 1;
	    if( (iz1+nz)*zPitch <= zEdgeT && zEdgeT < (iz1+nz+1)*zPitch && zEdgeT<=z2 ) EdgeHitT = 1;
	    if( (iz1+nz)*zPitch <= zEdgeB && zEdgeB < (iz1+nz+1)*zPitch && z1<=zEdgeB ) EdgeHitB = 1;

	    //--std::cout<<"range x:"<<(ix1+nx)*xPitch<<"-"<<(ix1+nx+1)*xPitch
            //--         <<    ", z:"<<(iz1+nz)*zPitch<<"-"<<(iz1+nz+1)*zPitch<<std::endl;
	    //--std::cout<<" xedgeL,R="<<xEdgeL<<" "<<xEdgeR<<" zedgeT,B="<<zEdgeT<<" "<<zEdgeB<<std::endl;
	    
	    //	    int TotalEdgeHit = EdgeHitL + EdgeHitR + EdgeHitT + EdgeHitB;
	    
	    int StartP = 0;
	    int EndP = 0;
	    if( nx == 0 && nz == 0 ){
	      StartP = 1;
	      if( slope == 0 ) EdgeHitB = 0;
	    }
	    if( nx == (nfiredx-1) && nz == (nfiredz-1) ){
	      EndP = 1;
	      if( slope == 0 ) EdgeHitT = 0;
	    }
 	    //--    printf("S E L R T B = %d %d %d %d %d %d\n",
 	    //--	   StartP,EndP,EdgeHitL,EdgeHitR,EdgeHitT,EdgeHitB);
 	    //--    printf("xEdgeT xEdgeB = %f %f\n",xEdgeT,xEdgeB);
 	    //--    printf("zEdge1-!-zEdge2 = %f-%f-%f\n",
 	    //--	   (iz1+nz)*zPitch,zEdgeT,(iz1+nz+1)*zPitch);

	    double dx = 0;
	    dx = StartP*EdgeHitT*
	      ( (x1-xEdgeT)*(x1-xEdgeT)+(z1-zEdgeT)*(z1-zEdgeT) ) +
	      StartP*EdgeHitR*
	      ( (x1-xEdgeR)*(x1-xEdgeR)+(z1-zEdgeR)*(z1-zEdgeR) ) +
	      EndP*EdgeHitB*
	      ( (x2-xEdgeB)*(x2-xEdgeB)+(z2-zEdgeB)*(z2-zEdgeB) ) +
	      EndP*EdgeHitL*
	      ( (x2-xEdgeL)*(x2-xEdgeL)+(z2-zEdgeL)*(z2-zEdgeL) ) +
	      EdgeHitL*EdgeHitR*
	      ( (xEdgeL-xEdgeR)*(xEdgeL-xEdgeR)+(zEdgeL-zEdgeR)*(zEdgeL-zEdgeR) ) +
	      EdgeHitL*EdgeHitT*
	      ( (xEdgeL-xEdgeT)*(xEdgeL-xEdgeT)+(zEdgeL-zEdgeT)*(zEdgeL-zEdgeT) ) +
	      EdgeHitL*EdgeHitB*
	      ( (xEdgeL-xEdgeB)*(xEdgeL-xEdgeB)+(zEdgeL-zEdgeB)*(zEdgeL-zEdgeB) ) +
	      EdgeHitR*EdgeHitT*
	      ( (xEdgeR-xEdgeT)*(xEdgeR-xEdgeT)+(zEdgeR-zEdgeT)*(zEdgeR-zEdgeT) ) +
	      EdgeHitR*EdgeHitB*
	      ( (xEdgeR-xEdgeB)*(xEdgeR-xEdgeB)+(zEdgeR-zEdgeB)*(zEdgeR-zEdgeB) ) +
	      EdgeHitT*EdgeHitB*
	      ( (xEdgeT-xEdgeB)*(xEdgeT-xEdgeB)+(zEdgeT-zEdgeB)*(zEdgeT-zEdgeB) );
	    dx = sqrt(dx);
	    //	    printf("%f\n",sqrt(StartP*EdgeHitT*(pow(x1-xEdgeT,2)+pow(z1-zEdgeT,2))));
	    //	    printf("%f\n",sqrt(StartP*EdgeHitR*(pow(x1-xEdgeR,2)+pow(z1-zEdgeR,2))));
	    //	    printf("%f\n",sqrt(EndP*EdgeHitB*(pow(x2-xEdgeB,2)+pow(z2-zEdgeB,2))));
	    //	    printf("%f\n",sqrt(EndP*EdgeHitL*(pow(x2-xEdgeL,2)+pow(z2-zEdgeL,2))));

	    int adc = (int) floor(dx/tracklength*siIonPot*chrg+0.5);	  
	    //std::cout << "adc = " << adc << std::endl;
	    
	    int chx = ix1 + nx;
	    int chz = iz1 + nz;
	    if( xroparity == 1 ) chz = iz2 - nz;
	    if( zroparity == 1 ) chx = ix2 - nx;

	    //--printf("firePixelsAndShareCharge: fired pixels: nx:%d nz:%d chx:%d chz:%d adc:%d dx:%f %f\n",nx,nz,chx,chz,adc,dx,tracklength);

	    if( adc==0 ) continue;

	    //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
	    SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
	    if ( (pixel->charge = adc) ) {
	      pixel->ix = chx;
	      pixel->iz = chz;		
	      ++nFiredPixels;
	    }
    
	  }
	}

      }else{ // for single pixel hit
        //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
        SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
        int adc = (int) floor(siIonPot*chrg+0.5);
	//std::cout << "adc = " << adc << std::endl;
        if ( (pixel->charge = adc) ) {
	  //          pixel->ix = (int) floor((xin+xhalfWidth)/xPitch);
	  //          pixel->iz = (int) floor((zin+zhalfWidth)/zPitch);
          pixel->ix = ix1;
          pixel->iz = iz1;
//	  std::cout << "firePixelsAndShareCharge: fired pixels: " << "0 " << adc << " " << xPitch << " " << ix1 << " " << iz1 << std::endl;
          ++nFiredPixels;
        }	
      }
      //      printf("nFiredPixels=%d\n",nFiredPixels);
      
      return nFiredPixels;
    }

  virtual int firePixelsAndShareCharge(double xin,double zin,double xout,double zout,double chrg)
    {
      nFiredPixels = 0;
      if ( fabs(xin) >= xhalfWidth || fabs(zin) >= zhalfWidth ) {
         //std::cout << "firePixelsAndShareCharge: coordinates outside bounds!" << std::endl; 
         return 0;
      }

      int ixin =  (int) floor((xin+xhalfWidth)/xPitch);
      int ixout = (int) floor((xout+xhalfWidth)/xPitch);
      int izin =  (int) floor((zin+zhalfWidth)/zPitch);
      int izout = (int) floor((zout+zhalfWidth)/zPitch);

      int nfiredx = abs(ixin-ixout)+1;
      int nfiredz = abs(izin-izout)+1;
      //      double totalcharge = siIonPot*chrg;
      //      double tracklength = sqrt((xin-xout)*(xin-xout)+(zin-zout)*(zin-zout));
      double tracklengthX = sqrt((xin-xout)*(xin-xout));
      //      double tracklengthZ = sqrt((zin-zout)*(zin-zout));
      
      std::cout << "firePixelsAndShareCharge: ============================= " << xin << " " << xout << " " << zin << " " << zout << " " << chrg << " " << tracklengthX << " " << siIonPot << std::endl;
      std::cout << "firePixelsAndShareCharge: nfired = " << nfiredx << ": " << ixin << " - " << ixout << "      " << nfiredz << ": " << izin << " " << izout << std::endl;


// this takes into account only charge sharing in phi direction
// charge sharing in Z should be rare

      if(nfiredx>1) {

        if(ixout>ixin) {
          for(int i=ixin; i<=ixout; i++) {
            double dx=0.;
            if(i==ixin) { dx = xPitch * fabs( (ixin+1) - (xin+xhalfWidth)/xPitch ); }
              else if(i==ixout) { dx = xPitch * fabs( (xout+xhalfWidth)/xPitch - ixout ); } 
                else { dx = xPitch; }
            int adc = (int) floor(dx/tracklengthX*siIonPot*chrg+0.5);
            std::cout << "firePixelsAndShareCharge: fired pixels: " << i << " " << adc << " " << dx << " " << ixin << " " << ixout << std::endl;
            //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
            SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
            if ( (pixel->charge = adc) ) {
              pixel->ix = i;
              pixel->iz = (int) floor((zin+zhalfWidth)/zPitch);
              ++nFiredPixels;
            }
          }
        } // ixout>ixin
        else {

          for(int i=ixout; i<=ixin; i++) {
            double dx=0.;
            if(i==ixin) { dx = xPitch * fabs( (xin+xhalfWidth)/xPitch - ixin ); }
              else if(i==ixout) { dx = xPitch * fabs( (ixout+1) - (xout+xhalfWidth)/xPitch ); }
                else { dx = xPitch; }
            int adc = (int) floor(dx/tracklengthX*siIonPot*chrg+0.5);
            std::cout << "firePixelsAndShareCharge: fired pixels: " << i << " " << adc << " " << dx << " " << ixin << " " << ixout << std::endl;
            //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
            SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
            if ( (pixel->charge = adc) ) {
              pixel->ix = i;
              pixel->iz = (int) floor((zin+zhalfWidth)/zPitch);
              ++nFiredPixels;
            }
          }
        
        }

      }
      else { // only single pixel was fired
        //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
        SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
        int adc = (int) floor(siIonPot*chrg+0.5);
        if ( (pixel->charge = adc) ) {
          pixel->ix = (int) floor((xin+xhalfWidth)/xPitch);
          pixel->iz = (int) floor((zin+zhalfWidth)/zPitch);
          std::cout << "firePixelsAndShareCharge: fired pixels: " << "0 " << adc << " " << xPitch << " " << ixin << " " << ixout << std::endl;
          ++nFiredPixels;
        }
      }
 
      std::cout << "firePixelsAndShareCharge: # of fired pixels = " << nFiredPixels << std::endl;
      return nFiredPixels;
    }



  virtual int firePixels(double xin,double zin,double xout,double zout,double chrg)
    {
      nFiredPixels = 0;
  
      double dx = xout - xin;
      double dz = zout - zin;
      if ( dx*dx + dz*dz < 1.e-10 ) return firePixels(xin, zin, chrg);

      if ( fabs(dx) < 1.e-5 ) dx = 1.e-5;   // < 0.1 mkm
      if ( fabs(dz) < 1.e-5 ) dz = 1.e-5;   // < 0.1 mkm
      int   ndx = ( dx < 0. ) ? -1 : 1;
      int   ndz = ( dz < 0. ) ? -1 : 1;

      double xc  = xin + xhalfWidth;
      double zc  = zin + zhalfWidth;
      int    ixc = (int) floor(xc/xPitch);
      int    izc = (int) floor(zc/zPitch);

      xc -= ixc*xPitch;
      if ( ndx < 0 ) {
	if ( xc != 0.) {
	  xc = xPitch - xc;
	} else {
	  ixc--;
	}
      }
      zc -= izc*zPitch;
      if ( ndz < 0 ) {
	if ( zc != 0.) {
	  zc = zPitch - zc;
	} else {
	  izc--;
	}
      }
      //SvxFiredPixel* pixel = pixelList->addPixel(nFiredPixels);
      SVXFIREDPIXEL_PTR pixel = pixelList->addPixel(nFiredPixels);
      pixel->ix = ixc;
      pixel->iz = izc;

      dx = fabs(dx);
      dz = fabs(dz);
      double sleft = 1.;
      double sn, snx, snz;
      do {
	snx = fabs(xPitch - xc)/dx;
	snz = fabs(zPitch - zc)/dz;
	sn  = ( snx > snz ) ? snz : snx;
	if ( sleft > sn ) {
	  if ( snx > snz ) {
	    izc += ndz;
	    xc  += sn*dx;
	    zc   = 0;
	  } else {
	    ixc += ndx;
	    zc  += sn*dz;
	    xc   = 0;
	  }
	} else {
	  sn    = sleft;
	  sleft = -1.;
	}

	if ( pixel->ix >= 0       ) {
	  if ( pixel->ix <  nXpitch ) {
	    if ( pixel->iz >= 0       ) {
	      if ( pixel->iz <  nZpitch ) {
		if ((pixel->charge = (int) floor(siIonPot*sn*chrg+0.5)) > 0) {
		  nFiredPixels++;
		  pixel = pixelList->addPixel(nFiredPixels);
		}
	      }
	    }
	  }
	}
	pixel->ix = ixc;
	pixel->iz = izc;
      } while ( (sleft -= sn) > 0. );

      return nFiredPixels;
    }

  // Print data
  virtual void printData()  const
    {
//       std::cout << "SvxPixStruct<" << TYPE << "> object: nRawptr = "
// 		<< nRawptr << ", nFiredPixels = " << nFiredPixels
// 		<< std::endl;
      std::cout << "SvxPixStruct<" << TYPE << "> object: nRawptr = "
		<< _rawPtr.size() << ", nFiredPixels = " << nFiredPixels
		<< std::endl;
      for (unsigned int i = 0; i < nFiredPixels; i++) {
	//SvxFiredPixel* pixel = pixelList->getPixel(i);
	SVXFIREDPIXEL_PTR pixel = pixelList->getPixel(i);
	std::cout << "  x-row = " << pixel->ix;
	std::cout << " z-column = " << pixel->iz;
	std::cout << " charge = " << pixel->charge << std::endl;
      }
      std::cout << std::endl; 
    }

 protected:

  // Inversed ionization potential for Silicon
  static const double siIonPot;                    // electron-hole pairs/GeV
  // PixStruct geometry
  static const int    nXpitch;                     // number of pixel rows
  static const double xPitch;                      // x-pitch size, cm
  static const double xhalfWidth;                  // X-half-width, cm
  static const int    nZpitch;                     // number of pixel columns
  static const double zPitch;                      // z-pitch size, cm
  static const double zhalfWidth;                  // Z-half-width, cm

  /** =================
   * @brief  Class for a fired pixel.
   *   This holds the xpos, zpos and charge of a fired pixel.
   */
  class SvxFiredPixel {

  public:
    // Constructor(s) & destructor
    // """""""""""""""""""""""""""
    SvxFiredPixel(const int vix=-1, const int viz=-1, const int vic=-1) {
      ix     = vix;
      iz     = viz;
      charge = vic;
      //std::cout << "SvxFiredPixel object created" << std::endl;
    }
    
    ~SvxFiredPixel() {
      //std::cout << "SvxFiredPixel object destroyed" << std::endl;
    }

    // Data
    // """"
    int   ix;              // x-index of fired pixel
    int   iz;              // z-index of fired pixel
    int   charge;          // charge (number of electrons) at fired pixel
  };

  typedef SvxFiredPixel* SVXFIREDPIXEL_PTR;

  /** =================
   * @brief  Class for a fired-pixel list.
   *   This holds the list of fired pixels in a sensor.
   */
  class SvxFiredPixelList {

  public:
    // Constructor(s) & destructor
    // """""""""""""""""""""""""""
    SvxFiredPixelList(const unsigned int length) :
      _pixelList(length,0)
    {}

    ~SvxFiredPixelList() {
      for (unsigned int i=0; i<_pixelList.size(); i++) delete _pixelList[i];
    }

    // Methods
    // """""""
    SVXFIREDPIXEL_PTR addPixel(const unsigned int ind)         {
      //SvxFiredPixel* addPixel(const unsigned int ind)         {
      // DLW: I find this design odd.  It departs from the common convention in 
      // a lot of PHENIX code, where the "add" function allocates a new object and
      // returns a pointer to it.  Here the add looks for a particular index and
      // expands the container if the index doesn't lie within it.  Not sure
      // what particular problem(s) this addresses.  A major change I have made
      // here is to use an STL container.  The vector handles it's own allocations, 
      // and will do it much better than anything we can do.  Plus it greatly simplifies
      // the code...
      
      if ( ind >= _pixelList.size() ) 
	{
	  _pixelList.resize(ind+1,0);
	}
      if ( !_pixelList[ind] ) _pixelList[ind] = SVXFIREDPIXEL_PTR(new SvxFiredPixel());
      return _pixelList[ind];
    }

    unsigned int   setListLength(const unsigned int length) {
      _pixelList.resize(length);
      return _pixelList.size();
    }

    unsigned int   getListLength()                  const   {
      return _pixelList.size();
    }

    SVXFIREDPIXEL_PTR getPixel(const unsigned int ind) const   {
      try 
	{
	  return _pixelList.at(ind);
	}
      catch (...) {}
      return SVXFIREDPIXEL_PTR(0);
    }

  private:
    // Data
    // """"
    std::vector<SvxFiredPixel*> _pixelList;
  };

  // Container for fired pixels
  // """"""""""""""""""""""""""
  unsigned int       nFiredPixels;    //! number of fired pixels
  SvxFiredPixelList *pixelList   ;    //! container for the fired pixels

  // Container for raw hit pointers
//   unsigned int nRawptr;               //! Number of valid pointers
//   unsigned int rawPtrLength;          //! List length (size)
//   SvxRawhit **rawPtr;                 //! raw hit list
  std::vector<SvxRawhit*> _rawPtr;       //! raw hit list

  //---
  ClassDefT(SvxPixStruct,1)

};

ClassDefT2(SvxPixStruct,TYPE)
ClassImpT(SvxPixStruct,TYPE)

#endif
