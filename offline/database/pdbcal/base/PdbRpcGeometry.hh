#ifndef __PDBRPCGEOMETRY_HH__
#define __PDBRPCGEOMETRY_HH__

//  Declaration of class PdbRpcGeometry
//  Purpose: Stores Geometry information for RPCs
//  Author: Richard Hollis (rhollis@ucr.edu)

#include <PdbCalChan.hh>
#include <iosfwd>
#include <string>
#include <iostream>

// The number of strips.  We fill one DB entry for each half octant
static const int NRPCSTRIPS = 64;

class PdbRpcGeometry : public PdbCalChan
{
public:

  //! constructor
  PdbRpcGeometry();

  //! constructor
  PdbRpcGeometry( const int iarm, const int istation, const int ioctant, const int ihalfoctant, const int iradsegment);

  //! copy constructor
  PdbRpcGeometry( const PdbRpcGeometry& );

  //! assignment operator
  PdbRpcGeometry& operator = (const PdbRpcGeometry& );

  //! zero all calibration parameters
  void zero();

  //! destructor
  virtual ~PdbRpcGeometry()
  {}

  virtual void print() const;

  virtual void write(std::ostream& os) const;

  virtual void read(std::istream& is);

  int getUniqueId() const;

  //-----------------
  // - GETTERS
  //-----------------
  //Identifiers
  int getArm()        const { return fArmNum;        }
  int getStation()    const { return fStationNum;    }
  int getOctant()     const { return fOctantNum;     }
  int getHalfOctant() const { return fHalfOctantNum; }
  int getRadSegment() const { return fRadSegment;    }
  //Variables
  int   get_Invalid(int istrip)   const {
    if(istrip<NRPCSTRIPS) { return fInvalid[istrip];   }
    else { std::cout << "PdbRpcGeometry::get_Invalid strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_PosXBegin(int istrip) const {
    if(istrip<NRPCSTRIPS) { return fPosXBegin[istrip]; }
    else { std::cout << "PdbRpcGeometry::get_PosXBegin strip "
		<< istrip << " out of range!" << std::endl;
	return -1; } }
  float get_PosYBegin(int istrip) const {
    if(istrip<NRPCSTRIPS) { return fPosYBegin[istrip]; }
    else { std::cout << "PdbRpcGeometry::get_PosYBegin strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_PosZBegin(int istrip) const {
    if(istrip<NRPCSTRIPS) { return fPosZBegin[istrip]; }
    else { std::cout << "PdbRpcGeometry::get_PosZBegin strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_PosXEnd(int istrip)   const {
    if(istrip<NRPCSTRIPS) { return fPosXEnd[istrip];   }
    else { std::cout << "PdbRpcGeometry::get_PosXEnd strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_PosYEnd(int istrip)   const {
    if(istrip<NRPCSTRIPS) { return fPosYEnd[istrip];   }
    else { std::cout << "PdbRpcGeometry::get_PosYEnd strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_PosZEnd(int istrip)   const {
    if(istrip<NRPCSTRIPS) { return fPosZEnd[istrip];   }
    else { std::cout << "PdbRpcGeometry::get_PosZEnd strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_StripArea(int istrip) const {
    if(istrip<NRPCSTRIPS) { return fStripArea[istrip]; }
    else { std::cout << "PdbRpcGeometry::get_StripArea strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  int   get_PacketOffset()        const {
    return fPacketOffset;      }
  
  static int get_MaxNumStrips() { return NRPCSTRIPS; }
  
  //-----------------
  // - SETTERS
  //-----------------
  //Identifiers
  void setArm ( const int temp)        { fArmNum = temp;        }
  void setStation ( const int temp)    { fStationNum = temp;    }
  void setOctant ( const int temp)     { fOctantNum = temp;     }
  void setHalfOctant (const int temp)  { fHalfOctantNum = temp; }
  void setRadSegment ( const int temp) { fRadSegment = temp;    }
  //Variablesx
  void set_Invalid( const int istrip, const int temp) {
    if(istrip<NRPCSTRIPS) { fInvalid[istrip] = temp; }
    else { std::cout << "PdbRpcGeometry::set_Invalid strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PosXBegin( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fPosXBegin[istrip] = temp; }
    else { std::cout << "PdbRpcGeometry::set_PosXBegin strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PosYBegin( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fPosYBegin[istrip] = temp; }
    else { std::cout << "PdbRpcGeometry::set_PosYBegin strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PosZBegin( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fPosZBegin[istrip] = temp; }
    else { std::cout << "PdbRpcGeometry::set_PosZBegin strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PosXEnd( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fPosXEnd[istrip] = temp;   }
    else { std::cout << "PdbRpcGeometry::set_PosXEnd strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PosYEnd( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fPosYEnd[istrip] = temp;   }
    else { std::cout << "PdbRpcGeometry::set_PosYEnd strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PosZEnd( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fPosZEnd[istrip] = temp;   }
    else { std::cout << "PdbRpcGeometry::set_PosZEnd strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_StripArea( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fStripArea[istrip] = temp;   }
    else { std::cout << "PdbRpcGeometry::set_StripArea strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_PacketOffset( const int temp)
  { fPacketOffset = temp;      }
  
protected:
  
  //Identifiers
  int fArmNum;
  int fStationNum;
  int fOctantNum;
  int fHalfOctantNum;
  int fRadSegment;

  //Variables
  int   fInvalid[NRPCSTRIPS];
  float fPosXBegin[NRPCSTRIPS];
  float fPosYBegin[NRPCSTRIPS];
  float fPosZBegin[NRPCSTRIPS];
  float fPosXEnd[NRPCSTRIPS];
  float fPosYEnd[NRPCSTRIPS];
  float fPosZEnd[NRPCSTRIPS];
  float fStripArea[NRPCSTRIPS];
  int   fPacketOffset;

  ClassDef(PdbRpcGeometry,1);
};
#endif /* __PDBRPCGEOMETRY_HH__ */
