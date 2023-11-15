#ifndef __PDBRPCDEADNOISE_HH__
#define __PDBRPCDEADNOISE_HH__

//  Declaration of class PdbRpcDeadNoise
//  Purpose: Stores dead channel, noisy channel, and cluster size information for RPCs
//  Author: Richard Hollis (rhollis@ucr.edu)

#include <PdbCalChan.hh>
#include <iosfwd>
#include <string>
#include <iostream>

// The number of strips.  We fill one DB entry for each half octant
static const int NRPCSTRIPS = 64;

class PdbRpcDeadNoise : public PdbCalChan
{
public:

  //! constructor
  PdbRpcDeadNoise();

  //! constructor
  PdbRpcDeadNoise( const int iarm, const int istation, const int ioctant, const int ihalfoctant, const int iradsegment);

  //! copy constructor
  PdbRpcDeadNoise( const PdbRpcDeadNoise& );

  //! assignment operator
  PdbRpcDeadNoise& operator = (const PdbRpcDeadNoise& );

  //! zero all calibration parameters
  void zero();

  //! destructor
  virtual ~PdbRpcDeadNoise()
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
  int   get_Dead(int istrip)        const {
    if(istrip<NRPCSTRIPS) { return fDead[istrip]; }
    else { std::cout << "PdbRpcDeadNoise::get_Dead strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_Noise(int istrip)       const {
    if(istrip<NRPCSTRIPS) { return fNoise[istrip]; }
    else { std::cout << "PdbRpcDeadNoise::get_Noise strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  float get_ClusterSize(int istrip) const {
    if(istrip<NRPCSTRIPS) { return fClusterSize[istrip]; }
    else { std::cout << "PdbRpcDeadNoise::get_ClusterSize strip "
		<< istrip << " out of range!" << std::endl;
      return -1; } }
  
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
  void set_Dead( const int istrip, const int temp) {
    if(istrip<NRPCSTRIPS) { fDead[istrip] = temp;   }
    else { std::cout << "PdbRpcDeadNoise::set_Dead strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_Noise( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fNoise[istrip] = temp; }
    else { std::cout << "PdbRpcDeadNoise::set_Noise strip "
		<< istrip << " out of range!" << std::endl; } }
  void set_ClusterSize( const int istrip, const float temp) {
    if(istrip<NRPCSTRIPS) { fClusterSize[istrip] = temp; }
    else { std::cout << "PdbRpcDedNoise::set_ClusterSize strip "
		<< istrip << " out of range!" << std::endl; } }
  
protected:

  //Identifiers
  int fArmNum;
  int fStationNum;
  int fOctantNum;
  int fHalfOctantNum;
  int fRadSegment;

  //Variables
  int   fDead[NRPCSTRIPS];
  float fNoise[NRPCSTRIPS];
  float fClusterSize[NRPCSTRIPS];

  ClassDef(PdbRpcDeadNoise,1);
};
#endif /* __PDBRPCDEADNOISE_HH__ */
