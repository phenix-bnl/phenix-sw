#ifndef __SVXCENTRALTRACKMAPENTRY_H_
#define __SVXCENTRALTRACKMAPENTRY_H_

#include <iostream>

class SvxCentralTrackMapEntry
{
public:
  SvxCentralTrackMapEntry();
  virtual ~SvxCentralTrackMapEntry() {}

  virtual void identify(std::ostream &os=std::cout) const;

  void set_DchIndex(const short int val) 		{DchIndex = val;}
  void set_HitPattern(const short int val) 		{HitPattern = val;}
  void set_Unique(const short int val) 			{Unique = val;}
  void set_DCA2D(const float val) 			{DCA2D = val;}
  void set_DCAZ(const float val) 			{DCAZ = val;}
  void set_ClosestApproach(const short int ixyz, const short int val) 	{ClosestApproach[ixyz] = val;}
  void set_ClosestApproachX(const short int val) 	{ClosestApproach[0] = val;}
  void set_ClosestApproachY(const short int val) 	{ClosestApproach[1] = val;}
  void set_ClosestApproachZ(const short int val) 	{ClosestApproach[2] = val;}
  void set_ChisquarePhi(const short int val) 		{ChisquarePhi = val;}
  void set_ChisquareZ(const short int val) 		{ChisquareZ = val;}
  void set_Chisquare(const short int val) 		{Chisquare = val;}
  void set_Chisquare2(const short int val) 		{Chisquare2 = val;}
  void set_NClusters(const short int val) 		{NClusters = val;}
  void set_LivePercent(const int layer, const short int val)
  {
    if (layer>=0 && layer<4) {
      LivePercent[layer] = val;
    }
    else 
      std::cerr << "ERROR in SvxCentralTrackMapEntry::set_LivePercent(): "
		<< "invalid layer index " << layer << std::endl; 
  }
  void set_LinkQuality (const short int val)            {LinkQuality = val;}
  void set_LinkScore   (const short int val)            {LinkScore   = val;}
  void set_MomentumX(const short int val)               {Momentum[0] = val;}
  void set_MomentumY(const short int val)               {Momentum[1] = val;}
  void set_MomentumZ(const short int val)               {Momentum[2] = val;}

  // for schema 1003
  void set_DCA2Dprimary(const short int val) 		{DCA2Dprimary = val;}
  void set_DCAZprimary(const short int val) 		{DCAZprimary  = val;}

  void set_ClusterID(const short int icl, const short int val) 		{ClusterID[icl] = val;}
  void set_ClusterDPhi(const short int icl, const short int val) 	{ClusterDPhi[icl] = val;}
  void set_ClusterDZ(const short int icl, const short int val) 		{ClusterDZ[icl] = val;}
  
  short int get_DchIndex()       const 			{return DchIndex;}
  short int get_HitPattern()     const 			{return HitPattern;}
  short int get_Unique()  	 const			{return Unique;}
  short int get_DCA2D()       	 const			{return DCA2D;}
  short int get_DCAZ()   	 const	    		{return DCAZ;}
  short int get_ClosestApproach(const short int ixyz) const
                                                        {return ClosestApproach[ixyz];}
  short int get_ChisquarePhi()   const         		{return ChisquarePhi;}
  short int get_ChisquareZ()     const         		{return ChisquareZ;}
  short int get_Chisquare()      const     		{return Chisquare;}
  short int get_Chisquare2()     const        		{return Chisquare2;}
  short int get_NClusters()      const         		{return NClusters;}

  short int get_LivePercent(const int layer) const      
  {
    if (layer>=0 && layer<4) {
      return LivePercent[layer];
    }
    std::cerr << "ERROR in SvxCentralTrackMapEntry::set_LivePercent(): "
	      << "invalid layer index " << layer << std::endl; 
    return -1;
  }
  short int get_LinkQuality ()   const                  {return LinkQuality ;}
  short int get_LinkScore   ()   const                  {return LinkScore   ;}
  short int get_Momentum (const short int ixyz) const   {return Momentum[ixyz];}
  short int get_MomentumX()      const                  {return Momentum[0];}
  short int get_MomentumY()      const                  {return Momentum[1];}
  short int get_MomentumZ()      const                  {return Momentum[2];}
  short int get_DCA2Dprimary()   const			{return DCA2Dprimary;} // for schema 1003
  short int get_DCAZprimary()  	 const	    		{return DCAZprimary;}  // for schema 1003

  short int get_ClusterID(const short int icl)  const   {return ClusterID[icl];}
  short int get_ClusterDPhi(const short int icl) const  {return ClusterDPhi[icl];}
  short int get_ClusterDZ(const short int icl) const    {return ClusterDZ[icl];}

protected:
  short int DchIndex;
  short int HitPattern;
  short int Unique;
  short int DCA2D;
  short int DCAZ;
  short int ClosestApproach[3];
  short int ChisquarePhi;
  short int ChisquareZ;
  short int Chisquare;
  short int Chisquare2;
  short int NClusters;

  short int LivePercent[4];
  short int LinkQuality;
  short int LinkScore;
  short int Momentum[3];

  short int ClusterID[8];
  short int ClusterDPhi[8];
  short int ClusterDZ[8];

  short int DCA2Dprimary; // schema 1003
  short int DCAZprimary;  // schema 1003
};

#endif 
