
#ifndef __DIAGNOSTIC_H__
#define __DIAGNOSTIC_H__

#include <string>
#include <sstream>

/// gamma pairs
struct Diagnostic {
  float trig;
  float cent;
  float vtxZ;
  float Epair;
  float pt;
  float costheta;
  float phi;
  float mass;
  float asym;

  float sec1;
  float iy1;
  float iz1;
  float Ecore1;
  float E1;
  float E91;
  float tof1;
  float tofrd1; // tof relative difference [sigma]
  float prob1;
  float disp1;
  float chisq1;

  float x1;
  float y1;
  float z1;
  float xp1;
  float yp1;
  float zp1; 
  float stoch1;
  float pc3dphi1;
  float pc3dz1;
  
//   float sx1;
//   float sy1;
//   float sz1;
//   float sxp1;
//   float syp1;
//   float szp1; 

  float sec2;
  float iy2;
  float iz2;
  float Ecore2;
  float E2;
  float E92; 
  float tof2;
  float tofrd2; // tof relative difference [sigma]
  float prob2;
  float disp2;
  float chisq2;

  float x2;
  float y2;
  float z2;
  float xp2;
  float yp2;
  float zp2; 
  float stoch2;
  float pc3dphi2;
  float pc3dz2;
  
  float et00ecore;
  float et02ecore;
  float et00e;
  float et02e;
  float bbcqn;
  float bbcqs;
  float nevents;

/*  float sx2;
  float sy2;
  float sz2;
  float sxp2;
  float syp2;
  float szp2; */

  float runn;

  void fill(TNtuple* n) { n->Fill((Float_t*)this); }

  static std::string getDescription();
};

inline std::string
Diagnostic::getDescription()
{
  std::ostringstream desc;
  desc << "trig:cent:vtxZ"                                               // event info
       << ":Epair:pt:costheta:phi:mass:asym"                        // pair info
       << ":sec1:iy1:iz1:Ecore1:E1:E91:tof1:tofrd1:prob1:disp1:chisq1" // cluster info
       << ":x1:y1:z1:xp1:yp1:zp1:stoch1:pc3dphi1:pc3dz1"                 // cluster pos, nearest track projection
//       << ":sx1:sy1:sz1:sxp1:syp1:szp1"                             // same for swapping
       << ":sec2:iy2:iz2:Ecore2:E2:E92:tof2:tofrd2:prob2:disp2:chisq2" // cluster info
       << ":x2:y2:z2:xp2:yp2:zp2:stoch2:pc3dphi2:pc3dz2"
       << ":et00ecore:et02ecore:et00e:et02e:bbcqn:bbcqs:nevents:runn" // cluster pos, nearest track projection
//       << ":sx2:sy2:sz2:sxp2:syp2:szp2"                             // same for swapping
    ;
  return desc.str();
}


/// gamma clusters
struct Diagamma {
  float trig;
  float cent;
  float vtxZ;
  float nclust;

  float pt;
  float ptnewe;
  float costheta;
  float phi;

  float sec;
  float iy;
  float iz;
  float ecore;
  float ecorenew;
  float ecent;
  float tof;
  float tofrd;  // tof relative difference [sigma]
  float prob;
  float disp;
  float chisq;
  float twrhit;
  float padispy;
  float padispz;
  float stoch;

  float x;
  float y;
  float z;
  float xp;
  float yp;
  float zp; 
  float pc3dphi; 
  float pc3dz; 
  float et00ecore; 
  float et02ecore; 
  float et00e; 
  float et02e; 

  float bbcqn; 
  float bbcqs; 
  float nevents; 

  float runn;

  void fill(TNtuple* ng) { ng->Fill((Float_t*)this); }

  static std::string getDescription();
};

inline std::string
Diagamma::getDescription()
{
  std::ostringstream desc;
  desc << "trig:cent:vtxZ:nclust"                            // event info
       << ":pt:ptnewe:costheta:phi"                        // gamma info
       << ":sec:iy:iz:ecore:ecorenew:ecent:tof:tofrd:prob:disp:chisq:twrhit:padispy:padispz:stoch" // cluster info
       << ":x:y:z:xp:yp:zp:pc3dphi:pc3dz:et00ecore:et02ecore:et00e:et02e:bbcqn:bbcqs:nevents:runn"   // cluster pos, nearest track projection
    ;
  return desc.str();
}


#endif // __DIAGNOSTIC_H__
