#ifndef __SVXTRACKER_H__
#define __SVXTRACKER_H__

#include <iostream>
#include <vector>
#include <cmath>

class svxDetectorGeo;
class SvxCluster;

//this class provides utility functions for the standalone tracking
//it calculates track quality, momentum, secondary vertices, DCA, etc.


struct SvxTrackInfo {
  ///
  /// contains some information of track
  /// d_Rrot       : rotation diameter
  /// (d_cx, d_cy) : rotation center
  ///
  SvxTrackInfo() : d_Rrot(0.), d_cx(0.), d_cy(0.){}
  double d_Rrot;
  double d_cx;
  double d_cy;
};


class SvxTracker
{
  public:
    SvxTracker();
    ~SvxTracker(){}
    
    float getMom();
    float get3Mom(int coor);
    bool  getHelicity();
    float getDPHI(int ind);
    float getDZ(int ind);
    float getScatter(int ind);
    float getSScatter(int ind);
    float getScatterXY(int ind);
    float getScatterRZ(int ind);
    void  getExpectedPosition(int ilayer, float &x, float &y, float &z, int ihit);
    float getMomOut(int ind, int xyz);

    void   setFitNoCNT(bool flag) { m_isFitNoCNT = flag; }
    double getChi2SvxCntNoCNT() { return m_chi2_svxcnt_nocnt;}
    double getNDFSvxCntNoCNT()  { return m_ndf_svxcnt_nocnt; }

    void probDers(bool hel, const std::vector<float> &pos, const std::vector<int> &is_strip, float P, float &prob, float &dprobdP);
    
    float solveTrack(const std::vector<float> &pos, const std::vector<int> &is_strip, float P);
        
    void calculateTangents(bool hel, float x1, float y1, float z1, float x2, float y2, float z2, float P, float &p2, float &dp2dP);
    
    void newtonIterate1D(float der, float der2, float &val);
    void calcHelixDistDers(bool hel1, bool hel2, const std::vector<float> &param1, const std::vector<float> &param2, const std::vector<float> &val, std::vector<float> &grad, std::vector<float> &hessian);
    
    void calcHelixDistDers(bool hel1, const std::vector<float> &param1, const std::vector<float> &point, float val, float &der, float &der2);
    void calculateTangentsNoDers(bool hel, float x1, float y1, float z1, float x2, float y2, float z2, float P, std::vector<float> &tans);
    
    double fullProbFunction(const std::vector<float>& params, const std::vector<float>& cluster_centers, float P);
    
    /// - solveTrack_byChiSquare
    ///      evaluate optimum pT^2 with least chi-square method
    float solveTrack_byChiSquare(const std::vector<float> &position,
                                 const std::vector<int> &is_strip,
                                 float pT2);
    
    double TrackFit(const std::vector<SvxCluster*> &vcluster,
		    bool helicity, double pT, float &chisq, int &ndf);
    //double TrackFit_SvxCNT(const std::vector<SvxCluster*> &vcluster,
    //			   bool helicity, double pT, double phi, double theta,
    //			   double cax, double cay,
    //			   float &chisq, int &ndf);
    double TrackFit_SvxCNT(const std::vector<SvxCluster*> &vcluster,
			   bool helicity, double pT, double phi, double theta,
			   double cax, double cay,
			   double &chisq, int &ndf,
			   double &chi_phi, double &chi_z, double &chi_angle);

  private:
   

  private:
    class  FitInfoContainer;

    double TrackFitCore(int nlength, FitInfoContainer *fitinfo);
    double LineMinimizationCore(FitInfoContainer* fitinfo);

    void   calculateMomentumAtInnerMost(FitInfoContainer *fitinfo);
    double calculateLogProb(const double chi, const int ndf);

  public:
    double LineMinimization(const std::vector<SvxCluster*> &vcluster,
			    bool helicity, double pT2,
			    std::vector<double> &dparam, std::vector<double> &ddparam);
    double LineMinim_SvxCNT(const std::vector<SvxCluster*> &vcluster,
			    bool helicity, double pT2, double phi, double theta,
			    double cax, double cay,
			    std::vector<double> &dparam, std::vector<double> &ddparam);

    double BrentMinimize(double x1, double x2, double x3,
			 double f1, double f2, double f3,
			 const std::vector<SvxCluster*> &vcluster,
			 bool helicity, double pT2,
			 /*const*/ std::vector<double> &dparam,
			 /*const*/ std::vector<double> &ddparam);
    double BrentMinim_SvxCNT(double x1, double x2, double x3,
			     double f1, double f2, double f3,
			     const std::vector<SvxCluster*> &vcluster,
			     bool helicity, double pT2,
			     double phi, double theta, double cax, double cay,
			     /*const*/ std::vector<double> &dparam,
			     /*const*/ std::vector<double> &ddparam);
    double BrentMinimizeCore(double x1, double x2, double x3,
			     double f1, double f2, double f3,
                             FitInfoContainer *fitinfo);


    /// - calculate_ChiSquare
    ///      calculate chi-square
    double calculate_ChiSquare(bool helicity,
			       double pT2,
			       const std::vector<float> &position,
			       const std::vector<int> &is_strip);

    double calculate_ChiSquare(bool helicity, double pT2,
			       const std::vector<SvxCluster*> &vcluster,
			       const std::vector<double> &dparam);

    double calculate_ChiSquare(bool helicity, double pT2,
			       const std::vector<SvxCluster*> &vcluster,
			       const std::vector<double> &dparam,
			       const std::vector<double> &ddparam,
			       double dl);

    /// - calc_Chi2_SvxCNT
    ///      calculate chi-square for SvxCentralTrack
    double calc_Chi2_SvxCNT(bool helicity,
			    double pT2, double phi, double theta,
			    const std::vector<double> &position,
			    const std::vector<int> &is_strip);

    double calc_Chi2_SvxCNT(bool helicity,
			    double pT2, double phi, double theta,
			    double cax, double cay,
			    const std::vector<SvxCluster*> &vcluster,
			    const std::vector<double> &dparam);

    double calc_Chi2_SvxCNT(bool helicity,
			    double pT2, double phi, double theta,
			    double cax, double cay,
			    const std::vector<SvxCluster*> &vcluster,
			    const std::vector<double> &dparam,
			    const std::vector<double> &ddparam,
			    double dl);
 
    /// - calc_Chi2_SvxCNT
    ///      calculate chi-square for SvxCentralTrack but doesn't use phi0 and the0
    double calc_Chi2_SvxCNT_NoCNT(bool helicity,
			    double pT2, double theta,
			    const std::vector<double> &position,
			    const std::vector<int> &is_strip);

    double calc_Chi2_SvxCNT_NoCNT(bool helicity,
			    double pT2, double theta,
			    double cax, double cay,
			    const std::vector<SvxCluster*> &vcluster,
			    const std::vector<double> &dparam);

    double calc_Chi2_SvxCNT_NoCNT(bool helicity,
			    double pT2, double theta,
			    double cax, double cay,
			    const std::vector<SvxCluster*> &vcluster,
			    const std::vector<double> &dparam,
			    const std::vector<double> &ddparam,
			    double dl);
    
    /// - simle_calcTangents
    ///      calculate tangent vector at (x1, y1, z1) and (x2, y2, z2)
    void simple_calcTangents(bool helicity,
                             double x1, double y1, double z1,
                             double x2, double y2, double z2,
                             double p2, double &pT2,
                             std::vector<double> &tan1, std::vector<double> &tan2);

    /// - calc_InfoAtClosestApproach
    ///      calculate position and momentum at the closest approach
    ///      of the track in xy-plane to a point.
    ///      this can also calculate sign of DCA.
    void calc_InfoAtClosestApproach(bool helicity,
                                    float x0, float y0, float z0,
                                    float px, float py, float pz,
                                    const std::vector<float> &point,
                                    std::vector <float> &pos_primary,
                                    std::vector <float> &mom_primary,
                                    float &DCA2D);

    SvxTrackInfo calc_TrackInfo(bool helicity,
                                const std::vector<double> &position,
                                const std::vector<double> &momentum);

    void setExpectedPosition(const std::vector<SvxCluster*> &vcluster,
			     const std::vector<double> &dparam);
    void setFitResult(bool helicity, double pT2,
		      const std::vector<SvxCluster*> &vcluster,
		      const std::vector<double> &dparam);
    void setFitResult_SvxCNT(bool helicity,
			     double pT2, double phi0, double theta0,
			     double cax, double cay,
			     const std::vector<SvxCluster*> &vcluster,
			     const std::vector<double> &dparam);

    /// - calc_scattering
    ///      calculate scattering angle
    float calc_scattering(float angle, int is_strip, double p2);
    
    /// - calc_dangle2D, calc_dangle3D
    ///      calculate angle difference of two vectors
    double calc_dangle2D(double vx1, double vy1, double vx2, double vy2);
    double calc_dangle3D(double vx1, double vy1, double vz1, 
			 double vx2, double vy2, double vz2);

    void set_DetectorGeo(svxDetectorGeo *svxgeo) { d_geometry = svxgeo; }
    void set_ZeroFieldFlag(bool t) { zerofield = t; }

    void set_sphiCNTout(double sphi)   { m_sphiCNTout   = sphi; }
    void set_sthetaCNTout(double sthe) { m_sthetaCNTout = sthe; }
    void set_MisAlignmentPhi(double phi) { m_misalign_phi = phi; }
    void set_MisAlignmentZ(double z)     { m_misalign_z   = z;   }
    
    void SetFitParameters(const char *mode);

  private:
  /// internal class used only in BrentMinimize
    class FitInfoContainer {
      public:
      FitInfoContainer(int FittingFlag, 
                       const std::vector<SvxCluster*>& Vcluster,
                       bool   Helicity,
                       double PT2,
                       double Phi0, double Theta0, double Cax, double Cay,
                       std::vector<double>& Dparam,
                       std::vector<double>& Ddparam ) :
                       isSvxCntTrackFitting(FittingFlag), 
                       vcluster(Vcluster),
                       helicity(Helicity),
                       pT2(PT2), phi0(Phi0), theta0(Theta0), cax(Cax),  cay(Cay),
                       dparam(Dparam), ddparam(Ddparam),
                       m_isFitNoCNT(false)
       {}
      virtual ~FitInfoContainer(){}

      double calculate_ChiSquare(SvxTracker* tracker, const std::vector<double>& DParam){
          if(isSvxCntTrackFitting==0){ // chi2 for SvxSegment
            return tracker->calculate_ChiSquare(helicity, pT2, vcluster, DParam);
          }
          else { // chi2 for SvxCentralTrack
            if(!m_isFitNoCNT) {
              //std::cout<<"FitInfoCont::calc_chi2 for SvxCnt" <<std::endl;
              return tracker->calc_Chi2_SvxCNT(
                       helicity, pT2, phi0, theta0, cax, cay, vcluster, DParam);
            }
            else {
              //std::cout<<"FitInfoCont::calc_chi2 for SvxCnt" <<std::endl;
              return tracker->calc_Chi2_SvxCNT_NoCNT(
                       helicity, pT2, theta0, cax, cay, vcluster, DParam);
            }
          }
        }

      double calculate_ChiSquare(SvxTracker* tracker, double u){
          if(isSvxCntTrackFitting==0){ // chi2 for SvxSegment
            return tracker->calculate_ChiSquare(helicity, pT2, vcluster, dparam, ddparam, u);
          }
          else { // chi2 for SvxCentralTrack
            if(!m_isFitNoCNT) {
              //std::cout<<"FitInfoCont::calc_chi2 for SvxCnt" <<std::endl;
              return tracker->calc_Chi2_SvxCNT(
                       helicity, pT2, phi0, theta0, cax, cay, vcluster, dparam, ddparam, u);
            }
            else {
              //std::cout<<"FitInfoCont::calc_chi2 for SvxCnt" <<std::endl;
              return tracker->calc_Chi2_SvxCNT_NoCNT(
                       helicity, pT2, theta0, cax, cay, vcluster, dparam, ddparam, u);
            }
          }
        }

      double calculate_ChiSquare_NoCNT(SvxTracker* tracker){
          { // chi2 for SvxCentralTrack
            return tracker->calc_Chi2_SvxCNT_NoCNT(
                     helicity, pT2, theta0, cax, cay, vcluster, dparam, ddparam, 1);
          }
        }

      int    isSvxCntTrackFitting; // 0: Segment fitting, 1: SvxCntTrackFitting
      const std::vector<SvxCluster*>& vcluster;
      bool   helicity;
      double pT2, phi0, theta0, cax,  cay;
      std::vector<double>& dparam;
      std::vector<double>& ddparam;
      bool   m_isFitNoCNT; // 0: with CNT, 1:without CNT
    };
    
  private:
        
    float X0_beampipe; //proportion of radiation length
    float X0_pixel;
    float X0_strip;
    float theta0p_pixel;
    float theta0p_strip;
    float B;
    float binv;
    float b;
    float currentmom;
    
    float mom3[3];
    float momtot;
    
    bool currenthelicity;
    
    float m_dphi[6];
    float m_dz[6];
    float m_sdphi[6];
    float m_sdz[6];
    float m_scatter[6];
    float m_scatter_xy[6];
    float m_scatter_rz[6];
    float m_sscatter[6];
    float m_momentum_out[6][3];
    float expected_position[4][2][3];
    /// expected_position[layer][hit][coordinate]

    // chi2 of SvxCnt w/o nocnt angle
    bool   m_isFitNoCNT;
    double m_chi2_svxcnt_nocnt;
    int    m_ndf_svxcnt_nocnt;
    
    
    double m_sphiCNTout;
    double m_sthetaCNTout;
    /// m_sphiCNTout   : uncertainty of track angle in xy-plane at B3
    ///                  projected from phi0 of CNT
    /// m_sthetaCNTout : uncertainty of track angle in rz-plane at B3
    ///                  projected from theta0 of CNT
    double m_sdphil[4];
    double m_sdzl[4];
    double m_misalign_phi;
    double m_misalign_z;
    /// m_misalign_phi : amount of misalignment in phi-direction
    /// m_misalign_z   : amount of misalignment in z-direction

    std::vector<float> t, dtdP, t_f, dtdP_f;
    
    svxDetectorGeo *d_geometry;

    bool zerofield;
    
    double m_sdpT;
    void   set_sdpT(double sdpT) { m_sdpT = sdpT; }
    double get_sdpT()            { return m_sdpT; }
    void Reset();

    double m_tolerance;
    int m_nitr;

};



inline bool SvxTracker::getHelicity(){return currenthelicity;}

inline float SvxTracker::getDPHI(int ind){return m_dphi[ind];}

inline float SvxTracker::getDZ(int ind){return m_dz[ind];}

inline float SvxTracker::getScatter(int ind){return m_scatter[ind];}

inline float SvxTracker::getScatterXY(int ind){return m_scatter_xy[ind];}

inline float SvxTracker::getScatterRZ(int ind){return m_scatter_rz[ind];}

inline float SvxTracker::getSScatter(int ind){return m_sscatter[ind];}

inline float SvxTracker::getMom(){return momtot;}

inline float SvxTracker::get3Mom(int coor){return mom3[coor];}

inline void SvxTracker::getExpectedPosition(int ilayer,
					    float &x, float &y, float &z,
					     int ihit)
{
  if ( ihit<0 || ihit>=2 || ilayer<0 || ilayer>=4 ) {
    std::cout << "hit ID is out of range" << std::endl;
    x = -9999.;
    y = -9999.;
    z = -9999.;
  } else {
    x = expected_position[ilayer][ihit][0];
    y = expected_position[ilayer][ihit][1];
    z = expected_position[ilayer][ihit][2];
  }
}

inline float SvxTracker::getMomOut(int ind, int xyz){return m_momentum_out[ind][xyz];}

inline void SvxTracker::newtonIterate1D(float der, float der2, float &val){val -= (der/der2);}

#endif
