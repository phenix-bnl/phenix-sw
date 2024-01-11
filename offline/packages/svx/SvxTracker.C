#include <SvxTracker.h>
#include <SvxCluster.h>
#include <SvxSensor.h>
#include <svxDetectorGeo.hh>
#include <TMath.h>
#include <TMatrixT.h>
#include <iostream>
#include <gsl/gsl_math.h>

using namespace std;


// float mass = 0.2;

static const double s_dphi[2] = {0.005 / sqrt(12.), 0.008 / sqrt(12.)};
/// 0.005  : pixel size of pixel detector in phi-direction
/// 0.008  : pixel size of stripixel detector in phi-direction
static const double s_dz[2] = {0.0425 / sqrt(12.), 0.1 / sqrt(12.)};
/// 0.0425 : pixel size of pixel detector in z-direction
/// 0.1    : pixel size of stripixel detector in z-direction
//static const double s_dphil[4] = {s_dphi[0],s_dphi[0],s_dphi[1],s_dphi[1]};
//static const double s_dzl[4] = {s_dz[0],s_dz[0],s_dz[1],s_dz[1]};
/// index : layer ID


SvxTracker::SvxTracker()
{
    X0_beampipe = .0015L;
    X0_pixel = .0128L;
    X0_strip = .0543L;
    /// These are formulae for small angle multiple scattering (see PDG
    /// (2012), Eqn. 30.15) times p.
    theta0p_pixel = (.0136L * sqrt(X0_pixel) * (1.L + .038L * TMath::Log(X0_pixel)));
    theta0p_strip = (.0136L * sqrt(X0_strip) * (1.L + .038L * TMath::Log(X0_strip)));
    //  B = 0.92;
    B = 0.9L;
    b = 0.003 * B;
    b *= b;
    binv = 1. / b;
    t.assign(6, 0.);
    dtdP.assign(6, 0.);
    t_f.assign(6, 0.);
    dtdP_f.assign(6, 0.);

    for ( int ilayer = 0; ilayer < 4; ilayer++ )
    {
        for ( int ihit = 0; ihit < 2; ihit++ )
        {
            expected_position[ilayer][ihit][0] = -9999.;
            expected_position[ilayer][ihit][1] = -9999.;
            expected_position[ilayer][ihit][2] = -9999.;
        }
    }

    for (int i = 0; i < 6; i++)
    {
        m_dphi[i] = -9999.;
        m_dz[i] = -9999.;
        m_scatter[i] = -9999.;
        m_scatter_xy[i] = -9999.;
        m_scatter_rz[i] = -9999.;
        m_sscatter[i] = -9999.;
        for (int j = 0; j < 3; j++)
        {
            m_momentum_out[i][j] = -9999.;
        }
    }

    // chi2 of svxcnt w/o vector angle
    m_chi2_svxcnt_nocnt = -9999.;
    m_ndf_svxcnt_nocnt  = -9999;

    zerofield = false;
    /// zerofield==true means zero magnetic field run.
    /// zerofield==false means normal magnetic field run.
    m_sdpT = 0.02L;
    m_tolerance = 0.001L;
    m_nitr = 50;

    //Set default values:
    //sphi: 1mrad based on DC r-phi resolution
    //stheta: 7mrad based on BBC z resolution in central Au+Au events
    m_sphiCNTout = 0.001L;
    m_sthetaCNTout = 0.007L;
    m_misalign_phi = 0.0L;
    m_misalign_z   = 0.0L;

    m_sdphil[0] = s_dphi[0];
    m_sdphil[1] = s_dphi[0];
    m_sdphil[2] = s_dphi[1];
    m_sdphil[3] = s_dphi[1];
    m_sdzl[0] = s_dz[0];
    m_sdzl[1] = s_dz[0];
    m_sdzl[2] = s_dz[1];
    m_sdzl[3] = s_dz[1];

    d_geometry = NULL;
}

void SvxTracker::Reset()
{
    t.assign(6, 0.);
    dtdP.assign(6, 0.);
    t_f.assign(6, 0.);
    dtdP_f.assign(6, 0.);

    for ( int ilayer = 0; ilayer < 4; ilayer++ )
    {
        for ( int ihit = 0; ihit < 2; ihit++ )
        {
            expected_position[ilayer][ihit][0] = -9999.;
            expected_position[ilayer][ihit][1] = -9999.;
            expected_position[ilayer][ihit][2] = -9999.;
        }
    }

    for (int i = 0; i < 6; i++)
    {
        m_dphi[i] = -9999.;
        m_dz[i] = -9999.;
        m_scatter[i] = -9999.;
        m_scatter_xy[i] = -9999.;
        m_scatter_rz[i] = -9999.;
        m_sscatter[i] = -9999.;
        for (int j = 0; j < 3; j++)
        {
            m_momentum_out[i][j] = -9999.;
        }
    }

    // chi2 of svxcnt w/o vector angle
    m_chi2_svxcnt_nocnt = -9999.;
    m_ndf_svxcnt_nocnt  = -9999;

    //  zerofield = false;
    /// zerofield==true means zero magnetic field run.
    /// zerofield==false means normal magnetic field run.
    m_sdpT = 0.02;
}

void SvxTracker::SetFitParameters(const char *mode)
///
/// This function is to set
{
    if ( !strcmp(mode, "SIM") )
    {
        m_sphiCNTout   = 0.001L;
        m_sthetaCNTout = 0.007L;
        m_misalign_phi = 0.0L;
        m_misalign_z   = 0.0L;
    }
    else if ( !strcmp(mode, "DATA") )
    {
        m_sphiCNTout   = 0.0025L;
        m_sthetaCNTout = 0.010L;
        m_misalign_phi = 0.00231L;
        m_misalign_z   = 0.00722L;
    }

    m_sdphil[0] = sqrt(s_dphi[0] * s_dphi[0] + m_misalign_phi * m_misalign_phi);
    m_sdphil[1] = sqrt(s_dphi[0] * s_dphi[0] + m_misalign_phi * m_misalign_phi);
    m_sdphil[2] = sqrt(s_dphi[1] * s_dphi[1] + m_misalign_phi * m_misalign_phi);
    m_sdphil[3] = sqrt(s_dphi[1] * s_dphi[1] + m_misalign_phi * m_misalign_phi);
    m_sdzl[0] = sqrt(s_dz[0] * s_dz[0] + m_misalign_z * m_misalign_z);
    m_sdzl[1] = sqrt(s_dz[0] * s_dz[0] + m_misalign_z * m_misalign_z);
    m_sdzl[2] = sqrt(s_dz[1] * s_dz[1] + m_misalign_z * m_misalign_z);
    m_sdzl[3] = sqrt(s_dz[1] * s_dz[1] + m_misalign_z * m_misalign_z);

    cout << "SvxTracker | uncertainty parameters : "
         << m_sphiCNTout   << ", "
         << m_sthetaCNTout << endl
         << m_sdphil[0] << ", " << m_sdphil[1] << ", "
         << m_sdphil[2] << ", " << m_sdphil[3] << endl
         << m_sdzl[0] << ", " << m_sdzl[1] << ", "
         << m_sdzl[2] << ", " << m_sdzl[3] << endl;
}


float SvxTracker::solveTrack(const vector<float> &pos, const vector<int> &is_strip, float P)
{
    // Annotated by YA. 2011/1/7
    // Input:
    //  pos       hit position. (pos[3*i+0],pos[3*i+1],pos[3*i+2]) is
    //            3D position of (x[i],y[i],z[i]) (i starts with 0)
    //
    //  is_strip  indicates if hit is pixel(=0) or strip (=1)
    //            is_strip[i] corresponds to hit (i+1)
    //            i.e is_strip[0]=0 means that hit 1 is pixel.
    //                is_strip[1]=1 means that hit 2 is strip.
    //  P         pt^2 of the track
    //
    // return value: probability that this is a good track


    float tprob = -1.;
    float tdprob = -1.;

    float dir = 1.;
    float dP = 0.5 * P;

    float cP = P;
    float tprob_f;
    probDers(true, pos, is_strip, P, tprob, tdprob);
    probDers(false, pos, is_strip, P, tprob_f, tdprob);
    currenthelicity = (tprob > tprob_f);

    for (int i = 0; i <= 6; i++)
    {
        probDers(currenthelicity, pos, is_strip, cP, tprob, tdprob);
        dir = (float)(2 * ((int)(tdprob > 0.)) - 1);
        cP += dir * dP;
        dP *= 0.5;
    }


    if (P > 10.)
    {
        dP = 0.5 * cP;

        probDers(true, pos, is_strip, cP, tprob, tdprob);
        probDers(false, pos, is_strip, cP, tprob_f, tdprob);
        currenthelicity = (tprob > tprob_f);

        for (int i = 0; i <= 6; i++)
        {
            probDers(currenthelicity, pos, is_strip, cP, tprob, tdprob);
            dir = (float)(2 * ((int)(tdprob > 0.)) - 1);
            cP += dir * dP;
            dP *= 0.5;
        }
    }


    currentmom = cP;
    return tprob;
}



float SvxTracker::solveTrack_byChiSquare(const vector<float> &pos,
        const vector<int> &is_strip,
        float pT2)
///
/// Solve track by least chi-square method
///
/// Input
///  - pos : hit position. (pos[3*i+0],pos[3*i+1],pos[3*i+2]) is
///          3D position of (x[i],y[i],z[i]) (i starts with 0)
///
///  - is_strip : indicates if hit is pixel(=0) or strip (=1)
///               is_strip[i] corresponds to hit (i+1)
///               i.e is_strip[0]=0 means that hit 1 is pixel.
///               is_strip[1]=1 means that hit 2 is strip.
///  - pT2 : pt^2 of the track
///
/// return value : log of probability
///
{
    if ( pos.size() % 3 != 0 )
    {
        cout << "solveTrack_byChiSquare::ERROR | pos is wrong for the input" << endl;
        return -9999.;
    }

    int nhits = pos.size() / 3;

    if ( nhits < 3 )
    {
        cout << "solveTrack_byChiSquare::ERROR | At least 3 hit points are necessary" << endl;
        return -9999.;
    }

    /// Calculate minimum pT from
    float max_L_xy2 = (pos[3] - pos[0]) * (pos[3] - pos[0]) + (pos[4] - pos[1]) * (pos[4] - pos[1]);
    for ( int i = 0; i < nhits - 2; i++ )
    {
        float tmp_L_xy2 = (pos[3 * (i + 2)] - pos[3 * (i + 1)]) * (pos[3 * (i + 2)] - pos[3 * (i + 1)])
                          + (pos[3 * (i + 2) + 1] - pos[3 * (i + 1) + 1]) * (pos[3 * (i + 2) + 1] - pos[3 * (i + 1) + 1]);
        if ( tmp_L_xy2 > max_L_xy2 )
        {
            max_L_xy2 = tmp_L_xy2;
        }
    }

    float min_pT = sqrt(max_L_xy2 * b) / 2.;

    /// Check helicity
    float chi_true = calculate_ChiSquare(true, pT2, pos, is_strip);
    float chi_false = calculate_ChiSquare(false, pT2, pos, is_strip);
    currenthelicity = ( chi_true < chi_false );

    float cP = sqrt(pT2);
    float dP = 0.5 * cP;
    float chi;

    /// To cacluate minimum chi-square, numerical differentiations are done
    /// 7 times. This may be improved.
    for ( int i = 0; i < 7; i++ )
    {
        float chi_0 = calculate_ChiSquare(currenthelicity,
                                          cP * cP,
                                          pos,
                                          is_strip);
        float chi_1 = calculate_ChiSquare(currenthelicity,
                                          (cP + dP) * (cP + dP),
                                          pos,
                                          is_strip);
        float chi_2 = calculate_ChiSquare(currenthelicity,
                                          ( cP - dP > min_pT ) ? (cP - dP) * (cP - dP) : min_pT * min_pT,
                                          pos,
                                          is_strip);

        if ( chi_0 > chi_1 && chi_0 > chi_2 )
        {
            /// in this case, currentmom = cP.
            if ( i > 4 )
            {
                cout << "solveTrack_byChiSquare::ERROR | No minimum value." << endl;
            }
            chi = chi_0;
            break;
        }

        if ( chi_0 > chi_1 )
        {
            chi = chi_1;
            cP += dP;
            dP *= 0.5;
        }
        else if (chi_0 > chi_2 )
        {
            chi = chi_2;
            cP -= dP;
            if ( cP < min_pT )
            {
                cP = min_pT;
            }
            dP *= 0.5;
        }
        else
        {
            chi = chi_0;
            dP *= 0.5;
        }
    }

    /// calculate momentum vector at the innermost layer.
    vector<double> tangent1(2);   /// tangent vector (px, py) at the first point
    vector<double> tangent2(2);   /// tangent vector (px, py) at the second point
    double Lxy2 = (pos[0] - pos[3]) * (pos[0] - pos[3]) + (pos[1] - pos[4]) * (pos[1] - pos[4]);
    double Lz2  = (pos[2] - pos[5]) * (pos[2] - pos[5]);
    double tmp_pT2 = cP * cP;
    double p2 = tmp_pT2 * (1. + Lz2 / Lxy2);
    simple_calcTangents(currenthelicity,
                        pos[0], pos[1], pos[2],
                        pos[3], pos[4], pos[5],
                        p2, tmp_pT2,
                        tangent1, tangent2);
    /// pos[0], pos[1], pos[2] : hit cluster position at the innermost layer
    /// pos[3], pos[4], pos[5] : hit cluster position at the second layer
    mom3[0] = tangent1[0];
    mom3[1] = tangent1[1];
    mom3[2] = ( pos[2] < pos[5] ) ? sqrt(p2 - tmp_pT2) : -sqrt(p2 - tmp_pT2);
    currentmom = cP;
    momtot = sqrt(p2);

    /// calculate probability
    int ndf = (nhits - 2) * 2 - 1;

    double LogProb;
    if ( TMath::Prob(chi, ndf) != 0. )
    {
        LogProb = TMath::Log(TMath::Prob(chi, ndf));
    }
    else
    {
        LogProb = -45.;
    }

    return LogProb;
}

double SvxTracker::TrackFitCore(int nlength, FitInfoContainer *fitinfo)
///
/// Fitting function commonly used for both SvxSegment and SvxCentralTrack.
///
/// INPUT
//    n : array length
//    FitInfoContainer : contain several info
//
///   vcluster : pointers of SvxCluster of associated hits
///              first entry is for innermost hit.
///              last entry is for outermost hit.
///   helicity : helicity of rotation
///   pT       : transverse momentum
///
///
/// RETURN : 
///   chisq     : chi-square
///
{
    Reset();

    double ftol = m_tolerance;
    double pT   = sqrt(fitinfo->pT2);

    int n = nlength;
    double mat_ddparam[n][n];
    vector<double> dparam_t(n);
    vector<double> dparam_tt(n);
 
    //vector<double> dparam(n, 0.);
    //vector<double> ddparam(n);
    /// mat_ddparam : ddparam for each of parameters deviated in fitting is stored.
    ///               mat_ddparam[j][i] is ddparam[i] of j-th direction.
    ///               The set of the directions will be changed during the fitting.
    /// dparam      : shift of parameters is stored.
    ///               dparam[i] is for i-th parameter.
    /// dparam_t    : Temporary storage of parameter shift.
    ///               Result at the previous iteration is stored.
    /// dparam_tt   : 2*dparam - dparam_t
    /// ddparam     : Direction of changing of dparam is stored.

    ///
    /// initailization of mat_ddparam
    ///
    for ( int i = 0; i < n; i++ )
    {
        for ( int j = 0; j < n; j++ )
        {
            mat_ddparam[j][i] = 0.;
        }
        
        if( fitinfo->isSvxCntTrackFitting ==0 ) // for segment
        {
          if ( i == n - 1 )        /// for pT
          {
              mat_ddparam[i][i] = 0.2 * pT;
          }
          else if ( i % 2 == 0 )   /// for local x direction
          {
              mat_ddparam[i][i] = m_sdphil[fitinfo->vcluster[i / 2]->get_layer()];
          }
          else                     /// for local z direction
          {
              mat_ddparam[i][i] = m_sdzl[fitinfo->vcluster[(i - 1) / 2]->get_layer()];
          }
        }
        else { // for SvxCentralTrack
          if ( i % 2 == 0 )   /// for local x direction
          {
              mat_ddparam[i][i] = m_sdphil[fitinfo->vcluster[i / 2]->get_layer()];
          }
          else                     /// for local z direction
          {
              mat_ddparam[i][i] = m_sdzl[fitinfo->vcluster[(i - 1) / 2]->get_layer()];
          }
        }
    }

    // calculate initial chi2 value using cluster position
    double chi = fitinfo->calculate_ChiSquare(this, fitinfo->dparam); // calculate chi2 w/ 0 shift

    // if SvxCentralTrack doesn't have enough number of hit when running w/ fit_noCNT. 
    if( fitinfo->isSvxCntTrackFitting == 1 && 
        fitinfo->m_isFitNoCNT == 1 && // 
        fitinfo->vcluster.size()<=2)
    { 
      return chi;
    }


    // start fitting
    for ( int j = 0; j < n; j++ )
    {
        dparam_t[j] = fitinfo->dparam[j];
    }

    for ( int itr = 0; itr < m_nitr; itr++ )
    {
        ///
        /// search minimum chi
        ///
        double chi0 = chi;
        int ibig = 0;
        double del = 0.;
        for ( int i = 0; i < n; i++ )
        {
            for ( int j = 0; j < n; j++ )
            {
                fitinfo->ddparam[j] = mat_ddparam[j][i];
            }
            double tmp_chi = chi;
            chi = LineMinimizationCore(fitinfo);
            /// Optimum dparam is found by changing it along with ddparam direction.
            /// dparam is updated to the optimum one.
            /// ddparam is also updated by multiplie optimum length (direction is not
            /// changed.) but this update is not used here.
            if ( tmp_chi - chi > del )
            {
                del = tmp_chi - chi;
                ibig = i;
            } // if : tmp_chi-chi>del
        } // for : i

        if ( (chi0 - chi) < ftol * 0.5 * (fabs(chi0) + fabs(chi)) )
        {
            /// if improvement of chi2 is enough small, stop iteration.
            break;
        }

        // var replace
        for ( int j = 0; j < n; j++ )
        {
            dparam_tt[j]        = 2.*fitinfo->dparam[j] - dparam_t[j];
            fitinfo->ddparam[j] = fitinfo->dparam[j] - dparam_t[j];
            dparam_t[j]         = fitinfo->dparam[j];
        } // for : i

        double chi_tt = fitinfo->calculate_ChiSquare(this, dparam_tt);

        if ( chi_tt < chi0 )
        {
            double t = 2.*(chi0 - 2.*chi + chi_tt) * (chi0 - chi - del) * (chi0 - chi - del)
                       - del * (chi0 - chi_tt) * (chi0 - chi_tt);
            if ( t < 0. )
            {
                chi = LineMinimizationCore(fitinfo);

                /// Optimum dparam is found by changing parameters in ddparam direction.
                /// dparam is updated to the optimum one.
                /// ddparam is also updated by multiplying optimum length. (direction is not
                /// changed.)
                for ( int j = 0; j < n; j++ )
                {
                    /// ddparam is updated.
                    /// ibig direction, which is the direction which makes the largest decrease
                    /// of chi-square, is discarded and replaced to new direction
                    ///   - new direction : (min. at this iteration) - (min. at last iteration)
                    /// dparam is also replaced to min. at this iteration (in LineMinim_SvxCNT).
                    mat_ddparam[j][ibig] = mat_ddparam[j][n - 1];
                    mat_ddparam[j][n - 1] = fitinfo->ddparam[j];
                } // for : j
            } // if : t<0
        } // if : chi_tt<chi0
    } // for : itr

    return chi;
}

void SvxTracker::calculateMomentumAtInnerMost(FitInfoContainer *fitinfo)
{
    vector<double> tangent1(2);   /// tangent vector (px, py) at the first point
    vector<double> tangent2(2);   /// tangent vector (px, py) at the second point
    float pos[6];
    int layer0 = fitinfo->vcluster[0]->get_layer();
    int layer1 = fitinfo->vcluster[1]->get_layer();
    getExpectedPosition(layer0, pos[0], pos[1], pos[2], 0);
    if ( layer0 != layer1 )
    {
        getExpectedPosition(layer1, pos[3], pos[4], pos[5], 0);
    }
    else
    {
        getExpectedPosition(layer0, pos[3], pos[4], pos[5], 1);
    }
    double Lxy2 = (pos[0] - pos[3]) * (pos[0] - pos[3]) + (pos[1] - pos[4]) * (pos[1] - pos[4]);
    double Lz2  = (pos[2] - pos[5]) * (pos[2] - pos[5]);
    double p2 = fitinfo->pT2 * (1. + Lz2 / Lxy2);
    simple_calcTangents(fitinfo->helicity,
                        pos[0], pos[1], pos[2],
                        pos[3], pos[4], pos[5],
                        p2, fitinfo->pT2,
                        tangent1, tangent2);
    mom3[0] = tangent1[0];
    mom3[1] = tangent1[1];
    mom3[2] = ( pos[2] < pos[5] ) ? sqrt(p2 - fitinfo->pT2) : -sqrt(p2 - fitinfo->pT2);
    momtot  = sqrt(p2);
}

double SvxTracker::calculateLogProb(const double chi, const int ndf)
{
  static const double MinimumProb = pow(TMath::E(), -45);

  double prob = TMath::Prob(chi, ndf);

  double LogProb = ( prob > MinimumProb ) ? TMath::Log(prob) : -45.;

  return LogProb;
}

double SvxTracker::TrackFit(const vector<SvxCluster *> &vcluster,
                            bool helicity, double pT, float &chisq, int &ndf)
///
/// Fitting function of SvxSegment.
///
/// INPUT
///   vcluster : pointers of SvxCluster of associated hits
///              first entry is for innermost hit.
///              last entry is for outermost hit.
///   helicity : helicity of rotation
///   pT       : transverse momentum
///
/// OUTPUT
///   chisq     : chi-square
///   ndf       : number of degree of freedom
///
/// RETURN : probalility calculated from chisq & ndf (larger is better)
///   The return value can be calculated from chisq and ndf, so this is not necessary.
///
{
    int n = 2 * vcluster.size() + 1;

    /// dparam      : shift of parameters is stored.
    ///               dparam[i] is for i-th parameter.
    /// ddparam     : Direction of changing of dparam is stored.
    vector<double> dparam(n, 0.);
    vector<double> ddparam(n);

    double pT2 = pT * pT;
    FitInfoContainer fitinfo(0, vcluster, helicity, pT2, -9999, -9999, -9999, -9999, dparam, ddparam);

    //--------------------------
    // Fitting
    double chi = TrackFitCore(n, &fitinfo);

    //--------------------------
    /// set expected hit position calculated at the fitting.
    setExpectedPosition(vcluster, dparam);

    //--------------------------
    /// calculate momentum vector at the innermost hit
    fitinfo.pT2 = (pT + fitinfo.dparam[n - 1]) * (pT + fitinfo.dparam[n - 1]);

    calculateMomentumAtInnerMost(&fitinfo);

    //--------------------------
    // fill output values
    chisq = chi; // convert to float
    ndf   = (fitinfo.vcluster.size() - 2) * 2 - 1;

    double LogProb = calculateLogProb(chi, ndf);

    //--------------------------
    //return large negative value if pT2==0. Since if pT2==0, tangent is nan.
    //if (pT2 == 0) return -9999;
    //D. McGlinchey 9/18/2014 - testing floats with equality is not good
    // change to check for lower bound
    if (fabs(fitinfo.pT2) < 0.01)
        return -9999;

    return LogProb;
}

double SvxTracker::TrackFit_SvxCNT(const vector<SvxCluster *> &vcluster,
                                   bool helicity,
                                   double pT, double phi0, double theta0,
                                   double cax, double cay,
                                   double &chisq, int &ndf,
                                   double &chi_phi, double &chi_z, double &chi_angle)
///
/// Fitting function of SvxCentralTrack.
///
/// INPUT
///   vcluster : pointers of SvxCluster of associated hits
///              first entry is for innermost hit.
///              last entry is for outermost hit.
///   helicity : helicity of rotation
///   pT       : transverse momentum
///   phi0     : azimuthal angle at the closest approach (used for CNT projection)
///   theta0   : polar angle at the closest approach (used for CNT projection)
///   cax, cay : position of closest approach (used for CNT projection)
///
/// OUTPUT
///   chisq     : chi-square
///   ndf       : number of degree of freedom
///   chi-phi   : residual of phi-direction part in chi-square
///   chi-z     : residual of z-direction part in chi-square
///   chi_angle : other part of chi-square including scattering angle & pT
///
/// RETURN : probalility calculated from chisq & ndf (larger is better)
///   The return value can be calculated from chisq and ndf, so this is not necessary.
///
{
    int n = 2 * vcluster.size();

    /// dparam      : shift of parameters is stored.
    ///               dparam[i] is for i-th parameter.
    /// ddparam     : Direction of changing of dparam is stored.
    vector<double> dparam(n, 0.);
    vector<double> ddparam(n);

    double pT2 = pT * pT;
    FitInfoContainer fitinfo(1, vcluster, helicity, pT2, phi0, theta0, cax, cay, dparam, ddparam);
    fitinfo.m_isFitNoCNT = m_isFitNoCNT; 

    //--------------------------
    // Fitting
    double chi = TrackFitCore(n, &fitinfo);

    //--------------------------
    /// set expected hit position calculated at the fitting.
    setFitResult_SvxCNT(helicity, pT2, phi0, theta0, cax, cay, vcluster, dparam);

    //--------------------------
    /// calculate momentum vector at the innermost hit
    //pT2 = (pT+dparam[n-1])*(pT+dparam[n-1]);
    calculateMomentumAtInnerMost(&fitinfo);

   
    //--------------------------
    // calculate chi2 w/o cnt angle
    m_chi2_svxcnt_nocnt = fitinfo.calculate_ChiSquare_NoCNT(this);
    m_ndf_svxcnt_nocnt  = (fitinfo.vcluster.size()-2)*2;

    //--------------------------
    /// decompose chi-square
    chi_phi = 0.;
    chi_z = 0.;
    int nhit[4] = {0, 0, 0, 0};
    for ( unsigned int i = 0; i < vcluster.size(); i++ )
    {
        int layer = vcluster[i]->get_layer();
        /// get position of fitting the result
        float x;
        float y;
        float z;
        getExpectedPosition(layer, x, y, z, nhit[layer]);
        nhit[layer] ++;
        /// get cluster position
        float xcls = vcluster[i]->get_xyz_global(0);
        float ycls = vcluster[i]->get_xyz_global(1);
        float zcls = vcluster[i]->get_xyz_global(2);
        chi_phi += ((x - xcls) * (x - xcls) + (y - ycls) * (y - ycls)) / (m_sdphil[layer] * m_sdphil[layer]);
        chi_z += (z - zcls) * (z - zcls) / (m_sdzl[layer] * m_sdzl[layer]);
    }
    chi_angle = chi - chi_phi - chi_z;


    //--------------------------
    // fill output values
    chisq = chi; // convert to float. logically unnecessary, 
                 // but to keep the interface and precision to be same . 2014.12.11 T.Hachiya
    ndf   = (fitinfo.vcluster.size() - 2) * 2 + 2;
    if(fitinfo.m_isFitNoCNT){
      ndf = m_ndf_svxcnt_nocnt;
    }

    double LogProb = calculateLogProb(chi, ndf);

    //--------------------------
    return LogProb;
}

/*
double SvxTracker::TrackFit_SvxCNT(const vector<SvxCluster *> &vcluster,
                                   bool helicity,
                                   double pT, double phi0, double theta0,
                                   double cax, double cay,
                                   float &chisq, int &ndf)
///
/// Fitting function of SvxCentralTrack.
///
/// INPUT
///   vcluster : pointers of SvxCluster of associated hits
///              first entry is for innermost hit.
///              last entry is for outermost hit.
///   helicity : helicity of rotation
///   pT       : transverse momentum
///   phi0     : azimuthal angle at the closest approach
///   theta0   : polar angle at the closest approach
///   cax, cay : position of closest approach (used for CNT projection)
///
/// OUTPUT
///   chisq     : chi-square
///   ndf       : number of degree of freedom
///
/// RETURN : probalility calculated from chisq & ndf (larger is better)
///   The return value can be calculated from chisq and ndf, so this is not necessary.
///
{
    int n = 2 * vcluster.size();

    /// dparam      : shift of parameters is stored.
    ///               dparam[i] is for i-th parameter.
    /// ddparam     : Direction of changing of dparam is stored.
    vector<double> dparam(n, 0.);
    vector<double> ddparam(n);

    double pT2 = pT * pT;
    FitInfoContainer fitinfo(1, vcluster, helicity, pT2, phi0, theta0, cax, cay, dparam, ddparam);

    //--------------------------
    // Fitting
    double chi = TrackFitCore(n, &fitinfo);

    //--------------------------
    /// set expected hit position calculated at the fitting.
    setFitResult_SvxCNT(helicity, pT2, phi0, theta0, cax, cay, vcluster, dparam);

    //--------------------------
    /// calculate momentum vector at the innermost hit
    //pT2 = (pT+dparam[n-1])*(pT+dparam[n-1]);
    calculateMomentumAtInnerMost(&fitinfo);

    //--------------------------
    // fill output values
    chisq = chi; // convert to float. logically unnecessary, 
                 // but to keep the interface and precision to be same . 2014.12.11 T.Hachiya
    ndf   = (fitinfo.vcluster.size() - 2) * 2 + 2;

    double LogProb = calculateLogProb(chi, ndf);

    //--------------------------
    return LogProb;
}
*/

double SvxTracker::LineMinimizationCore(FitInfoContainer *fitinfo)
///
/// - Minimization function with line minimization method in one dimension.
/// - Parameters are shifted by dparam+dl*ddparam and search minimum chi-square
///   by changing dl.
/// - dparam is modified to dparam+dlmin*ddparam
/// - ddparam is modified to dlmin*ddparam
///    --- dlmin : dl which minimize chi-square
/// - Algorithm is learned from the textbook "numerical recipes in C" section 10.1 
//    "Routine for Initially Bracketing a Minimum"
///
/// INPUT (FitInfoContainer contains)
///   vcluster : pointers of SvxCluster of associated hits
///              first entry is for innermost hit.
///              last entry is for outermost hit.
///   helicity : helicity of rotation
///   pT2      : square of transverse momentum
///   dparam   : base shift of parameters.
///     * dparam is changed along with ddparam and optimum shift is calculated
///       in the function. dparam is modified to the optimum.
///   ddparam  : direction with length defining shift of parameters
///     * The changing of dparam which minimize chi-square is calculated
///       in the function and ddparam is modified to the changing of dparam.
///
///   phi0     : only for SvxCNT : azimuthal angle at the closest approach 
///   theta0   : only for SvxCNT : polar angle at the closest approach
///   cax, cay : only for SvxCNT : position of closest approach (used for CNT projection)
///
/// RETURN : chi-square
///
{
    int n = fitinfo->dparam.size();
    double dl1 = 0.;
    double dl2 = 1.;
    double dl3;
    //vector<double> tmp_dparam;

    /// bracket
    double chi1 = fitinfo->calculate_ChiSquare(this, dl1);
    double chi2 = fitinfo->calculate_ChiSquare(this, dl2);

    if ( chi2 > chi1 )
    {
        /// swap dl1 <-> dl2
        double tmp = dl1;
        dl1 = dl2;
        dl2 = tmp;
        /// swap chi1 <-> chi2
        tmp = chi1;
        chi1 = chi2;
        chi2 = tmp;
    }

    static const double GOLD = 1.618034L;
    static const double GLIMIT = 10.0L;
    static const double TINY = 1.e-20;

    dl3 = dl2 + GOLD * (dl2 - dl1);
    double chi3 = fitinfo->calculate_ChiSquare(this, dl3);
    double chiu;

    while ( chi2 > chi3 )
    {
        double r = (dl2 - dl1) * (chi2 - chi3);
        double q = (dl2 - dl3) * (chi2 - chi1);
        double dlu;
        if ( fabs(q - r) > TINY ) /// to avoid to devide by 0
        {
            dlu = dl2 - ((dl2 - dl3) * q - (dl2 - dl1) * r) / (2.*(q - r));
        }
        else if ( q - r > 0. )
        {
            dlu = dl2 - ((dl2 - dl3) * q - (dl2 - dl1) * r) / (2.*TINY);
        }
        else
        {
            dlu = dl2 + ((dl2 - dl3) * q - (dl2 - dl1) * r) / (2.*TINY);
        }
        double dlulim = dl2 + GLIMIT * (dl3 - dl2);

        if ( (dl2 - dlu) * (dlu - dl3) > 0. )
        {
            chiu = fitinfo->calculate_ChiSquare(this, dlu);
            if ( chiu < chi3 )
            {
                dl1 = dl2;
                dl2 = dlu;
                chi1 = chi2;
                chi2 = chiu;
                break;
            }
            else if ( chiu > chi2 )
            {
                dl3 = dlu;
                chi3 = chiu;
                break;
            }
            dlu = dl3 + GOLD * (dl3 - dl2);
        }
        else if ( (dl3 - dlu) * (dlu - dlulim) > 0. )
        {
            chiu = fitinfo->calculate_ChiSquare(this, dlu);
            if ( chiu < chi3 )
            {
                dl2 = dl3;
                dl3 = dlu;
                dlu = dlu + GOLD * (dlu - dl3);
                chi2 = chi3;
                chi3 = chiu;
            }
        }
        else if ( (dlu - dlulim) * (dlulim - dl3) >= 0. )
        {
            dlu = dlulim;
        }
        else
        {
            dlu = dl3 + GOLD * (dl3 - dl2);
        }

        dl1 = dl2;
        dl2 = dl3;
        dl3 = dlu;
        chi1 = chi2;
        chi2 = chi3;
        chi3 = fitinfo->calculate_ChiSquare(this, dlu);
    }

    double dlmin = BrentMinimizeCore(dl1, dl2, dl3, chi1, chi2, chi3, fitinfo);


    for ( int i = 0; i < n; i++ )
    {
        fitinfo->ddparam[i] *= dlmin;
        fitinfo->dparam[i] += fitinfo->ddparam[i];
    }
    //double pT = sqrt(pT2);
    //if ( pT+dparam[n-1]<0. ) {
    //  dparam[n-1] = -pT;
    //}

    return fitinfo->calculate_ChiSquare(this, fitinfo->dparam);
    /// calculate again to set member valuable (mom3, momtot, scatter, etc...) correctly.
}

double SvxTracker::LineMinimization(const vector<SvxCluster *> &vcluster,
                                    bool helicity, double pT2,
                                    vector<double> &dparam, vector<double> &ddparam)
{
  FitInfoContainer fitinfo(0, vcluster, helicity, pT2, -9999, -9999, -9999, -9999, dparam, ddparam);
  return LineMinimizationCore(&fitinfo);
}
double SvxTracker::LineMinim_SvxCNT(const vector<SvxCluster *> &vcluster,
                                    bool helicity,
                                    double pT2, double phi0, double theta0,
                                    double cax, double cay,
                                    vector<double> &dparam, vector<double> &ddparam)
{
  FitInfoContainer fitinfo(1, vcluster, helicity, pT2, phi0, theta0, cax, cay, dparam, ddparam);
  return LineMinimizationCore(&fitinfo);
}


double SvxTracker::BrentMinimizeCore(double x1, double x2, double x3,
                                     double f1, double f2, double f3,
                                     FitInfoContainer *fitinfo
                                 )
///
/// - Minimization function with Brent's method
/// - Parameters are shifted by
///      dparam + dl * ddparam
///   and find dl which minimize chi-square.
/// - Minimization is done with parabolic interpolation and between (x1,f1) and (x3,f3).
///
/// INPUT
///   (x*, f*) : f* is chi-sqaure when dl=x*
///   vcluster : pointers of SvxCluster of associated hits
///              first entry is for innermost hit.
///              last entry is for outermost hit.
///   helicity : helicity of rotation
///   pT2      : square of transverse momentum
///   dparam   : base shift of parameters
///   ddparam  : direction of parameter shift
///
/// RETURN : dl which minimize chi-square
///
//
//
// this brent minimum method is learned from the chapter 10.2 of the book "Numerical recipes in C"
// The link to web is
//   http://www2.units.it/ipl/students_area/imm2/files/Numerical_Recipes.pdf
//   chapter10.2 : 10.2 Parabolic Interpolation and Brent's Method in One Dimension
//
{
    static const double CGOLD = 0.381966L;
    double tol = m_tolerance;

    double a = ( x1 < x3 ) ? x1 : x3;
    double b = ( x1 > x3 ) ? x1 : x3;
    double x = x2;   /// with the minimum chi
    double w = x2;   /// with the second minimum chi
    double v = x2;   /// the previous w
    double u;        /// evaluated most recently
    double fx = f2;
    double fw = f2;
    double fv = f2;
    double fu;

    double tol1;
    double tol2;
    double d = 0.;
    double e = 0.;

    for ( int itr = 0; itr < m_nitr; itr++ )
    {
        double xm = 0.5 * (a + b);
        tol1 = tol * fabs(x);
        tol2 = 2.*tol1;
        //    if ( fabs(x-xm)<=tol2-0.5*(b-a) ) {}
        if ( fx != fw && fabs(fx - fw) < tol * 0.5 * (fabs(fx) + fabs(fw)) )
        {
            return x;
        }
        if ( fabs(e) > tol1 )
        {
            double r = (x - w) * (fx - fv);
            double q = (x - v) * (fx - fw);
            double p = (x - v) * q - (x - w) * r;
            if ( q > r ) p = -p;
            double etmp = e;
            //double e;//-- = d; --> this must be problem. the 2nd variable "e" w/ the different scope is made.  should be checked
            e = d; //--> this must be problem. the 2nd variable "e" w/ the different scope is made.  should be checked
            if ( fabs(p) >= fabs((q - r)*etmp) || p <= 2.*fabs(q - r) * (a - x) || p >= 2.*fabs(q - r) * (b - x) )
            {
                e = ( x >= xm ) ? a - x : b - x;
                d = CGOLD * e;
            }
            else       /// parabolic fit
            {
                d = 0.5 * p / fabs(q - r);
                u = x + d;
                if ( u - a < tol2 || b - u < tol2 ) d = ( xm > x ) ? tol1 : -tol1;
            }
        }
        else
        {
            e = ( x >= xm ) ? a - x : b - x;
            d = CGOLD * e;
        }

        u = ( fabs(d) >= tol1 ) ? x + d : (( d > 0 ) ? x + tol1 : x - tol1);
        fu = fitinfo->calculate_ChiSquare(this, u);
        if ( fu <= fx )
        {
            if ( u >= x )
            {
                a = x;
            }
            else
            {
                b = x;
            }
            v = w;
            w = x;
            x = u;
            fv = fw;
            fw = fx;
            fx = fu;
        }
        else
        {
            if ( u < x )
            {
                a = u;
            }
            else
            {
                b = u;
            }
            if ( fu <= fw || fabs(w - x)< 0.0000000001L ) //D. McGlinchey - should not be checking for float equality with ==  -> fixed T.H (2014.12.11)
            {
                v = w;
                w = u;
                fv = fw;
                fw = fu;
            }
            else if ( fu <= fv || fabs(v - x)<0.0000000001L || fabs(v - w)<0.0000000001L ) //D. McGlinchey - should not be checking for float equality with ==  -> fixed T.H (2014.12.11)
            {
                v = u;
                fv = fu;
            }
        }
    }

    return x;
}

double SvxTracker::BrentMinimize(double x1, double x2, double x3,
                                 double f1, double f2, double f3,
                                 const vector<SvxCluster *> &vcluster,
                                 bool helicity, double pT2,
                                 vector<double> &dparam,
                                 vector<double> &ddparam)
{
  FitInfoContainer fitinfo(0, vcluster, helicity, pT2, -9999, -9999, -9999, -9999, dparam, ddparam);
  return BrentMinimizeCore(x1, x2, x3, f1, f2, f3, &fitinfo);
}

double SvxTracker::BrentMinim_SvxCNT(double x1, double x2, double x3,
                                     double f1, double f2, double f3,
                                     const vector<SvxCluster *> &vcluster,
                                     bool helicity, double pT2,
                                     double phi0, double theta0, double cax, double cay,
                                     vector<double> &dparam,
                                     vector<double> &ddparam)
{
  FitInfoContainer fitinfo(1, vcluster, helicity, pT2, phi0, theta0, cax, cay, dparam, ddparam);
  return BrentMinimizeCore(x1, x2, x3, f1, f2, f3, &fitinfo);
}


void SvxTracker::calculateTangents(bool hel, float x1, float y1, float z1, float x2, float y2, float z2, float P, float &p2, float &dp2dP)
{
    float PoB = P * binv; /// = R^2 (R : diameter of rotation)
    float dz = z2 - z1;
    float dz2 = dz * dz;
    float d2 = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);
    float d = sqrt(d2);
    float dinv = 1. / d;
    float d2inv = dinv * dinv;
    float temp1 = 1. - 0.5 * d2 / PoB; /// = 1 - 2*sin(theta/2)^2
    float dtheta;
    if (temp1 > 1.)
    {
        dtheta = 0.;
    }
    else if (temp1 <= -1.)
    {
        dtheta = 3.14159265358979312;
    }
    else
    {
        dtheta = acos(temp1);
    }
    temp1 = 1. - temp1 * temp1;
    if (temp1 <= 0.)
    {
        temp1 = 1.0e30;
    }
    else
    {
        temp1 = 1. / sqrt(temp1);
    }
    float ddthetadP = (-0.5 * temp1) * (d2 / (PoB * P));
    /// d(dtheta)/dP = - sin(dtheta)^(-1) * d(cos(dtheta))/dP = -tan(dtheta/2) / pT^2

    float dtheta_inv;
    if (dtheta == 0.)//D. McGlinchey - should not be checking for float equality with ==
    {
        dtheta_inv = 1.0e30;
    }
    else
    {
        dtheta_inv = 1. / dtheta;
    }
    float dzdt = dz * dtheta_inv; /// = dz / dtheta
    float ddzdtdP = -dzdt * dtheta_inv * ddthetadP;
    /// d(dz/dtheta)/dP = dz * (-1) * dtheta^(-2) * d(dtheta)/dP

    p2 = P + dz2 / (binv * dtheta * dtheta); /// = (1 + (dz/(R*dtheta))^2)*pT^2
    dp2dP = 1. - (2.*dz2 / (binv * dtheta * dtheta * dtheta)) * ddthetadP;
    /// d((1+(dz/(R*dtheta))^2)*pT^2)/dP = 1 + (-2) * (dz^2 / binv) * dtheta^(-3) * d(dtheta)/dP

    float dx = (x2 - x1);
    float dy = (y2 - y1);


    float dir = 1.;
    if (hel == false)
    {
        dir = -1.;
    }

    float temp2 = PoB * d2inv - 0.25; /// = 0.25*((sin(dtheta/2))^-2 - 1) = 0.25 * tan(dtheta/2)^-2
    if (temp2 <= 0.)
    {
        temp1 = 0.;
    }
    else
    {
        temp1 = sqrt(temp2);   /// temp1 = 1 / (2 * tan(dtheta/2))
    }
    temp2 = 1. / temp1; /// temp2 = 2 * tan(dtheta/2)
    float cx = 0.5 * (x1 + x2) - dir * temp1 * dy;
    float cy = 0.5 * (y1 + y2) + dir * temp1 * dx;
    /// (cx, cy) : center of rotation
    float dcxdP = -dir * 0.5 * dy * temp2 * binv * d2inv; /// d(cx)/dP = -dir * d(temp1)/dP * dy
    float dcydP = dir * 0.5 * dx * temp2 * binv * d2inv; /// d(cy)/dP =  dir * d(temp1)/dP * dx

    temp1 = 1. / (x1 - cx);
    temp2 = (y1 - cy);
    float theta0 = atan2(y1 - cy, x1 - cx);
    float dtheta0dP = (1. / (1. + temp2 * temp2 * temp1 * temp1)) * (-dcydP * temp1 + temp2 * temp1 * temp1 * dcxdP);

    float sint0 = sin(theta0);
    float cost0 = cos(theta0);
    temp1 = sqrt(PoB);   /// = R
    temp2 = 0.5 * binv / temp1; /// = 0.5 * R / pT^2
    t[0] = -dir * temp1 * sint0;
    dtdP[0] = dir * (-temp2 * sint0 - temp1 * cost0 * dtheta0dP);
    t[1] = dir * temp1 * cost0;
    dtdP[1] = dir * (temp2 * cost0 - temp1 * sint0 * dtheta0dP);
    t[2] = dzdt;
    dtdP[2] = ddzdtdP;
    sint0 = sin(theta0 + dir * dtheta);
    cost0 = cos(theta0 + dir * dtheta);
    t[3] = -dir * temp1 * sint0;
    dtdP[3] = dir * (-temp2 * sint0 - temp1 * cost0 * (dtheta0dP + dir * ddthetadP));
    t[4] = dir * temp1 * cost0;
    dtdP[4] = dir * (temp2 * cost0 - temp1 * sint0 * (dtheta0dP + dir * ddthetadP));
    t[5] = t[2];
    dtdP[5] = dtdP[2];
}




void SvxTracker::probDers(bool hel, const vector<float> &pos, const vector<int> &is_strip, float P, float &prob, float &dprobdP)
{
    float tprob = 0.;
    float tdprobdP = 0.;
    float p2;
    float dp2dP;

    vector<float> temptangent1, temptangent2;
    vector<float> dtemptangent1dP, dtemptangent2dP;
    vector<float> innertangent;


    for (unsigned int i = 0; i < (is_strip.size()); i++)
    {
        float theta0_r, theta0_z, theta0_p;
        float dr, dz;
        float del;
        if (is_strip[i] == 0)
        {
            dr = 0.005;
            dz = 0.0425;
        }
        else
        {
            dr = 0.008;
            dz = 0.1;
        }
        dr *= 0.5;
        dz *= 0.5;
        del = sqrt((pos[3 * (i + 1)] - pos[3 * (i + 2)]) * (pos[3 * (i + 1)] - pos[3 * (i + 2)])
                   + (pos[3 * (i + 1) + 1] - pos[3 * (i + 2) + 1]) * (pos[3 * (i + 1) + 1] - pos[3 * (i + 2) + 1]));
        theta0_r = atan(dr / del);
        del = sqrt((pos[3 * (i + 1)] - pos[3 * (i + 2)]) * (pos[3 * (i + 1)] - pos[3 * (i + 2)])
                   + (pos[3 * (i + 1) + 1] - pos[3 * (i + 2) + 1]) * (pos[3 * (i + 1) + 1] - pos[3 * (i + 2) + 1])
                   + (pos[3 * (i + 1) + 2] - pos[3 * (i + 2) + 2]) * (pos[3 * (i + 1) + 2] - pos[3 * (i + 2) + 2]));
        theta0_z = atan(dz / del);

        if (is_strip[i] == 0)
        {
            theta0_p = theta0p_pixel;
        }
        else
        {
            theta0_p = theta0p_strip;
        }
        /// theta0p_pixel (theta0p_strip) : width of straggling at pixel (stripixel) detector

        float theta0_p2 = theta0_p * theta0_p;

        calculateTangents(hel, pos[3 * i], pos[3 * i + 1], pos[3 * i + 2], pos[3 * (i + 1)], pos[3 * (i + 1) + 1], pos[3 * (i + 1) + 2], P, p2, dp2dP);

        if ( i == 0 )
        {
            innertangent = t;
            momtot = sqrt(p2);
        }
        float p4_inv = 1. / (p2 * p2);

        float sigma_r = sqrt(theta0_p2 / p2 + theta0_r * theta0_r);
        float sigma_z = sqrt(theta0_p2 / p2 + theta0_z * theta0_z);

        float sigma_rinv = 1. / sigma_r;
        float sigma_zinv = 1. / sigma_z;

        float dsigma_rdP = -0.5 * sigma_rinv * theta0_p2 * p4_inv * dp2dP;
        float dsigma_zdP = -0.5 * sigma_zinv * theta0_p2 * p4_inv * dp2dP;

        temptangent1 = t;
        dtemptangent1dP = dtdP;

        float x_in;
        float y_in;
        float z_in;

        x_in = temptangent1[3];
        y_in = temptangent1[4];
        z_in = temptangent1[5];
        /// (x_in, y_in, z_in) :
        ///         momentum vector at (pos[3*i+3], pos[3*i+4], pos[3*i+5])
        ///         expected with transverse momentum = sqrt(P)

        float dx_indP;
        float dy_indP;
        float dz_indP;

        dx_indP = dtemptangent1dP[3];
        dy_indP = dtemptangent1dP[4];
        dz_indP = dtemptangent1dP[5];
        /// (dx_indP, dy_indP, dz_indP) = d(x_in, y_in, z_in)/dP

        calculateTangents(hel, pos[3 * (i + 1)], pos[3 * (i + 1) + 1], pos[3 * (i + 1) + 2], pos[3 * (i + 2)], pos[3 * (i + 2) + 1], pos[3 * (i + 2) + 2], P, p2, dp2dP);
        temptangent2 = t;
        dtemptangent2dP = dtdP;

        float x_out;
        float y_out;
        float z_out;

        x_out = temptangent2[0];
        y_out = temptangent2[1];
        z_out = temptangent2[2];
        /// (x_out, y_out, z_out) :
        ///         momentum vector at (pos[3*(i+1)], pos[3*(i+1)+1], pos[3*(i+1)+2])
        ///         expected with transverse momentum = sqrt(P)

        float dx_outdP;
        float dy_outdP;
        float dz_outdP;

        dx_outdP = dtemptangent2dP[0];
        dy_outdP = dtemptangent2dP[1];
        dz_outdP = dtemptangent2dP[2];
        /// (dx_outdP, dy_outdP, dz_outdP) = d(x_out, y_out, z_out)/dP

        float r_in;
        r_in = sqrt(x_in * x_in + y_in * y_in);
        float r_in_inv;
        r_in_inv = 1. / r_in;

        float dr_indP;
        dr_indP = r_in_inv * (x_in * dx_indP + y_in * dy_indP);

        float temp1;

        float D;
        D = (x_in * x_out + y_in * y_out) * r_in_inv * r_in_inv;

        float dDdP;
        dDdP = (x_in * dx_outdP + dx_indP * x_out + y_in * dy_outdP + dy_indP * y_out) * r_in_inv * r_in_inv;
        temp1 = r_in_inv * r_in_inv;
        temp1 *= -temp1;
        temp1 *= 2.*(x_in * dx_indP + y_in * dy_indP);
        temp1 *= (x_in * x_out + y_in * y_out);
        dDdP += temp1;

        float temp3;
        float temp4;

        float r_out;
        temp3 = sqrt(x_out * x_out + y_out * y_out);
        temp4 = 1. / temp3;
        r_out = D * temp3;

        float dr_outdP;
        dr_outdP = dDdP * temp3 + D * temp4 * (x_out * dx_outdP + y_out * dy_outdP);

        float F;
        F = sqrt((x_in * x_in + y_in * y_in) / (x_out * x_out + y_out * y_out));

        float dFdP;
        temp3 = 1. / (x_out * x_out + y_out * y_out);
        temp4 = 1. / (F);
        dFdP = temp4 * (temp3 * (x_in * dx_indP + y_in * dy_indP) - temp3 * temp3 * (x_in * x_in + y_in * y_in) * (x_out * dx_outdP + y_out * dy_outdP));

        float theta_r;
        temp1 = D * F;
        if (temp1 > 1.)
        {
            theta_r = 0.;
        }
        else if (temp1 < -1.)
        {
            theta_r = 3.14159265358979312;
        }
        else
        {
            theta_r = acos(temp1);
        }

        float dtheta_rdP;
        temp3 = 1. - D * D * F * F;
        if (temp3 <= 0.)
        {
            temp3 = 1.0e30;
        }
        else
        {
            temp3 = 1. / sqrt(temp3);
        }
        dtheta_rdP = -temp3 * (D * dFdP + dDdP * F);

        r_in_inv = 1. / sqrt(r_in * r_in + z_in * z_in);

        float Dz;
        Dz = (r_in * r_out + z_in * z_out) * r_in_inv * r_in_inv;

        float dDzdP;
        dDzdP = (r_in * dr_outdP
                 + dr_indP * r_out
                 + z_in * dz_outdP
                 + dz_indP * z_out) * r_in_inv * r_in_inv;
        temp1 = r_in_inv * r_in_inv;
        temp1 *= -temp1;
        temp1 *= 2.*(r_in * dr_indP + z_in * dz_indP);
        temp1 *= (r_in * r_out + z_in * z_out);
        dDzdP += temp1;

        float Fz;
        Fz = sqrt((r_in * r_in + z_in * z_in) / (r_out * r_out + z_out * z_out));

        float dFzdP;
        temp3 = 1. / (r_out * r_out + z_out * z_out);
        temp4 = 1. / (Fz);
        dFzdP = temp4 * (temp3 * (r_in * dr_indP + z_in * dz_indP) - temp3 * temp3 * (r_in * r_in + z_in * z_in) * (r_out * dr_outdP + z_out * dz_outdP));

        float theta_z;
        temp1 = Dz * Fz;
        if (temp1 > 1.)
        {
            theta_z = 0.;
        }
        else if (temp1 < -1.)
        {
            theta_z = 3.14159265358979312;
        }
        else
        {
            theta_z = acos(temp1);
        }

        float dtheta_zdP;
        temp3 = 1. - Dz * Dz * Fz * Fz;
        if (temp3 <= 0.)
        {
            temp3 = 1.0e30;
        }
        else
        {
            temp3 = 1. / sqrt(temp3);
        }
        dtheta_zdP = -temp3 * (Dz * dFzdP + dDzdP * Fz);

        float prob_r;
        temp1 = sqrt(sigma_rinv);
        prob_r = -theta_r * temp1;

        float dprob_rdP;
        dprob_rdP = -dtheta_rdP * temp1 + 0.5 * theta_r * temp1 * sigma_rinv * dsigma_rdP;

        float prob_z;
        temp1 = sqrt(sigma_zinv);
        prob_z = -theta_z * temp1;

        float dprob_zdP;
        dprob_zdP = -dtheta_zdP * temp1 + 0.5 * theta_z * temp1 * sigma_zinv * dsigma_zdP;


        //     temp1 = exp(-theta_r*theta_r*sigma_rinv*sigma_rinv);
        //     float prob_r = sigma_rinv*temp1;
        //
        //     float dprob_rdP = sigma_rinv*(-sigma_rinv*dsigma_rdP*temp1 + 2.*temp1*theta_r*sigma_rinv*sigma_rinv*( theta_r*sigma_rinv*dsigma_rdP - dtheta_rdP ));
        //
        //     temp1 = exp(-theta_z*theta_z*sigma_zinv*sigma_zinv);
        //     float prob_z = sigma_zinv*temp1;
        //
        //     float dprob_zdP = sigma_zinv*(-sigma_zinv*dsigma_zdP*temp1 + 2.*temp1*theta_z*sigma_zinv*sigma_zinv*( theta_z*sigma_zinv*dsigma_zdP - dtheta_zdP ));




        tprob += prob_r;
        tprob += prob_z;

        tdprobdP += dprob_rdP;
        tdprobdP += dprob_zdP;

        m_scatter[i] = theta_r;
    }

    prob = tprob;
    dprobdP = tdprobdP;

    float scale = 1. / sqrt(innertangent[0] * innertangent[0]
                            + innertangent[1] * innertangent[1]
                            + innertangent[2] * innertangent[2]);
    mom3[0] = momtot * scale * innertangent[0];
    mom3[1] = momtot * scale * innertangent[1];
    mom3[2] = momtot * scale * innertangent[2];
}


double SvxTracker::calculate_ChiSquare(bool hel, double pT2,
                                       const vector<float> &pos,
                                       const vector<int> &is_strip)
/// Calculate log of probability taking into account
/// multiple scattering at each hit points.
///
/// Input
///  - hel      : Helicity
///  - pos      : Global position of all hit points. This function needs at least
///               three hit points.
///  - is_strip : If is_strip=0, hit layer is pixel. If not, hit layer is stripixel.
///               is_strip[i] is for (i+1)-th hit point.
///  - pT2      : Square of transverse momentum.
///
/// Return : chi-square
///
{
    double chi = 0;

    unsigned int nhits = is_strip.size() + 2;

    if ( pos.size() < 9 || nhits < 3 )
    {
        cout << "calculate_ChiSquare ERROR | At least 3 hits are necessary" << endl;
    }

    /// during this function, p2, not pT2, should be fixed.
    double Lxy2 = (pos[0] - pos[3]) * (pos[0] - pos[3]) + (pos[1] - pos[4]) * (pos[1] - pos[4]);
    double Lz2  = (pos[2] - pos[5]) * (pos[2] - pos[5]);
    double p2 = pT2 * (1. + Lz2 / Lxy2);
    /// p = pT*L/(R*a) (L: sqrt(Lxy^2+Lz^2))
    /// sin(a/2)~a/2=Lxy/2R (a: rotation angle; R: rotation diameter)
    /// -> p^2 = pT^2*(1+Lz^2/Lxy^2)

    for ( unsigned int i = 1; i < nhits - 1; i++ )
    {
        vector<double> tangent1(2);   /// tangent vector (px, py) at the first point
        vector<double> tangent2(2);   /// tangent vector (px, py) at the second point
        double tmp_pT2;   /// magnitude of total momentum

        simple_calcTangents(hel,
                            pos[3 * (i - 1)], pos[3 * (i - 1) + 1], pos[3 * (i - 1) + 2],
                            pos[3 * i], pos[3 * i + 1], pos[3 * i + 2],
                            p2, tmp_pT2, tangent1, tangent2);
        double momx_in = tangent2[0];
        double momy_in = tangent2[1];
        double momz_in = ( pos[3 * (i - 1) + 2] < pos[3 * i + 2] ) ? sqrt(p2 - tmp_pT2) : -sqrt(p2 - tmp_pT2);
        /// (momx_in, momy_in) :
        ///         momentum vector at (pos[3*i], pos[3*i+1], pos[3*i+2])
        ///         expected with (pos[3*(i-1)], pos[3*(i-1)+1], pos[3*(i-1)+2]),
        ///         (pos[3*i], pos[3*i+1], pos[3*i+2]),
        ///         and momentum = sqrt(p2)

        simple_calcTangents(hel,
                            pos[3 * i], pos[3 * i + 1], pos[3 * i + 2],
                            pos[3 * (i + 1)], pos[3 * (i + 1) + 1], pos[3 * (i + 1) + 2],
                            p2, tmp_pT2, tangent1, tangent2);
        double momx_out = tangent1[0];
        double momy_out = tangent1[1];
        double momz_out = ( pos[3 * i + 2] < pos[3 * (i + 1) + 2] ) ? sqrt(p2 - tmp_pT2) : -sqrt(p2 - tmp_pT2);
        /// (momx_out, momy_out) :
        ///         momentum vector at (pos[3*i], pos[3*i+1], pos[3*i+2])
        ///         expected with (pos[3*i], pos[3*i+1], pos[3*i+2]),
        ///         (pos[3*(i+1)], pos[3*(i+1)+1], pos[3*(i+1)+2]),
        ///         and momentum of sqrt(p2)

        double dtheta_xy = calc_dangle2D(momx_in, momy_in, momx_out, momy_out);
        double pT_in = sqrt(momx_in * momx_in + momy_in * momy_in);
        double pT_out = sqrt(momx_out * momx_out + momy_out * momy_out);
        double dtheta_rz = calc_dangle2D(pT_in, momz_in, pT_out, momz_out);
        double momx = (momx_in + momx_out) * 0.5;
        double momy = (momy_in + momy_out) * 0.5;
        double momz = (momz_in + momz_out) * 0.5;
        double inj_angle = calc_dangle3D(pos[3 * i], pos[3 * i + 1], 0., momx, momy, momz);
        double theta_p = calc_scattering(inj_angle, is_strip[i - 1], p2);
        /// is_strip[i] is for (i+1)-th hit.
        double theta_p_xy = sqrt(0.5) * theta_p;
        double theta_p_rz = sqrt(0.5) * theta_p;
        /// theta_p : width of straggling at hit point

        chi += (dtheta_xy * dtheta_xy) / (theta_p_xy * theta_p_xy);
        chi += (dtheta_rz * dtheta_rz) / (theta_p_rz * theta_p_rz);
        m_scatter[i] = dtheta_xy;
    }

    return chi;
}


double SvxTracker::calculate_ChiSquare(bool helicity, double pT2,
                                       const vector<SvxCluster *> &vcluster,
                                       const vector<double> &dparam)
{
    double chi = 0.;

    vector<float> pos3d;
    vector<int> is_strip;

    if (d_geometry == NULL)
    {
        cerr << PHWHERE << "ERROR: Cannot locate d_geometry." << endl;
        return 9999;
    }

    int ncls = vcluster.size();
    //  int nhit_layer[4] = {0,0,0,0};
    for ( int icls = 0; icls < ncls; icls++ )
    {
        int layer = vcluster[icls]->get_layer();
        if ( icls != 0 && icls != ncls - 1 )
        {
            /// skip innermost & outermost layers for is_strip
            if ( layer < 2 )
            {
                is_strip.push_back(0);
            }
            else
            {
                is_strip.push_back(1);
            }
        }
        int ladder = vcluster[icls]->get_ladder();
        int sensor = vcluster[icls]->get_sensor();
        SvxSensor *svxsen = d_geometry->GetSensorPtr(layer, ladder, sensor);
        double pos_local[3];
        double pos_global[3];
        /// modify local position
        pos_local[0] = vcluster[icls]->get_xyz_local(0) + dparam[2 * icls];
        pos_local[1] = vcluster[icls]->get_xyz_local(1);
        pos_local[2] = vcluster[icls]->get_xyz_local(2) + dparam[2 * icls + 1];
        /// calculate global position
        svxsen->position_local2global(pos_local, pos_global);
        for ( int i = 0; i < 3; i++ )
        {
            pos3d.push_back(pos_global[i]);
        }
        //double dlx = ( layer<2 ) ? s_dphi[0] : s_dphi[1];
        //double dlz = ( layer<2 ) ? s_dz[0] : s_dz[1];
        chi += (dparam[2 * icls] * dparam[2 * icls]) / (m_sdphil[layer] * m_sdphil[layer]);
        chi += (dparam[2 * icls + 1] * dparam[2 * icls + 1]) / (m_sdzl[layer] * m_sdzl[layer]);
    }

    double pT = sqrt(pT2) + dparam.back();
    pT2 = pT * pT;
    if ( pT < 0 ) helicity = 1 ^ helicity; /// this may happen when momentum is very large.
    chi += calculate_ChiSquare(helicity, pT2, pos3d, is_strip);

    return chi;
}


double SvxTracker::calculate_ChiSquare(bool helicity, double pT2,
                                       const vector<SvxCluster *> &vcluster,
                                       const vector<double> &dparam,
                                       const vector<double> &ddparam,
                                       double dl)
{
    vector<double> tmp_dparam;
    for ( unsigned int i = 0; i < dparam.size(); i++ )
    {
        tmp_dparam.push_back(dparam[i] + ddparam[i]*dl);
    }

    return calculate_ChiSquare(helicity, pT2, vcluster, tmp_dparam);
}


///
/// Next 3 cacl_Chi2_SvxCNT()s are calculate_ChiSquare() for SvxCentralTrack
///
double SvxTracker::calc_Chi2_SvxCNT(bool hel,
                                    double pT2, double phi, double theta,
                                    const vector<double> &pos,
                                    const vector<int> &layerlist)
///
/// Calculate log of probability taking into account
/// multiple scattering at each hit points.
///
/// Input
///  - hel       : helicity
///  - pT2       : square of transverse momentum
///  - phi       : azimuthal angle of momentum vector of CNT projected to outermost hit
///  - theta     : polar angle of momentum vector of CNT projected to outermost hit
///  - pos       : Global position of all hit points. This function needs at least
///                two hit points.
///  - layerlist : layer of each hit
///
/// Return : scattering angle part of chi-square
///
{
    double chi = 0;

    unsigned int nhits = layerlist.size();

    if ( pos.size() < 6 || nhits < 2 )
    {
        cout << "calc_Chi2_SvxCNT ERROR | At least 2 hits are necessary" << endl;
        cout << pos.size() << ", " << nhits << endl;
    }

    /// during this function, p2, not pT2, should be fixed.
    double p2 = pT2 / sin(theta) / sin(theta);

    for ( unsigned int i = 1; i < nhits; i++ )
    {
        double momx_in;
        double momy_in;
        double momz_in;
        /// (momx_in, momy_in, momz_in) :
        ///         incoming momentum vector at (pos[3*i], pos[3*i+1], pos[3*i+2])
        double momx_out;
        double momy_out;
        double momz_out;
        /// (momx_out, momy_out, momz_out) :
        ///         outgoing momentum vector at (pos[3*i], pos[3*i+1], pos[3*i+2])
        vector<double> tangent1(2);   /// tangent vector (px, py) at the first point
        vector<double> tangent2(2);   /// tangent vector (px, py) at the second point
        double tmp_pT2;   /// magnitude of total momentum

        /// incoming
        simple_calcTangents(hel,
                            pos[3 * (i - 1)], pos[3 * (i - 1) + 1], pos[3 * (i - 1) + 2],
                            pos[3 * i], pos[3 * i + 1], pos[3 * i + 2],
                            p2, tmp_pT2, tangent1, tangent2);
        momx_in = tangent2[0];
        momy_in = tangent2[1];
        momz_in = ( pos[3 * (i - 1) + 2] < pos[3 * i + 2] ) ? sqrt(p2 - tmp_pT2) : -sqrt(p2 - tmp_pT2);

        /// outgoing
        if ( i == nhits - 1 )
        {
            momx_out = sqrt(pT2) * cos(phi);
            momy_out = sqrt(pT2) * sin(phi);
            momz_out = sqrt(pT2) / tan(theta);
        }
        else
        {
            simple_calcTangents(hel,
                                pos[3 * i], pos[3 * i + 1], pos[3 * i + 2],
                                pos[3 * (i + 1)], pos[3 * (i + 1) + 1], pos[3 * (i + 1) + 2],
                                p2, tmp_pT2, tangent1, tangent2);
            momx_out = tangent1[0];
            momy_out = tangent1[1];
            momz_out = ( pos[3 * i + 2] < pos[3 * (i + 1) + 2] ) ? sqrt(p2 - tmp_pT2) : -sqrt(p2 - tmp_pT2);
        }

        double dtheta_xy = calc_dangle2D(momx_in, momy_in, momx_out, momy_out);
        double pT_in = sqrt(momx_in * momx_in + momy_in * momy_in);
        double pT_out = sqrt(momx_out * momx_out + momy_out * momy_out);
        double dtheta_rz = calc_dangle2D(pT_in, momz_in, pT_out, momz_out);
        int is_strip = ( layerlist[i] < 2 ) ? 0 : 1;
        /// if ( i==0 ) is_strip = -1; /// this is not used anymore.
        double momx = (momx_in + momx_out) * 0.5;
        double momy = (momy_in + momy_out) * 0.5;
        double momz = (momz_in + momz_out) * 0.5;
        double inj_angle = calc_dangle3D(pos[3 * i], pos[3 * i + 1], 0., momx, momy, momz);
        double theta_p = calc_scattering(inj_angle, is_strip, p2);
        double theta_p_xy = sqrt(0.5) * theta_p;
        double theta_p_rz = sqrt(0.5) * theta_p;
        /// theta_p : width of straggling at hit point
        /// theta_p_xy : width of straggling at hit point in xy-plane
        /// theta_p_rz : width of straggling at hit point in rz-plane
        if ( i == nhits - 1 )
        {
            if ( layerlist[i] != 3 )
            {
                /// if last hit is not at B3, additional material should be added
                /// to calculate theta_p.
                double tmp_theta_p = calc_scattering(inj_angle, 1, p2);
                /// changing of injection angle (=inj_angle) is assumed to be very small.
                if ( layerlist[i] == 1 ) /// last hit is at B1.
                {
                    theta_p = sqrt(theta_p * theta_p + 2.*tmp_theta_p * tmp_theta_p);
                }
                else if ( layerlist[i] == 2 )   /// last hit is at B2.
                {
                    theta_p = sqrt(theta_p * theta_p + tmp_theta_p * tmp_theta_p);
                }
            }
            theta_p_xy = sqrt(0.5 * theta_p * theta_p + m_sphiCNTout * m_sphiCNTout);
            theta_p_rz = sqrt(0.5 * theta_p * theta_p + m_sthetaCNTout * m_sthetaCNTout);
        }
        //chi += (dtheta_xy*dtheta_xy+dtheta_rz*dtheta_rz)/(theta_p*theta_p);
        chi += (dtheta_xy * dtheta_xy) / (theta_p_xy * theta_p_xy);
        chi += (dtheta_rz * dtheta_rz) / (theta_p_rz * theta_p_rz);
        m_scatter[i] = sqrt(dtheta_xy * dtheta_xy + dtheta_rz * dtheta_rz);
        m_scatter_xy[i] = dtheta_xy;
        m_scatter_rz[i] = dtheta_rz;
        m_sscatter[i] = theta_p;
        /// fill outgoing momentum values
        m_momentum_out[i][0] = momx_out;
        m_momentum_out[i][1] = momy_out;
        m_momentum_out[i][2] = momz_out;
    }
    return chi;
}


double SvxTracker::calc_Chi2_SvxCNT(bool helicity,
                                    double pT2, double phi0, double theta0,
                                    double cax, double cay,
                                    const vector<SvxCluster *> &vcluster,
                                    const vector<double> &dparam)
///
/// calculate chi-square shifting local-x & local-z positions and pT by dparam.
///
/// INPUT
///  - hel       : helicity
///  - pT2       : square of transverse momentum
///  - phi0      : azimuthal angle of momentum vector of CNT at DCA point
///  - theta0    : azimuthal angle of momentum vector of CNT at DCA point
///  - cax, cay  : DCA point
///  - vcluster  : list of associated hits
///  - dparam    : shift of local-x & local-z positions and pT
///
/// Return : chi-square
///
{
    double chi = 0.;

    //vector<float> pos3d;
    vector<double> pos3d;
    vector<int> layerlist;

    if (d_geometry == NULL)
    {
        cerr << PHWHERE << "ERROR: Cannot locate d_geometry." << endl;
        return 9999;
    }

    int ncls = vcluster.size();
    for ( int icls = 0; icls < ncls; icls++ )
    {
        int layer = vcluster[icls]->get_layer();
        layerlist.push_back(layer);
        int ladder = vcluster[icls]->get_ladder();
        int sensor = vcluster[icls]->get_sensor();
        SvxSensor *svxsen = d_geometry->GetSensorPtr(layer, ladder, sensor);
        double pos_local[3];
        double pos_global[3] = {vcluster[icls]->get_xyz_global(0),
                               vcluster[icls]->get_xyz_global(1),
                               vcluster[icls]->get_xyz_global(2)
                              };
        double phicls = atan2(pos_global[1], pos_global[0]);
        if ( phicls < -0.5 * M_PI ) phicls += 2.*M_PI;
        double rcls = sqrt(pos_global[0] * pos_global[0] + pos_global[1] * pos_global[1]);
        /// modify local position
        pos_local[0] = vcluster[icls]->get_xyz_local(0) + dparam[2 * icls];
        pos_local[1] = vcluster[icls]->get_xyz_local(1);
        pos_local[2] = vcluster[icls]->get_xyz_local(2) + dparam[2 * icls + 1];
        /// calculate global position
        svxsen->position_local2global(pos_local, pos_global);
        for ( int i = 0; i < 3; i++ )
        {
            pos3d.push_back(pos_global[i]);
        }
        chi += dparam[2 * icls] * dparam[2 * icls] / (m_sdphil[layer] * m_sdphil[layer]);
        chi += dparam[2 * icls + 1] * dparam[2 * icls + 1] / (m_sdzl[layer] * m_sdzl[layer]);
        double phimod = atan2(pos_global[1], pos_global[0]);
        if ( phimod < -0.5 * M_PI ) phimod += 2.*M_PI;
        m_dphi[icls] = (phimod - phicls) * rcls;
        m_dz[icls] = dparam[2 * icls + 1];
    }
    //chi += dparam.back()*dparam.back()/(get_sdpT()*get_sdpT());

    /*
    double oldpT = sqrt(pT2);
    double newpT = oldpT + dparam.back();
    if ( newpT<0 ) helicity = 1^helicity;  /// this may happen when momentum is very large.
    if ( fabs(newpT)<0.1 ) newpT = (newpT<0) ? -0.1 : 0.1;
    double newpT2 = newpT*newpT;
    int n = pos3d.size();
    double L = sqrt((pos3d[n-3]-cax)*(pos3d[n-3]-cax)+(pos3d[n-2]-cay)*(pos3d[n-2]-cay));
    double phi1;
    if ( 0.5*L*sqrt(b)/newpT>1 ) return 9999;
    if ( helicity ) {
      phi1 = phi0 + asin(0.5*L*sqrt(b)/oldpT)*2.;
    } else {
      phi1 = phi0 - asin(0.5*L*sqrt(b)/oldpT)*2.;
    }

    chi += calc_Chi2_SvxCNT(helicity, newpT2, phi1, theta0, pos3d, layerlist);
    */
    double pT = sqrt(pT2);
    int n = pos3d.size();
    double L = sqrt((pos3d[n - 3] - cax) * (pos3d[n - 3] - cax) + (pos3d[n - 2] - cay) * (pos3d[n - 2] - cay));
    double phi1;
    if ( helicity )
    {
        phi1 = phi0 + asin(0.5 * L * sqrt(b) / pT) * 2.;
    }
    else
    {
        phi1 = phi0 - asin(0.5 * L * sqrt(b) / pT) * 2.;
    }

    chi += calc_Chi2_SvxCNT(helicity, pT2, phi1, theta0, pos3d, layerlist);

    return chi;
}


double SvxTracker::calc_Chi2_SvxCNT(bool helicity,
                                    double pT2, double phi0, double theta0,
                                    double cax, double cay,
                                    const vector<SvxCluster *> &vcluster,
                                    const vector<double> &dparam,
                                    const vector<double> &ddparam,
                                    double dl)
///
/// calculate chi-square shifting local-x & local-z positions and pT
/// by dparam+dl*ddparam.
///
/// INPUT
///  - helicity  : helicity
///  - pT2       : square of transverse momentum
///  - phi0      : azimuthal angle of momentum vector of CNT at DCA point
///  - theta0    : azimuthal angle of momentum vector of CNT at DCA point
///  - cax, cay  : DCA point
///  - vcluster  : list of associated hits
///  - dparam, ddparam, dl
///              : used to define shift of local-x & local-z positions and pT
///                shift = dparam + dl * ddparam
///
/// Return : chi-square
///
{
    vector<double> tmp_dparam;
    for ( unsigned int i = 0; i < dparam.size(); i++ )
    {
        tmp_dparam.push_back(dparam[i] + ddparam[i]*dl);
    }

    //return calculate_ChiSquare(helicity, pT2, vcluster, tmp_dparam);
    return calc_Chi2_SvxCNT(helicity, pT2, phi0, theta0, cax, cay, vcluster, tmp_dparam);
}


///------------------------------------------------------
/// Next calc_Chi2_SvxCNT_NoCNT()s are calculate_ChiSquare() for SvxCentralTrack
///
double SvxTracker::calc_Chi2_SvxCNT_NoCNT(bool hel,
                                    double pT2, double theta,
                                    const vector<double> &pos,
                                    const vector<int> &layerlist)
///
/// Calculate log of probability taking into account
/// multiple scattering at each hit points.
///
/// Input
///  - hel       : helicity
///  - pT2       : square of transverse momentum
///  - theta     : polar angle of momentum vector of CNT projected to outermost hit
///  - pos       : Global position of all hit points. This function needs at least
///                two hit points.
///  - layerlist : layer of each hit
///
/// Return : scattering angle part of chi-square
///
{
    double chi = 0;

    unsigned int nhits = layerlist.size();

    //--if ( pos.size() < 9 || nhits < 3 )
    //--{
    //--    cout << "calc_Chi2_SvxCNT_NoCNT ERROR | At least 3 hits are necessary" << endl;
    //--    cout << pos.size() << ", " << nhits << endl;
    //--}

    /// during this function, p2, not pT2, should be fixed.
    double p2 = pT2 / (sin(theta) * sin(theta));

    for ( unsigned int i = 1; i < nhits-1; i++ )
    {
        ///incoming momentum vector at (pos[3*i], pos[3*i+1], pos[3*i+2])
        double momx_in, momy_in, momz_in;
        ///outgoing momentum vector at (pos[3*i], pos[3*i+1], pos[3*i+2])
        double momx_out, momy_out, momz_out;

        vector<double> tangent1(2);   /// tangent vector (px, py) at the first point
        vector<double> tangent2(2);   /// tangent vector (px, py) at the second point
        double tmp_pT2;   /// magnitude of total momentum

        /// incoming circle
        simple_calcTangents(hel,
                            pos[3 * (i - 1)], pos[3 * (i - 1) + 1], pos[3 * (i - 1) + 2],
                            pos[3 *  i     ], pos[3 *  i      + 1], pos[3 *  i      + 2],
                            p2, tmp_pT2, tangent1, tangent2);
        momx_in = tangent2[0];
        momy_in = tangent2[1];
        momz_in = sqrt(p2 - tmp_pT2) * (( pos[3 * (i - 1) + 2] < pos[3 * i + 2] ) ? 1 : -1.);

        /// outgoing circle
        simple_calcTangents(hel,
                            pos[3 *  i     ], pos[3 *  i      + 1], pos[3 *  i      + 2],
                            pos[3 * (i + 1)], pos[3 * (i + 1) + 1], pos[3 * (i + 1) + 2],
                            p2, tmp_pT2, tangent1, tangent2);
        momx_out = tangent1[0];
        momy_out = tangent1[1];
        momz_out = sqrt(p2 - tmp_pT2) * (( pos[3 * i + 2] < pos[3 * (i + 1) + 2] ) ? 1. : -1.);

        //----------------------------------
        // sigma calculation
        double pT_in  = sqrt(momx_in  * momx_in  + momy_in  * momy_in);
        double pT_out = sqrt(momx_out * momx_out + momy_out * momy_out);
        double dtheta_xy = calc_dangle2D(momx_in, momy_in, momx_out, momy_out);
        double dtheta_rz = calc_dangle2D(pT_in, momz_in, pT_out, momz_out);
        int is_strip = ( layerlist[i] < 2 ) ? 0 : 1;
        /// if ( i==0 ) is_strip = -1; /// this is not used anymore.
        double momx = (momx_in + momx_out) * 0.5;
        double momy = (momy_in + momy_out) * 0.5;
        double momz = (momz_in + momz_out) * 0.5;
        double inj_angle = calc_dangle3D(pos[3 * i], pos[3 * i + 1], 0., momx, momy, momz);
        double theta_p = calc_scattering(inj_angle, is_strip, p2);
        double theta_p_xy = sqrt(0.5) * theta_p;
        double theta_p_rz = sqrt(0.5) * theta_p;
        /// theta_p    : width of straggling at hit point
        /// theta_p_xy : width of straggling at hit point in xy-plane
        /// theta_p_rz : width of straggling at hit point in rz-plane

        //---------------------------
        // chi2 calculation
        chi += (dtheta_xy * dtheta_xy) / (theta_p_xy * theta_p_xy);
        chi += (dtheta_rz * dtheta_rz) / (theta_p_rz * theta_p_rz);
        // don't fill the value
        //--m_scatter[i] = sqrt(dtheta_xy * dtheta_xy + dtheta_rz * dtheta_rz);
        //--m_scatter_xy[i] = dtheta_xy;
        //--m_scatter_rz[i] = dtheta_rz;
        //--m_sscatter[i] = theta_p;
        /// fill outgoing momentum values
        //--m_momentum_out[i][0] = momx_out;
        //--m_momentum_out[i][1] = momy_out;
        //--m_momentum_out[i][2] = momz_out;
    }
    return chi;
}

double SvxTracker::calc_Chi2_SvxCNT_NoCNT(bool helicity,
                                    double pT2, double theta0,
                                    double cax, double cay,
                                    const vector<SvxCluster *> &vcluster,
                                    const vector<double> &dparam)
///
/// calculate chi-square shifting local-x & local-z positions and pT by dparam.
///
/// INPUT
///  - hel       : helicity
///  - pT2       : square of transverse momentum
///  - theta0    : azimuthal angle of momentum vector of CNT at DCA point
///  - cax, cay  : DCA point
///  - vcluster  : list of associated hits
///  - dparam    : shift of local-x & local-z positions and pT
///
/// Return : chi-square
///
{
    double chi = 0.;

    vector<double> pos3d;
    vector<int> layerlist;

    if (d_geometry == NULL)
    {
        cerr << PHWHERE << "ERROR: Cannot locate d_geometry." << endl;
        return 9999;
    }

    int ncls = vcluster.size();
    for ( int icls = 0; icls < ncls; icls++ )
    {
        int layer  = vcluster[icls]->get_layer();
        int ladder = vcluster[icls]->get_ladder();
        int sensor = vcluster[icls]->get_sensor();
        layerlist.push_back(layer);

        SvxSensor *svxsen = d_geometry->GetSensorPtr(layer, ladder, sensor);
        double pos_local[3];
        double pos_global[3] = {vcluster[icls]->get_xyz_global(0),
                               vcluster[icls]->get_xyz_global(1),
                               vcluster[icls]->get_xyz_global(2)
                              };

        /// modify local position
        pos_local[0] = vcluster[icls]->get_xyz_local(0) + dparam[2 * icls];
        pos_local[1] = vcluster[icls]->get_xyz_local(1);
        pos_local[2] = vcluster[icls]->get_xyz_local(2) + dparam[2 * icls + 1];
        /// calculate global position
        svxsen->position_local2global(pos_local, pos_global);
        for ( int i = 0; i < 3; i++ )
        {
            pos3d.push_back(pos_global[i]);
        }
        chi += ((dparam[2 * icls    ] * dparam[2 * icls    ]) / (m_sdphil[layer] * m_sdphil[layer]));
        chi += ((dparam[2 * icls + 1] * dparam[2 * icls + 1]) / (m_sdzl[layer]   * m_sdzl[layer])  );
    }

    chi += calc_Chi2_SvxCNT_NoCNT(helicity, pT2, theta0, pos3d, layerlist);

    return chi;
}

double SvxTracker::calc_Chi2_SvxCNT_NoCNT(bool helicity,
                                    double pT2, double theta0,
                                    double cax, double cay,
                                    const vector<SvxCluster *> &vcluster,
                                    const vector<double> &dparam,
                                    const vector<double> &ddparam,
                                    double dl)
///
/// calculate chi-square shifting local-x & local-z positions and pT
/// by dparam+dl*ddparam.
///
/// INPUT
///  - helicity  : helicity
///  - pT2       : square of transverse momentum
///  - theta0    : azimuthal angle of momentum vector of CNT at DCA point
///  - cax, cay  : DCA point
///  - vcluster  : list of associated hits
///  - dparam, ddparam, dl
///              : used to define shift of local-x & local-z positions and pT
///                shift = dparam + dl * ddparam
///
/// Return : chi-square
///
{
    vector<double> tmp_dparam;
    for ( unsigned int i = 0; i < dparam.size(); i++ )
    {
        tmp_dparam.push_back(dparam[i] + ddparam[i]*dl);
    }

    return calc_Chi2_SvxCNT_NoCNT(helicity, pT2, theta0, cax, cay, vcluster, tmp_dparam);
}

//--------------------------------------

void SvxTracker::simple_calcTangents(bool hel,
                                     double x1, double y1, double z1,
                                     double x2, double y2, double z2,
                                     double p2, double &pT2,
                                     vector<double> &tangent1, vector<double> &tangent2)
/// Simplified calculatetangent()
///
/// Input
///   - hel : helicity
///   - x, y, z : hit position
///   - p2 : Magnitude of total momentum
///
/// Output
///   - pT2 : Square of transverse momentum
///   - tangent1 : outgoing tangent vector at inner hit (only x and y)
///   - tangent2 : incoming tangent vector at outer hit (only x and y)
///
{
    double L_xy = sqrt( (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) );
    double L_z = z1 - z2;
    double L2 = L_xy * L_xy + L_z * L_z;
    pT2 = p2 * L_xy * L_xy / L2;
    double pT = sqrt(pT2);

    double dir;
    if ( hel == true )
    {
        dir =  1.;
    }
    else
    {
        dir = -1.;
    }
    /// dir : rotation direction
    /// dir ==  1 : counter-clockwise
    /// dir == -1 : clockwise

    double cx;
    double cy;
    double L_c;
    /// (cx, cy) : center of rotation
    /// L_c : distance between (cx, cy) and center of (x1, y1) and (x2, y2)

    if ( zerofield == true )
    {
        //p2 = ( 1. + (L_z*L_z)/(L_xy*L_xy) )*pT2;
        tangent1[0] = (x2 - x1) / L_xy * pT;
        tangent1[1] = (y2 - y1) / L_xy * pT;
        tangent2[0] = (x2 - x1) / L_xy * pT;
        tangent2[1] = (y2 - y1) / L_xy * pT;
    }
    else
    {
        double R = pT * sqrt(binv); /// R : diameter of rotation

        if ( R < L_xy / 2. )
        {
            //p2 = ( 1. + (L_z*L_z)/(M_PI*R*M_PI*R) )*pT2;
            cx = (x1 + x2) / 2.;
            cy = (y1 + y2) / 2.;
            //-- L_c = L_xy / 2.;
        }
        else
        {
            //--double cos_theta = 1. - 0.5 * (L_xy * L_xy) / (R * R); /// = 1 - 2*sin(theta/2)^2
            //--double theta;
            //--if ( cos_theta >= 1. )
            //--{
            //--    theta = 0.;
            //--}
            //--else
            //--{
            //--    theta = acos(cos_theta);   /// theta : rotation angle
            //--}
            //p2 = ( 1. + (L_z*L_z)/(theta*R*theta*R) )*pT2;

            L_c = sqrt(R * R - L_xy * L_xy / 4.);
            cx = 0.5 * (x1 + x2) - dir * (y2 - y1) / L_xy * L_c;
            cy = 0.5 * (y1 + y2) + dir * (x2 - x1) / L_xy * L_c;
        }

        tangent1[0] =  dir * (cy - y1) / R * pT;
        tangent1[1] = -dir * (cx - x1) / R * pT;
        tangent2[0] =  dir * (cy - y2) / R * pT;
        tangent2[1] = -dir * (cx - x2) / R * pT;
    }

}

//param has format (r, cx, cy, theta0, z0, dz/dtheta)
//val has format (s, t)
//grad has format(grad_t, grad_s)
//hessian has format (h_tt, h_ts, h_ss)
void SvxTracker::calcHelixDistDers(bool hel1, bool hel2, const vector<float> &param1, const vector<float> &param2, const vector<float> &val, vector<float> &grad, vector<float> &hessian)
{
    float h1 = 1.;
    float h2 = 1.;
    if (hel1 == false)
    {
        h1 = -1.;
    }
    if (hel2 == false)
    {
        h2 = -1.;
    }

    float sint = sin(param1[3] + h1 * val[0]);
    float cost = cos(param1[3] + h1 * val[0]);
    float x1 = param1[1] + param1[0] * cost;
    float y1 = param1[2] + param1[0] * sint;
    float z1 = param1[4] + val[0] * param1[5];

    float dx1dt = h1 * (-y1 + param1[2]);
    float dy1dt = h1 * (x1 - param1[1]);
    float dz1dt = param1[5];

    float d2x1dt2 = -dy1dt;
    float d2y1dt2 = dx1dt;
    float d2z1dt2 = 0.;

    float sins = sin(param2[3] + h2 * val[1]);
    float coss = cos(param2[3] + h2 * val[1]);
    float x2 = param2[1] + param2[0] * coss;
    float y2 = param2[2] + param2[0] * sins;
    float z2 = param2[4] + val[1] * param2[5];

    float dx2ds = h2 * (-y2 + param2[2]);
    float dy2ds = h2 * (x2 - param2[1]);
    float dz2ds = param2[5];

    float d2x2ds2 = -dy2ds;
    float d2y2ds2 = dx2ds;
    float d2z2ds2 = 0.;




    grad[0] = 2.*(x1 - x2) * dx1dt + 2.*(y1 - y2) * dy1dt + 2.*(z1 - z2) * dz1dt;
    grad[1] = -(2.*(x1 - x2) * dx2ds + 2.*(y1 - y2) * dy2ds + 2.*(z1 - z2) * dz2ds);

    hessian[0] = 2.*(x1 - x2) * d2x1dt2 + 2.*dx1dt * dx1dt + 2.*(y1 - y2) * d2y1dt2 + 2.*dy1dt * dy1dt + 2.*(z1 - z2) * d2z1dt2 + 2.*dz1dt * dz1dt;
    hessian[2] = -(2.*(x1 - x2) * d2x2ds2 - 2.*dx2ds * dx2ds + 2.*(y1 - y2) * d2y2ds2 - 2.*dy2ds * dy2ds + 2.*(z1 - z2) * d2z2ds2 - 2.*dz2ds * dz2ds);
    hessian[1] = -2.*dx2ds * dx1dt - 2.*dy2ds * dy1dt - 2.*dz2ds * dz1dt;
}

void SvxTracker::calcHelixDistDers(bool hel1, const vector<float> &param1, const vector<float> &point, float val, float &der, float &der2)
/// param1 : rotation diameter, cx, cy, z, atan2(y-cy,x-cx) (= theta), dz/dtheta
/// cf : (cx, cy) = rotation center
///     (x, y, z) = hit position at innermost layer
/// point : primary vertex position
///
/// der : d(r^2)/dt
/// der2 : d2(r^2)/dt2
{
    float h1 = 1.;
    if (hel1 == false)
    {
        h1 = -1.;
    }

    float sint = sin(param1[3] + h1 * val);
    float cost = cos(param1[3] + h1 * val);
    float x1 = param1[1] + param1[0] * cost;
    float y1 = param1[2] + param1[0] * sint;
    float z1 = param1[4] + val * param1[5];

    float dx1dt = h1 * (-y1 + param1[2]);
    float dy1dt = h1 * (x1 - param1[1]);
    float dz1dt = param1[5];

    float d2x1dt2 = -dy1dt;
    float d2y1dt2 = dx1dt;
    float d2z1dt2 = 0.;


    der = 2.*(x1 - point[0]) * dx1dt + 2.*(y1 - point[1]) * dy1dt + 2.*(z1 - point[2]) * dz1dt;

    der2 = 2.*(x1 - point[0]) * d2x1dt2 + 2.*dx1dt * dx1dt + 2.*(y1 - point[1]) * d2y1dt2 + 2.*dy1dt * dy1dt + 2.*(z1 - point[2]) * d2z1dt2 + 2.*dz1dt * dz1dt;
}

//params[0] = delta_x_0
//params[1] = delta_z_0
//etc...
//P = pt^2
double SvxTracker::fullProbFunction(const vector<float> &params, const vector<float> &cluster_centers, float P)
{
    double val = 0.;
    double scale = 0.05;

    double power = 8.;

    val += pow((params[0]) / (0.0025), power);
    val += pow((params[1]) / (0.02125), power);

    val += pow((params[2]) / (0.0025), power);
    val += pow((params[3]) / (0.02125), power);

    val += pow((params[4]) / (0.004), power);
    val += pow((params[5]) / (0.05), power);

    val += pow((params[6]) / (0.004), power);
    val += pow((params[7]) / (0.05), power);

    val *= scale;

    vector<int> is_strip;
    is_strip.push_back(0);
    is_strip.push_back(1);

    vector<float> pos;
    pos.assign(12, 0.);

    float dir_x = 0.;
    float dir_y = 0.;
    float r_inv = 0.;

    for (int l = 0; l < 4; l++)
    {
        r_inv = sqrt(cluster_centers[(3 * l) + 0] * cluster_centers[(3 * l) + 0] + cluster_centers[(3 * l) + 1] * cluster_centers[(3 * l) + 1]);
        r_inv = 1. / r_inv;
        dir_x = -r_inv * cluster_centers[(3 * l) + 1];
        dir_y = r_inv * cluster_centers[(3 * l) + 0];
        pos[(3 * l) + 0] = cluster_centers[(3 * l) + 0] + params[(2 * l) + 0] * dir_x;
        pos[(3 * l) + 1] = cluster_centers[(3 * l) + 1] + params[(2 * l) + 0] * dir_y;
        pos[(3 * l) + 2] = cluster_centers[(3 * l) + 2] + params[(2 * l) + 1];
    }

    //  float scatter_sum = solveTrack(pos, is_strip, P);
    float scatter_sum = solveTrack_byChiSquare(pos, is_strip, P);

    val -= scatter_sum;

    return val;
}


SvxTrackInfo SvxTracker::calc_TrackInfo(bool hel,
                                        const vector<double> &pos,
                                        const vector<double> &mom)
///
/// input
///    pos : (x,y,z)
///    mom : (px,py,pz)
/// output
///    SvxTrackInfo object (contains rotation diameter and rotation center)
///
{
    double dir = ( hel ) ? 1. : -1.;
    double pt = sqrt(mom[0] * mom[0] + mom[1] * mom[1]);
    double R = pt * sqrt(binv); /// = rotation diameter
    double phi = atan2(mom[1], mom[0]);

    /// cx = pos[0] + R*cos(phi+dir*pi/2)
    ///    = pos[0] - R*sin(phi)
    /// cy = pos[1] + R*sin(phi+dir*pi/2)
    ///    = pos[1] + R*cos(phi)
    double cx = pos[0] - dir * R * sin(phi);
    double cy = pos[1] + dir * R * cos(phi);

    SvxTrackInfo track;
    track.d_Rrot = R;
    track.d_cx = cx;
    track.d_cy = cy;

    return track;
}


void SvxTracker::calc_InfoAtClosestApproach(bool hel,
        float x, float y, float z,
        float px, float py, float pz,
        const vector<float> &point,
        vector<float> &pos_primary,
        vector<float> &mom_primary,
        float &DCA2D)
///
/// Calculate position and momentum at the closest approach of the track in xy-plane
/// to a point.
///
/// Any point on the track can be represented as (cx+R*cos(phi), cy+R*sin(phi))
/// and therefore, distance from a point (x0, y0) is
///
/// sqrt( (cx+R*cos(phi)-x0)^2 + (cy+R*sin(phi)-y0)^2 )
/// = sqrt( (cx-x0)^2 + (cy-y0)^2 + R^2 + 2*R*((cx-x0)*cos(phi) + (cy-y0)*sin(phi)) ).
///
/// Therefore, when atan2(cx-x0, cy-y0) - phi = pi, the distance should be smallest.
///
/// input
///    hel   : helicity of the track
///    pos   : hit position at innermost layer.
///    mom   : momentum vector at innermost layer.
///    point : 3D position of a point. This function calculate position and momentum
///            at the closest approach of the track in xy-plane to this point.
///
/// output
///    pos_primary : position of the closest approach
///    mom_primary : momentum vector at the closest approach
///    DCA2D       : DCA in XY-plane. This DCA2D has sign. If positive, pos_primary
///                  is not between (cx, cy) and (x0, y0). If negative, pos_primary
///                  is between (cx, cy) and (x0, y0).
///
{

    if ( pos_primary.size() != 3 || mom_primary.size() != 3 )
    {
        cout << "!!! ERROR : size of pos_primary and mom_primary must be 3 !!!" << endl;
    }

    vector<double> pos(3);
    pos[0] = x;
    pos[1] = y;
    pos[2] = z;
    vector<double> mom(3);
    mom[0] = px;
    mom[1] = py;
    mom[2] = pz;
    SvxTrackInfo track = calc_TrackInfo(hel, pos, mom);
    double phi0 = atan2(point[1] - track.d_cy, point[0] - track.d_cx);

    double cos_phi0 = cos(phi0);
    double sin_phi0 = sin(phi0);
    /// pos_primary[0] = cx + R*cos(phi0)
    /// pos_primary[1] = cy + R*sin(phi0)
    pos_primary[0] = track.d_cx + track.d_Rrot * cos_phi0;
    pos_primary[1] = track.d_cy + track.d_Rrot * sin_phi0;

    double dir = ( hel ) ? 1. : -1.;
    double pT = sqrt(px * px + py * py);
    /// mom_primary[0] = pT*cos(phi0+dir*pi/2)
    ///                = -dir*pT*sin(phi0)
    /// mom_primary[1] = pT*sin(phi0+dir*pi/2)
    ///                = dir*pT*cos(phi0)
    mom_primary[0] = -dir * pT * sin_phi0;
    mom_primary[1] = dir * pT * cos_phi0;
    mom_primary[2] = pz;

    double dzdphi = dir * pz * track.d_Rrot / pT;
    /// dzdphi is unchanged
    double phi_diff = phi0 - atan2(y - track.d_cy, x - track.d_cx);
    pos_primary[2] = z + phi_diff * dzdphi;

    /// evaluate sign of DCA
    /// if DCAsign=true, pos_primary is not between (cx, cy) and (point[0], point[1]),
    /// otherwise, pos_primary is between (cx, cy) and (point[0], point[1]).
    double length = sqrt( (track.d_cx - point[0]) * (track.d_cx - point[0])
                          + (track.d_cy - point[1]) * (track.d_cy - point[1]) );

    if ( length < track.d_Rrot )   /// DCA2D is positive
    {
        DCA2D = sqrt( (pos_primary[0] - point[0]) * (pos_primary[0] - point[0])
                      + (pos_primary[1] - point[1]) * (pos_primary[1] - point[1]) );
    }
    else       /// DCA2D is negative
    {
        DCA2D = -sqrt( (pos_primary[0] - point[0]) * (pos_primary[0] - point[0])
                       + (pos_primary[1] - point[1]) * (pos_primary[1] - point[1]) );
    }

}


void SvxTracker::setExpectedPosition(const vector<SvxCluster *> &vcluster,
                                     const vector<double> &dparam)
{
    int ncls = vcluster.size();
    int nhit_layer[4] = {0, 0, 0, 0};
    for ( int icls = 0; icls < ncls; icls++ )
    {
        int layer = vcluster[icls]->get_layer();
        int ladder = vcluster[icls]->get_ladder();
        int sensor = vcluster[icls]->get_sensor();
        SvxSensor *svxsen = d_geometry->GetSensorPtr(layer, ladder, sensor);
        double pos_local[3] = {0,0,0};
        double pos_global[3] = {0,0,0};
        /// modify local position
        pos_local[0] = vcluster[icls]->get_xyz_local(0) + dparam[2 * icls];
        pos_local[1] = vcluster[icls]->get_xyz_local(1);
        pos_local[2] = vcluster[icls]->get_xyz_local(2) + dparam[2 * icls + 1];
        /// calculate global position
        svxsen->position_local2global(pos_local, pos_global);
        /// save expected hit position
        if ( nhit_layer[layer] < 2 )
        {
            expected_position[layer][nhit_layer[layer]][0] = pos_global[0];
            expected_position[layer][nhit_layer[layer]][1] = pos_global[1];
            expected_position[layer][nhit_layer[layer]][2] = pos_global[2];
            nhit_layer[layer] ++;
        }
    }
}


void SvxTracker::setFitResult(bool helicity, double pT2,
                              const vector<SvxCluster *> &vcluster,
                              const vector<double> &dparam)
{
    calculate_ChiSquare(helicity, pT2, vcluster, dparam);
    setExpectedPosition(vcluster, dparam);
}


void SvxTracker::setFitResult_SvxCNT(bool helicity,
                                     double pT2, double phi0, double theta0,
                                     double cax, double cay,
                                     const vector<SvxCluster *> &vcluster,
                                     const vector<double> &dparam)
{
    calc_Chi2_SvxCNT(helicity, pT2, phi0, theta0, cax, cay, vcluster, dparam);
    setExpectedPosition(vcluster, dparam);
}


inline float SvxTracker::calc_scattering(float angle, int is_strip, double p2)
///
/// Calculate scattering angle taking account of incident angle
///
/// input :
///    angle : incident angle of track. Unit is radian
///    is_strip : If the injected detector is stripixel, 1. Otherwise 0.
///    p2 : square of momentum
/// output : RMS of scattering angle calculated from gaussian approximation
///
{
    float cos_angle = fabs(cos(angle));
    if ( cos_angle == 0. )//D. McGlinchey - should not be checking for float equality with ==
    {
        cout << "ERROR : incident angle is too large." << endl;
        return M_PI * 0.5; /// this should not be happen
    }

    float X0;
    /*
    ///
    /// For SvxCentralTrack, angle matching between momentum vector at the innermost hit
    /// and (phi0, the0) of CNT was required.
    /// But in the current code, that was changed from the innermost to the outermost.
    /// So, this part is not used anymore.
    ///
    if ( is_strip==1 ) {
      X0 = X0_strip;
    } else if ( is_strip==0 ) {
      X0 = X0_pixel;
    } else {
      X0 = X0_pixel + X0_beampipe;
    }
    */
    if ( is_strip == 1 )
    {
        X0 = X0_strip;
    }
    else
    {
        X0 = X0_pixel;
    }

    /// The formula for small angle multiple scatterng can be found in
    /// the PDG (2012), Sec. 30.3, Eqn. 30.15.
    float theta0p = 0.0136 * sqrt(X0 / cos_angle) * (1. + 0.038 * TMath::Log(X0 / cos_angle)) / sqrt(p2);

    // ADDED By theok theodore.koblesky@colorado.edu
    //on June 15, 2014
    //If we have ZF tracks, we don't know the momentum, so use something
    // reasonable like 500 MeV
    if (zerofield) theta0p = 0.0136 * sqrt(X0 / cos_angle) * (1. + 0.038 * TMath::Log(X0 / cos_angle)) / 0.5; //set momentum fo 0.5 GeV in ZERO FIELD
    /// aproximation : beta=1
    return theta0p;
}


inline double SvxTracker::calc_dangle2D(double vx1, double vy1, double vx2, double vy2)
///
/// calculate angle difference between (vx1,vy1) & (vx2,vy2) by cross product.
/// sign of output is defined by cross product of v1 & v2.
///
{
    double v1 = sqrt(vx1 * vx1 + vy1 * vy1);
    double v2 = sqrt(vx2 * vx2 + vy2 * vy2);
    if ( v1 == 0. || v2 == 0. ) return 0.;//D. McGlinchey - should not be checking for float equality with ==

    double sin = (vx1 * vy2 - vy1 * vx2) / v1 / v2;
    if ( sin >= 1. )
    {
        return M_PI * 0.5;
    }
    else if ( sin <= -1. )
    {
        return -M_PI * 0.5;
    }
    else
    {
        return asin(sin);
    }
}


inline double SvxTracker::calc_dangle3D(double vx1, double vy1, double vz1,
                                        double vx2, double vy2, double vz2)
///
/// calculate angle difference between (vx1,vy1,vz1) & (vx2,vy2,vz2) from scalar product.
/// all outputs are positive.
///
{
    double v1 = sqrt(vx1 * vx1 + vy1 * vy1 + vz1 * vz1);
    double v2 = sqrt(vx2 * vx2 + vy2 * vy2 + vz2 * vz2);
    if ( v1 == 0. || v2 == 0. ) return 0.;//D. McGlinchey - should not be checking for float equality with ==

    double cos = (vx1 * vx2 + vy1 * vy2 + vz1 * vz2) / v1 / v2;
    if ( cos >= 1. )
    {
        return 0.;
    }
    else if ( cos <= -1. )
    {
        return M_PI;
    }
    else
    {
        return acos(cos);
    }
}
