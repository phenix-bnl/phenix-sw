#include "ncfclProbDist.h"
#include "TF1.h"
#include "gsl/gsl_randist.h"
#include "gsl/gsl_sf_hyperg.h"
#include "gsl/gsl_errno.h"

//INCLUDECHECKER: Removed this line: #include <cmath>
#include <iostream>

 TH1* hGlauberDist = 0;
 TH1* hZdcNcolMean = 0;
 TH1* hZdcNcolSigma = 0;
 TF1* fMeanNgrey = 0;

gsl_error_handler_t *old_gslHandler;

void NcfclA::setHandler()
{
   gsl_set_error_handler(&NcfclA::my_gsl_error_handler);
}

void NcfclA::my_gsl_error_handler(const char *reason, const char *file, int line, int gsl_errno)
{
   fprintf(stderr,"GSL error: %s\n", reason);
}


Double_t NcfclA::norm0(Double_t* xa, Double_t* par)
{
  Double_t m = par[0];
  Double_t s = par[1];
  
  Double_t erfArg = -m/pow(2.0,0.5)/s;
  Double_t normInt = TMath::Erfc(erfArg);
  
  //  normInt = normInt*TMath::Exp(m*m/(s*s)/2.0*(s*s-1.0));
  normInt = normInt * sqrt(TMath::Pi()/2.0)*s;

  return normInt;
}

  Double_t NcfclA::norm(Double_t* x, Double_t* par)
  {
    if(x[0]<0) return 0.0;
    return(TMath::Exp(-(x[0]-par[0])*(x[0]-par[0])/2/(par[1]*par[1]))/norm0(x,par));
  }

Double_t NcfclA::normal(Double_t* x, Double_t* par)
{
   return(TMath::Exp(-(x[0]-par[0])*(x[0]-par[0])/2/(par[1]*par[1]))/par[1]/sqrt(TMath::TwoPi()));
}

Double_t NcfclA::Gamma(Double_t *x, Double_t *par)
  {
    return TMath::Gamma(x[0]);
  };

  Double_t NcfclA::GammaDist(Double_t *x, Double_t *par)
  {
      double p = par[0];
      double b = par[1];

      //double myln = (p-1)*log(x[0]) + p*log(b) - b*x[0] -  TMath::LnGamma(p);
      //return exp(myln);
      
      if(p<0) return 0.0;
      if(b<0) return 0.0;
      
      return gsl_ran_gamma_pdf(x[0],p,1.0/b);
  }

Double_t NcfclA::SmearedGammaDist(Double_t *x, Double_t *par)
{
   double &x1 = x[0];
   double gp = par[0];
   double gb = 1.0/par[1];
   double swidth = par[2];
   double gsl_arg1;
   double gsl_result1, gsl_result2;
   gsl_sf_result gsl_sf_result1, gsl_sf_result2;
   int status1, status2;
   static double result = 0.0;
   
   // From Mathematica 1F1 x argument
   gsl_arg1 = pow(-(swidth/gb) + x1/swidth,2.0)/2.0;
   
   // gsl Overflow Error seen for x_arg above 700 -- seems to corrupt future calculations?
   if(gsl_arg1 > 600.0) return NcfclA::GammaDist(x, par);
   
   
   // The integration of a GammaDistribution over a Gaussian was done in Mathematica.
   // It involves hypergeometric functions that we implore from gsl
   // However, the funtion is not valid over all ranges so we must catch 
   // the errors.  The only errors seen thus are beyond the significant values of the function,
   // so when the function blows up we will set it to zero.
   old_gslHandler = gsl_set_error_handler(&NcfclA::my_gsl_error_handler);
      
   //printf("%s %f %f %f %f %f\n",__PRETTY_FUNCTION__,x[0],par[0],par[1], par[2],result);
   status1 = gsl_sf_hyperg_1F1_e(gp/2.,0.5,gsl_arg1, &gsl_sf_result1);
   gsl_result1 = gsl_sf_result1.val;
   status2 = gsl_sf_hyperg_1F1_e((1 + gp)/2.0,1.5,gsl_arg1, &gsl_sf_result2);
   gsl_result2 = gsl_sf_result2.val;

   gsl_set_error_handler(old_gslHandler);

   if(status1||status2)
   {
      std::cerr<<__PRETTY_FUNCTION__<<": gsl_sf_hyperg_1F1 "<<gsl_arg1<<std::endl;
      return NcfclA::GammaDist(x, par);
   }

   
   result = (pow(2.0,-2.0 + gp/2.0)*pow(gb,-1.0 - gp)*pow(swidth,-2.0 + gp)*
       (sqrt(2.0)*gb*swidth*TMath::Gamma(gp/2.0)*gsl_result1 + 
        2.0*(-pow(swidth,2.0) + gb*x1)*TMath::Gamma((1.0 + gp)/2.0)* gsl_result2))/
      (exp(pow(x1,2.0)/(2.0*pow(swidth,2.0)))*sqrt(TMath::Pi())*TMath::Gamma(gp));
   
   // catch NaN
   if(result!=result) return 0.0;
   
   return result;
}

// Number of ways to choose N objects given M objects
Double_t NcfclA::Choose(Double_t M, Double_t N)
{
   return TMath::Exp(TMath::LnGamma(M+1.0)-TMath::LnGamma(M-N+1.0)-TMath::LnGamma(N+1.0));
   //    return TMath::Gamma(M+1.0)/(TMath::Gamma(M-N+1.0)*TMath::Gamma(N+1.0));
};

// Binomial Distribution of Likelyhood of N out M tries probability p
Double_t NcfclA::Binomial(Int_t N, Int_t M, Double_t p)
{
     //return gsl_ran_binomial_pdf(N,p,M);
     if(p>1.0) return 0.0;
     if(N>M) return 0.0;
     //return Choose(M,N)* pow(p,N)*pow(1.0-p,M-N);
     return Choose(M,N) * exp(N*log(p) + (M-N)*log(1.0-p) );
      return Choose(M,N) * TMath::Exp(N*TMath::Log(p) + (M-N)*TMath::Log(1.0-p) );
};

// Binomial Distribution of Likelyhood of N out M tries probability p
Double_t NcfclA::myBinomial(Double_t *x, Double_t* par)
{
   return NcfclA::Binomial((int) x[0],(int)par[0],par[1]);
};

// Binomial Distribution of Likelyhood of N out M tries probability p
Double_t NcfclA::myNegBinomial(Double_t *x, Double_t* par)
{
   int N = (int)(x[0]);
   int nu = (int)(par[0]);
   double p = par[1];
   
   int M = N + nu - 1;
   //return gsl_ran_binomial_pdf(N,p,M);
   if(N>M) return 0.0;
   //return Choose(M,N)* pow(p,N)*pow(1.0-p,M-N);
   return Choose(M,N) * exp(N*log(p) + (nu)*log(1.0-p) );   
}

Double_t NcfclA::fNgNbNcolRenorm(Double_t* x, Double_t* par)
{
  static Double_t c0;
  static Double_t c1;
  static Double_t c2;
  static Double_t Z_Target;
  static Double_t renorm[NcfclA::maxNcolSum];

  //  Double_t ngb = (int)(x[0]); //Number of greys + blacks
  bool parchange = false;
  Int_t Ncoll = (int)(par[4]);

  if( c0 != par[0]
     || c1 != par[1]
     || c2 != par[2]
     || Z_Target != par[3]
     ) parchange = true;

  c0 = par[0];
  c1 = par[1];
  c2 = par[2];
  Z_Target = par[3];

  if(parchange)
    //reset the normalization array
    for(int idnorm = 0; idnorm <NcfclA::maxNcolSum; idnorm++ )
    {
//      renorm[idnorm] = -1.0;
       renorm[idnorm] = 1.0;
    }

  //Renormalize
  if(renorm[Ncoll]<0)
  {
    Double_t sum = 0;
    Double_t rx[1];
    for(int iNg = 0; iNg < NcfclA::maxNcolSum; iNg++)
    {
      // Assume Ng < MaxNcoll
      rx[0]=iNg;
      sum+=fNgNbNcol(rx,par);
    }
    renorm[Ncoll] = sum;
  }

  return fNgNbNcol(x,par);///renorm[Ncoll];
}


Double_t NcfclA::fNgNbNcol(Double_t* x, Double_t* par)
{
  Int_t ngb = (int)(x[0]);
  Double_t c0 = par[0];
  Double_t c1 = par[1];
  Double_t c2 = par[2];
  Double_t Z_Target = par[3];
  Double_t Ncoll = par[4];
  Double_t fNgreyNcol_par[5];
  Double_t fNgreyNcol_x[1];
  Double_t fNblack_x[1];
  Double_t fNblack_par[5];
  Double_t result = 0.0;
  Double_t tresult;
  double tresult2;

  //Grey Params
  fNgreyNcol_par[0]=c0;
  fNgreyNcol_par[1]=c1;
  fNgreyNcol_par[2]=c2;
  fNgreyNcol_par[3]=Z_Target;
  fNgreyNcol_par[4]=(Double_t)Ncoll;

  //Black Params

  //P(Ng+Nb) = Sum_{Ng<(Ng+Nb)Pg(Ng)*Pb((Ng+Nb)-Ng)}
  for(int Ng = 0; Ng <= ngb; Ng++)
  {
    // Grey Probability
    fNgreyNcol_x[0]=Ng;
    tresult = NcfclA::fPNgreyNcol(fNgreyNcol_x,fNgreyNcol_par);

    //Black Probability fNBlack
    //fNblack_par[0] = Ncoll;
    //fNblack_x[0] = ngb - Ng;
    //tresult = tresult * NcfclA::fNBlack(fNblack_x,fNblack_par);
    //printf("%f G %f %f \n",Ncoll,fNgreyNcol_x[0],tresult);
    //Black Probability Binomial
    fNblack_x[0] = ngb - Ng;
    fNblack_par[0] = 0.794;
    fNblack_par[1] = 1.7/1.1*4.82;
    fNblack_par[2] = 4.7;
    fNblack_par[3] = Z_Target;
    fNblack_par[4] = Ncoll;
    tresult2 =  NcfclA::fPNgreyNcol2(fNblack_x,fNblack_par);
    //if(fNblack_x[0]==0.0) tresult2 = 1.0; else tresult2 = 0.0;
    result+= tresult*tresult2;
  
    //printf(" B %d %d %f %f %f %f\n",ngb, Ng, fNblack_x[0], tresult, tresult2 ,result);
    
  }

  return result;

}

Double_t NcfclA::fGlauberNgNbSumNcol(Double_t* x, Double_t* par)
{
   Double_t xglauber[1];
   Double_t tpar[5];
   Double_t result = 0.0;
   Double_t tresult = 0.0;

   for(int ipar = 0; ipar < 4; ipar++){ tpar[ipar]=par[ipar];}
   
   for(int Ncol = 1; Ncol < NcfclA::maxNcolSum; Ncol++)
   {
      tpar[4] = Ncol;
      xglauber[0]=Ncol;
      tresult = NcfclA::fPGlauber(xglauber,tpar) * NcfclA::fNgNbNcol(x, tpar);
      result+=tresult;
   }
   
   return result;
}


Double_t NcfclA::fNBlack(Double_t * nBlack, Double_t* par)
{
  double numGreys = par[0];
  //Stolen from Stephen Johnson's makeOscar.rb file
  double numBlacks = nBlack[0];
  double ratio = 1.35;
  double r3_ratio = pow(ratio,1.0/3.0);
  double meanBlacks;
  double sigBlacks;

  //Temp
  //Sanity Check
  //if(numBlacks<1) return 1.0;
  //return 0.0;
  //Temp

  //Try Binomial Rather than Stephens input distribution
  // when numGreys is substituted with Ncoll
//   Double_t c0 = 1.489;
//   Double_t c1 = 0.4065;
//   Double_t c2 = -0.00680;
//   Double_t Z_Target = 79.0;
  //  Double_t nP = (c0+c1*numGreys+c2*numGreys*numGreys)/Z_Target;

  //  return Binomial(numBlacks,(int)Z_Target,nP);

  if(numGreys > 13*r3_ratio)
    meanBlacks =12.5;
  else meanBlacks = (1.52 + 1.61*numGreys * r3_ratio
		     -0.0488*numGreys*numGreys*r3_ratio*r3_ratio+0.000475*numGreys*numGreys*numGreys*ratio)/ratio;
  if(numGreys>=4)
    sigBlacks=4;
  else sigBlacks = 2+ 2.65/4.0*numGreys;

 

//   return NcfclA::Binomial(numBlacks,
// 		  meanBlacks*meanBlacks/(meanBlacks-sigBlacks*sigBlacks),
// 		  1-sigBlacks*sigBlacks/meanBlacks);

  Double_t tx[1];
  Double_t tpar[2];
  tx[0]=numBlacks;
  tpar[0]=meanBlacks;
  tpar[1]=sigBlacks;
  return NcfclA::norm(tx,tpar);
}



// Exponential plus Gaussian
// par[0] theta
// par[1] decay expo
// par[2] mean gaus
// par[3] sigma gaus
Double_t NcfclA::gausexp(Double_t* x, Double_t* par)
{

   Double_t c0 = cos(par[0]);
   c0=c0*c0;
   c0 = c0*2.0;  //Tmep fix for only using half of the gaussion above mu=0
   Double_t c1 = sin(par[0]);
   c1=c1*c1;
   if(x[0]<0) c1 =0.0;
   return c0*normal(x, &par[2]) + c1*exp(-x[0]/par[1])/par[1];
   
}


  // Conditional Probability of observing Efcal energy in the forward
  // calorimeter when Ngrey grey protons are within the forward
  // calorimeter acceptance.  For now (10/27/2003), Ron is using a
  // Gamma function to describe the Fcal energy distribution to Ngrey grey
  // protons.  Analyzing E910 experimental tracks in a PISA
  // simulation, the mean energy is proportional to the number of grey
  // protons.
  Double_t NcfclA::fPEfcalNgrey(Double_t *x, Double_t *par)
  {
    Double_t Efcal = x[0]; //Ngreys in the Fcal Acceptance
    Double_t Ngrey = par[4];
    Double_t ZeroWidth = par[3]; // GeV

    Double_t gpar[3];
    // Linear model
    gpar[0]=par[1]+par[2]*Ngrey;
    // Quadratic model
    //gpar[0]=par[1]*(Ngrey+par[2]*Ngrey*Ngrey);
    gpar[1]=par[0];
    gpar[2]=ZeroWidth;
    
    // P(Efcal,Ngrey==0) is 0.0 for all Efcal > 0.0
    if(Ngrey==0.0||gpar[0]<=0.0)
    {
      //Assume a gaussian distribution... when Ng==0
      return TMath::Exp(-Efcal*Efcal/2.0/(ZeroWidth*ZeroWidth))/ZeroWidth/TMath::Sqrt(TMath::TwoPi());
       // Fit constants extracted from a fit of FclGreyS requiring 50<Zdce1<100GeV
      // Double_t gepar[4];
       //gepar[0] = 4.40059e-01;
       //gepar[1] = 5.17494e+00;
       //gepar[2] = 3.95356e-01;
       //gepar[3] = 1.75314;
       
       // values for RQMD
       //gepar[0] = 1.207;
       //gepar[1] = 23.85;
       //gepar[2] = 0.0;
       //gepar[3] = 0.8517;
       
       //return gausexp(x,gepar);
       
      //Assume a uniform distribution...when Ng==0
      //if(0.0<=Efcal && Efcal<=ZeroWidth) return 1.0/ZeroWidth;
      //return 0.0;
    }

    if(x[0]<0.0) return 0.0;
    
    //Double_t result = NcfclA::SmearedGammaDist(x,gpar);
    Double_t result = NcfclA::GammaDist(x,gpar);
    
    //Normal
    //gpar[0]=(par[1]+par[2]*Ngrey)/par[0];
    //gpar[1]=gpar[0]/par[0];
    //Double_t result = NcfclA::norm(x,gpar);
    
    return result;
  };

  // Mean Ngrey is assumed to follow second order polonomial in Ncoll
  // Mean Ngrey should always be >= 0 
  Double_t NcfclA::MeanNgrey(Double_t Ncoll, Double_t c0, Double_t c1,
		     Double_t c2)
  {
     Double_t result;
     //result = c0 + c1*Ncoll + c2*Ncoll*Ncoll;
     //result = c0*(TMath::Exp(c1*(Ncoll-c2))-1.0)/TMath::Exp(c1*(Ncoll-c2));
     //result = c1*(1.0 - exp(-pow(Ncoll/c2,c0))) + 1.1/1.7*4.81662*(1.0 - exp(-pow(Ncoll/4.70389,0.794)));
     //result = 1.1/1.7*c1*(1.0 - exp(-pow(Ncoll/c2,c0))) + 21.47*(1.0 - exp(-pow(Ncoll/15.39,1.068)));
     //result = c1*(1.0 - exp(-pow(Ncoll/c2,c0)));
     
     if(fMeanNgrey==0)
     {
        std::cout<<__PRETTY_FUNCTION__<<"Creating default fMeanGrey\n";
        fMeanNgrey = new TF1("fMeanNgrey","[1]*(1.0 - exp(-pow(x/[2],[0])))",0,20);
        fMeanNgrey->SetParNames("c0","c1","c2");
     }
     fMeanNgrey->SetParameter("c0",c0);
     fMeanNgrey->SetParameter("c1",c1);
     fMeanNgrey->SetParameter("c2",c2);
     
     result = fMeanNgrey->Eval(Ncoll);
     
     if(result < 0.0) result=0.0;
     return result;

  };

  // Conditional Probability Distribution for observing Ngrey grey protons
  // particles when Ncoll binary collisions occurred in a nucleus of
  // Z_Target. Mean Ngrey dependence assumed to be a 2nd order polonomial

  Double_t NcfclA::fPNgreyNcol(Double_t *x, Double_t *par)
  {
     
    Double_t c0 = par[0];
    Double_t c1 = par[1];
    Double_t c2 = par[2];
    Int_t Z_Target = (int)(par[3]);
    Int_t Ncoll = (int)(par[4]);
    Int_t Ngrey = (int)(x[0]);
    
    Double_t parbin[2];
    parbin[0] = par[4];
    parbin[1] = par[0];

    static double last_c0 = -9999;
    static double last_c1 = -9999;
    static double last_c2 = -9999;
    static double lastZTarget = -9999;
    static double cachedval[40][80] = {{-9999}};
    if(   c0 != last_c0
       || c1 != last_c1
       || c2 != last_c2
       || Z_Target != lastZTarget )
    {
       last_c0 = c0; last_c1 = c1; last_c2 = c2; lastZTarget = Z_Target;
       for(int icol = 0; icol < 40; icol++)
          for(int ing = 0; ing < 80; ing++) cachedval[icol][ing] = -9999;
       std::cout<<__func__<<" Resetting cache for new values: ";
       std::cout<< c0 << ":" << c1 << ":" << c2 << ":" <<Z_Target<<std::endl;
    }
    
    double meanNgrey = MeanNgrey(Ncoll,c0,c1,c2);
    
    if(meanNgrey<0.0)
    {
       //std::cout<<__FILE__<<":"<<__LINE__<<"ERROR: Negative Mean Ngrey "
       //<<c0<<":"<<c1<<":"<<c2<<std::endl;
       meanNgrey = 0.0;
    }

    if(cachedval[Ncoll][Ngrey] == -9999)
    {
       
    
    //result = NcfclA::myNegBinomial(x, parbin);    
    cachedval[Ncoll][Ngrey] = NcfclA::Binomial(Ngrey,
	 			       Z_Target,
	 			       MeanNgrey(Ncoll,c0,c1,c2)/Z_Target
	 			       );
    }
    
    return cachedval[Ncoll][Ngrey];
  };

// Conditional Probability Distribution for observing Ngrey grey protons
// particles when Ncoll binary collisions occurred in a nucleus of
// Z_Target. Mean Ngrey dependence assumed to be a 2nd order polonomial

Double_t NcfclA::fPNgreyNcol2(Double_t *x, Double_t *par)
{
   
   Double_t c0 = par[0];
   Double_t c1 = par[1];
   Double_t c2 = par[2];
   Int_t Z_Target = (int)(par[3]);
   Int_t Ncoll = (int)(par[4]);
   Int_t Ngrey = (int)(x[0]);
   
   Double_t parbin[2];
   parbin[0] = par[4];
   parbin[1] = par[0];
   
   static double _last_c0 = -9999;
   static double _last_c1 = -9999;
   static double _last_c2 = -9999;
   static double _lastZTarget = -9999;
   static double _cachedval[40][80] = {{-9999}};
   if(   c0 != _last_c0
         || c1 != _last_c1
         || c2 != _last_c2
         || Z_Target != _lastZTarget )
   {
      _last_c0 = c0; _last_c1 = c1; _last_c2 = c2; _lastZTarget = Z_Target;
      for(int icol = 0; icol < 40; icol++)
         for(int ing = 0; ing < 80; ing++) _cachedval[icol][ing] = -9999;
      std::cout<<__func__<<" Resetting cache for new values: ";
      std::cout<< c0 << ":" << c1 << ":" << c2 << ":" <<Z_Target<<std::endl;
   }
   
   double meanNgrey = MeanNgrey(Ncoll,c0,c1,c2);
   
   if(meanNgrey<0.0)
   {
      //std::cout<<__FILE__<<":"<<__LINE__<<"ERROR: Negative Mean Ngrey "
      //<<c0<<":"<<c1<<":"<<c2<<std::endl;
      meanNgrey = 0.0;
   }
   
   if(_cachedval[Ncoll][Ngrey] == -9999)
   {
      
      
      //result = NcfclA::myNegBinomial(x, parbin);    
      _cachedval[Ncoll][Ngrey] = NcfclA::Binomial(Ngrey,
                                                 Z_Target,
                                                 MeanNgrey(Ncoll,c0,c1,c2)/Z_Target
                                                 );
   }
   
   return _cachedval[Ncoll][Ngrey];
};

  //Probability of Ngreys in Fcal Acceptance given Ngreys total
  //Assume Binomial Distribution
  Double_t NcfclA::fPNgfcalNg(Double_t *x, Double_t *par)
  {
    Int_t Ngf = (int)(x[0]); // Number of greys in Efcal Acceptance
    Int_t Ngtot = (int)(par[0]); // Total Number of Greys in collision
    Double_t Acc = par[1]; // Relative Acceptance (eg Ngf/Ngrey=0.1538 RQMD)
   
    if(Acc == 1.0) if(Ngf == Ngtot) return 1.0; else return 0.0;
    
    return NcfclA::Binomial(Ngf,Ngtot,Acc);
  }

  Double_t NcfclA::fPEfcalNcol(Double_t *x, Double_t *par)
  {

    Double_t result = 0.0;
    Double_t tresult = 0.0;
    Double_t parNgrey[5];
    parNgrey[0]= par[0]; //gamma_b
    parNgrey[1]= par[1]; //a0
    parNgrey[2]= par[2]; //a1
    parNgrey[3]= par[3]; //0.0 width -- detector resolution
    //    parNgrey[4]; //Ngrey

    Double_t xNcol[1];
    Double_t parNcol[5];
    parNcol[0]=par[4];//c0
    parNcol[1]=par[5];//c1
    parNcol[2]=par[6];//c2
    parNcol[3]=par[7];//Z_Target
    parNcol[4]=par[9];//Ncol

    Double_t xFclAcc[1]; //Ngf
    //    Double_t parFclAcc[2];
    
    //Sum over Ngrey
    Double_t t1,t3;
    for(int nGrey = 0; nGrey<par[7]; nGrey++)
    {
       
       xNcol[0] = nGrey;
       xFclAcc[0]=nGrey;
       parNgrey[4]= nGrey;
       
       t1 = NcfclA::fPEfcalNgrey(x,parNgrey);
       if(t1!=t1) {std::cout<<"NcfclA::fPEfcalNgrey returned NaN "
          <<x[0]<<":"<<parNgrey[0]<<":"<<parNgrey[0]
          <<":"<<parNgrey[1]<<":"<<parNgrey[2]<<std::endl;
          return 0.0;}
       
       t3 = NcfclA::fPNgreyNcol(xNcol,parNcol);
       if(t3!=t3) {std::cout<<"NcfclA::fPNgreyNcol returned NaN "
          <<xNcol[0]<<":"<<parNcol[4]<<":"<<parNcol[0]<<":"
          <<parNcol[1]<<":"<<parNcol[2]<<":"<<std::endl; return 0.0;}
       
       tresult=t1*t3;
       result+=tresult;
              
       if(nGrey>par[7])
       {
          std::cout<<"Ngrey too large.!!!"<<std::endl;
          break;
       }
    }
       return result;
  };


  Double_t NcfclA::fPEfcalNcolRenorm(Double_t *x, Double_t *par)
  {

    int Npar = 9;
    int Ncol = (int)(par[9]);
    static double lastpar[9];
    static double tmpnorm;
    static double norm[NcfclA::maxNcolSum];
    // Determine if we need to recalculate the normalizations
    // Check if previous parameters were identical
    bool parchange = false;
    for(int idpar = 0; idpar < Npar; idpar++)
    {
      if(lastpar[idpar]!=par[idpar])
      {
	lastpar[idpar]=par[idpar];
	parchange = true;
      }
    }
    
    if(parchange)
    {
       //reset the normalization array
       std::cout<<"Params:";
       for(int idpar = 0; idpar < Npar+1; idpar++)
          {
            std::cout<<" :"<<par[idpar];
          }
       std::cout<<std::endl;
       for(int idnorm = 0; idnorm <NcfclA::maxNcolSum; idnorm++ )
      {
         // norm[idnorm] = -1.0;
      	norm[idnorm] = 1.0;
      }
    }

    
    static int nfits = 0;
    static float maxenergy = 200.0;
    
    if(norm[Ncol]<0.0)
    {
      TF1 Renorm("Renorm",NcfclA::fPEfcalNcol,0.0,maxenergy,10);
      for(int idpar = 0; idpar < Npar; idpar++)
      {
	Renorm.SetParameter(idpar, par[idpar]);
      }
      Renorm.SetParameter(Npar,Ncol);

     std::cout<<"Renormalizing ... "<<nfits++<<std::endl;
      norm[Ncol] = Renorm.Integral(1e-15,maxenergy);
      std::cout<<"Done. Ncol"<<Ncol<<":"<<norm[Ncol]<<std::endl;

      if(norm[Ncol]>0.999)
      {
	//Assume higher Ncol will be 1.0
	for(int idn = Ncol+1; idn < NcfclA::maxNcolSum; idn++) norm[idn] = 1.0;
      }



      //if(norm[Ncol]==NaN)
      {
	std::cout<<"ERROR:";
	for(int idpar = 0; idpar < Npar+1; idpar++)
        {
	  std::cout<<" :"<<Renorm.GetParameter(idpar);
        }
	std::cout<<std::endl;
      }
    }
    tmpnorm = norm[Ncol];

    return NcfclA::fPEfcalNcol(x,par)/tmpnorm;
    
  }


  Double_t NcfclA::fPGlauber(Double_t *x, Double_t *par)
  {
    Double_t PGlauber = hGlauberDist->GetBinContent(
				      hGlauberDist->FindBin(x[0])
				      );
    return PGlauber;
  }

  Double_t NcfclA::fPGlauberEfcalNcol(Double_t *x, Double_t *par)
  {
    Double_t xG[1];
    xG[0]=par[9]; //Ncol
//    return (NcfclA::fPGlauber(xG,par) * NcfclA::fPEfcalNcolRenorm(x, par));
    double t1 = NcfclA::fPGlauber(xG,par);
    double t2 =  NcfclA::fPEfcalNcol(x, par);
    return t1*t2;
  };

  Double_t NcfclA::fPEfcalSumNcol(Double_t *x, Double_t *par)
  {
    Double_t result = 0.0;
    Double_t tresult = 0.0;
    Double_t tpar[10];
    for(int ipar = 0; ipar < 9; ipar++){ tpar[ipar]=par[ipar];}

    for(int Ncol = 1; Ncol < NcfclA::maxNcolSum; Ncol++)
    {
      tpar[9] = Ncol;
      tresult = NcfclA::fPGlauberEfcalNcol(x,tpar);
      result+=tresult;

      if(result>0.0)
	if(fabs(tresult/result)<1E-5){
	  //cout<<"summed to nCol = "<<Ncol<<endl;
	  break;
	}
    }
    return result;
  };

Double_t NcfclA::fGlauberNgreyNcol(Double_t *x, Double_t *par){
   Double_t gx[1];
   Double_t Ncol = par[4];
   gx[0] = Ncol;

   return NcfclA::fPGlauber(gx,par)*NcfclA::fPNgreyNcol(x,par);
};

Double_t NcfclA::fNgreySumNcol(Double_t *x, Double_t *par)
{
   Double_t result = 0.0;
   Double_t tresult = 0.0;
   Double_t tpar[5];
   for(int ipar = 0; ipar < 4; ipar++){ tpar[ipar]=par[ipar];}
   
   for(int Ncol = 1; Ncol < NcfclA::maxNcolSum; Ncol++)
   {
      tpar[4] = Ncol;
      tresult = NcfclA::fGlauberNgreyNcol(x,tpar);
      result+=tresult;
   }
   return result;
   
};


  Double_t NcfclA::fPSimpleEfcalNcol(Double_t *x, Double_t *par)
  {
    //    Double_t Efcal = x[0];
    Double_t Ncol = par[5];
    Double_t gamma_p;
    Double_t result;
    static double lgamma_b = 0.0;
    static double la0 = 0.0;
    static double la1 = 0.0;
    static double la2 = 0.0;
    static double la3 = 0.0;
    
    //Two Slope
    //if(Ncol < par[3])
    //   gamma_p = par[1] + par[2]*Ncol;
    //else
    //   gamma_p = par[1] +(par[2]-par[4])*par[3] + par[4]*Ncol;
    
    //Quadratic
//    gamma_p = par[1] + par[2]*(Ncol + par[4]*Ncol*Ncol);
    
    // Saturation Function 1
//    gamma_p =  par[1] + par[2]*Ncol
//               + par[3]*(exp(Ncol/par[4])-1.0)/exp(Ncol/par[4]);
    
//     // Saturation Function 2
//     gamma_p = 
//        + par[3]*(1.0 - exp(-pow(Ncol/par[4],par[1])));

    // Poly 2
    gamma_p = par[4]*pow(Ncol-par[3],2.0)+par[1];
    
    if(gamma_p < 0.0) gamma_p=0.01;

    Double_t gamma_b = par[0];
    
//    if(x[0]<0.0) return 0.0;
    
    Double_t gpar[3];
    gpar[0]=gamma_p;
    gpar[1]=gamma_b;
    gpar[2]=par[2];
    
    result = NcfclA::SmearedGammaDist(x,gpar);
    //result = NcfclA::GammaDist(x,gpar);

    if(lgamma_b!=par[0]||la0!=par[1]||la1!=par[2]||la2!=par[3]||la3!=par[4])
    {
       lgamma_b=par[0]; la0=par[1]; la1=par[2]; la2=par[3];  la3=par[4];
       printf("%f %f %f %f %f %f\n",lgamma_b, la0, la1, la2, la3, gamma_p);
    }
    if(result < 0) result = 0.0;
    
    return result;
  };

  Double_t NcfclA::fPSimpleGlauberEfcalNcol(Double_t *x, Double_t *par)
  {
    Double_t xglauber[1];
    xglauber[0]=par[5];
    return NcfclA::fPGlauber(xglauber,par) * NcfclA::fPSimpleEfcalNcol(x, par);
  }

  Double_t NcfclA::fPSimpleGlauberEfcalSumNcol(Double_t *x, Double_t *par)
  {
    Double_t result = 0.0;
    Double_t tresult = 0.0;
    Double_t tpar[6];
    for(int parid = 0;parid < 5; parid++) tpar[parid]= par[parid];
    for(int Ncol = 1; Ncol < NcfclA::maxNcolSum; Ncol++)
    {
      tpar[5]=Ncol;
      tresult = NcfclA::fPSimpleGlauberEfcalNcol(x,tpar);
      result+=tresult;
    }
    return result;
  }

  Double_t NcfclA::fPZdcEBlack(Double_t* x, Double_t* par)
  {
    Double_t result;
    //    Double_t energy = x[0];
    Double_t mean = par[0];
    Double_t sigma = par[1];
    Int_t nBlack = (Int_t)(par[2]);
    Double_t tpar[2];
    tpar[0] = mean * nBlack;
    tpar[1] = sqrt(nBlack) * sigma;
    //Special Case
    if(nBlack==0) tpar[1]=1.0;
    result = normal(x, tpar);
    
    return result;
  }

Double_t NcfclA::fPZdcBlackNcol(Double_t* x, Double_t* par)
{
   //Assume a binomial distribution with <nBlack> O2 Polynomial
   
   Int_t nBlack = (int)(x[0]);
   Double_t a0 = par[0];
   Double_t a1 = par[1];
   Double_t a2 = par[2];
   //   Double_t a3 = par[3];
   //   Double_t a4 = par[4];
   Double_t a5 = par[5];
   Int_t N_Target = (Int_t)(par[6]);
   Int_t Ncol = (Int_t)(par[7]);
   
   Double_t mean;
   //mean = a0*(1.0-TMath::Exp(-a1*Ncol));
   
   
   mean = a0*(TMath::Exp(a1*(Ncol-a2))-1.0)/TMath::Exp(a1*(Ncol-a2));

   
	/*
	Double_t gb = a3;
	Double_t gp = mean*gb;
	Double_t gpar[2];
	gpar[0] = gp;
	gpar[1] = gb;
	return NcfclA::GammaDist(x,gpar);
	*/

	Double_t npar[2];
	npar[0]=mean;
	npar[1] = a5+0.1*Ncol;
	
   //npar[0] = hZdcNcolMean->GetBinContent(Ncol+1);
   //npar[1] = hZdcNcolSigma->GetBinContent(Ncol+1);
   
	return norm(x,npar);

   //if(x[0]<(mean-a2)||x[0]>mean+a2) return 0;
   //return 1.0/(2*a2);
   Double_t prob = mean/(Double_t)(N_Target);
   
   return NcfclA::Binomial(nBlack,N_Target,prob);
}

Double_t NcfclA::fPZdcNcolSumNblack(Double_t* x, Double_t* par)
{
   //Zdc Energy per black
   Double_t mean = par[0];
   Double_t sigma = par[1];
   //   Double_t nBlack;
   Double_t epar[3];
   epar[0]=mean;
   epar[1]=sigma;
   
   //Zdc blacks per Ncol
   Double_t a0 = par[2];
   Double_t a1 = par[3];
   Double_t a2 = par[4];
   Double_t a3 = par[5];
   Double_t b4 = par[6];
   Double_t b5 = par[7];
   //   Int_t N_Target = (Int_t) par[8]; //Au - 118
   //   Int_t Ncol = (Int_t) par[9];
   Double_t nbpar[8];
   Double_t xb[1];
   nbpar[0] = a0;
   nbpar[1] = a1;
   nbpar[2] = a2;
   nbpar[3] = a3;
   nbpar[4] = b4;
   nbpar[5] = b5;
   nbpar[6] = par[8];
   nbpar[7] = par[9];
   //Sum over nBlacks
   Double_t sum = 0;
   for(int nB = 0; nB <= 99; nB++){ //N_Target; nB++){
      epar[2]=nB;
      xb[0]=nB;
      sum+= NcfclA::fPZdcBlackNcol(xb,nbpar)*NcfclA::fPZdcEBlack(x,epar);
   }
   
   return sum;
}

Double_t NcfclA::fPZdcSumNcol(Double_t* x, Double_t* par)
{
   Double_t tpar[10];
   for(int tid = 0; tid < 9; tid++) tpar[tid] = par[tid];
   Double_t xglauber[1];
   Double_t parglauber[1];
   Double_t sum = 0;
   Double_t tval = 0;
   for(int ncol = 1; ncol<100; ncol++){
      xglauber[0] = ncol;
      tpar[9] = ncol;
      tval = NcfclA::fPGlauber(xglauber,parglauber)
         *NcfclA::fPZdcNcolSumNblack(x,tpar);
      sum+=tval;
      if(sum>0)
         if(tval/sum<1e-5) break;
   }
   return sum;
}

Double_t NcfclA::fPZNumBlackSumNcol(Double_t* x, Double_t* par)
{
   Double_t tpar[8];
   for(int tid = 0; tid < 7; tid++) tpar[tid] = par[tid];
   Double_t xglauber[1];
   Double_t parglauber[1];
   Double_t sum = 0;
   Double_t tval = 0;
   for(int ncol = 1; ncol<100; ncol++){
      xglauber[0] = ncol;
      tpar[7] = ncol;
      tval = NcfclA::fPGlauber(xglauber,parglauber)
         *NcfclA::fPZdcBlackNcol(x,tpar);
      sum+=tval;
      if(sum>0)
         if(tval/sum<1e-5) break;
   }
   return sum;   
}

Double_t NcfclA::fPGlauberEfcalGammaNcol(Double_t* x, Double_t* par)
{

   Double_t parNcol[5];
   parNcol[0]= par[0]; //gamma_b
   parNcol[1]= par[1]; //a0
   parNcol[2]= par[2]; //a1
   parNcol[3]= par[3]; //0.0 width -- detector resolution
   //   parNcol[4]; //Ncol
   
   Double_t parglauber[1];
   Double_t xglauber[1];

   Double_t tval, sum =0.0;
   for(int ncol = 1; ncol<NcfclA::maxNcolSum; ncol++){
      xglauber[0] = ncol;
      parNcol[4] = ncol;
      tval = NcfclA::fPGlauber(xglauber,parglauber)
         *NcfclA::fPEfcalNgrey(x,parNcol);
      sum+=tval;
   }
   
   return sum;
   
}
