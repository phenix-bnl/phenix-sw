#include <TF1.h>
#include <TGraph.h>
#include <TH1.h>
#include <TMath.h>
#include <TRandom.h>
#include <cmath>
#include <cstddef>
#include <iostream>
#include <iterator>
#include <list>
#include <string>
#include <utility>
#include <vector>
#include "Bremsstrahlung.h"
#include "Momentum.h"
#include "Particle.h"
#include "ParticleList.h"
#include "Tools.h"

class MyTH1: public TH1D {
  public:
    double GetRandomWithLower(double lower);
};
double MyTH1::GetRandomWithLower(double lower) {
  // Returns a random value above some lower value.
  // We need this to sample from Bethe-Heitler-approximation and guarantee a
  // minimal energy (since we used a cut on energy of the photon when
  // generating the data in GEANT4).
  //
  // This whole function is for the biggest part a verbatim copy of
  // TH1::GetRandom; the only real difference being how we calculate r1.
  //
  // TODO if this is supported in ROOT please remove this function

  if (fDimension>1) {
    Error("GetRandom","Function only valid for 1-d histograms");
    return 0;
  }
  int nbinsx = GetNbinsX();
  double integral = 0;
  if (fIntegral) {
    if (fIntegral[nbinsx+1] != fEntries)
      integral = static_cast<TH1*>(this)->ComputeIntegral();
  } else {
    integral = (static_cast<TH1*>(this))->ComputeIntegral();
  }
  if (integral==0 || fIntegral==0)
    return 0;

  // These 2 lines are the only real difference to TH1::GetRandom:
  //   * find the bin (double)lower sits in
  //   * do not choose a random with cumulative probability (0,1), but with
  //     a cumulative probability guaranteed to be higher than anything
  //     below (double) lower
  int binLower = this->GetXaxis()->FindBin(lower);
  double r1 = gRandom->Uniform(fIntegral[binLower],1);

  int ibin = static_cast<int>(TMath::BinarySearch(nbinsx,fIntegral,r1));
  double x = GetBinLowEdge(ibin+1);
  if (r1 > fIntegral[ibin]) x+=
    GetBinWidth(ibin+1)*
      (r1-fIntegral[ibin])/(fIntegral[ibin+1]-fIntegral[ibin]);
  return x;
}

namespace Bremsstrahlung {

  static TH1* dEE;
  static TGraph* freep;
  static TF1* angleE;
  const double m = 0.51099906e-3;

  ExternalBremsstrahlung::ExternalBremsstrahlung(const double thickness_):
    thickness(thickness_),
    pathLength() {
      std::cout << "Using Bremsstrahlung" << '\n';

      InitializeHistograms();
    }

  void ExternalBremsstrahlung::loop(ParticleList& event) {
    // The ugly looping over the event goes here.
    //
    // Input is an event list, we just add the bremsstrahled particles to it and
    // nothing is returned.

    // Fill a vector to save us the hassle when inserting element into the event
    // while we are still iteration over it.
    typedef std::pair<Particle*,PLNode*> EventEntry;
    std::vector<EventEntry> eventList;
    PLNode* currentNode = event.GetHeadNode();
    for (int i=1; i<=event.GetLength(); i++) {
      currentNode = currentNode->GetNextNode();
      eventList.push_back(std::make_pair(currentNode->Get(0),currentNode));
    }

    // Now loop over the eventList and process particles.
    for (size_t i=0; i<eventList.size(); ++i) {
      Particle* p = eventList.at(i).first;

      // Crudely determine if we have a final state particle and do not
      // process non-final states.
      bool isFinal = false;
      const Particle* nextP = (i+1==eventList.size()) ? 0 : eventList.at(i+1).first;
      if (nextP == 0 or p->GetGeneration()>=nextP->GetGeneration()) isFinal=true;
      if (not isFinal) continue;

      //// only for finals from here on ////
      run(*p);

      if (not secondaries.empty()) {
        // we have secondaries, but we only care about the last lepton
        std::list<Particle>::reverse_iterator it = secondaries.rbegin();
        p->Set4mom((*it).Get4mom());
      }
    }
  }

  bool ExternalBremsstrahlung::run(const Particle& particle) {
    secondaries.clear();
    pathLength = thickness;
    if (not (particle.GetID() == 11 or particle.GetID() == -11)) return false;
    return sample(particle);
  }

  bool ExternalBremsstrahlung::sample(const Particle& electron) {
    // The sampling of photons through the whole medium.
    //
    // The resulting photons and the scattered electron are stored in
    // this->secondaries, with the electron last.
    // This whole function is implemented below as a recursive function,
    // so if you want to do something else with the electron along its
    // voyage, you should break this function up.
    if (isHappening(electron.Get4mom().GetE())) {
      std::pair<Particle,Particle> secs = sampleSecondaries(electron);
      secondaries.push_front(secs.second);
      sample(secs.first);
      return true;
    } else {
      if (not secondaries.empty())
        secondaries.push_back(electron); // put the radiating electron in as first particle
      return false;
    }
  }

  double ExternalBremsstrahlung::getFreePath(const double energy) const {
    // Sample the cross section
    //
    // The numbers here are from a fit to values for the mean free path [mm]
    // for electrons calculated with GEANT4 for Beryllium and minimal photon
    // energies of 200keV. Before returning we transform this number to
    // radiation lengths with the density explicitly.

    // Get the mean free path from freep.
    // freep is for energies in MeV, exodus has GeV --> extra 1e+3
    double f1   = -1;
    double e1   = 0;
    double free = 0;
    if (not freep) InitializeHistograms();
    const int npoints = freep->GetN();
    const int n = static_cast<int>(TMath::BinarySearch(npoints, freep->GetX(), energy*1e3));
    freep->GetPoint(n, e1, f1);
    if (n==-1) { // below range of freep --> free path = inf
      return -1;
    } else if(n<npoints-1) { // in between: get next point and interpolate
      double e2 = 0;
      double f2 = 0;
      freep->GetPoint(n+1, e2, f2);
      free = f1 + (energy*1e3-e1) * (f2-f1)/(e2-e1);
    } else {
      // if we are at the upper edge of freep's range return the last known value
      free = f1;
    }

    static const double density = 1.85e-3 /* g/mm^3 */;
    static const double X0 = (716.4*9.01) /
      (4.*(4+1)*std::log(287/std::sqrt(4))); // in g/cm^2; from PDG for Be A=9.01, Z=4

    // go from distance-->radiation lengths; factor 100 for 1/mm^2-->1/cm^2
    return free*density*100/X0;
  }

  bool ExternalBremsstrahlung::isHappening(const double energy) {
    // Sample the traveled path until the interaction takes place.
    // Reduce the total path left by that amount and return if an
    // interaction takes place.

    double freePath = getFreePath(energy);
    if (freePath<0)
      return false;
    pathLength -= gRandom->Exp(freePath);
    if (pathLength>0)
      return true;
    else
      return false;
  }

  std::pair<Particle,Particle>
    ExternalBremsstrahlung::sampleSecondaries(const Particle& electron) {
      // return a tuple of scattered electron (first) and radiated photon (second)

      std::pair<Particle,Particle> secs;
      Particle& l = secs.first;  // electron/positron
      Particle& g = secs.second; // photon

      l.SetID(electron.GetID());
      g.SetID(22);

      // sample electron energy with minimal photon energy of 200keV
      if (not dEE) InitializeHistograms();
      double El = electron.Get4mom().GetE() -
        (reinterpret_cast<MyTH1*>(dEE))->GetRandomWithLower(200e-6/electron.Get4mom().GetE())
        * electron.Get4mom().GetE();
      if (m>El) El=m; // workaround for strange floating point issues

      // new bremsstrahled electron and photon
      // Angular distribution in lab: angle(old_e,new_e)*E_old[MeV]
      if (not angleE) InitializeHistograms();
      const double angle = angleE->GetRandom()/(electron.Get4mom().GetE()*1e+3);
      const double mom = El*El-m*m>=0? std::sqrt(El*El - m*m) : 0;
      const double f = gRandom->Rndm();
      Mom4 lmom(El,
          mom*std::sin(angle)*f,
          mom*std::sin(angle)*std::sqrt(1-f*f),
          mom*std::cos(angle));
      const double theta = electron.Get4mom().Theta();
      const double phi   = electron.Get4mom().Phi();
      l.Set4mom(El,
          Rotate(lmom,
            std::cos(theta),std::sin(theta),std::cos(phi),std::sin(phi)));
      g.Set4mom(electron.Get4mom() - l.Get4mom());

      l.SetWeight(electron.GetWeight());
      g.SetWeight(electron.GetWeight());

      l.SetDecaysum(electron.GetDecaysum());
      g.SetDecaysum(electron.GetDecaysum());
      l.SetWeight(electron.GetWeight());
      g.SetWeight(electron.GetWeight());
      l.SetGeneration(electron.GetGeneration()+1);
      g.SetGeneration(electron.GetGeneration()+1);
      l.SetVertex(electron.GetxVertex(),electron.GetyVertex(),electron.GetzVertex());
      g.SetVertex(electron.GetxVertex(),electron.GetyVertex(),electron.GetzVertex());

      return secs;
    }

  void ExternalBremsstrahlung::setThickness(double thickness_) {
    // set the material thickness in radiation lengths [g/cm^2]
    thickness = thickness_;
  }

  bool InitializeHistograms() {
    // initialze the histograms used

    static TF1 myAngleE("angleE",
        "exp(13.7633-1.77542*x)+exp(11.8845-0.852931*x)",0,20);
    angleE = &myAngleE;

    static TH1D mydEE("dEE","Bethe-Heitler photon energy for E_min(gamma) = 200keV",500,0,1.095);
    mydEE.SetBinContent(1,4.17284e+06);
    mydEE.SetBinContent(2,754028);
    mydEE.SetBinContent(3,437857);
    mydEE.SetBinContent(4,310000);
    mydEE.SetBinContent(5,240539);
    mydEE.SetBinContent(6,195465);
    mydEE.SetBinContent(7,165436);
    mydEE.SetBinContent(8,141937);
    mydEE.SetBinContent(9,124753);
    mydEE.SetBinContent(10,111568);
    mydEE.SetBinContent(11,101156);
    mydEE.SetBinContent(12,91784);
    mydEE.SetBinContent(13,83755);
    mydEE.SetBinContent(14,77627);
    mydEE.SetBinContent(15,72302);
    mydEE.SetBinContent(16,67695);
    mydEE.SetBinContent(17,63175);
    mydEE.SetBinContent(18,59296);
    mydEE.SetBinContent(19,56279);
    mydEE.SetBinContent(20,52871);
    mydEE.SetBinContent(21,50487);
    mydEE.SetBinContent(22,47962);
    mydEE.SetBinContent(23,45445);
    mydEE.SetBinContent(24,43605);
    mydEE.SetBinContent(25,41553);
    mydEE.SetBinContent(26,39693);
    mydEE.SetBinContent(27,38190);
    mydEE.SetBinContent(28,36669);
    mydEE.SetBinContent(29,35547);
    mydEE.SetBinContent(30,34284);
    mydEE.SetBinContent(31,33085);
    mydEE.SetBinContent(32,31614);
    mydEE.SetBinContent(33,30901);
    mydEE.SetBinContent(34,29526);
    mydEE.SetBinContent(35,28879);
    mydEE.SetBinContent(36,27759);
    mydEE.SetBinContent(37,26922);
    mydEE.SetBinContent(38,26369);
    mydEE.SetBinContent(39,25521);
    mydEE.SetBinContent(40,24959);
    mydEE.SetBinContent(41,24170);
    mydEE.SetBinContent(42,23723);
    mydEE.SetBinContent(43,22930);
    mydEE.SetBinContent(44,22445);
    mydEE.SetBinContent(45,21682);
    mydEE.SetBinContent(46,21121);
    mydEE.SetBinContent(47,20805);
    mydEE.SetBinContent(48,20458);
    mydEE.SetBinContent(49,19859);
    mydEE.SetBinContent(50,19209);
    mydEE.SetBinContent(51,18612);
    mydEE.SetBinContent(52,18912);
    mydEE.SetBinContent(53,18151);
    mydEE.SetBinContent(54,17714);
    mydEE.SetBinContent(55,17149);
    mydEE.SetBinContent(56,16817);
    mydEE.SetBinContent(57,16421);
    mydEE.SetBinContent(58,16045);
    mydEE.SetBinContent(59,16113);
    mydEE.SetBinContent(60,15456);
    mydEE.SetBinContent(61,15442);
    mydEE.SetBinContent(62,14806);
    mydEE.SetBinContent(63,14795);
    mydEE.SetBinContent(64,14601);
    mydEE.SetBinContent(65,13996);
    mydEE.SetBinContent(66,14040);
    mydEE.SetBinContent(67,13638);
    mydEE.SetBinContent(68,13419);
    mydEE.SetBinContent(69,13340);
    mydEE.SetBinContent(70,13101);
    mydEE.SetBinContent(71,12759);
    mydEE.SetBinContent(72,12647);
    mydEE.SetBinContent(73,12168);
    mydEE.SetBinContent(74,12197);
    mydEE.SetBinContent(75,11769);
    mydEE.SetBinContent(76,12054);
    mydEE.SetBinContent(77,11599);
    mydEE.SetBinContent(78,11665);
    mydEE.SetBinContent(79,11337);
    mydEE.SetBinContent(80,10975);
    mydEE.SetBinContent(81,10908);
    mydEE.SetBinContent(82,10735);
    mydEE.SetBinContent(83,10607);
    mydEE.SetBinContent(84,10458);
    mydEE.SetBinContent(85,10135);
    mydEE.SetBinContent(86,10098);
    mydEE.SetBinContent(87,10016);
    mydEE.SetBinContent(88,10024);
    mydEE.SetBinContent(89,9734);
    mydEE.SetBinContent(90,9686);
    mydEE.SetBinContent(91,9451);
    mydEE.SetBinContent(92,9285);
    mydEE.SetBinContent(93,9379);
    mydEE.SetBinContent(94,9034);
    mydEE.SetBinContent(95,8965);
    mydEE.SetBinContent(96,8975);
    mydEE.SetBinContent(97,8859);
    mydEE.SetBinContent(98,8674);
    mydEE.SetBinContent(99,8617);
    mydEE.SetBinContent(100,8482);
    mydEE.SetBinContent(101,8262);
    mydEE.SetBinContent(102,8078);
    mydEE.SetBinContent(103,8173);
    mydEE.SetBinContent(104,8104);
    mydEE.SetBinContent(105,8030);
    mydEE.SetBinContent(106,8053);
    mydEE.SetBinContent(107,7807);
    mydEE.SetBinContent(108,7635);
    mydEE.SetBinContent(109,7508);
    mydEE.SetBinContent(110,7446);
    mydEE.SetBinContent(111,7307);
    mydEE.SetBinContent(112,7402);
    mydEE.SetBinContent(113,7266);
    mydEE.SetBinContent(114,7187);
    mydEE.SetBinContent(115,7124);
    mydEE.SetBinContent(116,7030);
    mydEE.SetBinContent(117,6963);
    mydEE.SetBinContent(118,6895);
    mydEE.SetBinContent(119,6904);
    mydEE.SetBinContent(120,6846);
    mydEE.SetBinContent(121,6526);
    mydEE.SetBinContent(122,6663);
    mydEE.SetBinContent(123,6616);
    mydEE.SetBinContent(124,6476);
    mydEE.SetBinContent(125,6337);
    mydEE.SetBinContent(126,6315);
    mydEE.SetBinContent(127,6244);
    mydEE.SetBinContent(128,6183);
    mydEE.SetBinContent(129,6037);
    mydEE.SetBinContent(130,6134);
    mydEE.SetBinContent(131,6127);
    mydEE.SetBinContent(132,5969);
    mydEE.SetBinContent(133,5976);
    mydEE.SetBinContent(134,5775);
    mydEE.SetBinContent(135,5668);
    mydEE.SetBinContent(136,5863);
    mydEE.SetBinContent(137,5559);
    mydEE.SetBinContent(138,5574);
    mydEE.SetBinContent(139,5628);
    mydEE.SetBinContent(140,5509);
    mydEE.SetBinContent(141,5469);
    mydEE.SetBinContent(142,5368);
    mydEE.SetBinContent(143,5393);
    mydEE.SetBinContent(144,5292);
    mydEE.SetBinContent(145,5284);
    mydEE.SetBinContent(146,5316);
    mydEE.SetBinContent(147,5123);
    mydEE.SetBinContent(148,5354);
    mydEE.SetBinContent(149,5147);
    mydEE.SetBinContent(150,5087);
    mydEE.SetBinContent(151,4915);
    mydEE.SetBinContent(152,4992);
    mydEE.SetBinContent(153,4940);
    mydEE.SetBinContent(154,4892);
    mydEE.SetBinContent(155,4718);
    mydEE.SetBinContent(156,4792);
    mydEE.SetBinContent(157,4658);
    mydEE.SetBinContent(158,4730);
    mydEE.SetBinContent(159,4644);
    mydEE.SetBinContent(160,4624);
    mydEE.SetBinContent(161,4715);
    mydEE.SetBinContent(162,4597);
    mydEE.SetBinContent(163,4497);
    mydEE.SetBinContent(164,4547);
    mydEE.SetBinContent(165,4430);
    mydEE.SetBinContent(166,4429);
    mydEE.SetBinContent(167,4374);
    mydEE.SetBinContent(168,4329);
    mydEE.SetBinContent(169,4293);
    mydEE.SetBinContent(170,4254);
    mydEE.SetBinContent(171,4251);
    mydEE.SetBinContent(172,4186);
    mydEE.SetBinContent(173,4173);
    mydEE.SetBinContent(174,4057);
    mydEE.SetBinContent(175,4210);
    mydEE.SetBinContent(176,4132);
    mydEE.SetBinContent(177,4174);
    mydEE.SetBinContent(178,4080);
    mydEE.SetBinContent(179,3986);
    mydEE.SetBinContent(180,3998);
    mydEE.SetBinContent(181,3998);
    mydEE.SetBinContent(182,3992);
    mydEE.SetBinContent(183,3913);
    mydEE.SetBinContent(184,3858);
    mydEE.SetBinContent(185,3891);
    mydEE.SetBinContent(186,3784);
    mydEE.SetBinContent(187,3778);
    mydEE.SetBinContent(188,3747);
    mydEE.SetBinContent(189,3706);
    mydEE.SetBinContent(190,3687);
    mydEE.SetBinContent(191,3682);
    mydEE.SetBinContent(192,3665);
    mydEE.SetBinContent(193,3632);
    mydEE.SetBinContent(194,3526);
    mydEE.SetBinContent(195,3536);
    mydEE.SetBinContent(196,3547);
    mydEE.SetBinContent(197,3529);
    mydEE.SetBinContent(198,3487);
    mydEE.SetBinContent(199,3500);
    mydEE.SetBinContent(200,3422);
    mydEE.SetBinContent(201,3457);
    mydEE.SetBinContent(202,3402);
    mydEE.SetBinContent(203,3374);
    mydEE.SetBinContent(204,3276);
    mydEE.SetBinContent(205,3353);
    mydEE.SetBinContent(206,3303);
    mydEE.SetBinContent(207,3439);
    mydEE.SetBinContent(208,3296);
    mydEE.SetBinContent(209,3283);
    mydEE.SetBinContent(210,3345);
    mydEE.SetBinContent(211,3182);
    mydEE.SetBinContent(212,3226);
    mydEE.SetBinContent(213,3131);
    mydEE.SetBinContent(214,3231);
    mydEE.SetBinContent(215,3170);
    mydEE.SetBinContent(216,3146);
    mydEE.SetBinContent(217,3099);
    mydEE.SetBinContent(218,3072);
    mydEE.SetBinContent(219,3098);
    mydEE.SetBinContent(220,2971);
    mydEE.SetBinContent(221,2985);
    mydEE.SetBinContent(222,3076);
    mydEE.SetBinContent(223,3016);
    mydEE.SetBinContent(224,3041);
    mydEE.SetBinContent(225,3029);
    mydEE.SetBinContent(226,2943);
    mydEE.SetBinContent(227,2944);
    mydEE.SetBinContent(228,2929);
    mydEE.SetBinContent(229,2884);
    mydEE.SetBinContent(230,2924);
    mydEE.SetBinContent(231,2845);
    mydEE.SetBinContent(232,2825);
    mydEE.SetBinContent(233,2832);
    mydEE.SetBinContent(234,2838);
    mydEE.SetBinContent(235,2766);
    mydEE.SetBinContent(236,2797);
    mydEE.SetBinContent(237,2734);
    mydEE.SetBinContent(238,2698);
    mydEE.SetBinContent(239,2796);
    mydEE.SetBinContent(240,2739);
    mydEE.SetBinContent(241,2785);
    mydEE.SetBinContent(242,2636);
    mydEE.SetBinContent(243,2765);
    mydEE.SetBinContent(244,2617);
    mydEE.SetBinContent(245,2721);
    mydEE.SetBinContent(246,2654);
    mydEE.SetBinContent(247,2653);
    mydEE.SetBinContent(248,2564);
    mydEE.SetBinContent(249,2748);
    mydEE.SetBinContent(250,2602);
    mydEE.SetBinContent(251,2703);
    mydEE.SetBinContent(252,2541);
    mydEE.SetBinContent(253,2581);
    mydEE.SetBinContent(254,2587);
    mydEE.SetBinContent(255,2547);
    mydEE.SetBinContent(256,2481);
    mydEE.SetBinContent(257,2558);
    mydEE.SetBinContent(258,2522);
    mydEE.SetBinContent(259,2490);
    mydEE.SetBinContent(260,2477);
    mydEE.SetBinContent(261,2471);
    mydEE.SetBinContent(262,2537);
    mydEE.SetBinContent(263,2514);
    mydEE.SetBinContent(264,2377);
    mydEE.SetBinContent(265,2607);
    mydEE.SetBinContent(266,2489);
    mydEE.SetBinContent(267,2519);
    mydEE.SetBinContent(268,2474);
    mydEE.SetBinContent(269,2455);
    mydEE.SetBinContent(270,2391);
    mydEE.SetBinContent(271,2483);
    mydEE.SetBinContent(272,2485);
    mydEE.SetBinContent(273,2302);
    mydEE.SetBinContent(274,2391);
    mydEE.SetBinContent(275,2403);
    mydEE.SetBinContent(276,2323);
    mydEE.SetBinContent(277,2323);
    mydEE.SetBinContent(278,2236);
    mydEE.SetBinContent(279,2293);
    mydEE.SetBinContent(280,2315);
    mydEE.SetBinContent(281,2257);
    mydEE.SetBinContent(282,2250);
    mydEE.SetBinContent(283,2158);
    mydEE.SetBinContent(284,2207);
    mydEE.SetBinContent(285,2322);
    mydEE.SetBinContent(286,2155);
    mydEE.SetBinContent(287,2159);
    mydEE.SetBinContent(288,2248);
    mydEE.SetBinContent(289,2239);
    mydEE.SetBinContent(290,2253);
    mydEE.SetBinContent(291,2198);
    mydEE.SetBinContent(292,2152);
    mydEE.SetBinContent(293,2247);
    mydEE.SetBinContent(294,2141);
    mydEE.SetBinContent(295,2224);
    mydEE.SetBinContent(296,2134);
    mydEE.SetBinContent(297,2151);
    mydEE.SetBinContent(298,2073);
    mydEE.SetBinContent(299,2143);
    mydEE.SetBinContent(300,2186);
    mydEE.SetBinContent(301,2086);
    mydEE.SetBinContent(302,2036);
    mydEE.SetBinContent(303,2053);
    mydEE.SetBinContent(304,2111);
    mydEE.SetBinContent(305,2053);
    mydEE.SetBinContent(306,2071);
    mydEE.SetBinContent(307,2096);
    mydEE.SetBinContent(308,2018);
    mydEE.SetBinContent(309,2081);
    mydEE.SetBinContent(310,2121);
    mydEE.SetBinContent(311,2136);
    mydEE.SetBinContent(312,2095);
    mydEE.SetBinContent(313,2055);
    mydEE.SetBinContent(314,2089);
    mydEE.SetBinContent(315,1937);
    mydEE.SetBinContent(316,2072);
    mydEE.SetBinContent(317,2006);
    mydEE.SetBinContent(318,1993);
    mydEE.SetBinContent(319,2084);
    mydEE.SetBinContent(320,1988);
    mydEE.SetBinContent(321,1947);
    mydEE.SetBinContent(322,2053);
    mydEE.SetBinContent(323,1971);
    mydEE.SetBinContent(324,2004);
    mydEE.SetBinContent(325,2032);
    mydEE.SetBinContent(326,1939);
    mydEE.SetBinContent(327,2084);
    mydEE.SetBinContent(328,1910);
    mydEE.SetBinContent(329,1911);
    mydEE.SetBinContent(330,2012);
    mydEE.SetBinContent(331,1899);
    mydEE.SetBinContent(332,1985);
    mydEE.SetBinContent(333,1924);
    mydEE.SetBinContent(334,1967);
    mydEE.SetBinContent(335,1976);
    mydEE.SetBinContent(336,1930);
    mydEE.SetBinContent(337,1896);
    mydEE.SetBinContent(338,1893);
    mydEE.SetBinContent(339,1941);
    mydEE.SetBinContent(340,1938);
    mydEE.SetBinContent(341,1892);
    mydEE.SetBinContent(342,1921);
    mydEE.SetBinContent(343,1799);
    mydEE.SetBinContent(344,1907);
    mydEE.SetBinContent(345,1822);
    mydEE.SetBinContent(346,1877);
    mydEE.SetBinContent(347,1849);
    mydEE.SetBinContent(348,1854);
    mydEE.SetBinContent(349,1851);
    mydEE.SetBinContent(350,1876);
    mydEE.SetBinContent(351,1903);
    mydEE.SetBinContent(352,1842);
    mydEE.SetBinContent(353,1847);
    mydEE.SetBinContent(354,1830);
    mydEE.SetBinContent(355,1866);
    mydEE.SetBinContent(356,1818);
    mydEE.SetBinContent(357,1841);
    mydEE.SetBinContent(358,1800);
    mydEE.SetBinContent(359,1862);
    mydEE.SetBinContent(360,1788);
    mydEE.SetBinContent(361,1843);
    mydEE.SetBinContent(362,1784);
    mydEE.SetBinContent(363,1840);
    mydEE.SetBinContent(364,1783);
    mydEE.SetBinContent(365,1870);
    mydEE.SetBinContent(366,1846);
    mydEE.SetBinContent(367,1779);
    mydEE.SetBinContent(368,1774);
    mydEE.SetBinContent(369,1813);
    mydEE.SetBinContent(370,1781);
    mydEE.SetBinContent(371,1776);
    mydEE.SetBinContent(372,1761);
    mydEE.SetBinContent(373,1668);
    mydEE.SetBinContent(374,1780);
    mydEE.SetBinContent(375,1757);
    mydEE.SetBinContent(376,1749);
    mydEE.SetBinContent(377,1790);
    mydEE.SetBinContent(378,1821);
    mydEE.SetBinContent(379,1764);
    mydEE.SetBinContent(380,1788);
    mydEE.SetBinContent(381,1759);
    mydEE.SetBinContent(382,1674);
    mydEE.SetBinContent(383,1784);
    mydEE.SetBinContent(384,1797);
    mydEE.SetBinContent(385,1791);
    mydEE.SetBinContent(386,1797);
    mydEE.SetBinContent(387,1815);
    mydEE.SetBinContent(388,1797);
    mydEE.SetBinContent(389,1728);
    mydEE.SetBinContent(390,1770);
    mydEE.SetBinContent(391,1753);
    mydEE.SetBinContent(392,1746);
    mydEE.SetBinContent(393,1702);
    mydEE.SetBinContent(394,1718);
    mydEE.SetBinContent(395,1778);
    mydEE.SetBinContent(396,1730);
    mydEE.SetBinContent(397,1654);
    mydEE.SetBinContent(398,1693);
    mydEE.SetBinContent(399,1729);
    mydEE.SetBinContent(400,1754);
    mydEE.SetBinContent(401,1705);
    mydEE.SetBinContent(402,1736);
    mydEE.SetBinContent(403,1657);
    mydEE.SetBinContent(404,1658);
    mydEE.SetBinContent(405,1729);
    mydEE.SetBinContent(406,1707);
    mydEE.SetBinContent(407,1761);
    mydEE.SetBinContent(408,1826);
    mydEE.SetBinContent(409,1642);
    mydEE.SetBinContent(410,1659);
    mydEE.SetBinContent(411,1719);
    mydEE.SetBinContent(412,1705);
    mydEE.SetBinContent(413,1637);
    mydEE.SetBinContent(414,1772);
    mydEE.SetBinContent(415,1681);
    mydEE.SetBinContent(416,1684);
    mydEE.SetBinContent(417,1696);
    mydEE.SetBinContent(418,1627);
    mydEE.SetBinContent(419,1709);
    mydEE.SetBinContent(420,1717);
    mydEE.SetBinContent(421,1625);
    mydEE.SetBinContent(422,1694);
    mydEE.SetBinContent(423,1666);
    mydEE.SetBinContent(424,1576);
    mydEE.SetBinContent(425,1744);
    mydEE.SetBinContent(426,1635);
    mydEE.SetBinContent(427,1617);
    mydEE.SetBinContent(428,1666);
    mydEE.SetBinContent(429,1688);
    mydEE.SetBinContent(430,1631);
    mydEE.SetBinContent(431,1593);
    mydEE.SetBinContent(432,1719);
    mydEE.SetBinContent(433,1622);
    mydEE.SetBinContent(434,1675);
    mydEE.SetBinContent(435,1623);
    mydEE.SetBinContent(436,1651);
    mydEE.SetBinContent(437,1668);
    mydEE.SetBinContent(438,1704);
    mydEE.SetBinContent(439,1609);
    mydEE.SetBinContent(440,1659);
    mydEE.SetBinContent(441,1712);
    mydEE.SetBinContent(442,1650);
    mydEE.SetBinContent(443,1612);
    mydEE.SetBinContent(444,1658);
    mydEE.SetBinContent(445,1656);
    mydEE.SetBinContent(446,1544);
    mydEE.SetBinContent(447,1649);
    mydEE.SetBinContent(448,1619);
    mydEE.SetBinContent(449,1618);
    mydEE.SetBinContent(450,1571);
    mydEE.SetBinContent(451,1661);
    mydEE.SetBinContent(452,1560);
    mydEE.SetBinContent(453,1693);
    mydEE.SetBinContent(454,1592);
    mydEE.SetBinContent(455,1576);
    mydEE.SetBinContent(456,1473);
    mydEE.SetBinContent(457,754);
    mydEE.SetEntries(1e+07);

    static TGraph myGraph(97);
    myGraph.SetName("freep");
    myGraph.SetPoint(0,10,62.2473);
    myGraph.SetPoint(1,11,59.7521);
    myGraph.SetPoint(2,12.1,57.4206);
    myGraph.SetPoint(3,13.31,55.2963);
    myGraph.SetPoint(4,14.641,53.362);
    myGraph.SetPoint(5,16.1051,56.4242);
    myGraph.SetPoint(6,17.7156,55.2248);
    myGraph.SetPoint(7,19.4872,54.0754);
    myGraph.SetPoint(8,21.4359,52.9728);
    myGraph.SetPoint(9,23.5795,51.9143);
    myGraph.SetPoint(10,25.9374,50.8973);
    myGraph.SetPoint(11,28.5312,49.9193);
    myGraph.SetPoint(12,31.3843,48.9783);
    myGraph.SetPoint(13,34.5227,48.072);
    myGraph.SetPoint(14,37.975,47.1987);
    myGraph.SetPoint(15,41.7725,46.3566);
    myGraph.SetPoint(16,45.9497,45.5439);
    myGraph.SetPoint(17,50.5447,44.7593);
    myGraph.SetPoint(18,55.5992,44.0012);
    myGraph.SetPoint(19,61.1591,43.2684);
    myGraph.SetPoint(20,67.275,42.5596);
    myGraph.SetPoint(21,74.0025,41.8737);
    myGraph.SetPoint(22,81.4027,41.2095);
    myGraph.SetPoint(23,89.543,40.5661);
    myGraph.SetPoint(24,98.4973,39.9424);
    myGraph.SetPoint(25,108.347,39.3376);
    myGraph.SetPoint(26,119.182,38.7509);
    myGraph.SetPoint(27,131.1,38.1814);
    myGraph.SetPoint(28,144.21,37.6284);
    myGraph.SetPoint(29,158.631,37.0912);
    myGraph.SetPoint(30,174.494,36.5691);
    myGraph.SetPoint(31,191.943,36.0616);
    myGraph.SetPoint(32,211.138,35.5679);
    myGraph.SetPoint(33,232.252,35.0875);
    myGraph.SetPoint(34,255.477,34.62);
    myGraph.SetPoint(35,281.024,34.1647);
    myGraph.SetPoint(36,309.127,33.7213);
    myGraph.SetPoint(37,340.039,33.2892);
    myGraph.SetPoint(38,374.043,32.8681);
    myGraph.SetPoint(39,411.448,32.4574);
    myGraph.SetPoint(40,452.593,32.0569);
    myGraph.SetPoint(41,497.852,31.6662);
    myGraph.SetPoint(42,547.637,31.2849);
    myGraph.SetPoint(43,602.401,30.9127);
    myGraph.SetPoint(44,662.641,30.5492);
    myGraph.SetPoint(45,728.905,30.1942);
    myGraph.SetPoint(46,801.795,29.8473);
    myGraph.SetPoint(47,881.975,29.5083);
    myGraph.SetPoint(48,970.172,29.1769);
    myGraph.SetPoint(49,1067.19,28.8529);
    myGraph.SetPoint(50,1173.91,28.536);
    myGraph.SetPoint(51,1291.3,28.2259);
    myGraph.SetPoint(52,1420.43,27.9226);
    myGraph.SetPoint(53,1562.47,27.6267);
    myGraph.SetPoint(54,1718.72,27.3376);
    myGraph.SetPoint(55,1890.59,27.0544);
    myGraph.SetPoint(56,2079.65,26.7777);
    myGraph.SetPoint(57,2287.62,26.5067);
    myGraph.SetPoint(58,2516.38,26.2418);
    myGraph.SetPoint(59,2768.01,25.9825);
    myGraph.SetPoint(60,3044.82,25.7291);
    myGraph.SetPoint(61,3349.3,25.4812);
    myGraph.SetPoint(62,3684.23,25.2391);
    myGraph.SetPoint(63,4052.65,25.0024);
    myGraph.SetPoint(64,4457.92,24.7714);
    myGraph.SetPoint(65,4903.71,24.5463);
    myGraph.SetPoint(66,5394.08,24.3267);
    myGraph.SetPoint(67,5933.49,24.1132);
    myGraph.SetPoint(68,6526.83,23.9056);
    myGraph.SetPoint(69,7179.52,23.7044);
    myGraph.SetPoint(70,7897.47,23.514);
    myGraph.SetPoint(71,8687.22,23.3639);
    myGraph.SetPoint(72,9555.94,23.2683);
    myGraph.SetPoint(73,10511.5,23.2166);
    myGraph.SetPoint(74,11562.7,23.1985);
    myGraph.SetPoint(75,12719,23.2102);
    myGraph.SetPoint(76,13990.8,23.2436);
    myGraph.SetPoint(77,15389.9,23.2994);
    myGraph.SetPoint(78,16928.9,23.3692);
    myGraph.SetPoint(79,18621.8,23.4584);
    myGraph.SetPoint(80,20484,23.5561);
    myGraph.SetPoint(81,22532.4,23.6724);
    myGraph.SetPoint(82,24785.6,23.7927);
    myGraph.SetPoint(83,27264.2,23.9325);
    myGraph.SetPoint(84,29990.6,24.0828);
    myGraph.SetPoint(85,32989.7,24.2439);
    myGraph.SetPoint(86,36288.7,24.4087);
    myGraph.SetPoint(87,39917.5,24.5875);
    myGraph.SetPoint(88,43909.3,24.7706);
    myGraph.SetPoint(89,48300.2,24.9657);
    myGraph.SetPoint(90,53130.2,25.1666);
    myGraph.SetPoint(91,58443.2,25.3768);
    myGraph.SetPoint(92,64287.6,25.5953);
    myGraph.SetPoint(93,70716.3,25.8198);
    myGraph.SetPoint(94,77788,26.0557);
    myGraph.SetPoint(95,85566.8,26.294);
    myGraph.SetPoint(96,94123.4,26.5475);

    dEE = &mydEE;
    freep = &myGraph;
    return true;
  }

  bool InternalBremsstrahlung(Particle& p1, Particle& p2) {
    if (not dEE) InitializeHistograms();
    if (not ((p1.GetID()== 11 and p2.GetID()==-11 )
          or (p1.GetID()==-11 and p2.GetID()== 11)))
      return false; // Hey! I said for electron-positron pairs only!

    const double alpha = 1/137.036;
    const double pi    = TMath::Pi();

    // Calculate probability for radiation:
    // Take a fixed C_hard here since we are only dealing with J/Psi->ee.
    // For other cases C_hard can be calculated with eq.(6) of [1].
    const double M = (p1.Get4mom()+p2.Get4mom()).Abs();
    const double m = 0.51099906e-3;
    const double C_hard = alpha/(2*pi) *
      (4*std::log(M/(2*0.01))*(2*std::log((M)/(m))-1)
       -6*std::log((M)/(m))-2*pi*pi/3+11./2); // TODO cache this
    const double W_hard = C_hard/(1+3*alpha/(4*pi)); // (15) of [1]

    const double radiating_fraction = W_hard;
    // anything happening here?
    if (gRandom->Rndm() < 1-radiating_fraction) return false;

    Particle* pa[] = {&p1, &p2};

    // we only care about one single photon being emitted so
    // select one random lepton
    Particle& p = *pa[gRandom->Integer(2)];

    // Sample the photon energy
    double El = p.Get4mom().GetE() - dEE->GetRandom() * p.Get4mom().GetE();
    if (m>El) El=m;

    // adjust the particle's momentum and direction
    if (not angleE) InitializeHistograms();
    const double angle = angleE->GetRandom()/(p.Get4mom().GetE()*1e+3);
    const double mom = std::sqrt(El*El - m*m);
    const double f = gRandom->Rndm();
    Mom4 lmom(El,
        mom*std::sin(angle)*f,
        mom*std::sin(angle)*std::sqrt(1-f*f),
        mom*std::cos(angle));
    const double theta = p.Get4mom().Theta();
    const double phi   = p.Get4mom().Phi();
    p.Set4mom(El,
        Rotate(lmom,
          std::cos(theta),std::sin(theta),std::cos(phi),std::sin(phi)));

    return true;
  }
}
