// $Id: MWGInclusiveNanoCutsv1.h,v 1.1 2009/07/04 18:32:20 hpereira Exp $
/*  Créé le        : Wed Jul 17 16:41:46 2002
 *  Auteur         : Fleuret
 *  Modifié le     : Time-stamp: <03/07/02 11:16:16 fleuret> 
 *  Numéro version : 0
 *  Status         : 
 */
#ifndef MWGINCLUSIVENANOCUTSV1_H
#define MWGINCLUSIVENANOCUTSV1_H

#include "PHInclusiveNanoCuts.h"
#include "phool.h"

class PHCompositeNode;
class PHMuoTracksOut;

/** Muon NanoDSTs Inclusive cuts 

This class is intended to apply several cuts on incoming events and tracks. In a first step,
In order to be stored in the nanoDST output, all incoming events must pass the selection cuts applied 
in {\it GlobalOK}. In a second step, all incoming particles from the selected event must pass the selection 
cuts applied in {\it MuonOK}. Finally, all incoming dimuon candidates must pass the selection cuts applied
in {\it diMuonOK}.

\paragraph{History}
 \begin{enumerate}
  \item Class representing a collection of cuts. \\
       {\bf Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html} }\\
       {\bf Author: Sasha Lebedev (ISU)} (lebedev@iastate.edu)
  \item {\bf Thomas K. Hemmick  3-12-2002:} This class inherits from the PHnanoCuts class
       and implements the global event and track cuts
       which are appropriate for the Electron Working 
       Group.  The structure is largely due to the 
       original PHCut code from Sasha Lebedev.  It has
       been adapted to inherit from the PHnanoCuts base
       class as a mechanism to inforce the interface upon
       all the cut objects used for the formation of
       nDSTs.
  \item {\bf Thomas K. Hemmick  3-13-2002:} This routine is a shell into which the MWG should
       code their actual cutting routines.  It has already
       satisfied the inheritance from the base class and
       has "examples" of a pt cut.  Noone forces you to actually
       *use* a pt cut, but this is at least an example of a
       cut which any particle could have applied to it.
       I hope you find coding in here convenient and to your
       liking.

  \item {\bf Fr\'ed\'eric Fleuret 7-17-2002:} Add event selection on minimum number of 
       muon tracks per event (default value set to 1)

  \item {\bf Fr\'ed\'eric Fleuret 7-17-2002:} Add event selection on maximum number of 
       muon tracks per event (default value set to 1000)

  \item {\bf Fr\'ed\'eric Fleuret 8-22-2002:} Selection cut values are read from the
       file ./work/MWG.rcp (it must stand in the same directory as make_MWG.C). If
       MWG.rcp does not exist, default values are used.
 \end{enumerate}
*/
class MWGInclusiveNanoCutsv1 : public PHInclusiveNanoCuts {

public:

  MWGInclusiveNanoCutsv1();
  virtual ~MWGInclusiveNanoCutsv1() { }

  // These are necessary for all PHObjects:
  int  isValid() const {return(1);}
  void identify(std::ostream &os=std::cout) const {os << "identify yourself: MWGInclusiveNanoCutsv1 Object" << std::endl;}

    /**@name Initialization

    This method set event, track and dimuon cut values which will be used to make event,
    track and dimuon selections in {\it GlobalOK}, {\it MuonOK} and {\it diMuonOK} methods. 
    Cut values are read from the
    file {\it MWG.rcp} (see /packages/MWG/work directory) which must stand in the 
    directory where one runs {\it make_MWG.C}. If {\it MWG.rcp} does not exist, then default values
    are read. Default values are set to: 
    \begin{itemize}
    \item {\bf event selections}
    \begin{itemize}
    \item $vertexcut$ = 10000 (in cm)
    \end{itemize}
    \item {\bf track selections}
    \begin{itemize}
    \item $ghostsel$ = 1
    \item $ptlowcut$ = 0 (in GeV)
    \item $pthighcut$ = 1000 (in GeV)
    \item $minhitcut$ = 0
    \end{itemize}
    \item {\bf dimuon selections}
    \begin{itemize}
    \item $dodimu$ = true
    \item $dimasscut$ = 0.5
    \end{itemize}
    \end{itemize}
     */
  void Initialize(PHCompositeNode* topnode=0);
  // These routines override and implement the pure virtual 
  // ones from the base class

    /**@name GlobalOK

    This method makes event selection. Input parameters are : $vertexcut$
    \begin{enumerate}
    \item {\bf Event Z$_{vertex}$ cut:} keep event if $vertexcut$ > $|$event Z$_{vertex}|$
    \end{enumerate}
    If the event candidate pass these cuts, then GlobalOK returns {\bf true} and one
    proceeds.
    */

  PHBoolean GlobalOK  (PHCompositeNode* topnode);
    /**@name MuonOK 

    This method checks if track candidates pass the cuts. It makes sanity check and particle 
    cuts. Input parameters are : 
    $ptlowcut$, $pthighcut$ and $minhitcut$.
    \begin{enumerate}
    \item Track's Index Sanity Check : 0$\leq$ track index $\leq$ number of entries  
    \item Track's P$_T$ cut : $ptlowcut$ < track P$_T$ < $pthighcut$ 
    \item Track's number of hits : $minhitcut$ < track's number of hits    
    \end{enumerate}
    If track candidate pass these cuts, then MuonOK returns {\bf true} and the 
    track is stored in the output (if event pass the GlobalOK selections).
    */
  PHBoolean MuonOK    (PHMuoTracksOut* php, const unsigned int itrk);
  /**@name diMuonOK 

  This method checks if dimuon candidates pass the cuts. It makes sanity check and dimuon cuts.
  Input parameters are : $dodimu$ and $dimasscut$.
  \begin{enumerate}
  \item Dimu's Index sanity check: 0$\leq$ dimu index $\leq$ number of entries
  \item Dimu's invariant mass cut: $dimasscut$ < dimu mass
  \end{enumerate}
  If dimuon candidate pass these cuts, then diMuonOK returns {\bf true} and the
  dimuon candidate is stored in the output (if event pass the GlobalOK selections).
  */
  PHBoolean diMuonOK  (PHMuoTracksOut* php, const unsigned int idimu);
  void Reset();

  //------------------------
  // Below this point, we implement variables and functions
  // which are specific to the application of MWG cuts.
  // Since this is an example routine, I have tried to
  // confine the variable choices to those variables
  // which any analysis might consider.  However, you don't
  // *have* to use these...
  //------------------------

  // Sets...
  void set_ghostsel    (const bool val){ ghostsel = val;}
  void set_ptlowcut    (const float val){ ptlowcut = val;} 
  void set_pthighcut   (const float val){ pthighcut = val;} 
  void set_vertexcut   (const float val){ vertexcut = val;} 
  void set_minhitcut   (const int   val){ minhitcut = val;} 
  void set_dodimu      (const bool  val){dodimu=val;}
  void set_dimasscut     (const float val){dimasscut=val;}

  // Gets...
  bool get_ghostsel     () const { return ghostsel;}
  float get_ptlowcut    () const { return ptlowcut;} 
  float get_pthighcut   () const { return pthighcut;} 
  float get_vertexcut   () const { return vertexcut;} 
  int   get_minhitcut   () const { return minhitcut;} 
  bool get_dodimu       () const { return dodimu;}
  float get_dimasscut     () const { return dimasscut;}

protected:
  bool ghostsel;
  float ptlowcut;
  float pthighcut;
  float vertexcut;
  int   minhitcut;
  int   mintrackcut;
  int   maxtrackcut;
  bool  dodimu;
  float dimasscut;

  ClassDef(MWGInclusiveNanoCutsv1,1)

};
#endif
