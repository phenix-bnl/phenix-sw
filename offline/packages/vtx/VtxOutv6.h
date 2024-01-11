#ifndef __VTXOUTV6_H__
#define __VTXOUTV6_H__

#include <string>
#include <iostream>
#include <map>
#include <VtxOut.h>

class TClonesArray;

class VtxOutv6 : public VtxOut
{
 public:
  VtxOutv6();
  VtxOutv6(const VtxOutv6&);

  virtual ~VtxOutv6();

  VtxOutv6* clone() const { return new VtxOutv6(*this); }
  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;
  int DeepCopy(const VtxOut *vtxout);

  //! add vertex with a given name and with a given order
  virtual void AddVtx(const char *name, const float *vertex, const float *vertexerr, const short int order);
  virtual void AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order);

  PHPoint get_Vertex() const;
  PHPoint get_VertexError() const;
  PHPoint get_Vertex(const char *name) const;
  PHPoint get_VertexError(const char *name) const;
  float get_ZVertex() const;
  float get_ZVertex(const char *name) const;
  float get_ZVertexError() const;
  float get_ZVertexError(const char *name) const;
  const char *which_Vtx() const;
  bool isVtx(const char *name) const;

 protected:
  void copyTo(VtxOutv6& dest) const;
  void identifyv6(std::ostream &os = std::cout) const;

  TClonesArray *VtxSngl;
  std::map<int, int> VtxOrder;


  ClassDef(VtxOutv6,1)
};

#endif /*__VTXOUTV6_H__*/
