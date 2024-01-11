
#include <PHGenEventListV1.h>

#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

ClassImp(PHGenEventListV1)

PHGenEventListV1::PHGenEventListV1()
: PHGenEventList(),
  _genevents(),
  _stale(true),
  _id_genevent_map() {
}

PHGenEventListV1::~PHGenEventListV1() {
  _genevents.clear();
  _id_genevent_map.clear();
}

void PHGenEventListV1::identify(std::ostream& out) const {
  if (_stale) refresh();
  out << "PHGenEventListV1" << endl;
}

void PHGenEventListV1::print(std::ostream& out) const {
  if (_stale) refresh();

  identify(out);
  out << " size = " << size() << endl;
  for (size_t i=0; i<size(); ++i) {
    _genevents[i].print(out);
  }
}

void PHGenEventListV1::Reset() {
  clear();
  _stale = true;
}

const PHGenEvent* PHGenEventListV1::at(size_t i) const {
  if (_stale) refresh();
  const PHGenEventVersion* versioned_const = &_genevents[i];
  const PHGenEvent* genevent
    = static_cast<const PHGenEvent*>(versioned_const);
  return genevent;
}

PHGenEvent* PHGenEventListV1::at(size_t i) {
  if (_stale) refresh();
  PHGenEventVersion* versioned = &_genevents[i];
  PHGenEvent* genevent = static_cast<PHGenEvent*>(versioned);
  return genevent;
}

bool PHGenEventListV1::has(unsigned int id) const {
  if (_stale) refresh();
  if (_id_genevent_map.find(id) == _id_genevent_map.end()) return false;
  return true;
}

size_t PHGenEventListV1::find(unsigned int id) const {
  if (_stale) refresh();
  for (size_t i = 0; i < _genevents.size(); ++i) {
    if (_genevents[i].get_id() == id) return i;
  }
  return _genevents.size();
}

const PHGenEvent* PHGenEventListV1::fetch(unsigned int id) const {
  if (_stale) refresh();  
  if (_id_genevent_map.find(id) != _id_genevent_map.end()) {
    return (const PHGenEvent*)_id_genevent_map[id];
  }
  return NULL;
}

PHGenEvent* PHGenEventListV1::fetch(unsigned int id) {
  if (_stale) refresh();  
  if (_id_genevent_map.find(id) != _id_genevent_map.end()) {
    return (PHGenEvent*)_id_genevent_map[id];
  }
  return NULL;
}

void PHGenEventListV1::insert(const PHGenEvent* genevent) {
  if (_stale) refresh();

  if (has(genevent->get_id())) {
    size_t pos = find(genevent->get_id());
    remove(pos);
  }

  const PHGenEventVersion* versioned_const
    = dynamic_cast<const PHGenEventVersion*>(genevent);
  if (versioned_const) {
    _genevents.push_back(PHGenEventVersion(*versioned_const));
    _id_genevent_map.insert(make_pair(genevent->get_id(),
				      &_genevents[_genevents.size()-1]));
  }
  return;
}

unsigned int PHGenEventListV1::generate_id() const {
  if (_stale) refresh();
  
  unsigned int key = 0;
  if (_id_genevent_map.empty()) return key;
  
  key = _id_genevent_map.rbegin()->first + 1;  
  return key;
}
  
void PHGenEventListV1::remove(size_t i) {
  _genevents.erase(_genevents.begin()+i);
  refresh();
}

void PHGenEventListV1::clear() {
  _genevents.clear();
  _id_genevent_map.clear();
}

void PHGenEventListV1::refresh() const {

  _id_genevent_map.clear();

  for (size_t i = 0; i < _genevents.size(); ++i) {
    _id_genevent_map.insert(make_pair(_genevents[i].get_id(),&_genevents[i]));
  }

  _stale = false;
}
