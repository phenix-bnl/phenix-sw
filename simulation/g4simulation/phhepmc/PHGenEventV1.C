
#include <PHGenEventV1.h>

#include <HepMC/GenEvent.h>

#include <sstream>

ClassImp(PHGenEventV1)

using namespace std;

PHGenEventV1::PHGenEventV1()
  : _id(0),
    _event_record(),
    _stale(true),
    _event(NULL)
{}

PHGenEventV1::PHGenEventV1(unsigned int id, HepMC::GenEvent& event)
   : _id(id),
     _event_record(),
     _stale(true),
     _event(NULL) {  
   set_event(event);
 }

PHGenEventV1::PHGenEventV1(const PHGenEventV1& phevent)
   : _id(phevent.get_id()),
     _event_record(phevent.get_event_record()),
     _stale(true),
     _event(NULL)
{}

PHGenEventV1::PHGenEventV1(const PHGenEventV1* phevent)
   : _id(phevent->get_id()),
     _event_record(phevent->get_event_record()),
     _stale(true),
     _event(NULL)
{}

PHGenEventV1::~PHGenEventV1() {
  if (_event) delete _event;  
}

const HepMC::GenEvent* PHGenEventV1::get_event() const {
  if (stale()) refresh();
  return (const HepMC::GenEvent*)_event;
}

HepMC::GenEvent* PHGenEventV1::get_event() {
  if (stale()) refresh();
  return _event;
}

void PHGenEventV1::set_event(HepMC::GenEvent& event) {

  _event_record.Clear();
  if (_event) {
     delete _event;
     _event = NULL;
  }

  std::stringstream streamer;
  event.write(streamer);
  _event_record = streamer.str();
  
  refresh();
}

void PHGenEventV1::set_event(HepMC::GenEvent* event) {

  _event_record.Clear();
  if (_event) {
    delete _event;
    _event = NULL;
  }

  std::stringstream streamer;
  event->write(streamer);
  _event_record = streamer.str();
    
  refresh();
}

size_t PHGenEventV1::particles_size() const {
  if (stale()) refresh();
  return _event->particles_size();
}

size_t PHGenEventV1::vertices_size() const {
  if (stale()) refresh();
  return _event->vertices_size();
}

void PHGenEventV1::Reset() {
  _id = 0;
  _event_record.Clear();
  _stale = true;
  if (_event) {
    delete _event;
    _event = NULL;
  }
}

void PHGenEventV1::print(std::ostream& out) const {
  if (stale()) refresh();
  identify(out);
  out << " id = " << _id << endl;
  _event->print(out);
}

void PHGenEventV1::refresh() const {

  if (_event) {
    delete _event;
    _event = NULL;
  }

  _event = new HepMC::GenEvent();
  
  std::stringstream streamer;
  streamer << _event_record;
  _event->read(streamer);
  _stale = false;
}
