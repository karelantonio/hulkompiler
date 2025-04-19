// Hulk Standard Library Implementation
#include <cmath>
#include <iostream>
#include <memory>
#include <random>
#include <sstream>
#include <string>

// Forward declarations
class HkObject;
class HkString;
class HkBoolean;
class HkNone;
class HkNumber;

// The base of the hierarchy
class HkObject {
public:
  HkObject() {}

  virtual void _repr(std::ostream &out) { out << "Object"; }

  std::string to_string() {
    std::ostringstream out;
    _repr(out);
    return out.str();
  }

  virtual HkString *cat(HkObject *right);
};

// A string type
class HkString : public HkObject {
public:
  HkString(std::string val) : value(std::make_shared<std::string>(val)) {}

  HkString(HkObject *obj) {
    std::ostringstream out;
    obj->_repr(out);
    value = std::make_shared<std::string>(out.str());
  }

  virtual void _repr(std::ostream &out) override { out << *value; }

  std::shared_ptr<std::string> value;

  HkString *cat(HkObject *right) override;
};

HkString *HkObject::cat(HkObject *right) {
  std::string l = to_string(), r = right->to_string();
  return new HkString(l + r);
}

HkString *HkString::cat(HkObject *right) {
  std::string r = right->to_string();
  return new HkString(*value + r);
}

// The boolean type
class HkBoolean : public HkObject {
public:
  HkBoolean(bool v) : value(v) {}

  virtual void _repr(std::ostream &out) override {
    if (value)
      out << "true";
    else
      out << "false";
  }

  HkBoolean *operator!() { return new HkBoolean(!value); }

  bool value;
};

// The None type (returned by print)
class HkNone : public HkObject {
public:
  HkNone() {}

  virtual void _repr(std::ostream &out) override { out << "None"; }
};

// A Number
class HkNumber : public HkObject {
public:
  HkNumber(long double ld) : value(ld) {}

  virtual void _repr(std::ostream &out) override { out << value; }

  HkNumber *pow(HkNumber *ex) {
    return new HkNumber(std::pow(value, ex->value));
  }

  long double value;
};

// Operators
HkNumber *opadd(HkNumber *l, HkNumber *r) {
  return new HkNumber(l->value + r->value);
}
HkNumber *opsub(HkNumber *l, HkNumber *r) {
  return new HkNumber(l->value - r->value);
}
HkNumber *opmult(HkNumber *l, HkNumber *r) {
  return new HkNumber(l->value * r->value);
}
HkNumber *opdiv(HkNumber *l, HkNumber *r) {
  return new HkNumber(l->value / r->value);
}
HkBoolean *opeq(HkNumber *l, HkNumber *r) {
  return new HkBoolean(l->value == r->value);
}
HkBoolean *opneq(HkNumber *l, HkNumber *r) {
  return new HkBoolean(l->value != r->value);
}
HkBoolean *lte(HkNumber *l, HkNumber *r) {
  return new HkBoolean(l->value <= r->value);
}
HkBoolean *oplt(HkNumber *l, HkNumber *r) {
  return new HkBoolean(l->value < r->value);
}
HkBoolean *opgte(HkNumber *l, HkNumber *r) {
  return new HkBoolean(l->value >= r->value);
}
HkBoolean *opgt(HkNumber *l, HkNumber *r) {
  return new HkBoolean(l->value > r->value);
}
// Print an object or its representation
HkObject *hk_print(HkObject *obj) {
  obj->_repr(std::cout);
  std::cout << std::endl;
  return new HkNone();
}

// Get the floor division
HkNumber *hk_floor(HkNumber *num) {
  return new HkNumber(std::floor(num->value));
}

// Get the sine
HkNumber *hk_sin(HkNumber *num) { return new HkNumber(std::sin(num->value)); }

// Get the cosine
HkNumber *hk_cos(HkNumber *num) { return new HkNumber(std::cos(num->value)); }

// The exponential function
HkNumber *hk_exp(HkNumber *num) { return new HkNumber(std::exp(num->value)); }

// The square root function
HkNumber *hk_sqrt(HkNumber *num) { return new HkNumber(std::sqrt(num->value)); }

// The logarithm function
HkNumber *hk_log(HkNumber *base, HkNumber *arg) {
  return new HkNumber(std::log(arg->value) / std::log(arg->value));
}

std::random_device rd;
std::mt19937 rng(rd());
std::uniform_real_distribution<> dist(0.0, 1.0);

// The rand function
HkNumber *hk_rand() { return new HkNumber(dist(rng)); }

HkNumber *hk_PI = new HkNumber(std::acos(-1));
