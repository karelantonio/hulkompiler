// Hulk Standard Library Implementation
#include <cmath>
#include <iostream>
#include <memory>
#include <random>
#include <sstream>
#include <string>

template <typename T> using shp = std::shared_ptr<T>;

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

  virtual shp<HkString> cat(shp<HkObject> right);
};

// A string type
class HkString : public HkObject {
public:
  HkString(std::string val) : value(std::make_shared<std::string>(val)) {}

  HkString(shp<HkObject> obj) {
    std::ostringstream out;
    obj->_repr(out);
    value = std::make_shared<std::string>(out.str());
  }

  virtual void _repr(std::ostream &out) override { out << *value; }

  std::shared_ptr<std::string> value;

  shp<HkString> cat(shp<HkObject> right) override;
};

shp<HkString> HkObject::cat(shp<HkObject> right) {
  std::string l = to_string(), r = right->to_string();
  return std::make_shared<HkString>(HkString(l + r));
}

shp<HkString> HkString::cat(shp<HkObject> right) {
  std::string r = right->to_string();
  return std::make_shared<HkString>(HkString(*value + r));
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

  shp<HkBoolean> operator!() {
    return std::make_shared<HkBoolean>(HkBoolean(!value));
  }

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

  shp<HkNumber> pow(shp<HkNumber> ex) {
    return std::make_shared<HkNumber>(HkNumber(std::pow(value, ex->value)));
  }

  long double value;
};

// Operators
shp<HkNumber> opadd(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkNumber>(HkNumber(l->value + r->value));
}
shp<HkNumber> opsub(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkNumber>(HkNumber(l->value - r->value));
}
shp<HkNumber> opmult(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkNumber>(HkNumber(l->value * r->value));
}
shp<HkNumber> opdiv(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkNumber>(HkNumber(l->value / r->value));
}
shp<HkBoolean> opeq(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkBoolean>(HkBoolean(l->value == r->value));
}
shp<HkBoolean> opneq(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkBoolean>(HkBoolean(l->value != r->value));
}
shp<HkBoolean> lte(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkBoolean>(HkBoolean(l->value <= r->value));
}
shp<HkBoolean> oplt(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkBoolean>(HkBoolean(l->value < r->value));
}
shp<HkBoolean> opgte(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkBoolean>(HkBoolean(l->value >= r->value));
}
shp<HkBoolean> opgt(shp<HkNumber> l, shp<HkNumber> r) {
  return std::make_shared<HkBoolean>(HkBoolean(l->value > r->value));
}
// Print an object or its representation
shp<HkObject> hk_print(shp<HkObject> obj) {
  obj->_repr(std::cout);
  std::cout << std::endl;
  return std::make_shared<HkNone>(HkNone());
}

// Get the floor division
shp<HkNumber> hk_floor(shp<HkNumber> num) {
  return std::make_shared<HkNumber>(HkNumber(std::floor(num->value)));
}

// Get the sine
shp<HkNumber> hk_sin(shp<HkNumber> num) {
  return std::make_shared<HkNumber>(HkNumber(std::sin(num->value)));
}

// Get the cosine
shp<HkNumber> hk_cos(shp<HkNumber> num) {
  return std::make_shared<HkNumber>(HkNumber(std::cos(num->value)));
}

// The exponential function
shp<HkNumber> hk_exp(shp<HkNumber> num) {
  return std::make_shared<HkNumber>(HkNumber(std::exp(num->value)));
}

// The square root function
shp<HkNumber> hk_sqrt(shp<HkNumber> num) {
  return std::make_shared<HkNumber>(HkNumber(std::sqrt(num->value)));
}

// The logarithm function
shp<HkNumber> hk_log(shp<HkNumber> base, shp<HkNumber> arg) {
  return std::make_shared<HkNumber>(
      HkNumber(std::log(arg->value) / std::log(arg->value)));
}

std::random_device rd;
std::mt19937 rng(rd());
std::uniform_real_distribution<> dist(0.0, 1.0);

// The rand function
shp<HkNumber> hk_rand() {
  return std::make_shared<HkNumber>(HkNumber(dist(rng)));
}

shp<HkNumber> hkv_PI = std::make_shared<HkNumber>(HkNumber(std::acos(-1)));
