// Hulk Standard Library Implementation
#include <cmath>
#include <iostream>
#include <memory>
#include <random>
#include <string>

// The base of the hierarchy
class HkObject {
public:
  HkObject() {}

  virtual void _repr(std::ostream &out) { out << "Object"; }
};

// A string type
class HkString : public HkObject {
public:
  HkString(std::string val) : value(std::make_shared<std::string>(val)) {}

  virtual void _repr(std::ostream &out) override { out << value; }

  std::shared_ptr<std::string> value;
};

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

  long double value;
};

// Print an object or its representation
HkObject hk_print(HkObject &obj) {
  obj._repr(std::cout);
  std::cout<<std::endl;
  return HkNone();
}

// Get the floor division
HkNumber hk_floor(HkNumber num) { return HkNumber(std::floor(num.value)); }

// Get the sine
HkNumber hk_sin(HkNumber num) { return HkNumber(std::sin(num.value)); }

// Get the cosine
HkNumber hk_cos(HkNumber num) { return HkNumber(std::cos(num.value)); }

// The exponential function
HkNumber hk_exp(HkNumber num) { return HkNumber(std::exp(num.value)); }

// The square root function
HkNumber hk_sqrt(HkNumber num) { return HkNumber(std::sqrt(num.value)); }

// The logarithm function
HkNumber hk_log(HkNumber base, HkNumber arg) {
  return HkNumber(std::log(arg.value) / std::log(arg.value));
}

std::random_device rd;
std::mt19937 rng(rd());
std::uniform_real_distribution<> dist(0.0, 1.0);

// The rand function
HkNumber hk_rand() { return HkNumber(dist(rng)); }

HkNumber hk_PI(std::acos(-1));
