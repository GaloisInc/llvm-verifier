/**
 * Contain test code for throwing and catching exceptions.
 */
#include <exception>
#include <string>

/** Class for exceptions. */
class my_exception_t : public std::exception {
private:
  std::string msg_;
public:
  my_exception_t(const char* msg) : msg_(msg) {}

  virtual ~my_exception_t() throw() {}


  virtual const char* what() const throw() { return msg_.c_str(); }
};

void throw_except() {
  throw my_exception_t("Throw Exception");
}

void catch_except() {
  try {
    throw_except();
  } catch (std::exception& e) {
  }
}

int try_and_throw_except() {
  try {
    throw my_exception_t("Throw Exception");
    return 0;
  } catch (std::exception& e) {
    return -1;
  }
}
