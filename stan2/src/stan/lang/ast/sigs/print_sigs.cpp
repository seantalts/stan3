#include <stan/lang/ast/sigs/function_signatures.hpp>

#include <iostream>

int main() {
  stan::lang::function_signatures::instance().print_signatures(std::cout);
  return 0;
}
