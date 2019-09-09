#include <emscripten.h>
#include <emscripten/bind.h>

#include <string>

using namespace emscripten;

EMSCRIPTEN_KEEPALIVE
int energy(std::string inStr) {
    return inStr.length();
}

//https://emscripten.org/docs/porting/connecting_cpp_and_javascript/embind.html#embind
EMSCRIPTEN_BINDINGS(my_module) {
    function("energyWrapped", &energy);
}