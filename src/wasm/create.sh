# source ~/emsdk/emsdk_env.sh
emcc fibonacci.c -o fibonacci.js -s EXTRA_EXPORTED_RUNTIME_METHODS="['ccall', 'cwrap']" -s EXPORT_ES6=1 -s MODULARIZE=1 -s ENVIRONMENT=web
emcc --bind dna.cpp -o dna.js -s EXTRA_EXPORTED_RUNTIME_METHODS="['ccall', 'cwrap']" -s EXPORT_ES6=1 -s MODULARIZE=1 -s ENVIRONMENT=web
cp ./fibonacci.wasm ../../public
cp ./dna.wasm ../../public