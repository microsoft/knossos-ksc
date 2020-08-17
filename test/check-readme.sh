# Run test/check-readme.sh to ensure the code in README.md compiles
sed '/```scheme/,/```/!d;//d' README.md  > obj/README.ks
./build/bin/ksc --compile-and-run\
    --ks-source-file src/runtime/prelude.ks\
    --ks-source-file obj/README.ks\
    --ks-output-file obj/README.kso\
    --cpp-output-file obj/README.cpp\
    --c++ g++\
    --exe-output-file obj/README.exe     
