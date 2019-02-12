
Tests for KSC.

There are two sorts of tests here.

- Files such as *.ks, which are tested by running GHCI, loading src/ksc/Main.hs, and running doall "test/ksc/file.ks"

- Compile test-ksc.cpp, which by default includes GMM.cpp, to run standalone tests.
