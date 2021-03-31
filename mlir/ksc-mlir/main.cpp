#include <iostream>
#include <fstream>

#include "Parser/Parser.h"
#include "Parser/MLIR.h"
#include "mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h"

using namespace Knossos::AST;
using namespace Knossos::MLIR;
using namespace std;

// Fwd declaration of tests (use gtest)
int test_all(int v=0);

// Simple source selection based table
enum class Source {
  NONE,
  KSC,
  MLIR
};

// Simple action based table
enum class Action {
  NONE,
  TEST,
  EMIT_AST,
  EMIT_MLIR,
  EMIT_LLVM
};

void help() {
  cout << "Unit Test Syntax: ksc-mlir TEST [-v(v(v))]\n";
  cout << " Compiler Syntax: ksc-mlir AST|MLIR|LLVM [-v(v(v))] <filename.ks>\n";
}

int main(int argc, char **argv) {
  // Command line options
  if (argc < 2) {
    help();
    return 1;
  }

  // Action
  Action action = Action::NONE;

  int optlevel = 0;
  
  size_t nextarg = 1;
  string aStr(argv[nextarg++]);
  if (aStr == "TEST")
    action = Action::TEST;
  else if (aStr == "AST")
    action = Action::EMIT_AST;
  else if (aStr == "MLIR")
    action = Action::EMIT_MLIR;
  else if (aStr == "LLVM")
    action = Action::EMIT_LLVM;
  if (action == Action::NONE) {
    cout << "Invalid action!\n";
    help();
    return 1;
  }

  int verbosity = 0;
  if (nextarg < argc && argv[nextarg][0] == '-') {
    string arg(argv[nextarg++]);
    if (arg == "-v")
      verbosity = 1;
    else if (arg == "-vv")
      verbosity = 2;
    else if (arg == "-vvv")
      verbosity = 3;
    else {
      std::cerr << "ksc-mlir: bad switch [" << arg << "]" << std::endl;
      return 2;
    }
  }

  // Unit tests
  // FIXME: Use gtest or similar
  if (action == Action::TEST)
    return test_all(verbosity);

  // Filename
  if (!(nextarg < argc)) {
    help();
    return 1;
  }
  auto source = Source::NONE;
  llvm::StringRef filename(argv[nextarg++]);
  if (filename.endswith(".ks") || filename.endswith(".kso"))
    source = Source::KSC;
  else if (filename.endswith(".mlir"))
    source = Source::MLIR;
  else {
    cerr << "ERROR: Unknown source file " << filename.str()
         << ". Must be [ks, kso, mlir]\n";
    return 1;
  }
  ifstream file(filename.str());
  if (!file.is_open()) {
    cerr << "Invalid filename [" << filename.str() << "]!\n";
    help();
    return 1;
  }

  // Read whole file into a string
  // TODO: Don't
  string code((istreambuf_iterator<char>(file)),istreambuf_iterator<char>());

  // Parse and output AST if requested
  Parser p(Location {filename.str(), 1, 0}, code, verbosity);
  if (source == Source::KSC) {
    p.parse();
    if (!p.getRootNode()) {
      cerr << "ERROR: AST lowering failed\n";
      return 1;
    }
    if (action == Action::EMIT_AST) {
      p.getExtraDecls()->dump(std::cout);
      p.getRootNode()->dump(std::cout);
      return 0;
    }
  }

  // FIXME: registering dialects must happen before building the context
  // Create a more logical API that doesn't require it to be done by the caller
  mlir::MLIRContext context;
  mlir::registerAllDialects(context);
  mlir::registerLLVMDialectTranslation(context);
  context.loadDialect<mlir::StandardOpsDialect, mlir::math::MathDialect>();

  // Call generator and print output (MLIR/LLVM)
  Generator g(context);
  mlir::ModuleOp module;
  if (source == Source::KSC)
    module = g.build(p.getExtraDecls(), p.getRootNode());
  else if (source == Source::MLIR)
    module = g.build(code);

  if (!module) {
    cerr << "ERROR: MLIR lowering failed\n";
    return 1;
  }
  if (action == Action::EMIT_MLIR) {
    module.print(llvm::outs());
  } 
  else if (action == Action::EMIT_LLVM) {
    llvm::LLVMContext llvmContext;
    auto llvm = g.emitLLVM(optlevel, llvmContext);
    if (!llvm) {
      cerr << "ERROR: LLVM lowering failed\n";
      return 1;
    }
    llvm->print(llvm::outs(), nullptr, false, true);
  }

  return 0;
}
