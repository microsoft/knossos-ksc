/* Copyright Microsoft Corp. 2020 */
#ifndef _MLIR_H_
#define _MLIR_H_

#include <map>

#include "mlir/IR/Builders.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/Pass/PassManager.h"
#include "mlir/Target/LLVMIR/Import.h"
#include "mlir/Transforms/Passes.h"
#include "mlir/InitAllDialects.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Module.h"

#include "AST.h"

namespace Knossos {
namespace MLIR {

// MLIR generator
using Types = llvm::SmallVector<mlir::Type, 4>;
using Values = llvm::SmallVector<mlir::Value, 4>;
class Generator {
  // The main module
  mlir::OwningModuleRef module;
  // The global context
  mlir::MLIRContext & context;
  // The current builder
  mlir::OpBuilder builder;
  // A default location, ignoring source loc for now
  mlir::Location UNK;
  // Current function (for basic block placement)
  mlir::FuncOp currentFunc;

  // Cache for functions and variables
  std::map<std::string, mlir::FuncOp> functions;
  std::map<std::string, Values> variables;

  // Helpers
  Types ConvertType(const AST::Type &type, size_t dim=0);
  mlir::Value memrefCastForCall(mlir::Value orig);
  mlir::Attribute getAttr(const AST::Expr* op);

  // Module level builders
  void buildGlobal(const AST::Block* block);
  mlir::FuncOp buildDecl(const AST::Declaration* decl);
  mlir::FuncOp buildDef(const AST::Definition* def);

  // Function level builders
  Values buildNode(const AST::Expr*);
  Values buildBlock(const AST::Block*);
  Values buildCall(const AST::Call*);
  mlir::Value buildArg(const AST::Call*, size_t);
  Values buildCond(const AST::Condition*);
  Values buildLet(const AST::Let*);
  Values buildLiteral(const AST::Literal*);
  Values buildVariable(const AST::Variable*);
  Values buildBuild(const AST::Build*);
  Values buildTuple(const AST::Tuple*);
  Values buildGet(const AST::Get*);
  Values buildFold(const AST::Fold*);

  // Function level builders for parallel lowering
  Values buildBuildParallel(const AST::Build*);


  // Variables
  void declareVariable(std::string const& name, Values vals);

  // Argument serialisation (tuples)
  void serialiseArgs(const AST::Definition *def, mlir::Block &entry);

public:
  explicit Generator(mlir::MLIRContext &context);

  // Build from MLIR source
  const mlir::ModuleOp build(const std::string& mlir);
  // Build from KSC AST
  const mlir::ModuleOp build(const AST::Block* extraDecls, const AST::Expr* root);
  // Emit LLVM IR
  std::unique_ptr<llvm::Module> emitLLVM(int optLevel, llvm::LLVMContext & llvmContext);
};

} // namespace MLIR
} // namespace Knossos

#endif // _MLIR_H_
