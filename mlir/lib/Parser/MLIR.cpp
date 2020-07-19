#include <cstdlib>
#include <numeric>
#include <iostream>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SourceMgr.h"

#include "mlir/IR/Verifier.h"
#include "mlir/Dialect/StandardOps/IR/Ops.h"
#include "mlir/IR/Attributes.h"
#include "mlir/IR/Dialect.h"
#include "mlir/Transforms/Passes.h"
#include "mlir/Parser.h"
#include "mlir/Conversion/StandardToLLVM/ConvertStandardToLLVMPass.h"
#include "mlir/IR/StandardTypes.h"
#include "mlir/Pass/Pass.h"

#include "Parser/MLIR.h"

using namespace Knossos::MLIR;
using namespace std;

//============================================================ Helpers

// Convert from AST type to MLIR
Types Generator::ConvertType(const AST::Type &type, size_t dim) {
  switch (type) {
  case AST::Type::None:
    return {};
  case AST::Type::Bool:
    return {builder.getI1Type()};
  case AST::Type::Integer:
    return {builder.getIntegerType(64)};
  case AST::Type::Float:
    return {builder.getF64Type()};
  case AST::Type::Vector:
    // FIXME: support nested vectors
    assert(type.getSubType() != AST::Type::Vector);
    if (dim)
      return {mlir::MemRefType::get(dim, ConvertType(type.getSubType())[0])};
    else
      return {mlir::MemRefType::get(-1, ConvertType(type.getSubType())[0])};
  case AST::Type::Tuple: {
    // FIXME: support nested tuples
    Types subTys;
    for (auto &ts: type.getSubTypes()) {
      auto tys = ConvertType(ts);
      subTys.append(tys.begin(), tys.end());
    }
    return subTys;
  }
  default:
    assert(0 && "Unsupported type");
  }
}

// Cast to memref<?xTy> from any static type, so that we can call functions
// that have vectors as arguments (as they're all unknwon size)
mlir::Value Generator::memrefCastForCall(mlir::Value orig) {
  assert(orig.getType().isa<mlir::MemRefType>());
  auto type = orig.getType().cast<mlir::MemRefType>();
  auto subTy = type.getElementType();
  auto newTy = mlir::MemRefType::get(-1, subTy);
  return builder.create<mlir::MemRefCastOp>(UNK, orig, newTy);
}

// Tuple arguments are serialised, but still accessed by the tuple name (via
// get), so we need to add the same number of arguments as the function
// declaration (flattened tuple), with each argument as a value of the tuple.
//
// Example: fun(float a, tuple<int, float> b, bool c) -> (f0, i1, f2, b3)
//    Then: a = f0, b = { i1, f2 }, c = b3
void Generator::serialiseArgs(const AST::Definition *def, mlir::Block &entry) {
  // Get all serialised arguments
  auto serialised = entry.getArguments();
  size_t idx = 0, last = serialised.size() - 1;

  // For each declared variable, initialise it with the right number of arguments
  for (auto &arg: def->getArguments()) {
    auto var = llvm::dyn_cast<AST::Variable>(arg.get());
    assert(var && idx <= last);
    // Non-tuple args are simple
    if (var->getType() != AST::Type::Tuple) {
      declareVariable(var->getName(), {serialised[idx++]});
      continue;
    }

    // Tuples need to know how many arguments, recursively, they have
    auto type = ConvertType(arg->getType());
    Values args;
    size_t end = idx + type.size();
    while (idx < end)
      args.push_back(serialised[idx++]);
    declareVariable(var->getName(), args);
    idx += type.size();
  }
}

// Inefficient but will do for now
static void dedup_declarations(vector<mlir::FuncOp> &decl, vector<mlir::FuncOp> def) {
  for (auto &d: def) {
    auto it = std::find(decl.begin(), decl.end(), d);
    if (it != decl.end())
      decl.erase(it);
  }
}

// Helpers for number conversion
static int64_t toInteger(llvm::StringRef str) {
  int64_t num = 0;
  bool failed = str.getAsInteger(0, num);
  assert(!failed && "Bad integer conversion");
  return num;
}
static double toDouble(llvm::StringRef str) {
  double num = 0.0;
  bool failed = str.getAsDouble(num);
  assert(!failed && "Bad double conversion");
  return num;
}

// We can return several values, but sometimes we just want one. These
// functions make sure we only have one and return it.
static mlir::Value Single(const Values &v) {
  assert(v.size() == 1);
  return v[0];
}
static mlir::Type Single(const Types &t) {
  assert(t.size() == 1);
  return t[0];
}

//============================================================ MLIR Generator

// Global context, allowing declarations and definitions.
void Generator::buildGlobal(const AST::Block* block) {
  // First we need to make sure we don't emit declarations when definitions
  // are available.
  // FIXME: Find a way to use the functions map instead
  vector<mlir::FuncOp> declarations;
  vector<mlir::FuncOp> definitions;
  for (auto &op: block->getOperands()) {
    switch (op->kind) {
    case AST::Expr::Kind::Declaration:
      declarations.push_back(buildDecl(llvm::dyn_cast<AST::Declaration>(op.get())));
      continue;
    case AST::Expr::Kind::Definition:
      definitions.push_back(buildDef(llvm::dyn_cast<AST::Definition>(op.get())));
      continue;
    case AST::Expr::Kind::Rule:
      // Ignore rules for now
      continue;
    default:
      assert(0 && "unexpected node");
    }
  }

  // Types were already checked by the AST, so if there's an
  // outline declaration already, we only insert the definition.
  dedup_declarations(declarations, definitions);

  // Now we add declarations first, then definitions
  for (auto decl: declarations)
    module->push_back(decl);
  for (auto def: definitions)
    module->push_back(def);
}

// Declaration only, no need for basic blocks
mlir::FuncOp Generator::buildDecl(const AST::Declaration* decl) {
  assert(!functions.count(decl->getName()) && "Duplicated function declaration");
  Types argTypes;
  for (auto &t: decl->getArgTypes()) {
    auto tys = ConvertType(t);
    argTypes.append(tys.begin(), tys.end());
  }
  auto retTy = ConvertType(decl->getType());
  auto type = builder.getFunctionType(argTypes, retTy);
  auto func = mlir::FuncOp::create(UNK, decl->getName(), type);
  functions.insert({decl->getName(), func});
  return func;
}

// Definition of the whole function starts here
mlir::FuncOp Generator::buildDef(const AST::Definition* def) {
  // Make sure we have its declaration cached
  if (!functions.count(def->getName()))
    buildDecl(def->getDeclaration());
  auto func = functions[def->getName()];
  assert(func);

  // First basic block, with args
  auto &entryBlock = *func.addEntryBlock();
  builder.setInsertionPointToStart(&entryBlock);
  serialiseArgs(def, entryBlock);

  // Lower body
  currentFunc = func;
  auto last = buildNode(def->getImpl());

  // Return the last value
  builder.create<mlir::ReturnOp>(UNK, last);
  return func;
}

// Declare a variable
void Generator::declareVariable(llvm::StringRef name,
                                     Values vals) {
  assert(!vals.empty() && "Variable must have initialiser");
  auto res = variables.insert({name, vals});
  // Already exists, replace
  if (!res.second)
    variables[name] = vals;
}

void Generator::declareVariable(const AST::Variable* var,
                                     Values vals) {
  if (vals.empty() && var->getInit())
    vals = buildNode(var->getInit());
  declareVariable(var->getName(), vals);
}

// Get the variable assigned value
Values Generator::buildVariable(const AST::Variable* var) {
  assert(variables.count(var->getName()) && "Variable not declared");
  return variables[var->getName()];
}

// Build node by type
Values Generator::buildNode(const AST::Expr* node) {
  if (AST::Block::classof(node))
    return buildBlock(llvm::dyn_cast<AST::Block>(node));
  if (AST::Literal::classof(node))
    return buildLiteral(llvm::dyn_cast<AST::Literal>(node));
  if (AST::Call::classof(node))
    return buildCall(llvm::dyn_cast<AST::Call>(node));
  if (AST::Let::classof(node))
    return buildLet(llvm::dyn_cast<AST::Let>(node));
  if (AST::Condition::classof(node))
    return buildCond(llvm::dyn_cast<AST::Condition>(node));
  if (AST::Variable::classof(node))
    return buildVariable(llvm::dyn_cast<AST::Variable>(node));
  if (AST::Build::classof(node))
    return buildBuild(llvm::dyn_cast<AST::Build>(node));
  if (AST::Tuple::classof(node))
    return buildTuple(llvm::dyn_cast<AST::Tuple>(node));
  if (AST::Get::classof(node))
    return buildGet(llvm::dyn_cast<AST::Get>(node));
  if (AST::Fold::classof(node))
    return buildFold(llvm::dyn_cast<AST::Fold>(node));
  if (AST::Print::classof(node))
    return buildPrint(llvm::dyn_cast<AST::Print>(node));
  assert(0 && "unexpected node");
}

// Builds blocks
Values Generator::buildBlock(const AST::Block* block) {
  if (block->size() == 0)
    return {};
  if (block->size() == 1)
    return buildNode(block->getOperand(0));
  for (auto &op: block->getOperands())
    buildNode(op.get());
  return {};
}

// Builds literals
Values Generator::buildLiteral(const AST::Literal* lit) {
  assert(lit->getType().isScalar() && "Only scalar literals supported");
  mlir::Type type = Single(ConvertType(lit->getType()));
  return {builder.create<mlir::ConstantOp>(UNK, type, getAttr(lit))};
}

// Build from one Argument (Operand) of a call
mlir::Value Generator::buildArg(const AST::Call* call, size_t i) 
{
  return Single(buildNode(call->getOperand(i)));
}

// Builds calls
Values Generator::buildCall(const AST::Call* call) {
  auto name = call->getName();
  size_t arity = call->size();

  // Various functions get special treatment, e.g. primitive arithmetic, array access etc
#define IF_MATCH_1(NAME, TYPE)\
    if (arity == 1 && name == NAME &&\
        call->getOperand(0)->getType() == AST::Type::TYPE)
#define CREATE_1(MLIR_OP)\
      return {builder.create<mlir::MLIR_OP>(UNK, buildArg(call,0))}

#define IF_MATCH_2(NAME, TYPE0, TYPE1)\
    if (arity == 2 && name == NAME &&\
        call->getOperand(0)->getType() == AST::Type::TYPE0 &&\
        call->getOperand(1)->getType() == AST::Type::TYPE1)
#define CREATE_2(MLIR_OP)\
      return {builder.create<mlir::MLIR_OP>(UNK, buildArg(call,0), buildArg(call,1))}

  IF_MATCH_1("abs", Float)  CREATE_1(AbsFOp);
  IF_MATCH_1("neg", Float)  CREATE_1(NegFOp);
  IF_MATCH_1("exp", Float)  CREATE_1(ExpOp);
  IF_MATCH_1("log", Float)  CREATE_1(LogOp);

  IF_MATCH_1("to_float", Integer) 
    return {builder.create<mlir::SIToFPOp>(UNK, buildArg(call,0), builder.getF64Type())};

  IF_MATCH_1("to_int", Float)  assert(0 && "Cast to_int not implemented yet");
  
  IF_MATCH_2("add", Integer, Integer)   CREATE_2(AddIOp);
  IF_MATCH_2("add", Float, Float)       CREATE_2(AddFOp);
  IF_MATCH_2("sub", Integer, Integer)   CREATE_2(SubIOp);
  IF_MATCH_2("sub", Float, Float)       CREATE_2(SubFOp);
  IF_MATCH_2("mul", Integer, Integer)   CREATE_2(MulIOp);
  IF_MATCH_2("mul", Float, Float)       CREATE_2(MulFOp);
  IF_MATCH_2("div", Integer, Integer)   CREATE_2(SignedDivIOp);
  IF_MATCH_2("div", Float, Float)       CREATE_2(DivFOp);

  IF_MATCH_2("and", Bool, Bool)   CREATE_2(AndOp);
  IF_MATCH_2("or", Bool, Bool)    CREATE_2(OrOp);

    // Comparison
#define CREATE_CMP(MLIR_OP, CMP)\
       return {builder.create<mlir::MLIR_OP>(UNK, mlir::CMP, buildArg(call,0), buildArg(call,1))}

  IF_MATCH_2("eq", Integer, Integer)   CREATE_CMP(CmpIOp, CmpIPredicate::eq);
  IF_MATCH_2("eq", Float, Float)       CREATE_CMP(CmpFOp, CmpFPredicate::OEQ);
  IF_MATCH_2("ne", Integer, Integer)   CREATE_CMP(CmpIOp, CmpIPredicate::ne);
  IF_MATCH_2("ne", Float, Float)       CREATE_CMP(CmpFOp, CmpFPredicate::ONE);
  IF_MATCH_2("lte", Integer, Integer)  CREATE_CMP(CmpIOp, CmpIPredicate::sle);
  IF_MATCH_2("lte", Float, Float)      CREATE_CMP(CmpFOp, CmpFPredicate::OLE);
  IF_MATCH_2("gte", Integer, Integer)  CREATE_CMP(CmpIOp, CmpIPredicate::sge);
  IF_MATCH_2("gte", Float, Float)      CREATE_CMP(CmpFOp, CmpFPredicate::OGE);
  IF_MATCH_2("gt", Integer, Integer)   CREATE_CMP(CmpIOp, CmpIPredicate::sgt);
  IF_MATCH_2("gt", Float, Float)       CREATE_CMP(CmpFOp, CmpFPredicate::OGT);
  IF_MATCH_2("lt", Integer, Integer)   CREATE_CMP(CmpIOp, CmpIPredicate::slt);
  IF_MATCH_2("lt", Float, Float)       CREATE_CMP(CmpFOp, CmpFPredicate::OLT);

  IF_MATCH_2("index", Integer, Vector) {
    auto idx = buildArg(call,0);
    auto vec = buildArg(call,1);
    auto indTy = builder.getIndexType();
    auto indIdx = builder.create<mlir::IndexCastOp>(UNK, idx, indTy);
    mlir::ValueRange rangeIdx {indIdx};
    return {builder.create<mlir::LoadOp>(UNK, vec, rangeIdx)};
  }

  IF_MATCH_1("size", Vector) {
    auto vec = buildArg(call,0);
    // FIXME: Support multi-dimensional vectors
    auto dim = builder.create<mlir::DimOp>(UNK, vec, 0);
    auto intTy = builder.getIntegerType(64);
    return {builder.create<mlir::IndexCastOp>(UNK, dim, intTy)};
  }

  // Function call -- not a prim, should be known
  if (functions.count(name) == 0) {
    asserter a("", __FILE__, __LINE__);
    a << "Unknown function [" << std::string(name) << "(";
    for(size_t i = 0; i < arity; ++i) {
      a << call->getOperand(i)->getType();
      if (i+1 < arity)
        a << ", ";
    } 
    a << "]";
  }

  auto func = functions[name];

  // Operands (tuples expand into individual operands)
  Values operands;
  for (auto &arg: call->getOperands()) {
    auto range = buildNode(arg.get());
    // Tuples
    if (range.size() > 1)
      operands.append(range.begin(), range.end());
    // Static vectors need to be made dynamic
    else if (Single(range).getType().isa<mlir::MemRefType>() &&
              Single(range).getType().cast<mlir::MemRefType>().getDimSize(0) != -1)
      operands.push_back(memrefCastForCall(Single(range)));
    // Everything else
    else
      operands.push_back(Single(range));
  }
  assert(func.getNumArguments() == operands.size() && "Arguments mismatch");

  // Function
  auto mlir_call = builder.create<mlir::CallOp>(UNK, func, operands);
  return mlir_call.getResults();
}

// Builds variable declarations
Values Generator::buildLet(const AST::Let* let) {
  // Bind the variable to an expression
  for (auto &v: let->getVariables())
    declareVariable(llvm::dyn_cast<AST::Variable>(v.get()));
  // Lower the body, using the variable
  if (let->getExpr())
    return buildNode(let->getExpr());
  // Otherwise, the let is just a declaration, return void
  return mlir::ValueRange();
}

// Builds conditions using select
Values Generator::buildCond(const AST::Condition* cond) {

  // Constant booleans aren't allowed on selects / cond_branch in LLVM
  auto lit = llvm::dyn_cast<AST::Literal>(cond->getCond());
  if (lit) {
    if (lit->getValue() == "true")
      return buildNode(cond->getIfBlock());
    else
      return buildNode(cond->getElseBlock());
  }

  // Check for the boolean result of the conditional block
  auto iB = buildNode(cond->getIfBlock());
  auto eB = buildNode(cond->getElseBlock());
  assert(iB.size() == eB.size() && "Uneven condition return");
  auto c = buildNode(cond->getCond());
  assert(c.size() == 1);

  // Return one conditional for each value returned
  Values rets;
  for (size_t i=0, e=iB.size(); i<e; i++) {
    assert(iB[i].getType() == eB[i].getType() && "Type mismatch");
    rets.push_back(builder.create<mlir::SelectOp>(UNK, c[0], iB[i], eB[i]));
  }
  return mlir::ValueRange{rets};
}

// Builds loops creating vectors
// FIXME: Use loop.for dialect
Values Generator::buildBuild(const AST::Build* b) {
  // Declare the bounded vector variable and allocate it
  auto dim = Single(buildNode(b->getRange()));
  auto indTy = builder.getIndexType();
  auto dimIdx = builder.create<mlir::IndexCastOp>(UNK, dim, indTy);
  auto elmTy = Single(ConvertType(b->getExpr()->getType()));
  auto ivTy = dim.getType();
  auto vecTy = mlir::MemRefType::get(-1, elmTy);
  mlir::ValueRange dimArg {dimIdx};
  auto vec = builder.create<mlir::AllocOp>(UNK, vecTy, dimArg);

  // Declare the range, initialised with zero
  auto zeroAttr = builder.getIntegerAttr(ivTy, 0);
  auto zero = builder.create<mlir::ConstantOp>(UNK, ivTy, zeroAttr);
  auto range = Single(buildNode(b->getRange()));

  // Create all basic blocks and the condition
  auto headBlock = currentFunc.addBlock();
  headBlock->addArgument(ivTy);
  auto bodyBlock = currentFunc.addBlock();
  bodyBlock->addArgument(ivTy);
  auto exitBlock = currentFunc.addBlock();
  mlir::ValueRange indArg {zero};
  builder.create<mlir::BranchOp>(UNK, headBlock, indArg);

  // HEAD BLOCK: Compare induction with range, exit if equal or greater
  builder.setInsertionPointToEnd(headBlock);
  auto headIv = headBlock->getArgument(0);
  auto cond = builder.create<mlir::CmpIOp>(UNK, mlir::CmpIPredicate::slt,
                                           headIv, range);
  mlir::ValueRange bodyArg{headIv};
  mlir::ValueRange exitArgs{};
  builder.create<mlir::CondBranchOp>(UNK, cond, bodyBlock, bodyArg, exitBlock,
                                     exitArgs);

  // BODY BLOCK: Lowers expression, store and increment
  builder.setInsertionPointToEnd(bodyBlock);
  auto bodyIv = bodyBlock->getArgument(0);
  // Declare the local induction variable before using in body
  auto var = llvm::dyn_cast<AST::Variable>(b->getVariable());
  declareVariable(var);
  variables[var->getName()] = {bodyIv};
  // Build body and store result (no vector of tuples supported)
  auto expr = Single(buildNode(b->getExpr()));
  auto indIv = builder.create<mlir::IndexCastOp>(UNK, bodyIv, indTy);
  mlir::ValueRange indices{indIv};
  builder.create<mlir::StoreOp>(UNK, expr, vec, indices);
  // Increment induction and loop
  auto oneAttr = builder.getIntegerAttr(ivTy, 1);
  auto one = builder.create<mlir::ConstantOp>(UNK, ivTy, oneAttr);
  auto incr = builder.create<mlir::AddIOp>(UNK, bodyIv, one);
  mlir::ValueRange headArg {incr};
  builder.create<mlir::BranchOp>(UNK, headBlock, headArg);

  // EXIT BLOCK: change insertion point before returning the final vector
  builder.setInsertionPointToEnd(exitBlock);
  return {memrefCastForCall(vec)};
}

// Builds tuple creation
Values Generator::buildTuple(const AST::Tuple* t) {
  auto type = ConvertType(t->getType());
  Values elms;
  // Nested tuples are serialised into one long list
  for (auto &e: t->getElements()) {
    elms.push_back(Single(buildNode(e.get())));
  }
  return mlir::ValueRange{elms};
}

// Builds index access to tuples
Values Generator::buildGet(const AST::Get* g) {
  // A get on a variable, returns the Nth element declared
  auto var = llvm::dyn_cast<AST::Variable>(g->getExpr());
  if (var) {
    auto tuple = variables[var->getName()];
    return {tuple[g->getIndex()-1]};
  }

  // Calls return multiple values, we need to lower the call first
  auto call = llvm::dyn_cast<AST::Call>(g->getExpr());
  if (call) {
    auto res = buildNode(call);
    return {res[g->getIndex()-1]};
  }

  // A get on a constant, just returns the element directly
  return buildNode(g->getElement());
}

// Builds fold
// FIXME: use loop.for, common up with build
Values Generator::buildFold(const AST::Fold* f) {
  // Fold needs a tuple of two variables: the accumulator and the induction
  auto v = f->getVector();
  auto acc = llvm::dyn_cast<AST::Variable>(f->getAcc());
  assert(acc && "Wrong AST node for vector and/or accumulator");
  assert(f->getType().isScalar() && "Bad accumulator type in fold");
  assert(acc->getType() == AST::Type::Tuple && "Bad accumulator type in fold");
  assert(f->getType() == acc->getType().getSubType(0));
  assert(v->getType() == AST::Type::Vector && "Bad vector type in fold");
  assert(v->getType().getSubType() == acc->getType().getSubType(1));

  // We can't build the variable here yet because this is an SSA representation
  // and the body will get the wrong reference, so we just initialise the
  // accumulator (the element x will be initialised by the load block)
  auto init = buildNode(acc->getInit());

  // Context variables: vector (and elm type), max, IV init to zero
  auto ivTy = builder.getIntegerType(64);
  auto accTy = init[0].getType();
  auto elmTy = init[1].getType();
  auto vec = Single(buildNode(v));
  auto dim = builder.create<mlir::DimOp>(UNK, vec, 0);
  auto max = builder.create<mlir::IndexCastOp>(UNK, dim, ivTy);
  auto zeroAttr = builder.getIntegerAttr(ivTy, 0);
  auto zero = builder.create<mlir::ConstantOp>(UNK, ivTy, zeroAttr);

  // Create all basic blocks and the condition
  auto headBlock = currentFunc.addBlock();
  headBlock->addArgument(accTy);
  headBlock->addArgument(ivTy);
  auto loadBlock = currentFunc.addBlock();
  loadBlock->addArgument(accTy);
  loadBlock->addArgument(ivTy);
  auto bodyBlock = currentFunc.addBlock();
  bodyBlock->addArgument(accTy);
  bodyBlock->addArgument(elmTy);
  bodyBlock->addArgument(ivTy);
  auto tailBlock = currentFunc.addBlock();
  tailBlock->addArgument(accTy);
  mlir::ValueRange indArg {init[0], zero};
  builder.create<mlir::BranchOp>(UNK, headBlock, indArg);

  // The head block only checks the condition, we can't load anything until
  // we know that it's safe to do so (for example, empty vectors)
  builder.setInsertionPointToEnd(headBlock);
  auto headAcc = headBlock->getArgument(0);
  auto headIv = headBlock->getArgument(1);
  auto cond = builder.create<mlir::CmpIOp>(UNK, mlir::CmpIPredicate::slt,
                                           headIv, max);
  mlir::ValueRange loadArgs{headAcc, headIv};
  mlir::ValueRange exitArgs{headAcc};
  builder.create<mlir::CondBranchOp>(UNK, cond, loadBlock, loadArgs, tailBlock,
                                     exitArgs);

  // The load block just fetches the element from the vector and passes as
  // a serialised tuple { acc, x } to the body. If got here, assume it's safe
  // to load the element "loadIv" from the vector
  builder.setInsertionPointToEnd(loadBlock);
  auto loadAcc = loadBlock->getArgument(0);
  auto loadIv = loadBlock->getArgument(1);
  auto indTy = builder.getIndexType();
  auto indIdx = builder.create<mlir::IndexCastOp>(UNK, loadIv, indTy);
  mlir::ValueRange rangeIdx {indIdx};
  auto loaded = builder.create<mlir::LoadOp>(UNK, vec, rangeIdx);
  mlir::ValueRange loadArg {loadAcc, loaded, loadIv};
  builder.create<mlir::BranchOp>(UNK, bodyBlock, loadArg);

  // Loop over the body of the lambda
  builder.setInsertionPointToEnd(bodyBlock);
  auto bodyAcc = bodyBlock->getArgument(0);
  auto bodyElm = bodyBlock->getArgument(1);
  auto bodyIv = bodyBlock->getArgument(2);
  declareVariable(acc->getName(), {bodyAcc, bodyElm});
  auto newAcc = Single(buildNode(f->getBody()));

  // Increment IV
  auto oneAttr = builder.getIntegerAttr(ivTy, 1);
  auto one = builder.create<mlir::ConstantOp>(UNK, ivTy, oneAttr);
  auto incr = builder.create<mlir::AddIOp>(UNK, bodyIv, one);
  mlir::ValueRange headArg {newAcc, incr};
  builder.create<mlir::BranchOp>(UNK, headBlock, headArg);

  // And return the accumulator
  builder.setInsertionPointToEnd(tailBlock);
  auto tailAcc = tailBlock->getArgument(0);
  return mlir::ValueRange{tailAcc};
}

// Lower constant literals
mlir::Attribute Generator::getAttr(const AST::Expr* op) {
  auto lit = llvm::dyn_cast<AST::Literal>(op);
  assert(lit && "Can only get attributes from lits");
  switch (lit->getType()) {
  case AST::Type::Bool:
    if (lit->getValue() == "true")
      return builder.getBoolAttr(true);
    else
      return builder.getBoolAttr(false);
  case AST::Type::Float:
    return builder.getFloatAttr(builder.getF64Type(),
                                toDouble(lit->getValue()));
  case AST::Type::Integer:
    return builder.getI64IntegerAttr(toInteger(lit->getValue()));
  case AST::Type::String:
    return builder.getStringAttr(lit->getValue());
  default:
    assert(0 && "Unimplemented literal type");
  }
}

// Builds print
Values Generator::buildPrint(const AST::Print* p) {
  if (p->size() > 0) {
    for (auto &op: p->getExprs()) {
      // For now, we ignore string operations
      if (op->getType() == AST::Type::String)
        continue;
      buildNode(op.get());
    }
  }
  // Return the nummber of elements
  size_t len = p->getExprs().size();
  auto att = builder.getIntegerAttr(builder.getIntegerType(64), len);
  auto elms = builder.create<mlir::ConstantOp>(UNK, builder.getIntegerType(64), att);
  return {elms};
}

//============================================================ MLIR from AST

const mlir::ModuleOp Generator::build(const AST::Expr* root) {
  module = mlir::ModuleOp::create(UNK);
  assert(root->kind == AST::Expr::Kind::Block);
  auto rB = llvm::dyn_cast<AST::Block>(root);
  buildGlobal(rB);
  if (mlir::failed(mlir::verify(*module))) {
    module->dump();
    return nullptr;
  }
  return module.get();
}

//============================================================ MLIR round-trip

const mlir::ModuleOp Generator::build(const std::string& mlir) {
  llvm::SourceMgr sourceMgr;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> srcOrErr =
      llvm::MemoryBuffer::getMemBufferCopy(mlir);
  sourceMgr.AddNewSourceBuffer(std::move(*srcOrErr), llvm::SMLoc());
  module = mlir::parseSourceFile(sourceMgr, builder.getContext());
  if (mlir::failed(mlir::verify(*module))) {
    module->dump();
    return nullptr;
  }
  return module.get();
}

//============================================================ LLVM IR Lowering

unique_ptr<llvm::Module> Generator::emitLLVM(int optLevel) {
  // The lowering "pass manager"
  mlir::PassManager pm(&context);
  if (optLevel > 0) {
    pm.addPass(mlir::createInlinerPass());
    pm.addPass(mlir::createSymbolDCEPass());
    mlir::OpPassManager &optPM = pm.nest<mlir::FuncOp>();
    optPM.addPass(mlir::createCanonicalizerPass());
    optPM.addPass(mlir::createCSEPass());
  }
  pm.addPass(mlir::createLowerToLLVMPass());

  // First lower to LLVM dialect
  if (mlir::failed(pm.run(module.get()))) {
    module->dump();
    return nullptr;
  }

  // Then lower to LLVM IR
  auto llvmModule = mlir::translateModuleToLLVMIR(module.get());
  assert(llvmModule);
  return llvmModule;
}
