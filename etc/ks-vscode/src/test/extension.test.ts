import * as assert from 'assert'
import { formatKnossosIR } from '../knossos_ir_formatter'

suite('Knossos IR Formatter Extension Tests', function() {
  // Defines a Mocha unit test
  test('Correctly handles comments and strings', function() {
    const formattedDocument = formatKnossosIR(`
; comment
(def "this is a test" (Vec m (Vec n Float)) ((m : Float) (n : Float)) ; comment with whitespace
((build " " (lam (mi : Integer) ; (()) comment with parentheses
(build " " (lam (ni : Integer) ")))"))))))`);
    const expectedFormattedDocument = `
; comment
(def "this is a test" 
  (Vec m (Vec n Float)) 
  (
    (m : Float) 
    (n : Float)) ; comment with whitespace
  ((build " " (lam (mi : Integer) ; (()) comment with parentheses
    (build " " (lam (ni : Integer) ")))"))))))`;
    assert.equal(formattedDocument, expectedFormattedDocument);
  });

  test('Do not lose indent after newline', function() {
    const formattedDocument = formatKnossosIR(`
(build n (lam (i : Integer)
  (to_float i)))`);
    const expectedFormattedDocument = `
(build n (lam (i : Integer)
  (to_float i)))`;
    assert.equal(formattedDocument, expectedFormattedDocument);
  });

  test('Handle let correctly', function() {
    const formattedDocument = formatKnossosIR(`
(let ((a 1.0) (b 2.0) (c (add a b))) (pr c))`);
    const expectedFormattedDocument = `
(let 
  (
    (a 1.0) 
    (b 2.0) 
    (c (add a b))) 
  (pr c))`;
    assert.equal(formattedDocument, expectedFormattedDocument);
  });

  test('Handle assert correctly', function() {
    const formattedDocument = formatKnossosIR(`
(assert (gt (add (sub x 1.0) 1.0) 0.0) (div 1.0 x))`);
    const expectedFormattedDocument = `
(assert (gt (add (sub x 1.0) 1.0) 0.0) 
  (div 1.0 x))`;
  assert.equal(formattedDocument, expectedFormattedDocument);
  });

  test('Handle pr correctly', function() {
    const formattedDocument = formatKnossosIR(`
(pr (add x y) (sub x y))`);
    const expectedFormattedDocument = `
(pr 
  (add x y) 
  (sub x y))`;
    assert.equal(formattedDocument, expectedFormattedDocument);
  });

  test('Handle trailing whitespace', function() {
    const formattedDocument = formatKnossosIR(`
(add x y) `);
    const expectedFormattedDocument = `
(add x y)`
      assert.equal(formattedDocument, expectedFormattedDocument);
  });
});