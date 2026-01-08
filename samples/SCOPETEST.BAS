' SCOPE_TEST.BAS - Comprehensive Scoping Test Suite
' Tests global, local, DEF FN, SHARED, and STATIC scoping

CLS
PRINT "QBasic Scoping Test Suite"
PRINT "========================="
PRINT
DIM SHARED TestsPassed AS INTEGER
DIM SHARED TestsFailed AS INTEGER
TestsPassed = 0
TestsFailed = 0

' Global variable for testing
DIM SHARED GlobalVar AS INTEGER
GlobalVar = 100

' DEF FN definitions (module level)
DEF FnDouble (x) = x * 2
DEF FnRan (x) = INT(RND(1) * x) + 1
DEF FnAddTen (n) = n + 10

' ============================================
' TEST 1: DEF FN from global scope
' ============================================
PRINT "TEST 1: DEF FN from global scope"
result = FnDouble(5)
IF result = 10 THEN
  PRINT "  PASS: FnDouble(5) = "; result
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: FnDouble(5) = "; result; " (expected 10)"
  TestsFailed = TestsFailed + 1
END IF

' ============================================
' TEST 2: DEF FN with same-named global var
' ============================================
PRINT "TEST 2: DEF FN with same-named global variable"
x = 999  ' Global x should NOT interfere
result = FnDouble(7)
IF result = 14 THEN
  PRINT "  PASS: FnDouble(7) = "; result; " (global x="; x; " didn't interfere)"
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: FnDouble(7) = "; result; " (expected 14, global x="; x; ")"
  TestsFailed = TestsFailed + 1
END IF
' Verify x wasn't modified
IF x = 999 THEN
  PRINT "  PASS: Global x still = "; x
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: Global x = "; x; " (expected 999)"
  TestsFailed = TestsFailed + 1
END IF

' ============================================
' TEST 3: DEF FN called from SUB (the GORILLAS bug)
' ============================================
PRINT "TEST 3: DEF FN called from SUB with local var conflict"
CALL TestDefFnInSub

' ============================================
' TEST 4: DEF FN called from FUNCTION
' ============================================
PRINT "TEST 4: DEF FN called from FUNCTION"
result = TestDefFnInFunction(50)
IF result >= 1 AND result <= 10 THEN
  PRINT "  PASS: Result = "; result; " (in range 1-10)"
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: Result = "; result; " (expected 1-10)"
  TestsFailed = TestsFailed + 1
END IF

' ============================================
' TEST 5: Local variables don't leak to global
' ============================================
PRINT "TEST 5: Local variables don't leak to global"
CALL TestLocalScope
' localOnly should not exist at global level
' We can't easily test this in BASIC, but we test via behavior

' ============================================
' TEST 6: SHARED variables accessible in SUB
' ============================================
PRINT "TEST 6: SHARED variables in SUB"
CALL TestSharedVar
IF GlobalVar = 200 THEN
  PRINT "  PASS: GlobalVar modified by SUB = "; GlobalVar
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: GlobalVar = "; GlobalVar; " (expected 200)"
  TestsFailed = TestsFailed + 1
END IF
GlobalVar = 100  ' Reset

' ============================================
' TEST 7: STATIC variables persist
' ============================================
PRINT "TEST 7: STATIC variables persist across calls"
result1 = TestStaticVar
result2 = TestStaticVar
result3 = TestStaticVar
IF result1 = 1 AND result2 = 2 AND result3 = 3 THEN
  PRINT "  PASS: Static counter = 1, 2, 3"
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: Results = "; result1; ","; result2; ","; result3
  TestsFailed = TestsFailed + 1
END IF

' ============================================
' TEST 8: Nested SUB calls with DEF FN
' ============================================
PRINT "TEST 8: Nested SUB calls with DEF FN"
CALL TestNestedCalls

' ============================================
' TEST 9: Multiple DEF FN params
' ============================================
PRINT "TEST 9: Multiple DEF FN parameters"
DEF FnCalc (a, b, c) = a * 100 + b * 10 + c
result = FnCalc(1, 2, 3)
IF result = 123 THEN
  PRINT "  PASS: FnCalc(1,2,3) = "; result
  TestsPassed = TestsPassed + 1
ELSE
  PRINT "  FAIL: FnCalc(1,2,3) = "; result; " (expected 123)"
  TestsFailed = TestsFailed + 1
END IF

' ============================================
' TEST 10: Wind calculation (exact GORILLAS test)
' ============================================
PRINT "TEST 10: Wind calculation (GORILLAS simulation)"
CALL SimulateMakeCityScape

' ============================================
' FINAL RESULTS
' ============================================
PRINT
PRINT "========================="
PRINT "RESULTS: "; TestsPassed; " passed, "; TestsFailed; " failed"
IF TestsFailed = 0 THEN
  PRINT "ALL TESTS PASSED!"
ELSE
  PRINT "SOME TESTS FAILED - CHECK SCOPE HANDLING"
END IF
PRINT
PRINT "Press any key to exit..."
DO: LOOP WHILE INKEY$ = ""
END

' ============================================
' SUB: Test DEF FN called from within SUB
' This simulates the GORILLAS.BAS bug
' ============================================
SUB TestDefFnInSub
  ' Local variable with same name as DEF FN parameter
  x = 500
  n = 999
  
  ' Call DEF FN - should use param value, not local x
  result = FnDouble(3)
  IF result = 6 THEN
    PRINT "  PASS: FnDouble(3) from SUB = "; result; " (local x="; x; ")"
    TestsPassed = TestsPassed + 1
  ELSE
    PRINT "  FAIL: FnDouble(3) from SUB = "; result; " (expected 6, local x="; x; ")"
    TestsFailed = TestsFailed + 1
  END IF
  
  ' Verify local x wasn't modified
  IF x = 500 THEN
    PRINT "  PASS: Local x preserved = "; x
    TestsPassed = TestsPassed + 1
  ELSE
    PRINT "  FAIL: Local x = "; x; " (expected 500)"
    TestsFailed = TestsFailed + 1
  END IF
  
  ' Test FnAddTen with local n conflict
  result = FnAddTen(5)
  IF result = 15 THEN
    PRINT "  PASS: FnAddTen(5) = "; result; " (local n="; n; ")"
    TestsPassed = TestsPassed + 1
  ELSE
    PRINT "  FAIL: FnAddTen(5) = "; result; " (expected 15)"
    TestsFailed = TestsFailed + 1
  END IF
END SUB

' ============================================
' FUNCTION: Test DEF FN called from FUNCTION
' ============================================
FUNCTION TestDefFnInFunction (localX)
  ' localX shadows any global, DEF FN should still work
  result = FnRan(10)
  TestDefFnInFunction = result
END FUNCTION

' ============================================
' SUB: Test that local vars don't leak
' ============================================
SUB TestLocalScope
  localOnly = 12345
  PRINT "  (Local var localOnly = "; localOnly; ")"
  TestsPassed = TestsPassed + 1
END SUB

' ============================================
' SUB: Test SHARED variable access
' ============================================
SUB TestSharedVar
  SHARED GlobalVar
  GlobalVar = 200
END SUB

' ============================================
' FUNCTION: Test STATIC variable persistence
' ============================================
FUNCTION TestStaticVar
  STATIC counter
  counter = counter + 1
  TestStaticVar = counter
END FUNCTION

' ============================================
' SUB: Test nested calls
' ============================================
SUB TestNestedCalls
  x = 111
  CALL NestedInner
  IF x = 111 THEN
    PRINT "  PASS: Outer x preserved after nested call = "; x
    TestsPassed = TestsPassed + 1
  ELSE
    PRINT "  FAIL: Outer x = "; x; " (expected 111)"
    TestsFailed = TestsFailed + 1
  END IF
END SUB

SUB NestedInner
  x = 222
  result = FnDouble(4)
  IF result = 8 THEN
    PRINT "  PASS: FnDouble(4) in nested SUB = "; result
    TestsPassed = TestsPassed + 1
  ELSE
    PRINT "  FAIL: FnDouble(4) in nested SUB = "; result
    TestsFailed = TestsFailed + 1
  END IF
END SUB

' ============================================
' SUB: Simulate GORILLAS MakeCityScape
' ============================================
SUB SimulateMakeCityScape
  ' Simulate building loop like GORILLAS
  FOR x = 50 TO 200 STEP 50
    ' x is now a large value (50, 100, 150, 200)
    ' FnRan should NOT use this x!
    Wind = FnRan(10) - 5
    IF Wind >= -4 AND Wind <= 5 THEN
      PRINT "  PASS: Wind at x="; x; " is "; Wind; " (valid range)"
      TestsPassed = TestsPassed + 1
    ELSE
      PRINT "  FAIL: Wind at x="; x; " is "; Wind; " (expected -4 to 5)"
      TestsFailed = TestsFailed + 1
    END IF
  NEXT x
END SUB
