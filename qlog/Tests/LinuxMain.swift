import XCTest

import qlogTests

var tests = [XCTestCaseEntry]()
tests += qlogTests.allTests()
XCTMain(tests)