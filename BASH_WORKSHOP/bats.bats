#!/usr/bin/env bats


load "./test/test_helper/bats-assert/load"    # obligatory
load "./test/test_helper/bats-support/load"   # obligatory

@test "trying to use run command" {
   run sh run_test.sh
   [ "$status" -eq 42 ]
   [ "$output" = "test run!" ]
   run cat Hello.txt
   [ "$output" = "Hello, Bats!" ]
   my_true=false
   # assert [ "$my_true" ]
   # refute [ -e 'not_exists' ]
   assert [ -e 'not_exists' ]
   touch 'not_exists'
   assert [ -e 'not_exists' ]
   refute [ $mama  ]
}


@test "addition using bc" {
  result="$(echo 2+2 | bc)"
  [ "$result" -eq 4 ]
}

@test "addition using dc" {
  result="$(echo 2 2+p | dc)"
  [ "$result" -eq 4 ]
}

@test "my first TAP test" {
  result="`./my_test1.sh`"
  [ "$result" != "Tanya" ]
}

@test "my second TAP test" {
  result1="`./my_test1.sh`"
  result2="`./my_test2.sh`"
  [ "$result1" = "$result2" ]
}
